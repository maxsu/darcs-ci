{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Darcs.UI.External
    ( sendEmail
    , generateEmail
    , sendEmailDoc
    , signString
    , verifyPS
    , execDocPipe
    , pipeDoc
    , pipeDocSSH
    , viewDoc
    , viewDocWith
    , checkDefaultSendmail
    , diffProgram
    , darcsProgram
    , editText
    , editFile
    --  * Locales
    , setDarcsEncodings
    , getSystemEncoding
    , isUTF8Locale
    ) where

import Darcs.Prelude

import Data.Maybe ( isJust )
#ifndef WIN32
import Data.Maybe ( isNothing, maybeToList )
#endif
import Control.Monad ( unless, when, filterM, void )
#ifndef WIN32
import Control.Monad ( liftM2 )
#endif
import Control.Concurrent.MVar ( MVar )
import System.Exit ( ExitCode(..) )
import System.Environment
    ( getEnv
    , getExecutablePath
    )
import System.Directory ( doesFileExist, findExecutable )
import System.IO
    ( Handle
    , hClose
    , hIsTerminalDevice
    , hPutStr
    , stderr
    , stdout
    )
#ifndef WIN32
import System.FilePath.Posix ( (</>) )
#endif
import System.Process ( createProcess, proc, CreateProcess(..), runInteractiveProcess, waitForProcess, StdStream(..) )
import System.Process.Internals ( ProcessHandle )

#ifndef WIN32
import GHC.IO.Encoding
    ( getFileSystemEncoding
    , setForeignEncoding
    , setLocaleEncoding )
#endif

import Foreign.C.String ( CString, peekCString )

import Control.Concurrent ( forkIO, newEmptyMVar, putMVar, takeMVar )
import Control.Exception ( IOException, finally, try )
import System.IO.Error ( ioeGetErrorType )
import GHC.IO.Exception ( IOErrorType(ResourceVanished) )
import Data.Char ( toLower )
import Text.Regex
#ifdef WIN32
import Foreign.C ( withCString )
import Foreign.Ptr ( nullPtr )
import Darcs.Util.Lock ( canonFilename, writeDocBinFile )
#endif

import Darcs.UI.Options.All ( Sign(..), Verify(..), Compression(..) )
import Darcs.Util.Path
    ( AbsolutePath
    , toFilePath
    , FilePathLike
    )
import Darcs.Util.Progress ( withoutProgress, debugMessage )

import Darcs.Util.ByteString (linesPS, unlinesPS)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Darcs.Util.Lock
    ( withTemp
    , withNamedTemp
    , withOpenTemp
    )
import Darcs.Util.Ssh ( getSSH, SSHCmd(..) )
import Darcs.Util.CommandLine ( parseCmd, addUrlencoded )
#ifndef WIN32
import Darcs.Util.English ( orClauses )
#endif
import Darcs.Util.Exception ( catchall )
import Darcs.Util.Exec ( execInteractive, exec, Redirect(..), withoutNonBlock )
import Darcs.Util.URL ( SshFilePath, sshUhost )
import Darcs.Util.Printer
    ( Doc
    , Printers
    , hPutDoc
    , hPutDocCompr
    , hPutDocLn
    , hPutDocLnWith
    , hPutDocWith
    , packedString
    , renderPS
    , renderString
    , simplePrinters
    , text
    )
import qualified Darcs.Util.Ratified as Ratified
import Darcs.UI.Email ( formatHeader )

#ifndef WIN32
sendmailPath :: IO String
sendmailPath = do
  let searchPath = [ "/usr/sbin", "/sbin", "/usr/lib" ]
  l <- filterM doesFileExist $ liftM2 (</>)
                searchPath
                [ "sendmail" ]
  ex <- findExecutable "sendmail"
  when (isNothing ex && null l) $
    fail $ "Cannot find the 'sendmail' program in " ++
      orClauses ("your PATH" : searchPath) ++ "."
  return $ head $ maybeToList ex ++ l
#endif

diffProgram :: IO String
diffProgram = do
  l <- filterM (fmap isJust . findExecutable) [ "gdiff", "gnudiff", "diff" ]
  when (null l) $ fail "Cannot find the \"diff\" program."
  return $ head l

-- |Get the name of the darcs executable (as supplied by @getExecutablePath@)
darcsProgram :: IO String
darcsProgram = getExecutablePath

pipeDoc :: String -> [String] -> Doc -> IO ExitCode
pipeDoc = pipeDocInternal (PipeToOther simplePrinters)

data WhereToPipe = PipeToSsh Compression -- ^ if pipe to ssh, can choose to compress or not
                 | PipeToOther Printers  -- ^ otherwise, can specify printers

pipeDocInternal :: WhereToPipe -> String -> [String] -> Doc -> IO ExitCode
pipeDocInternal whereToPipe c args inp = withoutNonBlock $ withoutProgress $
    do debugMessage $ "Exec: " ++ unwords (map show (c:args))
       (Just i,_,_,pid) <- createProcess (proc c args){ std_in = CreatePipe
                                                      , delegate_ctlc = True}
       debugMessage "Start transferring data"
       case whereToPipe of
          PipeToSsh GzipCompression -> hPutDocCompr i inp
          PipeToSsh NoCompression   -> hPutDoc i inp
          PipeToOther printers      -> hPutDocWith printers i inp
       hClose i
       rval <- waitForProcess pid
       debugMessage "Finished transferring data"
       when (rval == ExitFailure 127) $
            putStrLn $ "Command not found:\n   "++ show (c:args)
       return rval

pipeDocSSH :: Compression -> SshFilePath -> [String] -> Doc -> IO ExitCode
pipeDocSSH compress remoteAddr args input = do
    (ssh, ssh_args) <- getSSH SSH
    pipeDocInternal (PipeToSsh compress) ssh (ssh_args ++ ("--":sshUhost remoteAddr:args)) input

sendEmail :: String -> String -> String -> String -> Maybe String -> String -> IO ()
sendEmail f t s cc scmd body =
  sendEmailDoc f t s cc scmd Nothing (text body)


generateEmail
    :: Handle  -- ^ handle to write email to
    -> String  -- ^ From
    -> String  -- ^ To
    -> String  -- ^ Subject
    -> String  -- ^ CC
    -> Doc     -- ^ body
    -> IO ()
generateEmail h f t s cc body = do
     putHeader "To" t
     putHeader "From" f
     putHeader "Subject" s
     unless (null cc) $ putHeader "Cc" cc
     putHeader "X-Mail-Originator" "Darcs Version Control System"
     hPutDocLn h body
  where putHeader field value
            = B.hPut h (B.append (formatHeader field value) newline)
        newline = B.singleton 10

checkDefaultSendmail :: IO ()
#ifndef WIN32
checkDefaultSendmail = void sendmailPath
#else
-- FIXME find a way to detect if sending via MAPI would work
checkDefaultSendmail = return ()
{-
  TODO do we want a warning message like this one here?
  hPutStr stderr
    "Warning: Using MAPI for sending mail. This may hang indefinitely " ++
    "if not properly configured.\n"
-}
#endif

-- | Send an email, optionally containing a patch bundle
--   (more precisely, its description and the bundle itself)
sendEmailDoc
  :: String           -- ^ from
  -> String           -- ^ to
  -> String           -- ^ subject
  -> String           -- ^ cc
  -> Maybe String     -- ^ send command
  -> Maybe (Doc, Doc) -- ^ (content,bundle)
  -> Doc              -- ^ body
  -> IO ()
sendEmailDoc _ "" _ "" _ _ _ = return ()
sendEmailDoc f "" s cc scmd mbundle body =
  sendEmailDoc f cc s "" scmd mbundle body
#ifdef WIN32
sendEmailDoc f t s cc Nothing _mbundle body = do
  r <- withCString t $ \tp ->
           withCString f $ \fp ->
            withCString cc $ \ccp ->
             withCString s $ \sp ->
              withOpenTemp $ \(h,fn) -> do
               hPutDoc h body
               hClose h
               writeDocBinFile "mailed_patch" body
               cfn <- canonFilename fn
               withCString cfn $ \pcfn ->
                c_send_email fp tp ccp sp nullPtr pcfn
  when (r /= 0) $ do
    fail $ "Failed to send mail via MAPI to: " ++ recipients t cc
#endif
sendEmailDoc f t s cc scmd mbundle body =
  withOpenTemp $ \(h,fn) -> do
    generateEmail h f t s cc body
    hClose h
    withOpenTemp $ \(hat,at) -> do
      ftable' <- case mbundle of
                 Just (content,bundle) -> do
                     hPutDocLn hat bundle
                     return [ ('b', renderString content) , ('a', at) ]
                 Nothing ->
                     return [ ('b', renderString body) ]
      hClose hat
      let ftable = [ ('t',addressOnly t),('c',cc),('f',f),('s',s) ] ++ ftable'
      r <- execSendmail ftable scmd fn
      when (r /= ExitSuccess) $
        fail ("Failed to send mail to: " ++ recipients t cc)
  where addressOnly a =
          case dropWhile (/= '<') a of
          ('<':a2) -> takeWhile (/= '>') a2
          _        -> a

recipients :: String -> String -> String
recipients to "" = to
recipients to cc = to ++ " and cc'ed " ++ cc

execSendmail :: [(Char,String)] -> Maybe String -> String -> IO ExitCode
#ifdef WIN32
execSendmail _ Nothing _ = error "impossible"
#else
execSendmail _ Nothing fn = do
  scmd <- sendmailPath
  exec scmd ["-i", "-t"] (File fn, Null, AsIs)
#endif
execSendmail ftable (Just scmd) fn =
  case parseCmd (addUrlencoded ftable) scmd of
    Right (arg0:opts, wantstdin) ->
      let stdin = if wantstdin then File fn else Null
      in do
        debugMessage $ unwords $ "execSendmail:" : map show (arg0 : opts)
        exec arg0 opts (stdin, Null, AsIs)
    Right ([], _) ->
      fail $ "Invalid sendmail-command "++show scmd
    Left e ->
      fail $ "Invalid sendmail-command "++show scmd++"\n"++show e

#ifdef WIN32
foreign import ccall "win32/send_email.h send_email" c_send_email
             :: CString -> {- sender -}
                CString -> {- recipient -}
                CString -> {- cc -}
                CString -> {- subject -}
                CString -> {- body -}
                CString -> {- path -}
                IO Int
#endif

execPSPipe :: String -> [String] -> B.ByteString -> IO B.ByteString
execPSPipe c args ps = fmap renderPS
                     $ execDocPipe c args
                     $ packedString ps

execAndGetOutput :: FilePath -> [String] -> Doc
                 -> IO (ProcessHandle, MVar (), B.ByteString)
execAndGetOutput c args instr = do
       (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
       _ <- forkIO $ hPutDoc i instr >> hClose i
       mvare <- newEmptyMVar
       _ <- forkIO ((Ratified.hGetContents e >>= -- ratify: immediately consumed
                hPutStr stderr)
               `finally` putMVar mvare ())
       out <- B.hGetContents o
       return (pid, mvare, out)

execDocPipe :: String -> [String] -> Doc -> IO Doc
execDocPipe c args instr = withoutProgress $ do
       (pid, mvare, out) <- execAndGetOutput c args instr
       rval <- waitForProcess pid
       takeMVar mvare
       case rval of
         ExitFailure ec ->fail $ "External program '"++c++
                          "' failed with exit code "++ show ec
         ExitSuccess -> return $ packedString out

signString :: Sign -> Doc -> IO Doc
signString NoSign d = return d
signString Sign d = signPGP [] d
signString (SignAs keyid) d = signPGP ["--local-user", keyid] d
signString (SignSSL idf) d = signSSL idf d

signPGP :: [String] -> Doc -> IO Doc
signPGP args = execDocPipe "gpg" ("--clearsign":args)

signSSL :: String -> Doc -> IO Doc
signSSL idfile t =
    withTemp $ \cert -> do
    opensslPS ["req", "-new", "-key", idfile,
               "-outform", "PEM", "-days", "365"]
                (BC.pack "\n\n\n\n\n\n\n\n\n\n\n")
                >>= opensslPS ["x509", "-req", "-extensions",
                               "v3_ca", "-signkey", idfile,
                               "-outform", "PEM", "-days", "365"]
                >>= opensslPS ["x509", "-outform", "PEM"]
                >>= B.writeFile cert
    opensslDoc ["smime", "-sign", "-signer", cert,
                "-inkey", idfile, "-noattr", "-text"] t
    where opensslDoc = execDocPipe "openssl"
          opensslPS = execPSPipe "openssl"


verifyPS :: Verify -> B.ByteString -> IO (Maybe B.ByteString)
verifyPS NoVerify ps = return $ Just ps
verifyPS (VerifyKeyring pks) ps = verifyGPG pks ps
verifyPS (VerifySSL auks) ps = verifySSL auks ps

verifyGPG :: AbsolutePath -> B.ByteString -> IO (Maybe B.ByteString)
verifyGPG goodkeys s =
    withOpenTemp $ \(th,tn) -> do
      B.hPut th s
      hClose th
      rval <- exec "gpg"  ["--batch","--no-default-keyring",
                           "--keyring",fix_path $ toFilePath goodkeys, "--verify"]
                           (File tn, Null, Null)
      case rval of
          ExitSuccess -> return $ Just gpg_fixed_s
          _ -> return Nothing
      where gpg_fixed_s = let
                not_begin_signature x =
                    x /= BC.pack "-----BEGIN PGP SIGNED MESSAGE-----"
                    &&
                    x /= BC.pack "-----BEGIN PGP SIGNED MESSAGE-----\r"
                in unlinesPS $ map fix_line $ tail $ dropWhile not_begin_signature $ linesPS s
            fix_line x | B.length x < 3 = x
                       | BC.pack "- -" `B.isPrefixOf` x = B.drop 2 x
                       | otherwise = x
#ifdef WIN32
            fix_sep c | c=='/' = '\\'   | otherwise = c
            fix_path p = map fix_sep p
#else
            fix_path p = p
#endif

verifySSL :: AbsolutePath -> B.ByteString -> IO (Maybe B.ByteString)
verifySSL goodkeys s = do
    certdata <- opensslPS ["smime", "-pk7out"] s
                >>= opensslPS ["pkcs7", "-print_certs"]
    cruddy_pk <- opensslPS ["x509", "-pubkey"] certdata
    let key_used = B.concat $ tail $
                   takeWhile (/= BC.pack"-----END PUBLIC KEY-----")
                           $ linesPS cruddy_pk
        in do allowed_keys <- linesPS `fmap` B.readFile (toFilePath goodkeys)
              if key_used `notElem` allowed_keys
                then return Nothing -- Not an allowed key!
                else withTemp $ \cert ->
                     withTemp $ \on ->
                     withOpenTemp $ \(th,tn) -> do
                     B.hPut th s
                     hClose th
                     B.writeFile cert certdata
                     rval <- exec "openssl" ["smime", "-verify", "-CAfile",
                                             cert, "-certfile", cert]
                                             (File tn, File on, Null)
                     case rval of
                       ExitSuccess -> Just `fmap` B.readFile on
                       _ -> return Nothing
    where opensslPS = execPSPipe "openssl"

viewDoc :: Doc -> IO ()
viewDoc = viewDocWith simplePrinters

viewDocWith :: Printers -> Doc -> IO ()
viewDocWith pr msg = do
  isTerminal <- hIsTerminalDevice stdout
  void $ if isTerminal && lengthGreaterThan (20 :: Int) (lines $ renderString msg)
     then do mbViewerPlusArgs <- getViewer
             case mbViewerPlusArgs of
                  Just viewerPlusArgs -> do
                    let (viewer : args) = words viewerPlusArgs
                    pipeDocToPager viewer args pr msg
                  Nothing -> return $ ExitFailure 127 -- No such command
               -- TEMPORARY passing the -K option should be removed as soon as
               -- we can use the delegate_ctrl_c feature in process
               `ortryrunning` pipeDocToPager  "less" ["-RK"] pr msg
               `ortryrunning` pipeDocToPager  "more" [] pr msg
#ifdef WIN32
               `ortryrunning` pipeDocToPager  "more.com" [] pr msg
#endif
               `ortryrunning` pipeDocToPager "" [] pr msg
     else pipeDocToPager "" [] pr msg
              where lengthGreaterThan n _ | n <= 0 = True
                    lengthGreaterThan _ [] = False
                    lengthGreaterThan n (_:xs) = lengthGreaterThan (n-1) xs

getViewer :: IO (Maybe String)
getViewer = Just `fmap` (getEnv "DARCS_PAGER" `catchall` getEnv "PAGER")
            `catchall`
            return Nothing

pipeDocToPager :: String -> [String] -> Printers -> Doc -> IO ExitCode

pipeDocToPager "" _ pr inp = do
  hPutDocLnWith pr stdout inp
  return ExitSuccess

pipeDocToPager c args pr inp = pipeDocInternal (PipeToOther pr) c args inp

-- | Given two shell commands as arguments, execute the former.  The
-- latter is then executed if the former failed because the executable
-- wasn't found (code 127), wasn't executable (code 126) or some other
-- exception occurred (save from a resource vanished/broken pipe error).
-- Other failures (such as the user holding ^C)
-- do not cause the second command to be tried.
ortryrunning :: IO ExitCode
             -> IO ExitCode
             -> IO ExitCode
a `ortryrunning` b = do
  ret <- try a
  case ret of
    (Right (ExitFailure 126)) -> b -- command not executable
    (Right (ExitFailure 127)) -> b -- command not found
#ifdef WIN32
    (Right (ExitFailure 9009)) -> b -- command not found by cmd.exe on Windows
#endif
    (Right x) -> return x          -- legitimate success/failure
    (Left (e :: IOException)) -> case ioeGetErrorType e of
                                   -- case where pager is quit before darcs has fed it entirely:
                                   ResourceVanished -> return ExitSuccess
                                   -- other exception:
                                   _                -> b


editText :: String -> B.ByteString -> IO B.ByteString
editText desc txt = withNamedTemp desc $ \f -> do
  B.writeFile f txt
  _ <- runEditor f
  B.readFile f

-- | @editFile f@ lets the user edit a file which could but does not need to
-- already exist. This function returns the exit code from the text editor and a
-- flag indicating if the user made any changes.
editFile :: FilePathLike p
         => p
         -> IO (ExitCode, Bool)
editFile ff = do
    old_content <- file_content
    ec <- runEditor f
    new_content <- file_content
    return (ec, new_content /= old_content)
  where
    f = toFilePath ff
    file_content = do
      exists <- doesFileExist f
      if exists then do content <- B.readFile f
                        return $ Just content
                else return Nothing


runEditor :: FilePath
          -> IO ExitCode
runEditor f = do
    ed <- getEditor
    let mf = Just f
    execInteractive ed mf
         `ortryrunning` execInteractive "vi" mf
         `ortryrunning` execInteractive "emacs" mf
         `ortryrunning` execInteractive "emacs -nw" mf
#ifdef WIN32
         `ortryrunning` execInteractive "edit" mf
#endif


getEditor :: IO String
getEditor = getEnv "DARCS_EDITOR" `catchall`
             getEnv "VISUAL" `catchall`
             getEnv "EDITOR" `catchall` return "nano"

-- | On Posix systems, GHC by default uses the user's locale encoding to
-- determine how to decode/encode the raw byte sequences in the Posix API
-- to/from 'String'. It also uses certain special variants of this
-- encoding to determine how to handle encoding errors.
--
-- See "GHC.IO.Encoding" for details.
--
-- In particular, the default variant used for command line arguments and
-- environment variables is //ROUNDTRIP, which means that /any/ byte sequence
-- can be decoded and re-encoded w/o failure or loss of information. To
-- enable this, GHC uses code points that are outside the range of the regular
-- unicode set. This is what you get with 'getFileSystemEncoding'.
--
-- We need to preserve the raw bytes e.g. for file names passed in by the
-- user and also when reading file names from disk; also when re-generating
-- files from patches, and when we display them to the user.
--
-- So we want to use this encoding variant for *all* IO and for (almost) all
-- conversions between raw bytes and 'String's. The encoding used for IO from
-- and to handles is controlled by 'setLocaleEncoding' which we use here to
-- make it equal to the //ROUNDTRIP variant.
--
-- @setDarcsEncoding@ should be called before the
-- first time any darcs operation is run, and again if anything else might have
-- set those encodings to different values.
--
-- Note that it isn't thread-safe and has a global effect on your program.
--
-- On Windows, this function does (and should) not do anything.
setDarcsEncodings :: IO ()
#ifdef WIN32
setDarcsEncodings = return ()
#else
setDarcsEncodings = do
    e <- getFileSystemEncoding
    -- TODO check if we have to set this, too.
    setForeignEncoding e
    setLocaleEncoding e
#endif

-- The following functions are copied from the encoding package (BSD3
-- licence, by Henning Günther).

-- | @getSystemEncoding@ fetches the current encoding from locale
foreign import ccall "system_encoding.h get_system_encoding"
     get_system_encoding :: IO CString


getSystemEncoding :: IO String
getSystemEncoding = do
    enc <- get_system_encoding
    peekCString enc


-- | @isUTF8@ checks if an encoding is UTF-8 (or ascii, since it is a
-- subset of UTF-8).
isUTF8Locale :: String -> Bool
isUTF8Locale codeName = case normalizeEncoding codeName of
    -- ASCII
    "ascii"              -> True
    "646"                -> True
    "ansi_x3_4_1968"     -> True
    "ansi_x3.4_1986"     -> True
    "cp367"              -> True
    "csascii"            -> True
    "ibm367"             -> True
    "iso646_us"          -> True
    "iso_646.irv_1991"   -> True
    "iso_ir_6"           -> True
    "us"                 -> True
    "us_ascii"           -> True
    -- UTF-8
    "utf_8"              -> True
    "u8"                 -> True
    "utf"                -> True
    "utf8"               -> True
    "utf8_ucs2"          -> True
    "utf8_ucs4"          -> True
    -- Everything else
    _                    -> False
  where
    normalizeEncoding s = map toLower $ subRegex sep s "_"
    sep = mkRegex "[^0-9A-Za-z]+"
