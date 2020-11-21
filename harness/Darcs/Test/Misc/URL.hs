{-# LANGUAGE CPP #-}
module Darcs.Test.Misc.URL ( testSuite ) where

import Darcs.Prelude

import Test.HUnit ( assertEqual )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework ( Test, testGroup )

import Darcs.Util.URL ( isValidLocalPath, isHttpUrl, isSshUrl )

cases :: [(String, Bool, Bool, Bool)]
cases =
  -- input                                local   http    ssh
  [ ("http://host.domain/path/to/repo",   False,  True,   False)
  , ("https://host.domain/path/to/repo",  False,  True,   False)
  , ("http://host.domain/path:to/repo",   False,  True,   False)
  , ("https://host.domain/path:to/repo",  False,  True,   False)
  , ("http://host.domain/",               False,  True,   False)
  , ("https://host.domain/",              False,  True,   False)
#ifdef WIN32
  -- local paths valid only on Windows
  , ("a:/",                               True,   False,  False)
  , ("b:/absolute/path",                  True,   False,  False)
  , ("c:relative/path",                   True,   False,  False)
  , ("d:.",                               True,   False,  False)
  , ("e:..",                              True,   False,  False)
  , ("f:./relative/path",                 True,   False,  False)
  , ("g:../relative/path",                True,   False,  False)
  , ("A:\\",                              True,   False,  False)
  , ("B:\\absolute\\path",                True,   False,  False)
  , ("C:relative\\path",                  True,   False,  False)
  , ("D:.",                               True,   False,  False)
  , ("E:..",                              True,   False,  False)
  , ("F:.\\relative\\path",               True,   False,  False)
  , ("G:..\\relative\\path",              True,   False,  False)
#else
  -- local paths valid only on Posix
  , ("/absolute/path:with/colons",        True,   False,  False)
  , ("relative/path:with/colons",         True,   False,  False)
  , ("./relative/path:with/colons",       True,   False,  False)
  , ("../relative/path:with/colons",      True,   False,  False)
#endif
  , ("/",                                 True,   False,  False)
  , ("/absolute/path",                    True,   False,  False)
  , ("/absolute/path",                    True,   False,  False)
  , ("relative/path",                     True,   False,  False)
  , (".",                                 True,   False,  False)
  , ("..",                                True,   False,  False)
  , ("./relative/path",                   True,   False,  False)
  , ("../relative/path",                  True,   False,  False)
  -- ssh "URL"s
  , ("user@host:/path/to/repo",           False,  False,  True)
  , ("host:/path/to/repo",                False,  False,  True)
  , ("user@host:/path:with/colons/",      False,  False,  True)
  , ("host:/path:with/colons/",           False,  False,  True)
  ]

test :: (String, Bool, Bool, Bool) -> Test
test (input, local, http, ssh) = testCase input $ do
  assertEqual "isValidLocalPath" (isValidLocalPath input) local
  assertEqual "isHttpUrl" (isHttpUrl input) http
  assertEqual "isSshUrl" (isSshUrl input) ssh

testSuite :: Test
testSuite = testGroup "Darcs.Util.URL" $ map test cases
