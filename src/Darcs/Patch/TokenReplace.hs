module Darcs.Patch.TokenReplace
    ( tryTokReplace
    , forceTokReplace
    , annotateReplace
    , breakToTokens
    , defaultToks
    ) where

import Darcs.Prelude

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.RegChars ( regChars )

-- | @breakOutToken tokChars input@ splits the @input@ 'ByteString' into
-- @'Just' (before, token, after)@, where @token@ is the first non-empty
-- substring consisting only of 'Char's in @tokChars@, or 'Nothing' if no token
-- was found. The 'Char's in @tokChars@ should not have code points larger than
-- 255 (0xff).
breakOutToken :: String -> BC.ByteString
              -> Maybe (BC.ByteString, BC.ByteString, BC.ByteString)
breakOutToken tokChars input
  | not (B.null tok) = Just (before, tok, remaining)
  | otherwise = Nothing
  where
    isTokChar = regChars tokChars
    (before, tokAndRest) = BC.break isTokChar input
    (tok, remaining) = BC.break (not . isTokChar) tokAndRest

-- | @tryTokReplace tokChars old new input@ tries to find the token @old@ and
-- replace it with the token @new@ everywhere in the @input@, returning 'Just'
-- the modified @input@, unless the token @new@ is already in the @input@ in
-- which case 'Nothing' is returned. A token is a sequence of bytes that match
-- the class defined by @tokChars@. This function is supposed to work
-- efficiently with large @input@s i.e. whole files.
tryTokReplace :: String -> B.ByteString -> B.ByteString
              -> B.ByteString -> Maybe B.ByteString
tryTokReplace tokChars old new
  | B.null old = error "tryTokInternal called with empty old token"
  | BC.any (not . isTokChar) old = error "tryTokInternal called with old non-token"
  | BC.any (not . isTokChar) new = error "tryTokInternal called with new non-token"
  | otherwise = fmap B.concat . loop 0
    where
      isTokChar = regChars tokChars
      loop !from input =
        case BC.findIndex isTokChar (B.drop from input) of
          Nothing -> Just [input]
          Just start ->
            case BC.span isTokChar (B.drop (from + start) input) of
              (tok, rest)
                | tok == old ->
                    (B.take (from + start) input :).(new :) <$> loop 0 rest
                | tok == new -> Nothing
                | otherwise ->
                    loop (from + start + B.length tok) input

-- | @forceTokReplace tokChars old new input@ replaces all occurrences of
-- the @old@ token with the @new@ one, throughout the @input@.
forceTokReplace :: String -> B.ByteString -> B.ByteString
                -> B.ByteString -> B.ByteString
forceTokReplace tokChars old new
  | B.null old = error "tryTokInternal called with empty old token"
  | BC.any (not . isTokChar) old = error "tryTokInternal called with old non-token"
  | BC.any (not . isTokChar) new = error "tryTokInternal called with new non-token"
  | otherwise = B.concat . loop 0
    where
      isTokChar = regChars tokChars
      len = B.length old
      loop !from input =
        case B.breakSubstring old (B.drop from input) of
          (before, match)
            | B.null match -> [input] -- not found
            | B.null before || not (isTokChar (BC.last before))
            , B.length match == len || not (isTokChar (BC.index match len)) ->
                -- found and is token
                B.take (from + B.length before) input : new :
                  loop 0 (B.drop len match)
            | otherwise ->
                -- found but not a token
                loop (from + B.length before + len) input

-- | Check if a token replace operation touches the given line.
annotateReplace :: String -> B.ByteString -> B.ByteString -> B.ByteString -> Bool
annotateReplace tokChars old new input =
  case breakOutToken tokChars input of
    Just (_, tok, remaining) ->
      (tok == old || annotateReplace tokChars old new remaining)
    Nothing -> False

-- | Break a 'Bytestring' into tokens, according to 'defaultToks',
-- discarding non-tokens.
breakToTokens :: BC.ByteString -> [BC.ByteString]
breakToTokens input =
  case breakOutToken defaultToks input of
    Nothing -> []
    Just (_, tok, remaining) -> tok : breakToTokens remaining

defaultToks :: String
defaultToks = "A-Za-z_0-9"
