module Darcs.Test.Misc.CommandLine
  (
    commandLineTestSuite
  ) where

import Darcs.Prelude

import Test.HUnit ( assertEqual, assertFailure )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework ( Test, testGroup )

import Darcs.Util.CommandLine ( parseCmd )

formatTable :: [(Char, String)]
formatTable = [('s',"<insert subject here>"),
               ('a',"<insert author here>"),
               ('d',"date")
              ]

testParser :: (String, ([String], Bool)) -> Test
testParser (s, ok) =
  testCase ("Parse: " ++ show s) $
    case parseCmd formatTable s of
      Left e -> assertFailure $ "Parser failed with: " ++ show e
      Right res -> assertEqual ("Parsing: " ++ show s) ok res

testCases :: [(String, ([String], Bool))]
testCases = [("a b",(["a","b"], False)),
             ("a b %<",(["a","b"], True)),
             ("a b %< ",(["a","b"], True)),
             ("\"arg0 contains spaces \\\"quotes\\\"\" b",
              (["arg0 contains spaces \"quotes\"","b"],False)),
             ("a %s %<",(["a","<insert subject here>"], True)),
             ("\"%d\"", (["date"], False)),
             ("\"d %d\"", (["d date"], False)),
             ("\\\a", (["\\\a"], False)),
             ("\"\\\a\"", (["\a"], False)),
             ("\"/foo:%d\"", (["/foo:date"], False))
            ]

commandLineTestSuite :: Test
commandLineTestSuite =
  testGroup "Darcs.Util.CommandLine" $ map testParser testCases
