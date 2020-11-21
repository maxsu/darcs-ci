--  Copyright (C) 2002-2003 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# LANGUAGE OverloadedStrings #-}
module Darcs.UI.Commands.Init ( initialize, initializeCmd ) where

import Darcs.Prelude

import Control.Monad ( when )

import Darcs.Repository ( createRepository, withUMaskFlag )
import Darcs.UI.Commands
    ( DarcsCommand(..)
    , amNotInRepository
    , nodefaults
    , putFinished
    , withStdOpts
    , putWarning
    )
import Darcs.UI.Completion ( noArgs )
import Darcs.UI.Flags ( DarcsFlag, withNewRepo )
import Darcs.UI.Options ( defaultFlags, ocheck, odesc, (?), (^) )
import Darcs.UI.Options.All ()
import qualified Darcs.UI.Options.All as O
import Darcs.Util.Path ( AbsolutePath )
import Darcs.Util.Printer
    ( Doc
    , formatWords
    , quoted
    , renderString
    , text
    , vsep
    , ($$)
    , (<+>)
    )

initializeDescription :: String
initializeDescription = "Create an empty repository."

initializeHelp :: Doc
initializeHelp = vsep $ map formatWords
  [ [ "The `darcs initialize` command creates an empty repository in the"
    , "current directory. This repository lives in a new `_darcs` directory,"
    , "which stores version control metadata and settings."
    ]
  , [ "Existing files and subdirectories are not touched. You can"
    , "record them with `darcs record --look-for-adds`."
    ]
  , [ "Initialize is commonly abbreviated to `init`."
    ]
  , [ "Darcs currently supports three kinds of patch semantics. These are called"
    , "`darcs-1`, `darcs-2`, and `darcs-3`. They are mutually incompatible, that"
    , "is, you cannot exchange patches between repos with different semantics."
    ]
  , [ "By default, patches of the new repository are in the darcs-2 semantics."
    , "However it is possible to create a repository in darcs-1 semantics with"
    , "the flag `--darcs-1`, althought this is not recommended except for sharing"
    , "patches with a project that uses patches in the darcs-1 semantics."
    ]
  ] ++ [darcs3Warning]

darcs3Warning :: Doc
darcs3Warning = formatWords
  [ "The `darcs-3` semantics is EXPERIMENTAL and new in version 2.16. It is"
  , "included only as a technology preview and we do NOT recommend to use it"
  , "for any serious work. The on-disk format is not yet finalized and we"
  , "cannot and will not promise that later releases will work with darcs-3"
  , "repos created with any darcs version before 3.0."
  ]

initialize :: DarcsCommand
initialize = DarcsCommand
    { commandProgramName = "darcs"
    , commandName = "initialize"
    , commandHelp = initializeHelp
    , commandDescription = initializeDescription
    , commandExtraArgs = -1
    , commandExtraArgHelp = ["[<DIRECTORY>]"]
    , commandPrereq = \_ -> return $ Right ()
    , commandCommand = initializeCmd
    , commandCompleteArgs = noArgs
    , commandArgdefaults = nodefaults
    , commandAdvancedOptions = odesc initAdvancedOpts
    , commandBasicOptions = odesc initBasicOpts
    , commandDefaults = defaultFlags initOpts
    , commandCheckOptions = ocheck initOpts
    }
  where
    initBasicOpts = O.patchFormat ^ O.withWorkingDir ^ O.newRepo
    initAdvancedOpts = O.patchIndexNo ^ O.hashed ^ O.umask
    initOpts = initBasicOpts `withStdOpts` initAdvancedOpts

initializeCmd :: (AbsolutePath, AbsolutePath) -> [DarcsFlag] -> [String] -> IO ()
initializeCmd _ opts [outname]
  | Nothing <- O.newRepo ? opts = doInit (withNewRepo outname opts)
initializeCmd _ opts [] = doInit opts
initializeCmd _ _ _ =
  fail "You must provide 'initialize' with either zero or one argument."

doInit :: [DarcsFlag] -> IO ()
doInit opts =
  withUMaskFlag (O.umask ? opts) $ do
    location <- amNotInRepository opts
    case location of
      Left msg -> fail $ renderString $
        "Unable to" <+> quoted ("darcs " ++ commandName initialize)
                    <+> "here:" $$ text msg
      Right () -> do
        when (O.patchFormat ? opts == O.PatchFormat3) $
          putWarning opts $
            "============================= WARNING =============================" $$
            darcs3Warning $$
            "==================================================================="
        _ <- createRepository
          (O.patchFormat ? opts)
          (O.withWorkingDir ? opts)
          (O.patchIndexNo ? opts)
          (O.useCache ? opts)
        putFinished opts $ "initializing repository"
