--  Copyright (C) 2002-2004 David Roundy
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

module Darcs.UI.Commands.Show ( showCommand ) where

import Darcs.Prelude

import Darcs.UI.Commands ( DarcsCommand(..)
                         , normalCommand
                         , amInRepository
                         )
import Darcs.UI.Commands.ShowAuthors ( showAuthors )
import Darcs.UI.Commands.ShowContents ( showContents )
import Darcs.UI.Commands.ShowDependencies ( showDeps )
import Darcs.UI.Commands.ShowFiles ( showFiles )
import Darcs.UI.Commands.ShowTags ( showTags )
import Darcs.UI.Commands.ShowRepo ( showRepo )
import Darcs.UI.Commands.ShowIndex ( showIndex, showPristine )
import Darcs.UI.Commands.ShowPatchIndex ( showPatchIndex )
import Darcs.Util.Printer ( Doc, formatWords )

showDescription :: String
showDescription = "Show information about the given repository."

showHelp :: Doc
showHelp = formatWords
  [ "Display various information about a repository. See description of the"
  , "subcommands for details."
  ]

showCommand :: DarcsCommand
showCommand = SuperCommand
    { commandProgramName = "darcs"
    , commandName = "show"
    , commandHelp = showHelp
    , commandDescription = showDescription
    , commandPrereq = amInRepository
    , commandSubCommands =
      [ normalCommand showContents
      , normalCommand showDeps
      , normalCommand showFiles
      , normalCommand showIndex
      , normalCommand showPristine
      , normalCommand showRepo
      , normalCommand showAuthors
      , normalCommand showTags
      , normalCommand showPatchIndex ]
    }

