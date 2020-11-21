--  Copyright (C) 2002-2014 David Roundy, Petr Rockai, Owen Stephens
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

module Darcs.UI.Commands.Convert ( convert ) where

import Darcs.Prelude

import Darcs.UI.Commands (DarcsCommand(..), amInRepository, normalCommand)
import Darcs.UI.Commands.Convert.Darcs2 (convertDarcs2)
import Darcs.UI.Commands.Convert.Import (convertImport)
import Darcs.UI.Commands.Convert.Export (convertExport)
import Darcs.Util.Printer ( text, ($+$) )

convertDescription :: String
convertDescription = "Convert repositories between various formats."

convert :: DarcsCommand
convert = SuperCommand
    { commandProgramName = "darcs"
    , commandName = "convert"
    , commandHelp =
        text convertDescription $+$
        text "See description of the subcommands for details."
    , commandDescription = convertDescription
    , commandPrereq = amInRepository
    , commandSubCommands =
        [ normalCommand convertDarcs2
        , normalCommand convertExport
        , normalCommand convertImport
        ]
    }
