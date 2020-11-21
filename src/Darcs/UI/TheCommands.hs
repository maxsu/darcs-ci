-- Copyright (C) 2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.UI.TheCommands ( commandControlList ) where


import Darcs.UI.Commands.Add ( add )
import Darcs.UI.Commands.Amend ( amend, amendrecord )
import Darcs.UI.Commands.Annotate ( annotate )
import Darcs.UI.Commands.Apply ( apply )
import Darcs.UI.Commands.Clone ( clone, get, put )
import Darcs.UI.Commands.Convert ( convert )
import Darcs.UI.Commands.Diff ( diffCommand )
import Darcs.UI.Commands.Dist ( dist )
import Darcs.UI.Commands.GZCRCs ( gzcrcs )
import Darcs.UI.Commands.Init ( initialize )
import Darcs.UI.Commands.Log ( log, changes )
import Darcs.UI.Commands.Show ( showCommand )
import Darcs.UI.Commands.MarkConflicts ( markconflicts )
import Darcs.UI.Commands.Move ( move, mv )
import Darcs.UI.Commands.Optimize ( optimize )
import Darcs.UI.Commands.Pull ( pull, fetch )
import Darcs.UI.Commands.Push ( push )
import Darcs.UI.Commands.Rebase ( rebase )
import Darcs.UI.Commands.Record ( record, commit )
import Darcs.UI.Commands.Remove ( remove, rm, unadd )
import Darcs.UI.Commands.Repair ( repair, check )
import Darcs.UI.Commands.Replace ( replace )
import Darcs.UI.Commands.Revert ( revert )
import Darcs.UI.Commands.Rollback ( rollback )
import Darcs.UI.Commands.Send ( send )
import Darcs.UI.Commands.SetPref ( setpref )
import Darcs.UI.Commands.Tag ( tag )
import Darcs.UI.Commands.Test ( test )
import Darcs.UI.Commands.TransferMode ( transferMode )
import Darcs.UI.Commands.Unrecord ( unrecord, unpull, obliterate )
import Darcs.UI.Commands.Unrevert ( unrevert )
import Darcs.UI.Commands.WhatsNew ( whatsnew, status )
import Darcs.UI.Commands ( CommandControl, normalCommand, hiddenCommand, commandGroup )

-- | The commands that darcs knows about (e.g. whatsnew, record),
--   organized into thematic groups.  Note that hidden commands
--   are also listed here.
commandControlList :: [CommandControl]
commandControlList =
    [ commandGroup "Most used/starting out:"
    , normalCommand initialize
    , normalCommand add
    , normalCommand whatsnew, hiddenCommand status
    , normalCommand record, hiddenCommand commit
    , normalCommand clone, hiddenCommand get, hiddenCommand put
    , normalCommand pull
    , normalCommand push
    , commandGroup "Preparing patches before recording:"
    , normalCommand move, hiddenCommand mv
    , normalCommand remove, hiddenCommand unadd, hiddenCommand rm
    , normalCommand replace
    , commandGroup "Querying the repository:"
    , normalCommand log, hiddenCommand changes
    , normalCommand annotate
    , normalCommand diffCommand
    , normalCommand showCommand
    , normalCommand test
    , commandGroup "Undoing and correcting:"
    , normalCommand revert
    , normalCommand unrevert
    , normalCommand amend, hiddenCommand amendrecord
    , normalCommand rebase
    , normalCommand rollback
    , normalCommand unrecord
    , normalCommand obliterate, hiddenCommand unpull
    , commandGroup "Direct modification of the repository:"
    , normalCommand tag
    , normalCommand setpref
    , commandGroup "Exchanging patches by e-mail:"
    , normalCommand send
    , normalCommand apply
    , commandGroup "Other commands:"
    , normalCommand optimize
    , normalCommand dist
    , normalCommand markconflicts
    , normalCommand repair, hiddenCommand check
    , normalCommand convert
    , normalCommand fetch
    , hiddenCommand gzcrcs
    , hiddenCommand transferMode
    ]
