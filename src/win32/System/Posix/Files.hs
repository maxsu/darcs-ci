module System.Posix.Files
    ( isNamedPipe, isDirectory, isRegularFile, isSymbolicLink
    , getFdStatus, getFileStatus, getSymbolicLinkStatus
    , modificationTime, setFileMode, fileSize, fileMode, fileOwner
    , stdFileMode, FileStatus, fileID
    , linkCount, createLink
    ) where

import System.PosixCompat.Files
    ( isNamedPipe, isDirectory, isRegularFile, isSymbolicLink
    , getFdStatus, getFileStatus, getSymbolicLinkStatus
    , modificationTime, setFileMode, fileSize, fileMode, fileOwner
    , stdFileMode, FileStatus, fileID
    , linkCount, createLink
    )
