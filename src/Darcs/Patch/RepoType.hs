module Darcs.Patch.RepoType
  ( RepoType(..), IsRepoType(..), SRepoType(..)
  , RebaseType(..), IsRebaseType, RebaseTypeOf, SRebaseType(..)
  ) where

-- |This type is intended to be used as a phantom type via
-- the 'DataKinds' extension, normally as part of 'RepoType'.
-- Indicates whether or not a rebase is in progress.
data RebaseType = IsRebase | NoRebase

-- |A reflection of 'RebaseType' at the value level so that
-- code can explicitly switch on it.
data SRebaseType (rebaseType :: RebaseType) where
  SIsRebase :: SRebaseType 'IsRebase
  SNoRebase :: SRebaseType 'NoRebase

class IsRebaseType (rebaseType :: RebaseType) where
  -- |Reflect 'RebaseType' to the value level so that
  -- code can explicitly switch on it.
  singletonRebaseType :: SRebaseType rebaseType

instance IsRebaseType 'IsRebase where
  singletonRebaseType = SIsRebase

instance IsRebaseType 'NoRebase where
  singletonRebaseType = SNoRebase

-- |This type is intended to be used as a phantom type via the 'DataKinds'
-- extension. It tracks different types of repositories, e.g. to
-- indicate when a rebase is in progress.
data RepoType = RepoType { rebaseType :: RebaseType }

-- |Extract the 'RebaseType' from a 'RepoType'
type family RebaseTypeOf (rt :: RepoType) :: RebaseType
type instance RebaseTypeOf ('RepoType rebaseType) = rebaseType

class IsRepoType (rt :: RepoType) where
  -- |Reflect 'RepoType' to the value level so that
  -- code can explicitly switch on it.
  singletonRepoType :: SRepoType rt

-- |A reflection of 'RepoType' at the value level so that
-- code can explicitly switch on it.
data SRepoType (repoType :: RepoType) where
  SRepoType :: SRebaseType rebaseType -> SRepoType ('RepoType rebaseType)

instance IsRebaseType rebaseType => IsRepoType ('RepoType rebaseType) where
  singletonRepoType = SRepoType singletonRebaseType
