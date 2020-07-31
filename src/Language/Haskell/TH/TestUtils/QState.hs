{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.TestUtils.QState
  ( QState(..)
  , ReifyInfo(..)
  , loadNames
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax (Lift)
#if MIN_VERSION_template_haskell(2,16,0)
import qualified Language.Haskell.TH.Syntax as TH
#endif

import Language.Haskell.TH.TestUtils.QMode (MockedMode, QMode)

-- | State information for mocking Q functionality.
data QState (mode :: MockedMode) = QState
  { mode       :: QMode mode
  , knownNames :: [(String, Name)]
    -- ^ Names that can be looked up with `lookupName`/`lookupTypeName`/`lookupValueName`
  , reifyInfo  :: [(Name, ReifyInfo)]
    -- ^ Reification information for Names to return when 'reify' is called.
  } deriving (Show, Lift)

data ReifyInfo = ReifyInfo
  { reifyInfoInfo   :: Info
  , reifyInfoFixity :: Maybe Fixity
  , reifyInfoRoles  :: Maybe [Role]
  , reifyInfoType   :: Type
  } deriving (Show, Lift)

-- | A helper for loading names for 'reifyInfo'
--
-- Usage:
--   QState
--     { reifyInfo = $(loadNames [''Int, ''Maybe])
--     , ...
--     }
loadNames :: [Name] -> ExpQ
loadNames names = listE $ flip map names $ \name -> do
  info <- reify name
  fixity <- reifyFixity name
  roles <- recover (pure Nothing) $ Just <$> reifyRoles name
#if MIN_VERSION_template_haskell(2,16,0)
  let infoType = reifyType name >>= TH.lift
#else
  let infoType = [| error "Your version of template-haskell does not have 'reifyType'" |]
#endif

  [| (name, ReifyInfo info fixity roles $infoType) |]
