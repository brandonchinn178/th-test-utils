{-|
Module      :  Language.Haskell.TH.TestUtils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines utilites for testing Template Haskell code.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.TestUtils
  ( -- * Error recovery
    -- $tryQ
    tryQ'
  , tryQ
  , tryQErr
  , tryQErr'
  ) where

import Control.Monad ((>=>))
import qualified Control.Monad.Fail as Fail
#if MIN_VERSION_template_haskell(2,13,0)
import Control.Monad.IO.Class (MonadIO)
#endif
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State (StateT, put, runStateT)
import Language.Haskell.TH (Exp, Q, appE, runQ)
#if MIN_VERSION_template_haskell(2,12,0)
import qualified Language.Haskell.TH as TH
#endif
import Language.Haskell.TH.Syntax (Quasi(..), lift)

-- $tryQ
--
-- Unfortunately, there is no built-in way to get an error message of a Template Haskell
-- computation, since 'Language.Haskell.TH.recover' throws away the error message. If
-- 'Language.Haskell.TH.recover' was defined differently, we could maybe do:
--
-- > recover' :: (String -> Q a) -> Q a -> Q a
-- >
-- > spliceFail :: Q Exp
-- > spliceFail = fail "This splice fails"
-- >
-- > spliceInt :: Q Exp
-- > spliceInt = [| 1 |]
-- >
-- > test1 :: Either String Int
-- > test1 = $(recover' (pure . Left) $ Right <$> spliceFail) -- generates `Left "This splice fails"`
-- >
-- > test2 :: Either String Int
-- > test2 = $(recover' (pure . Left) $ Right <$> spliceInt) -- generates `Right 1`
--
-- But for now, we'll have to use 'tryQ':
--
-- > test1 :: Either String Int
-- > test1 = $(tryQ spliceFail) -- generates `Left "This splice fails"`
-- >
-- > test2 :: Either String Int
-- > test2 = $(tryQ spliceInt) -- generates `Right 1`
--
-- ref. https://ghc.haskell.org/trac/ghc/ticket/2340

newtype TryQ a = TryQ { unTryQ :: ExceptT () (StateT (Maybe String) Q) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    #if MIN_VERSION_template_haskell(2,13,0)
    , MonadIO
    #endif
    )

liftQ :: Q a -> TryQ a
liftQ = TryQ . Trans.lift . Trans.lift

instance Fail.MonadFail TryQ where
  fail _ = TryQ $ throwE ()

instance Quasi TryQ where
  qNewName name = liftQ $ qNewName name

  qReport False msg = liftQ $ qReport False msg
  qReport True msg = TryQ . Trans.lift . put $ Just msg

  qRecover (TryQ handler) (TryQ action) = TryQ $ catchE action (const handler)
  qLookupName b name = liftQ $ qLookupName b name
  qReify name = liftQ $ qReify name
  qReifyInstances name types = liftQ $ qReifyInstances name types
  qReifyRoles name = liftQ $ qReifyRoles name
  qReifyAnnotations ann = liftQ $ qReifyAnnotations ann
  qReifyModule m = liftQ $ qReifyModule m
  qLocation = liftQ qLocation
  qRunIO m = liftQ $ qRunIO m
  qAddDependentFile fp = liftQ $ qAddDependentFile fp
  qAddTopDecls decs = liftQ $ qAddTopDecls decs
  qAddModFinalizer q = liftQ $ qAddModFinalizer q
  qGetQ = liftQ qGetQ
  qPutQ x = liftQ $ qPutQ x

  #if MIN_VERSION_template_haskell(2,11,0)
  qReifyFixity name = liftQ $ qReifyFixity name
  qReifyConStrictness name = liftQ $ qReifyConStrictness name
  qIsExtEnabled ext = liftQ $ qIsExtEnabled ext
  qExtsEnabled = liftQ qExtsEnabled
  #endif

  #if MIN_VERSION_template_haskell(2,13,0)
  qAddCorePlugin s = liftQ $ qAddCorePlugin s
  #endif

  #if MIN_VERSION_template_haskell(2,14,0)
  qAddTempFile s = liftQ $ qAddTempFile s
  qAddForeignFilePath lang s = liftQ $ qAddForeignFilePath lang s
  #endif

-- | Run the given Template Haskell computation, returning either an error message or the final
-- result.
tryQ' :: Q a -> Q (Either String a)
tryQ' = fmap cast . (`runStateT` Nothing) . runExceptT . unTryQ . runQ
  where
    cast (Left (), Nothing) = Left "Q monad failure"
    cast (Left (), Just msg) = Left msg
    cast (Right a, _) = Right a

-- | Run the given Template Haskell computation, returning a splicable expression that resolves
-- to 'Left' the error message or 'Right' the final result.
--
-- > -- Left "This splice fails"
-- > $(tryQ spliceFail) :: Either String Int
-- >
-- > -- Right 1
-- > $(tryQ spliceInt) :: Either String Int
tryQ :: Q Exp -> Q Exp
tryQ = tryQ' >=> either
  (appE (typeAppString [| Left |]) . lift)
  (appE (typeAppString [| Right |]) . pure)

-- | 'tryQ', except returns 'Just' the error message or 'Nothing' if the computation succeeded.
--
-- > -- Just "This splice fails"
-- > $(tryQErr spliceFail) :: Maybe String
-- >
-- > -- Nothing
-- > $(tryQErr spliceInt) :: Maybe String
tryQErr :: Q a -> Q Exp
tryQErr = tryQ' >=> either
  (appE (typeAppString [| Just |]) . lift)
  (const (typeAppString [| Nothing |]))

-- | 'tryQ', except returns the error message or fails if the computation succeeded.
--
-- > -- "This splice fails"
-- > $(tryQErr' spliceFail) :: String
-- >
-- > -- compile time error: "Q monad unexpectedly succeeded"
-- > $(tryQErr' spliceInt) :: String
tryQErr' :: Q a -> Q Exp
tryQErr' = tryQ' >=> either
  lift
  (const $ fail "Q monad unexpectedly succeeded")

{- Helpers -}

typeAppString :: Q Exp -> Q Exp
typeAppString expQ =
  #if MIN_VERSION_template_haskell(2,12,0)
    TH.appTypeE expQ [t| String |]
  #else
    expQ
  #endif
