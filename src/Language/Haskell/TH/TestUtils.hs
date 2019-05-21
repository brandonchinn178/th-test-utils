{-|
Module      :  Language.Haskell.TH.TestUtils
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines utilites for testing Template Haskell code.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.TestUtils
  ( -- * Error recovery
    -- $tryQ
    tryQ
  , tryQ'
  , tryQErr
  , tryQErr'
  ) where

import Control.Monad ((>=>))
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State (StateT, put, runStateT)
import Language.Haskell.TH (Exp, Q, appE, runQ)
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
-- > test1 = $(tryQ' spliceFail) -- generates `Left "This splice fails"`
-- >
-- > test2 :: Either String Int
-- > test2 = $(tryQ' spliceInt) -- generates `Right 1`
--
-- ref. https://ghc.haskell.org/trac/ghc/ticket/2340

newtype TryQ a = TryQ { unTryQ :: ExceptT () (StateT (Maybe String) Q) a }
  deriving (Functor, Applicative, Monad, MonadIO)

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
  qReifyFixity name = liftQ $ qReifyFixity name
  qReifyInstances name types = liftQ $ qReifyInstances name types
  qReifyRoles name = liftQ $ qReifyRoles name
  qReifyAnnotations ann = liftQ $ qReifyAnnotations ann
  qReifyModule m = liftQ $ qReifyModule m
  qReifyConStrictness name = liftQ $ qReifyConStrictness name
  qLocation = liftQ qLocation
  qAddDependentFile fp = liftQ $ qAddDependentFile fp
  qAddTempFile s = liftQ $ qAddTempFile s
  qAddTopDecls decs = liftQ $ qAddTopDecls decs
  qAddForeignFilePath lang s = liftQ $ qAddForeignFilePath lang s
  qAddModFinalizer q = liftQ $ qAddModFinalizer q
  qAddCorePlugin s = liftQ $ qAddCorePlugin s
  qGetQ = liftQ qGetQ
  qPutQ x = liftQ $ qPutQ x
  qIsExtEnabled ext = liftQ $ qIsExtEnabled ext
  qExtsEnabled = liftQ qExtsEnabled

-- | Run the given Template Haskell computation, returning either an error message or the final
-- result.
tryQ :: Q a -> Q (Either String a)
tryQ = fmap cast . (`runStateT` Nothing) . runExceptT . unTryQ . runQ
  where
    cast (Left (), Nothing) = Left "Q monad failure"
    cast (Left (), Just msg) = Left msg
    cast (Right a, _) = Right a

-- | 'tryQ', except returns 'Left' the error message or 'Right' the final result.
--
-- > -- Left "This splice fails"
-- > $(tryQ' spliceFail) :: Either String Int
-- >
-- > -- Right 1
-- > $(tryQ' spliceInt) :: Either String Int
tryQ' :: Q Exp -> Q Exp
tryQ' = tryQ >=> either
  (appE [| Left |] . lift)
  (appE [| Right |] . pure)

-- | 'tryQ', except returns 'Just' the error message or 'Nothing' if the computation succeeded.
--
-- > -- Just "This splice fails"
-- > $(tryQErr spliceFail) :: Maybe String
-- >
-- > -- Nothing
-- > $(tryQErr spliceInt) :: Maybe String
tryQErr :: Q a -> Q Exp
tryQErr = tryQ >=> either
  (appE [| Just |] . lift)
  (const [| Nothing |])

-- | 'tryQ', except returns the error message or fails if the computation succeeded.
--
-- > -- "This splice fails"
-- > $(tryQErr' spliceFail) :: String
-- >
-- > -- compile time error: "Q monad unexpectedly succeeded"
-- > $(tryQErr' spliceInt) :: Maybe String
tryQErr' :: Q a -> Q Exp
tryQErr' = tryQ >=> either lift (const $ fail "Q monad unexpectedly succeeded")
