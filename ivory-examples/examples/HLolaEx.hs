{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module HLolaEx where

import Ivory.Language
import Data.String
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

data HList :: [*] -> * where
  HNil :: HList '[]
  HCons :: (StreamType a, IvoryVar a) => Expr a -> HList as -> HList (a ': as)

data Expr a where
  Leaf :: IvoryType a => a -> Expr a
  -- I think we can no longer define an Applicative
  App :: Def (xs ':-> a) -> HList xs -> Expr a
  ----------------------------------------
  Now :: Declaration a -> Expr a
  (:@) :: Declaration a -> (Int, a) -> Expr a
  -- Native operations
  Add :: Num a => Expr a -> Expr a -> Expr a

data Declaration a where
  Input :: String -> Declaration a
  Output :: (String, Expr a) -> Declaration a

class StreamType a where
  getCurrentValueOf :: Def ('[IString] ':-> a)
  getDeferredValueOf :: Def ('[IString, Sint32, a] ':-> a)

instance StreamType IString where
  getCurrentValueOf = getStringCurrentValueOf
  getDeferredValueOf = getStringDeferredValueOf

instance StreamType Sint32 where
  getCurrentValueOf = getIntCurrentValueOf
  getDeferredValueOf = getIntDeferredValueOf

getId :: Declaration a -> String
getId (Input x) = x
getId (Output (x,_)) = x

-- C struct def
[ivory|
struct Foo
  { field0 :: Stored Uint32
  ; field1 :: Array 10 (Stored IBool)
  }
|]

myString :: Expr IString
myString = Leaf "mystring"

myInt :: Expr Sint32
myInt = Leaf 36

myFun :: Def ('[IString, Sint32] ':-> Sint32)
myFun = importProc "afun" "funheader.h"

-- definable, yet uncallable fun (Def ... does not implement IvoryType)
myHOFun :: Def ('[IString, Def ('[IString] ':-> Sint32)] ':-> Sint32)
myHOFun = importProc "hofun" "funheader.h"
-- Apparently, this is how you use high order functions
invoke :: Def ('[ ProcPtr ('[Sint32] ':-> Sint32), Sint32] ':-> Sint32)
invoke  = proc "invoke" (\ k n -> body (ret =<< indirect k n))

myApp :: Expr Sint32
myApp = App myFun (HCons myString (HCons myInt HNil))

myStream :: Declaration Sint32
myStream = Output ("myStream", myApp)

instantN :: Declaration Sint32
instantN = Output ("instantN", Add (instantN :@ (-1, 0)) (Leaf 1))

runSpec :: [Declaration Sint32] -> Module
runSpec decs = let
  x = concatMap getFunsFromDec decs
  mainModuleDef = getMainModule decs
  registerer = incl registerStream
  intdeferred = incl getIntDeferredValueOf
  structdef = defStruct (Proxy :: Proxy "Foo")
  in package "hlolaPackage" $ sequence_ (structdef:intdeferred:registerer:mainModuleDef:x)

getIntCurrentValueOf :: Def ('[IString] ':-> Sint32)
getIntCurrentValueOf = importProc "getIntCurrentValueOf" "valueGetters.h"
getIntDeferredValueOf :: Def ('[IString, Sint32, Sint32] ':-> Sint32)
getIntDeferredValueOf = importProc "getIntDeferredValueOf" "valueGetters.h"
getStringCurrentValueOf :: Def ('[IString] ':-> IString)
getStringCurrentValueOf = importProc "getStringCurrentValueOf" "valueGetters.h"
getStringDeferredValueOf :: Def ('[IString, Sint32, IString] ':-> IString)
getStringDeferredValueOf = importProc "getStringDeferredValueOf" "valueGetters.h"

getBodyFromExpr :: (StreamType a, IvoryVar a) => Expr a -> Ivory eff a
getBodyFromExpr (Leaf x) = return x
getBodyFromExpr (App f HNil) = call f
getBodyFromExpr (App f (HCons x HNil)) = do
  fx <- getBodyFromExpr x
  call f fx
getBodyFromExpr (App f (HCons x (HCons y HNil))) = do
  fx <- getBodyFromExpr x
  fy <- getBodyFromExpr y
  call f fx fy
getBodyFromExpr (Add e1 e2) = do
  e1b <- getBodyFromExpr e1
  e2b <- getBodyFromExpr e2
  return $ e1b + e2b
getBodyFromExpr (Now x) = call getCurrentValueOf (fromString$getId x)
getBodyFromExpr (x :@ (i,d)) = call getDeferredValueOf (fromString$getId x) (fromInteger$toInteger i) d

getFunForDec :: (StreamType a, IvoryVar a) => Declaration a -> Def ('[] ':-> a)
getFunForDec (Input _) = undefined -- TODO do this
getFunForDec (Output (x, expr)) = proc x $ body $ getBodyFromExpr expr >>= ret

-- We need to keep a state of visited declarations to avoid circularity
getFunsFromDec :: (StreamType a, IvoryVar a) => Declaration a -> [ModuleDef]
getFunsFromDec (Input _) = []
getFunsFromDec dec@(Output (_, expr)) = incl (getFunForDec dec):getFunsFromExpr expr

getFunsFromExpr :: Expr a -> [ModuleDef]
getFunsFromExpr (Now _) = undefined -- visit decl
getFunsFromExpr (_ :@ _) = [] -- undefined -- visit decl
getFunsFromExpr (Leaf _) = []
getFunsFromExpr (App f exprs) = incl f : (concat $ hmap getFunsFromExpr exprs)
getFunsFromExpr (Add e1 e2) = getFunsFromExpr e1 ++ getFunsFromExpr e2

hmap :: (forall a0. Expr a0 -> a) -> HList xs -> [a]
hmap _ HNil = []
hmap f (HCons a1 rest) = f a1 : hmap f rest

getMainModule :: [Declaration Sint32] -> ModuleDef
getMainModule decs = incl $ getMainFun decs

registerStream :: Def ('[IString, ProcPtr ('[] ':-> Sint32)] ':-> ())
registerStream = importProc "registerStream" "streamRegisterer.h"

getMainFun :: [Declaration Sint32] -> Def ('[] ':-> ())
getMainFun [dec] = proc "init_streams" $ body $ do
  call_ registerStream (fromString$getId dec) (procPtr $ getFunForDec dec)

main :: IO ()
main = C.compile [ runSpec [instantN] ] []
