{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module FibTutorial where

import Ivory.Language
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

data HList :: [*] -> * where
  HNil :: HList '[]
  HCons :: Expr a -> HList as -> HList (a ': as)

data Expr a where
  Leaf :: IvoryType a => a -> Expr a
  -- I think we can no longer define an Applicative
  App :: Def (xs ':-> a) -> HList xs -> Expr a
  ----------------------------------------
  Now :: Declaration a -> Expr a
  (:@) :: Declaration a -> (Int, a) -> Expr a

data Declaration a where
  Input :: String -> Declaration a
  Output :: (String, Expr a) -> Declaration a

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

runSpec :: [Declaration Sint32] -> Module
runSpec decs = let x = concatMap getFunsFromDec decs :: [ModuleDef] in package "hlolaPackage" $ sequence_ x

getFunsFromDec :: Declaration a -> [ModuleDef]
getFunsFromDec (Input _) = []
getFunsFromDec (Output (_, expr)) = getFunsFromExpr expr

getFunsFromExpr :: Expr a -> [ModuleDef]
getFunsFromExpr (Now _) = undefined
getFunsFromExpr (_ :@ _) = undefined
getFunsFromExpr (Leaf _) = []
getFunsFromExpr (App f exprs) = incl f : (concat $ hmap getFunsFromExpr exprs)

hmap :: (forall a0. Expr a0 -> [ModuleDef]) -> HList xs -> [[ModuleDef]]
hmap _ HNil = [[]]
hmap f (HCons a1 rest) = f a1 : hmap f rest

-- Original example:

puts :: Def ('[IString] ':-> Sint32)
puts  = importProc "puts" "stdio.h"

fib_loop :: Def ('[Ix 1000] ':-> Uint32)
fib_loop  = proc "fib_loop" $ \ n -> body $ do
  a <- local (ival 0)
  b <- local (ival 1)

  n `times` \ _ -> do
    a' <- deref a
    b' <- deref b
    store a b'
    store b (a' + b')
    call_ puts "hello, world\n"

  result <- deref a
  ret result

fib_tutorial_module :: Module
fib_tutorial_module = package "fib_tutorial" $ do
  incl fib_loop
  incl puts

main :: IO ()
main = C.compile [ fib_tutorial_module ] []
