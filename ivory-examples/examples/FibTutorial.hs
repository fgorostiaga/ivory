{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module FibTutorial where

import Ivory.Language
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

type family TypeMap (a :: * -> *) (xs :: [*]) :: [*]
type instance TypeMap t '[] = '[]
type instance TypeMap t (x ': xs) = t x ': TypeMap t xs

data HList :: [*] -> * where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

data Expr a where
  Leaf :: IvoryType a => a -> Expr a
  -- I think we can no longer define an Applicative
  App :: Def (xs ':-> a) -> HList (TypeMap Expr xs) -> Expr a
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

myApp :: Expr Sint32
myApp = App myFun (HCons myString (HCons myInt HNil))

myStream :: Declaration Sint32
myStream = Output ("myStream", myApp)

runSpec :: [Declaration Sint32] -> Module
runSpec = undefined

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
