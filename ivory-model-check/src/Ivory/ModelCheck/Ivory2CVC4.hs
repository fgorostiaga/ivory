
module Ivory.ModelCheck.Ivory2CVC4
--  ( modelCheckMod )
 where

import           Prelude hiding (exp)
-- import           Data.Word
import           Control.Monad
import qualified Ivory.Language.Syntax as I
import           Ivory.Opts.ConstFold (constFold)
-- import           Ivory.Opts.Overflow (overflowFold)

import           Ivory.ModelCheck.CVC4
import           Ivory.ModelCheck.Monad

-- XXX testing
--import Debug.Trace

--------------------------------------------------------------------------------

modelCheckMod :: I.Module -> ModelCheck ()
modelCheckMod m =
  -- We rely on constant folding having happend in the model-checker.  E.g., in
  -- computing the number of loop iterations.
  -- mapM_ (modelCheckProc . overflowFold . constFold) (getProcs $ I.modProcs m)
  mapM_ (modelCheckProc . constFold) (getProcs $ I.modProcs m)
  where
  getProcs ps = I.public ps ++ I.private ps

--------------------------------------------------------------------------------

modelCheckProc :: I.Proc -> ModelCheck ()
modelCheckProc I.Proc { I.procSym      = sym
                      , I.procRetTy    = ret
                      , I.procArgs     = args
                      , I.procBody     = body
                      , I.procRequires = requires
                      , I.procEnsures  = ensures
                      }
    -- XXX ignore requires/ensures for now
  =  do
  let ens  = map I.getEnsure ensures
  -- let reqs = map (toRequire . I.getRequire) requires
  mapM_ toParam args
  mapM_ (toBody ens) body

--------------------------------------------------------------------------------

toParam :: I.Typed I.Var -> ModelCheck ()
toParam (I.Typed t val) = void $ addEnvVar t (toVar val)

--------------------------------------------------------------------------------

-- | Symbolically execute statements, carrying the return requirements forward
-- to each location that there is a return statement.
toBody :: [I.Cond] -> I.Stmt -> ModelCheck ()
toBody ens stmt =
  let toBody' = toBody ens in
  case stmt of
    I.IfTE exp blk0 blk1   -> toIfTE ens exp blk0 blk1
    I.Assert exp           -> addQuery =<< toExpr I.TyBool exp
    I.CompilerAssert exp   -> addQuery =<< toExpr I.TyBool exp
    I.Return (I.Typed t e) -> void (toExpr t e)
    I.ReturnVoid           -> return ()
    I.Deref t v ref        -> toDeref t v ref
    I.Store t ptr exp      -> toStore t ptr exp
    I.Assign t v exp       -> toAssign t v exp
    I.Local t v inits      -> toLocal t v inits

    I.AllocRef t ref name  -> toAlloc t ref name

    I.Loop v exp inc blk   -> toLoop ens v exp inc blk
    I.Call t retV nm args  -> stub'
    I.Assume _             -> stub'
    I.RefCopy _ _ _        -> stub'
    I.Forever _            -> stub'
    I.Break                -> stub'
    I.Comment _            -> stub'
    where
    stub' = stub "toBody" $ show stmt

toDeref :: I.Type -> I.Var -> I.Expr -> ModelCheck ()
toDeref t v ref = do
  v' <- addEnvVar t (toVar v)
  r  <- toRef ref
  addInvariant (var v' .== var r)

toAlloc :: I.Type -> I.Var -> I.Name -> ModelCheck ()
toAlloc t ref name = do
  v' <- addEnvVar t (toVar ref)
  addInvariant (var v' .== var (toName name))

toStore :: I.Type -> I.Expr -> I.Expr -> ModelCheck ()
toStore t ptr exp = do
  v' <- updateEnvRef t ptr
  e  <- toExpr t exp
  addInvariant (var v' .== e)

toLocal :: I.Type -> I.Var -> I.Init -> ModelCheck ()
toLocal t v inits = do
  v' <- addEnvVar t (toVar v)
  is <- toInit inits
  addInvariant (var v' .== is)

toInit :: I.Init -> ModelCheck Expr
toInit init =
  case init of
    I.InitZero       -> return $ intLit 0
    I.InitExpr t exp -> toExpr t exp
    I.InitStruct _   -> stub'
    I.InitArray  _   -> stub'
    where
    stub' = stub "toInit" $ show init

toAssign :: I.Type -> I.Var -> I.Expr -> ModelCheck ()
toAssign t v exp = do
  v' <- addEnvVar t (toVar v)
  e  <- toExpr t exp
  addInvariant $ (var v' .== e)

-- XXX Abstraction (to implement): If there is load/stores in the block, the we
-- don't care how many times it iterates.  It's pure.
toLoop :: [I.Cond] -> I.Var -> I.Expr -> I.LoopIncr -> [I.Stmt] -> ModelCheck ()
toLoop ens v start end blk =
  mapM_ go ixs
  where
  go :: Integer -> ModelCheck ()
  go ix = do
    v' <- addEnvVar (I.TyInt I.Int32) $ toVar v
    addInvariant (var v' .== intLit ix)
    mapM_ (toBody ens) blk
  ----
  ixs = case lOp of
    Incr -> takeWhile (<= lEnd) $ iterate succ lStart
    Decr -> takeWhile (>= lEnd) $ iterate pred lStart
    where
    Loop lStart lEnd lOp = loopIterations start end


toIfTE :: [I.Cond] -> I.Expr -> [I.Stmt] -> [I.Stmt] -> ModelCheck ()
toIfTE ens cond blk0 blk1 = do
  st  <- getState
  b   <- toExpr I.TyBool cond
  runBranch st b blk0
  st' <- joinState st
  runBranch st (not' b) blk1
  void (joinState st')
  where
  runBranch :: SymExecSt -> Expr -> [I.Stmt] -> ModelCheck ()
  runBranch st b blk = do
    resetSt st                   -- Empty state except environment
    mapM_ (toBody ens) blk -- Body under the invariant
    branchSt b                   -- Make conditions under hypothesis b

--------------------------------------------------------------------------------

toExpr :: I.Type -> I.Expr -> ModelCheck Expr
toExpr t exp = case exp of
  I.ExpSym s                 -> return (var s)
  I.ExpVar v                 -> lookupVar (toVar v) >>= return . var
  I.ExpLit lit               -> return $
    case lit of
      I.LitInteger i -> intLit i
      I.LitBool b    -> if b then T else F
      I.LitFloat  _  -> stub'
      I.LitDouble _  -> stub'
      I.LitString _  -> stub'
      I.LitChar   _  -> stub'
      I.LitNull      -> stub'
  I.ExpSafeCast t' e         -> do e' <- toExpr t' e
                                   assertBoundedVar t e'
                                   return e'
  I.ExpOp op args            -> toExprOp t op args
  I.ExpAddrOfGlobal s        -> return (var s)
  I.ExpIndex _ _ _ _         -> stub'
  I.ExpMaxMin _              -> stub'
  I.ExpLabel{}               -> err "toExpr" (show exp) -- Already covered
  I.ExpToIx e i              -> err "toExpr" (show exp)
  where
  stub' = stub "toExpr" $ show exp

--------------------------------------------------------------------------------

toExprOp :: I.Type -> I.ExpOp -> [I.Expr] -> ModelCheck Expr
toExprOp t op args = case op of
  I.ExpEq t'      -> op2 t' Eq
  I.ExpNeq t'     -> toExpr t (I.ExpOp I.ExpNot [I.ExpOp (I.ExpEq t') args])
  I.ExpCond       -> stub' "ExpCond"
  ----------------------------------------
  I.ExpGt orEq t' ->
    case orEq of
      True  -> op2 t' Geq
      False -> op2 t' Ge
  I.ExpLt orEq t' ->
    case orEq of
      True  -> op2 t' Leq
      False -> op2 t' Le
  ----------------------------------------
  I.ExpNot -> toExpr I.TyBool arg0 >>= return . not'
  I.ExpAnd -> op2 t And
  I.ExpOr  -> op2 t Or
  ----------------------------------------
  I.ExpMul    -> stub' "ExpMul"
  I.ExpAdd    -> op2 t Add
  I.ExpSub    -> op2 t Sub
  I.ExpNegate ->
    let neg = I.ExpOp I.ExpSub [litOp t 0, arg0] in
    toExpr t neg
  I.ExpAbs    -> stub' "ExpAbs"
  I.ExpSignum -> stub' "ExpSignum"
  ----------------------------------------
  I.ExpMod   -> toMod t arg0 arg1
  I.ExpDiv   -> stub' "ExpDiv"
  I.ExpRecip -> stub' "ExpRecip"
  ----------------------------------------
  I.ExpFExp     -> stub' "ExpFExp"
  I.ExpFSqrt    -> stub' "ExpFSqrt"
  I.ExpFLog     -> stub' "ExpFLog"
  I.ExpFPow     -> stub' "ExpFPow"
  I.ExpFLogBase -> stub' "ExpFLogBase"
  I.ExpFSin     -> stub' "ExpFSin"
  I.ExpFTan     -> stub' "ExpFTan"
  I.ExpFCos     -> stub' "ExpFCos"
  I.ExpFSinh    -> stub' "ExpFASinh"
  I.ExpFCosh    -> stub' "ExpFACosh"
  I.ExpFTanh    -> stub' "ExpFATanh"
  I.ExpFAsin    -> stub' "ExpFAsin"
  I.ExpFAtan    -> stub' "ExpFAtan"
  I.ExpFAcos    -> stub' "ExpFACos"
  I.ExpFAsinh   -> stub' "ExpFAsinh"
  I.ExpFAtanh   -> stub' "ExpFAtanh"
  I.ExpFAcosh   -> stub' "ExpFACosh"
  ----------------------------------------
  I.ExpIsNan t' -> stub' "ExpIsNan"
  I.ExpIsInf t' -> stub' "ExpIsInf"
  I.ExpRoundF   -> stub' "ExpRoundF"
  I.ExpCeilF    -> stub' "ExpCeilF"
  I.ExpFloorF   -> stub' "ExpFlorF"
  ----------------------------------------
  I.ExpBitAnd        -> stub' "ExpBitAnd"
  I.ExpBitOr         -> stub' "ExpBitOr"
  I.ExpBitXor        -> stub' "ExpBitXor"
  I.ExpBitComplement -> stub' "ExpBitComplement"
  I.ExpBitShiftL     -> stub' "ExpBitShiftL"
  I.ExpBitShiftR     -> stub' "ExpBitShiftR"
  where
  stub' op = stub "toExprOp" $ op ++ ": " ++ show args
  arg0 = args !! 0
  arg1 = args !! 1
  ----
  op2 t' op = do
    e0 <- toExpr t' arg0
    e1 <- toExpr t' arg1
    return (e0 `op` e1)

-- Abstraction: a % b (C semantics) implies
--
-- (   ((a => 0) && (a % b => 0) && (a % b < b) && (a % b <= a))
--  || ((a < 0)  && (a % b <= 0) && (a % b > b) && (a % b => a)))
--
-- make a fresh variable v == a % b
-- and assert the above for v then returning it.
toMod :: I.Type -> I.Expr -> I.Expr -> ModelCheck Expr
toMod t e0 e1 = do
  v <- incReservedVar =<< toType t
  a <- toExpr t e0
  b <- toExpr t e1
  let v' = var v
  addInvariant (call modAbs [v', a, b])
  return v'

--------------------------------------------------------------------------------
-- Helpers

toName :: I.Name -> Var
toName name =
  case name of
    I.NameSym s -> s
    I.NameVar v -> toVar v

toVar :: I.Var -> Var
toVar v =
  case v of
    I.VarName n     -> n
    I.VarInternal n -> n
    I.VarLitName n  -> n

baseType :: I.Type -> Bool
baseType t = case t of
  I.TyBool     -> True
  (I.TyWord _) -> True
  (I.TyInt  _) -> True
  I.TyFloat    -> True
  I.TyDouble   -> True
  _            -> False

-- Abstraction: collapse references.
toType :: I.Type -> ModelCheck Type
toType t = case t of
  I.TyVoid         -> return Void
  (I.TyWord _)     -> return Integer
  (I.TyInt  _)     -> return Integer
  I.TyBool         -> return Bool
  I.TyChar         -> return Char
  I.TyFloat        -> return Real
  I.TyDouble       -> return Real
  I.TyRef t'       -> toType t'
  I.TyConstRef t'  -> toType t'
  I.TyPtr t'       -> toType t'
  I.TyArr i t'     -> return . (Arr i) =<< toType t'
  I.TyCArray t'    -> err "toType" "carray"
  I.TyOpaque       -> return Opaque
  I.TyStruct name  -> do let ty = Struct name
                         addType ty
                         return ty
  I.TyProc t' ts   -> do ts' <- mapM toType ts
                         r   <- toType t'
                         return $ Fun ts' r
  -- _            -> error $ show t

updateEnvRef :: I.Type -> I.Expr -> ModelCheck Var
updateEnvRef t ref =
  case ref of
    I.ExpLabel ty (I.ExpVar struct) field
      -> do struct' <- addEnvVar ty (toVar struct)
            addEnvVar t (struct' ++ '_' : field)
    I.ExpVar v
      -> addEnvVar t (toVar v)
    _ -> error $ "Unexpected expression " ++ show ref
      ++ " to updateEnvRef."

toRef :: I.Expr -> ModelCheck Var
toRef ref =
  case ref of
    I.ExpLabel _ty (I.ExpVar struct) field
      -> lookupVar (toVar struct ++ '_' : field)
    I.ExpVar v
      -> lookupVar (toVar v)
    _ -> error $ "Unexpected expression " ++ show ref
      ++ " to toRef."

data LoopOp = Incr | Decr deriving (Show, Read, Eq)
data Loop = Loop
  { startVal :: Integer
  , endVal   :: Integer
  , loopOp   :: LoopOp
  } deriving (Show, Read, Eq)

-- Compute the number of iterations in a loop.  Assume the constant folder has
-- run.
loopIterations :: I.Expr -> I.LoopIncr -> Loop
loopIterations start end = Loop (getLit start) (snd fromIncr) (fst fromIncr)
  where
  getLit e = case e of
    I.ExpLit l   -> case l of
      I.LitInteger i -> i
      _              -> err "loopIterations" (show e)
    _            -> err "loopIterations" (show e)

  fromIncr = case end of
               I.IncrTo e -> (Incr, getLit e)
               I.DecrTo e -> (Decr, getLit e)

--------------------------------------------------------------------------------
-- Language construction helpers

binOp :: I.ExpOp -> I.Expr -> I.Expr -> I.Expr
binOp op e0 e1 = I.ExpOp op [e0, e1]

-- orOp, andOp :: I.Expr -> I.Expr -> I.Expr
-- orOp  = binOp I.ExpOr
-- andOp = binOp I.ExpAnd

-- leOp, leqOp, geOp, geqOp :: I.Type -> I.Expr -> I.Expr -> I.Expr
-- leOp  t = binOp (I.ExpLt False t)
-- leqOp t = binOp (I.ExpLt True  t)
-- geOp  t = binOp (I.ExpGt False t)
-- geqOp t = binOp (I.ExpGt True  t)

-- negOp :: I.Expr -> I.Expr
-- negOp e = I.ExpOp I.ExpNot [e]

-- addOp :: I.Expr -> I.Expr -> I.Expr
-- addOp e0 e1 = binOp I.ExpAdd e0 e1

-- subOp :: I.Expr -> I.Expr -> I.Expr
-- subOp e0 e1 = binOp I.ExpSub e0 e1

-- incrOp :: I.Type -> I.Expr -> I.Expr
-- incrOp t e = addOp e (litOp t 1)

-- decrOp :: I.Type -> I.Expr -> I.Expr
-- decrOp t e = subOp e (litOp t 1)

litOp :: I.Type -> Integer -> I.Expr
litOp t n = I.ExpLit e
  where
  e = case t of
        I.TyWord _ -> I.LitInteger n
        I.TyInt  _ -> I.LitInteger n
        I.TyFloat  -> I.LitFloat   (fromIntegral n)
        I.TyDouble -> I.LitDouble  (fromIntegral n)
        _          -> error $ "impossible lit in litOp: " ++ show t

varOp :: Var -> I.Expr
varOp = I.ExpVar . I.VarName

-- toRequire = undefined
-- toRequire :: I.Cond -> C.BlockItem
-- toRequire = toAssertion id "REQUIRES"

-- -- | Takes the return expression, the condition, and returns a 'BlockItem'.
-- toEnsure :: I.Expr -> I.Cond -> C.BlockItem
-- toEnsure retE = toAssertion (loop retE) "ENSURES"
--   where
--   -- Replace ensures variable with the return expression.
--   loop :: I.Expr -> I.Expr -> I.Expr
--   loop e = case e of
--     I.ExpSym{}             -> e
--     I.ExpVar v             -> if v == I.retval then retE else e
--     I.ExpLit{}             -> e
--     I.ExpOp op args        -> I.ExpOp op (map loop args)
--     I.ExpLabel t e0 s      -> I.ExpLabel t (loop e0) s
--     I.ExpIndex t e0 t1 e1  -> I.ExpIndex t (loop e0) t1 (loop e1)
--     I.ExpSafeCast t e0     -> I.ExpSafeCast t (loop e0)
--     I.ExpToIx e0 maxSz     -> I.ExpToIx (loop e0) maxSz
--     I.ExpAddrOfGlobal{}    -> e

-- toAssertion :: (I.Expr -> I.Expr) -> String -> I.Cond -> C.BlockItem
-- toAssertion trans call cond = C.BlockStm $
--   case cond of
--     I.CondBool e          ->
--       [cstm| $id:call($exp:(toExpr I.TyBool (trans e))); |]
--     I.CondDeref t e var c ->
--       let res = (toBody []) (I.Deref t var (trans e)) in
--       let c1  = toAssertion trans call c in
--       [cstm| { $items:res $item:c1 } |]

--------------------------------------------------------------------------------

addEnvVar :: I.Type -> Var -> ModelCheck Var
addEnvVar t v = do
  t' <- toType t
  v' <- declUpdateEnv t' v
  assertBoundedVar t (var v')
  return v'

-- Call the appropriate cvc4lib functions.
assertBoundedVar :: I.Type -> Expr -> ModelCheck ()
assertBoundedVar t e = getBounds t
  where
  getBounds t' = case t' of
    I.TyWord w -> case w of
      I.Word8  -> c word8
      I.Word16 -> c word16
      I.Word32 -> c word32
      I.Word64 -> c word64

    I.TyInt i -> case i of
      I.Int8  -> c int8
      I.Int16 -> c int16
      I.Int32 -> c int32
      I.Int64 -> c int64

    _         -> return ()

  c f = addInvariant (call f [e])


err :: String -> String -> a
err f msg = error $ "in ivory-model-check. Unexpected: " ++ msg
         ++ " in function " ++ f

stub :: String -> String -> a
stub f case_ = error $ "in ivory-model-check. Case undefined for: " ++ case_
         ++ " in function " ++ f

--------------------------------------------------------------------------------

