module Optimizer
    ( optimize
    ) where

import qualified Data.Map                      as Map
import           Types
import           Utils

-- | env cotains only literals
type Env = Map.Map Idx (Expr Idx)

-- | optimization
optimize :: Bool -> Expr Idx -> Expr Idx
optimize True  = foldExpr Map.empty
optimize False = id

-- | constant folding optimization
foldExpr :: Env -> Expr Idx -> Expr Idx
foldExpr env expr = case expr of
    Variable a -> case Map.lookup a env of
        Nothing -> Variable a
        Just ex -> ex
    LitInt    n  -> LitInt n
    LitBool   b  -> LitBool b
    LitString s  -> LitString s
    LitUnit      -> LitUnit
    Plus  ex ex' -> handleSeq (foldExpr env ex) (foldExpr env ex') setPlus
    Minus ex ex' -> handleSeq (foldExpr env ex)
                              (foldExpr env ex')
                              (\x y -> setPlus x (setNeg y))
    Mult ex ex' -> handleSeq (foldExpr env ex) (foldExpr env ex') setMult
    Div ex ex' ->
        handleSeq (foldExpr env ex) (foldExpr env ex') (setDivLike div Div)
    Mod ex ex' ->
        handleSeq (foldExpr env ex) (foldExpr env ex') (setDivLike mod Mod)
    LessThan ex ex' ->
        handleSeq (foldExpr env ex) (foldExpr env ex') (setCmpLike (<) LessThan)
    LessEqual ex ex' -> handleSeq (foldExpr env ex)
                                  (foldExpr env ex')
                                  (setCmpLike (<=) LessEqual)
    And    ex ex'       -> handleSeq (foldExpr env ex) (foldExpr env ex') setAnd
    Or     ex ex'       -> handleSeq (foldExpr env ex) (foldExpr env ex') setOr
    Equals ex ex'       -> handleSeq (foldExpr env ex) (foldExpr env ex') setEq
    Concat ex ex' -> handleSeq (foldExpr env ex) (foldExpr env ex') setConcat
    Seq    ex ex'       -> Seq ex ex'
    Not ex              -> handleSeq1 (foldExpr env ex) setNot
    Neg ex              -> handleSeq1 (foldExpr env ex) setNeg
    Call a exs          -> Call a (map (foldExpr env) exs)
    ConstrCall a at exs -> ConstrCall a at (map (foldExpr env) exs)
    Let pd@(ParamDef n _) ex ex' ->
        let r = foldExpr env ex
        in  if isLit r then foldExpr (Map.insert n r env) ex' else Let pd r ex'
    IfElse ex et ee -> case foldExpr env ex of
        LitBool True  -> foldExpr env et
        LitBool False -> foldExpr env ee
        r             -> IfElse r (foldExpr env et) (foldExpr env ee)
    Match ex mcs -> Match ex mcs    -- TODO
    Bottom ex    -> Bottom (foldExpr env ex)
  where
    setPlus lhs rhs = case (lhs, rhs) of
        (LitInt i         , LitInt j) -> LitInt (i + j)
        (LitInt _         , _       ) -> setPlus rhs lhs
        (Plus x (LitInt i), LitInt j) -> setPlus x (LitInt (i + j))
        (Plus x (LitInt i), Plus y (LitInt j)) ->
            setPlus (Plus x y) (LitInt (i + j))
        (x, LitInt 0) -> x
        _             -> Plus lhs rhs

    setNeg (LitInt i) = LitInt (negate i)
    setNeg (Neg    i) = i
    setNeg e          = Neg e

    setNot (LitBool b) = LitBool (not b)
    setNot (Not     p) = p
    setNot e           = Not e

    setMult lhs rhs = case (lhs, rhs) of
        (LitInt i         , LitInt j) -> LitInt (i * j)
        (LitInt _         , _       ) -> setMult rhs lhs
        (Mult x (LitInt i), LitInt j) -> setMult x (LitInt (i * j))
        (Mult x (LitInt i), Mult y (LitInt j)) ->
            setMult (Mult x y) (LitInt (i * j))
        (_    , LitInt 0) -> if isLit lhs then LitInt 0 else Seq lhs (LitInt 0)
        (Neg i, LitInt j) -> Mult i (LitInt (-j))
        (Neg i, Neg j   ) -> Mult i j
        _                 -> Mult lhs rhs

    setDivLike f _ (LitInt i) (LitInt j) | j /= 0 = LitInt (i `f` j)
    setDivLike _ c lhs rhs                        = c lhs rhs

    setCmpLike f _ (LitInt i) (LitInt j) = LitBool (i `f` j)
    setCmpLike _ c lhs        rhs        = c lhs rhs

    setAnd lhs rhs = case (lhs, rhs) of
        (LitBool p    , LitBool q    ) -> LitBool (p && q)
        (LitBool True , _            ) -> rhs
        (LitBool False, _            ) -> LitBool True
        (_            , LitBool True ) -> lhs
        (_            , LitBool False) -> Seq lhs (LitBool False)
        _                              -> And lhs rhs

    setOr lhs rhs = case (lhs, rhs) of
        (LitBool p    , LitBool q    ) -> LitBool (p || q)
        (LitBool True , _            ) -> LitBool True
        (LitBool False, _            ) -> rhs
        (_            , LitBool True ) -> Seq lhs (LitBool True)
        (_            , LitBool False) -> lhs
        _                              -> Or lhs rhs

    setEq lhs rhs | isLit lhs && isLit rhs = LitBool (lhs == rhs)
                  | otherwise              = Equals lhs rhs

    setConcat lhs rhs = case (lhs, rhs) of
        (LitString i, LitString j) -> LitString (i ++ j)
        (Concat p (LitString i), LitString j) -> Concat p (LitString (i ++ j))
        (LitString i, Concat (LitString j) q) -> Concat (LitString (i ++ j)) q
        (Concat p (LitString i), Concat (LitString j) q) ->
            Concat (Concat p (LitString (i ++ j))) q
        _ -> Concat lhs rhs

    handleSeq lhs rhs cont = case (lhs, rhs) of
        (Seq p n   , r)           -> Seq p (handleSeq n r cont)
        (Let pd b e, r)           -> Let pd b (handleSeq e r cont)
        (i, Seq p q) | isLit i    -> Seq p (handleSeq i q cont)
        (i, Let pd b e) | isLit i -> Let pd b (handleSeq i e cont)
        _                         -> cont lhs rhs

    handleSeq1 ex cont = case ex of
        Seq p q    -> Seq p (handleSeq1 q cont)
        Let bd b e -> Let bd b (handleSeq1 e cont)
        _          -> cont ex

