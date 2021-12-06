module Optimizer
    ( optimize
    ) where

import           Data.Foldable                  ( Foldable(foldr') )
import qualified Data.Map                      as Map
import           Types
import           Utils                          ( isLit )

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
        Just ex -> if isLit ex then ex else Variable a
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
    Seq    ex ex'       -> setSequence' env $ setSequence ex ex'
    Not ex              -> handleSeq1 (foldExpr env ex) setNot
    Neg ex              -> handleSeq1 (foldExpr env ex) setNeg
    Call a exs          -> Call a (map (foldExpr env) exs)
    ConstrCall a at exs -> ConstrCall a at (map (foldExpr env) exs)
    Let pd@(ParamDef n _) ex ex' ->
        let r  = foldExpr env ex
            e' = Map.insert n r env
        in  if isLit r then foldExpr e' ex' else Let pd r (foldExpr e' ex')
    IfElse ex et ee -> case foldExpr env ex of
        LitBool True  -> foldExpr env et
        LitBool False -> foldExpr env ee
        r             -> IfElse r (foldExpr env et) (foldExpr env ee)
    Match ex mcs -> handleMatch env (foldExpr env ex) mcs
    Bottom ex    -> Bottom (foldExpr env ex)
  where
    setPlus lhs rhs = case (lhs, rhs) of
        (LitInt i         , LitInt j) -> LitInt (i + j)
        (LitInt _         , _       ) -> setPlus rhs lhs
        (Plus x (LitInt i), LitInt j) -> setPlus x (LitInt (i + j))
        (Plus x (LitInt i), Plus y (LitInt j)) ->
            setPlus (setPlus x y) (LitInt (i + j))
        (x, LitInt 0) -> x
        (x, Plus y z) -> setPlus (setPlus x y) z
        _             -> Plus lhs rhs

    setNeg exp = case exp of
        LitInt i                -> LitInt (negate i)
        Neg    i                -> i
        Plus x       y          -> setPlus (setNeg x) (setNeg y)
        Mult x       (LitInt i) -> setMult x (LitInt (-i))
        Mult x       (Neg    y) -> setMult x y
        Mult (Neg x) y          -> setMult x y
        e                       -> Neg e

    setNot exp = case exp of
        LitBool b -> LitBool (not b)
        Not     p -> p
        And x y   -> setOr (setNot x) (setNot y)
        Or  x y   -> setAnd (setNot x) (setNot y)
        e         -> Not e

    setMult lhs rhs = case (lhs, rhs) of
        (LitInt i         , LitInt j) -> LitInt (i * j)
        (LitInt _         , _       ) -> setMult rhs lhs
        (Mult x (LitInt i), LitInt j) -> setMult x (LitInt (i * j))
        (Mult x (LitInt i), Mult y (LitInt j)) ->
            setMult (setMult x y) (LitInt (i * j))
        (_, LitInt 0) ->
            if isLit lhs then LitInt 0 else setSequence lhs (LitInt 0)
        (Neg i, LitInt j) -> setMult i (LitInt (-j))
        (Neg i, Neg j   ) -> setMult i j
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
        (_            , LitBool False) -> setSequence lhs (LitBool False)
        _                              -> And lhs rhs

    setOr lhs rhs = case (lhs, rhs) of
        (LitBool p    , LitBool q    ) -> LitBool (p || q)
        (LitBool True , _            ) -> LitBool True
        (LitBool False, _            ) -> rhs
        (_            , LitBool True ) -> setSequence lhs (LitBool True)
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

    setSequence' env ~(Seq lhs rhs) = Seq (foldExpr env lhs) (foldExpr env rhs)

    setSequence lhs rhs = case lhs of
        Variable  _         -> rhs
        LitInt    _         -> rhs
        LitBool   _         -> rhs
        LitString _         -> rhs
        LitUnit             -> rhs
        Seq p n             -> setSequence p (setSequence n rhs)
        Let        pd b e   -> Let pd b (setSequence e rhs)
        ConstrCall _  _ exs -> foldr setSequence rhs exs
        Plus      x y       -> setSequence x (setSequence y rhs)
        Minus     x y       -> setSequence x (setSequence y rhs)
        Mult      x y       -> setSequence x (setSequence y rhs)
        Div       x y       -> setSequence x (setSequence y rhs)
        Mod       x y       -> setSequence x (setSequence y rhs)
        LessThan  x y       -> setSequence x (setSequence y rhs)
        LessEqual x y       -> setSequence x (setSequence y rhs)
        And       x y       -> setSequence x (setSequence y rhs)
        Or        x y       -> setSequence x (setSequence y rhs)
        Equals    x y       -> setSequence x (setSequence y rhs)
        Concat    x y       -> setSequence x (setSequence y rhs)
        Not x               -> setSequence x rhs
        Neg x               -> setSequence x rhs
        _                   -> Seq lhs rhs

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

    getExprValue env ex = case ex of
        Variable nm                     -> case Map.lookup nm env of
                                                Nothing -> ex
                                                Just ex' -> getExprValue env ex'
        ConstrCall nm tp args           -> ConstrCall nm tp (map (getExprValue env) args)
        Let pd@(ParamDef n _) vl ex'    -> getExprValue (Map.insert n vl env) ex'
        Seq _ nxt                       -> getExprValue env nxt
        _                               -> ex

    handlePattern scrt pat@WildcardPattern = (2, pat, Map.empty)
    handlePattern scrt pat@(IdPattern id) = (2, pat, Map.singleton id scrt)
    handlePattern scrt pat@(LiteralPattern lit) = if isLit scrt
        then (if scrt == lit then 2 else 0, WildcardPattern, Map.empty)
        else (1, pat, Map.empty)
    handlePattern scrt pat@(EnumPattern cons tp args) = case scrt of
        ConstrCall nm _ as -> if nm /= cons
            then (0, pat, Map.empty)
            else
                let (possib, newArgs, idMap) = foldr'
                        (\x (i, acc, mp) ->
                            let (i', p, m') = uncurry handlePattern x
                            in  (min i i', p : acc, Map.union m' mp)
                        )
                        (2, [], Map.empty)
                        (zip as args)
                in  (possib, EnumPattern cons tp newArgs, idMap)
        _ -> (1, pat, Map.empty)
    handleCases scrt (MatchCase pat expr) =
        let (possib, newPat, idMap) = handlePattern scrt pat
        in  [ MatchCase newPat (foldExpr (Map.union env idMap) expr)
            | possib /= 0
            ]

    handleMatch env scrt cases = Match scrt (concatMap (handleCases (getExprValue env scrt)) cases)
