{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Ski where

import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Maybe

data RawSki a = (:+) (Ski a) (Ski a)
  deriving (Eq, Functor, Foldable, Traversable)
infixl 9 :+

step :: RawSki a -> Maybe (Ski a)
step (Ski{rawExpr=(Ski{rawExpr=(S :+ x)} :+ y)} :+ z) =
  Just $ x !+ z !+ (y !+ z)
step (Ski{rawExpr=(K :+ x)} :+ _)                     = Just x
step (I :+ x)                                         = Just x
step _                                                = Nothing

data Ski a = S | K | I | Var a | Ski{
  rawExpr      :: RawSki a,
  leftResolved :: Maybe (Ski a),
  resolved     :: Maybe (Ski a)
}

matched :: String -> Bool
matched = maybe False (null . snd) . readToClose

readToClose :: String -> Maybe (String, String)
readToClose = helper 0
  where
    helper :: Int -> String -> Maybe (String, String)
    helper 0 "" = Just ("", "")
    helper _ "" = Nothing
    helper 0 s@('}' : _) = Just ("", s)
    helper remCloses (c : s) = first (c :) <$> helper (remCloses + incr) s
      where
        incr = case c of
          '{' -> 1
          '}' -> -1
          _   -> 0

instance Show (Ski String) where
  show (Var x)
    | matched x =
        case x of
          [c] -> if isUpper c then "{" ++ [c] ++ "}" else [c]
          _   -> "{" ++ x ++ "}"
    | otherwise = error "Variable name " ++ x ++ " has unmatched {}'s'"
  show Ski{rawExpr=x :+ y@Ski{}} = show x ++ "(" ++ show y ++ ")"
  show Ski{rawExpr=x :+ y}       = show x ++ show y
  show S                         = "S"
  show K                         = "K"
  show I                         = "I"

changeVarType :: Ski a -> Ski b
changeVarType = fmap $ error "free variable"

noFree :: Ski a -> Bool
noFree = null

instance Functor Ski where
  fmap op (Var x)        = Var (op x)
  fmap op Ski{rawExpr=e} = ski $ fmap op e
  fmap _ S               = S
  fmap _ K               = K
  fmap _ I               = I

instance Foldable Ski where
  foldr op base (Var x)        = x `op` base
  foldr op base Ski{rawExpr=e} = foldr op base e
  foldr _ base _               = base

instance Traversable Ski where
  sequenceA (Var x)        = Var <$> x
  sequenceA Ski{rawExpr=e} = ski <$> sequenceA e
  sequenceA prim           = pure $ changeVarType prim

instance Applicative Ski where
  pure = Var
  (<*>) = ap

instance Monad Ski where
  (>>=) (Var x) f             = f x
  (>>=) Ski{rawExpr=l :+ r} f = ski $ (l >>= f) :+ (r >>= f)
  (>>=) prim _                = changeVarType prim

substitute :: (a -> Maybe (Ski a)) -> Ski a -> Ski a
substitute lookupFn = (>>= \x -> fromMaybe (Var x) (lookupFn x))

instance Eq a => Eq (Ski a) where
  (==) S S                           = True
  (==) K K                           = True
  (==) I I                           = True
  (==) (Var x) (Var y)               = x == y
  (==) Ski{rawExpr=x} Ski{rawExpr=y} = x == y
  (==) _ _                           = False

(!+) :: Ski a -> Ski a -> Ski a
(!+) x y = ski $ x :+ y
infixl 9 !+

(!^) :: Ski a -> Ski a -> Ski a
(!^) x y = compute $ x !+ y

ski :: RawSki a -> Ski a
ski x = self
  where
    self = Ski{rawExpr=x, leftResolved=doLeftCompute x, resolved=doCompute self}

tryLeftCompute :: Ski a -> Maybe (Ski a)
tryLeftCompute Ski{leftResolved=lr} = lr
tryLeftCompute _                    = Nothing

leftCompute :: Ski a -> Ski a
leftCompute x = fromMaybe x $ tryLeftCompute x

doLeftCompute :: RawSki a -> Maybe (Ski a)
doLeftCompute v@(x :+ y) =
  case tryLeftCompute x of
    Nothing -> leftCompute <$> step v
    Just rx -> Just $ leftCompute $ rx !+ y

tryCompute :: Ski a -> Maybe (Ski a)
tryCompute Ski{resolved=r} = r
tryCompute _               = Nothing

compute :: Ski a -> Ski a
compute x = fromMaybe x $ tryCompute x

doCompute :: Ski a -> Maybe (Ski a)
doCompute Ski{rawExpr=x :+ y, leftResolved=Nothing} =
  case (tryCompute x, tryCompute y) of
    (Nothing, Nothing) -> Nothing
    (l, r)             -> Just $ fromMaybe x l !+ fromMaybe y r
doCompute Ski{leftResolved=Just lr}                 = Just $ compute lr
doCompute _prim                                     = Nothing

lambda :: Eq a => a -> Ski a -> Ski a
lambda v = helper
  where
    helper y@(Var w)
      | v == w = I
      | otherwise = K !+ y
    helper Ski{rawExpr=x :+ y} =
      case (helper x, helper y) of
        (Ski{rawExpr=K :+ x'}, I)                   -> x'
        (Ski{rawExpr=K :+ I}, y')                   -> y'
        (Ski{rawExpr=K :+ x'}, Ski{rawExpr=K:+ y'}) -> K !+ (x' !+ y')
        (x', y')                                    -> S !+ x' !+ y'
    helper prim                = K !+ prim
