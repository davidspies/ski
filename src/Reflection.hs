module Reflection
    ( reflect
    ) where

import           Ski

rS :: Ski a
rS = S !+ (K !+ K) !+ K

rK :: Ski a
rK = K !+ K

rI :: Ski a
rI = K !+ (K !+ I)

leaf :: Ski a -> Ski a
leaf v = S !+ (K !+ K) !+ (S !+ I !+ (K !+ v))

node :: Ski a -> Ski a -> Ski a
node left right = S !+ (S !+ (K !+ S) !+ (S !+ (K !+ (S !+ I)) !+ left)) !+ right

reflect :: (a -> b) -> Ski a -> Ski b
reflect op = helper
  where
    helper I                = leaf rI
    helper K                = leaf rK
    helper S                = leaf rS
    helper (Var v)          = Var (op v)
    helper Ski{expr=l :+ r} = node (helper l) (helper r)
