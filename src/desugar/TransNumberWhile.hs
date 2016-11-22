module TransNumberWhile(transNumber) where

import AbsWhile
import Data.Data                -- Data
import Data.Generics.Schemes    -- everywhere
import Data.Generics.Aliases    -- mkT

-- 数値 n を nil^n に置き換える

transNumber :: Data a => a -> a
transNumber = everywhere (mkT tNum)

tNum :: Val -> Val
tNum x = case x of
    VInt n -> foldr VCons VNil $ replicate (fromInteger n) VNil
    VCons val1 val2 -> VCons (transNumber val1) (transNumber val2)
    _ -> x
