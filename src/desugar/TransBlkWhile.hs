module TransBlkWhile(transBlk) where

import AbsWhile
import Data.Data                -- Data
import Data.Generics.Schemes    -- everywhere
import Data.Generics.Aliases    -- mkT

-- begin { ... } end ブロックを外側のコマンド列に展開する
-- (transCase, doInline, expandIf が生成する CBlk はコア言語にないため、
--  パイプラインの最後に平坦化する)

transBlk :: Data a => a -> a
transBlk = everywhere (mkT tBlk)

tBlk :: [Com] -> [Com]
tBlk = concatMap f
  where f (CBlk coms) = coms
        f c = [c]
