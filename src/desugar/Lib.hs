module Lib where

import AbsWhile

false = EVal VNil
true = ECons (EVal VNil) (EVal VNil)

skip = CLoop true []
