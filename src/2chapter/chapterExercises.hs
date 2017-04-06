module ChapterExercieses where

-- 1. (2 + (2 * 3)) -1
-- 2. (^) 10 $ (1 + 1)
-- 3. ()(2 ^ 2) * (4 ^ 5)) + 1
--
-- Equivalent Expressions
--
-- 1. same
-- 2. same
-- 3. different 400 - 37 vs 37 - 400
-- 4. different 33 vs 33.3333
-- 5. different (2 * 5) + 18 vs 2 * (5 + 18)
--
-- in REPEL
--   let x = y ^ 2
--   let y = z + 8
--   let z = 7
--   let waxOn = x * 5
--
--   1. 10 + waxOn     = 1135
--      (+10) + waxOn  = 1135
--      (-) 15 waxOn   = 1110
--      (-) waxOn - 15 = 1110
--   2. let triple x = x * 3
--   3. triple waxOn = 3375
--   4.
waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7
-- 5.
triple x = x * 3

-- 6.
waxOff x = triple x
