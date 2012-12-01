--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

import Data.List
import Text.Printf
import Test.QuickCheck

import Stratify

main :: IO ()
main = mapM_ (\(s,a) -> printf "%25s: " s >> a) tests
 
--- to ensure that the tests are meaningful, we embed
--- some number of occurrences of s in l before we start
prop_intercalate_stratify :: [Int] -> [Int] -> Property
prop_intercalate_stratify s l =
    forAll l' $ \l'' -> (intercalate s $ stratify s l'') == l''
    where
      l' = do
        k <- choose (0, 2 * length l)
        fixup k l
      fixup 0 l0 = return l0
      fixup n l0 = do
        k <- choose (0, length l0)
        let (l01, l02) = splitAt k l0
        fixup (n - 1) (l01 ++ s ++ l02)

--- stratify can invert intercalate except when information
--- is lost by intercalate s l and there is thus no unique inverse.
--- This happens when:
---   1) s is [].  intercalate [] l == concat l.  We
---      choose stratify [] l = [l] .
---   2) l is [] or [[]].  intercalate s [] ==
---      intercalate s [[]] == [].  We choose
---      stratify s [] = [[]], for compatibility with (1).
---   3) s "overlaps" intercalate s l.  This is true when
---      there are more than length l - 1 occurrences of s
---      in the intercalation, because the intercalation
---      created new ones.  We ensure this to not be the
---      the case by preventing elements of s from overlapping
---      elements of concat l, which is unnecessarily strong.
prop_stratify_intercalate :: Int -> [Int] -> [Int] -> [[Int]] -> Property
prop_stratify_intercalate s ss ls lss =
  property $ stratify ss' (intercalate ss' lss') == lss'
  where
    lss' = map (map abs) (ls : lss) 
    ss'
      | concat lss' == [] = (s : ss)
      | otherwise = map ((+ (maximum . concat $ lss')) . (+ 1) . abs) 
                    (s : ss)
    
tests :: [(String, IO ())]
tests = [("intercalate.stratify/id", quickCheck prop_intercalate_stratify),
         ("stratify.intercalate/id", quickCheck prop_stratify_intercalate)]
