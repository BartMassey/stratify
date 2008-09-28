--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Stratify
where

import Data.List (isPrefixOf)

-- |Given a list representing a logical "separator", and a
-- sequences of elements that may contain this separator,
-- 'stratify' returns a list of the (possibly empty)
-- subsequences "between" instances of the separator.
-- 
-- 'stratify' is the inverse of 'intercalate'.
-- 	'intercalate' s ('stratify' s l) == l
-- Also
-- 	'stratify' s ('intercalate' s l) == l
-- except in the cases where 'intercalate' loses
-- information.  These are: when l == [[]]; when s == [];
-- when s overlaps the elements of l in such a way as to
-- add extra occurrences of s.
-- 
-- 	"Intercalated: A body of material interbedded or
-- 	interlaminated with another."
-- 
-- 	"Stratified: A term applied to rocks deposited in
-- 	nearly horizontal layers or strata on the earth's
-- 	surface."
-- 
-- 	US Geological Survey Bulletin 587
-- 	http://www.nps.gov/history/history/online_books/
-- 	geology/publications/pp/587/glossary.htm	
stratify :: (Eq a) => [a] -> [a] -> [[a]]
stratify _ [] = []
stratify [] l = map (:[]) l
stratify sep l = go l where
    nsep = length sep
    go l | l0 == sep = [] : go l1
        where (l0, l1) = splitAt nsep l
    go (c : cs) = (c : l0) : l1
        where (l0 : l1) = go cs
    go [] = [[]]


-- intercalate "" [] -> ""
-- intercalate "" [""] -> ""
-- intercalate "" l -> concat l
-- intercalate "x" [] -> ""
-- intercalate "x" [""] -> ""
-- intercalate "x" ["",""] -> "x"
-- intercalate "x" ["a"] -> "a"
-- intercalate "x" ["a", "b"] -> "axb"
-- intercalate "x" ["a", ""] -> "ax"
-- intercalate "x" ["", "b"] -> "xb"
