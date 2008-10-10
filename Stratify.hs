--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

module Stratify
where

import Data.List  

-- |Given a list representing a logical \"separator\", and a
-- list that may contain occurrences of this separator,
-- 'stratify' returns a list of the (possibly empty)
-- subsequences \"between\" instances of the separator.
-- 
-- 'stratify' is the inverse of 'intercalate'.
-- 
-- >  intercalate s (stratify s l) == l
-- 
-- Also
-- 
-- >  stratify s (intercalate s l) == l
-- 
-- except in the cases where 'intercalate' loses
-- information.  These are:
-- 
--    (1) When @s == []@.
--
-- >        intercalate [] l == concat l
--
--        We choose
--
-- >        stratify [] l == [l]
--
--    (2) When @l == []@.  
--
-- >        intercalate s [] == intercalate s [[]] == []
--
--        We choose
--
-- >        stratify s [] == [[]]
--
--    (3) When @s@ overlaps the elements of @l@ in such a way as to
--        add extra occurrences of @s@.  We find occurrences of
--        @s@ via an eager left-to-right scan.
-- 
-- \"Intercalated\: A body of material interbedded or
-- interlaminated with another.\"
-- 
-- \"Stratified\: A term applied to rocks deposited in
-- nearly horizontal layers or strata on the earth's
-- surface.\"
-- 
-- \--US Geological Survey Bulletin 587
-- <http://www.nps.gov/history/history/online_books/geology/publications/pp/587/glossary.htm>
stratify :: (Eq a) => [a] -> [a] -> [[a]]
stratify [] l = [l]
stratify _ [] = [[]]
stratify s l@(e : es) =
    case stripPrefix s l of
      Nothing -> let (l0 : ls) = stratify s es in (e : l0) : ls
      Just l' -> [] : stratify s l'
