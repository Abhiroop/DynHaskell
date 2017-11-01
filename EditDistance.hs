module EditDistance where

naive a b = d (length a) (length b)
  where d i 0 = i
        d 0 j = j
        d i j
          | a !! (i - 1) == b !! (j -1) = d (i - 1) (j - 1)
          | otherwise = minimum [ d (i - 1) j         + 1
                                , d i      (j -1)     + 1
                                , d (i -1) (j - 1)    + 1]

