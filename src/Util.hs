module Util where

findLastIndex :: (a -> Bool) -> [a] -> Maybe Int
findLastIndex f s = do
    let v = zip [0..] s
        fv = filter (\(a,b) -> f b) v
    if null fv then Nothing else Just (fst (last fv))