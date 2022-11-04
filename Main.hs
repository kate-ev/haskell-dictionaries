 -- Author: Jekaterina Jevtejeva

compose = (.) . (.)

filterOutDuplRaw :: (Eq a) => [a] -> [a]
filterOutDuplRaw [] = []
filterOutDuplRaw (v:vs) 
  | checkIfPresentElswere v vs = filterOutDuplRaw vs
  | otherwise =  v:filterOutDuplRaw vs

filterOutDuplicates :: (Eq a) => [a] -> [a]
filterOutDuplicates list = reverse (filterOutDuplRaw (reverse list))

checkIfPresentElswere :: (Eq a) => a -> [a] -> Bool
checkIfPresentElswere _ [] = False
checkIfPresentElswere a (v:vs)
  | a == v = True
  | otherwise = checkIfPresentElswere a vs

aa :: (Eq a) => [a] -> [(a, a)] -> [a]
aa [] _ = []
aa (a:as) dictionary = compose filterOutDuplicates aaTranslate (a:as) dictionary

aaTranslate :: (Eq a) => [a] -> [(a, a)] -> [a]
aaTranslate [] _ = []
aaTranslate (a:as) dictionary =  map (`aaTranslateElement` dictionary) (a:as)

aaTranslateElement :: (Eq a) => a -> [(a, a)] -> a
aaTranslateElement a [] = a
aaTranslateElement a (v:vs)
  | a == fst v = snd v
  | otherwise = aaTranslateElement a vs

-- Expected: [-6,-5,-4,-3,-2,-1]
aa1 :: IO ()
aa1 = do
  putStrLn "Test aa1"
  print (aa [6, 5, 4, 3, 2, 1, 1] [(1, -1), (2, -2), (3, -3), (4, -4), (5, -5), (6, -6)])

-- Expected: ["AA", "BB", "CC", "DD", "EE"]
aa2 :: IO ()
aa2 = do
  putStrLn "Test aa2"
  print (aa ["a", "b", "c", "b", "d", "e"] [("a", "AA"), ("b", "BB"), ("c", "CC"), ("d", "DD"), ("e", "EE")])

-- MD21.B

bb :: (Eq a) => [(a, a)] -> [(a, a)] -> [(a, a)]
bb = compose filterOutDuplicates bbComposeDicts 

bbComposeDicts :: (Eq a) => [(a, a)] -> [(a, a)] -> [(a, a)]
bbComposeDicts _ [] = []
bbComposeDicts [] _ = []
bbComposeDicts (b:bs) (d:ds) = compose bbRemoveRedundantLayer map (\b -> bbMatchPairs b (d:ds)) (b:bs)

bbMatchPairs :: (Eq a) => (a, a) -> [(a, a)] -> [(a, a)]
bbMatchPairs _ [] = []
bbMatchPairs v (d:ds)
  | snd v == fst d = (fst v, snd d):bbMatchPairs v ds
  | otherwise = bbMatchPairs v ds

bbRemoveRedundantLayer :: (Eq a) => [[(a, a)]] -> [(a, a)]
bbRemoveRedundantLayer = concat

-- Expected: [("a","c"),("f","h")]
bb1 :: IO ()
bb1 = do
  putStrLn "Test bb1"
  print (bb [("a", "b"), ("a", "d"), ("a", "e"), ("e", "f"), ("f", "g")] [("b", "c"), ("d", "c"), ("a", "f"), ("c", "g"), ("g", "h")])

-- Expected: [(1,4),(3,4)]
bb2 :: IO ()
bb2 = do  
  putStrLn "Tests bb2:"
  print (bb [(1, 2), (3, 2), (2, 8), (8, 9), (1, 9)] [(1, 3), (7, 8), (2, 4), (5, 6), (1, 2)])

cc :: (Eq a) => [(a, a)] -> (Int, [(a, a)])
cc dictionary = ccExpandDictionary dictionary (1, dictionary)

ccExpandDictionary :: (Eq a) => [(a, a)] -> (Int, [(a, a)]) -> (Int, [(a, a)])
ccExpandDictionary dictionary (m, expanded)
  | expanded == result = (m, result)
  | otherwise = ccExpandDictionary dictionary (m + 1, result)
  where result = filterOutDuplicates (expanded ++ bb expanded dictionary)

-- Expected: 
-- (3,[(4,5),(5,6),(6,4),(6,7),(7,5),(4,6),(5,4),(5,7),(6,5),(7,6),(4,4),(4,7),(5,5),(6,6),(7,4),(7,7)])
cc1 :: IO ()
cc1 = do
  putStrLn "Tests cc1:"
  print (cc [(4, 5), (5, 6), (6, 4), (6, 7), (7, 5)])

-- Expected: 
-- (4,[("a","b"),("a","c"),("c","d"),("d","e"),("e","a"),("a","d"),("c","e"),("d","a"),("e","b"),("e","c"),("a","e"),("c","a"),("d","b"),("d","c"),("e","d"),("a","a"),("c","b"),("c","c"),("d","d"),("e","e")])
cc2 :: IO ()
cc2 = do
  putStrLn "Tests cc2:"
  print (cc [("a", "b"), ("a", "c"), ("c", "d"), ("d", "e"), ("e", "a")])

-- Demo
main :: IO ()
main = do
  aa1
  aa2
  bb1
  bb2
  cc1
  cc2