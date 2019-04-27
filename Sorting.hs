module Sorting
(
     insert'
    ,insertionSort
    ,columnize
    ,decolumnize
    ,shellSortPhase
    ,shellSort
    ,msort
    ,merge
    ,pmsort
    ,bmsort
    ,pbmsort
    ,bmerge
    ,pbmerge
    ,insertAt
    ,swapElems
    ,stoogeSort
    ,doss
    ,rank
    ,mk
    ,merge'
    ,toList
    ,heapSort
    ,beadSort
    ,bubbleSort
    ,cocktailSort   
    ,introSort
    ,selection_sort'
    ,strandSort
    ,tree_sort
    ,timesort
    ,merge'''
    ,quicksort
    ,mergesort
)where



    import Control.Applicative
    import Control.Arrow 
    import Control.Monad
    import Data.Function 
    import Data.List 
    import Data.Ord
    import Text.Printf 
    import Control.Applicative
    import Control.Arrow (second)
    import Control.Monad (liftM, replicateM)
    import Control.Monad.Random
    import Data.Function (on)
    import Data.List 
    import Data.Ord (comparing)
    import Text.Printf (printf)
    import Test.QuickCheck
    import Test.QuickCheck.Monadic
    import Test.HUnit 
    import Data.List (transpose)
    import Control.Monad.Par
    import Control.Concurrent 
    import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan, dupChan)
    import Control.Exception (Exception, throw)
    import  Control.Monad.Loops (iterateUntil)
    import  Control.Monad.Primitive (PrimMonad, PrimState)
    import  Data.Typeable (Typeable)
    import  GHC.Conc (getNumCapabilities)
    import  Data.Bits (shift, (.&.), Bits)
    import  Data.IORef
    import qualified Data.Vector.Unboxed.Mutable as MV
    import System.IO.Unsafe (unsafePerformIO)
    import Data.Bits
    import Data.List
    


    insert' :: Ord a => a -> [a] -> [a]
    insert' x [] = [x]
    insert' x (y:ys) 
     | x <= y    = x : y:ys
     | otherwise = y : insert' x ys
        
    insertionSort :: Ord a => [a] -> [a]
    insertionSort = foldr insert' []
     
        
    columnize :: Int -> [a] -> [[a]]
    columnize k = transpose . takeWhile (not . null) . map (take k) . iterate (drop k)
        
    decolumnize :: [[a]] -> [a]
    decolumnize = concat . transpose
        
        
    
    shellSortPhase :: (Ord a) => Int -> [a] -> [a]
    shellSortPhase k = decolumnize . map insertionSort . columnize k 
        
       
    shellSort :: (Ord a) => [a] -> [a]
    shellSort xs = foldr shellSortPhase xs gaps
      where gaps = takeWhile (< length xs) sedgewick
            sedgewick = concat [[9 * 2^n - 9 * 2^(n `div` 2) + 1, 8 * 2^(n+1) - 6 * 2^(n `div` 2) + 1] | n <- [0..]]
    
    ----------------------------------------------------------------------------------------------------------------------------------
    
    
    msort :: (Ord a) => [a] -> [a]
    msort [] = []
    msort [x] = [x]
    msort xs = let (ys, zs) = splitAt (length xs `div` 2) xs
                 in merge (msort ys) (msort zs)
    
    merge :: (Ord a) => [a] -> [a] -> [a]
    merge [] xs = xs
    merge xs [] = xs
    merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                        | otherwise = y : merge (x:xs) ys
    
    pmsort :: (Ord a, NFData a) => [a] -> Int -> [a]
    pmsort xs cutoff | length xs <= cutoff = msort xs
                     | otherwise           = runPar $ do
      let (ys, zs) = splitAt (length xs `div` 2) xs
      sys <- spawnP (pmsort ys cutoff)
      szs <- spawnP (pmsort zs cutoff)
      gys <- get sys
      gzs <- get szs
      return $ merge gys gzs
    
    bmsort :: (Ord a) => [a] -> [a]
    bmsort []  = []
    bmsort [x] = [x]
    bmsort xs  = let (ys, zs) = splitAt (length xs `div` 2) xs
                   in bmerge (bmsort ys) ++ reverse (bmsort zs)
    
    pbmsort :: (Ord a, NFData a) => [a] -> Int -> [a]
    pbmsort xs cutoff | length xs <= cutoff = msort xs
                      | otherwise           = runPar $ do
      let (ys, zs) = splitAt (length xs `div` 2) xs
      sys <- spawnP (pbmsort ys cutoff)
      szs <- spawnP (reverse $ pbmsort zs cutoff)
      gys <- get sys
      gzs <- get szs
      return $ pbmerge (gys ++ gzs) cutoff
    
    bmerge :: (Ord a) => [a] -> [a]
    bmerge [] = []
    bmerge [x] = [x]
    bmerge xs = let (ys, zs) = splitAt (length xs `div` 2) xs
                    (mins, maxs) = unzip [(min x y, max x y) | (x, y) <- zip ys zs]
                      in bmerge mins ++ bmerge maxs
    
    -- nf ->A typical use is to prevent resource leaks in lazy IO programs, by forcing all characters from a file to be read(normal form)
    pbmerge :: (Ord a, NFData a) => [a] -> Int -> [a] 
    pbmerge xs cutoff | length xs <= cutoff = bmerge xs
                      | otherwise           =
                 let (ys, zs) = splitAt (length xs `div` 2) xs
                     (mins, maxs) = unzip [(min x y, max x y) | (x, y) <- zip ys zs]
                      in runPar $ do
                        mmins <- spawnP (pbmerge mins cutoff)
                        mmaxs <- spawnP (pbmerge maxs cutoff)
                        gmins <- get mmins
                        gmaxs <- get mmaxs
                        return $ gmins ++ gmaxs
    ----------------------------------------------------------------------------------------------------------------------------------
    insertAt e k = uncurry(++).second ((e:).drop 1). splitAt k
     
    swapElems :: [a] -> Int -> Int -> [a]
    swapElems xs i j = insertAt (xs!!j) i $ insertAt (xs!!i) j xs --($ for avoiding extra parthn,lazy doesnt compute)--
     
    stoogeSort [] = []
    stoogeSort [x] = [x]
    stoogeSort xs = doss 0 (length xs - 1) xs
    
    doss :: (Ord a) => Int -> Int -> [a] -> [a]
    doss i j xs
          | j-i>1 = doss i (j-t) $ doss (i+t) j $ doss i (j-t) xs
          | otherwise = xs'
       where t = (j-i+1)`div`3
             xs'
               |xs!!j < xs!!i = swapElems xs i j
               |otherwise = xs
    
    
    ----------------------------------------------------------------------------------------------------------------------------------
    
    data Heap a = E | T Int a (Heap a) (Heap a) 
    
    rank E = 0 
    rank (T r _ _ _) = r 
    
    
    mk x a b = 
       if rank a > rank b then 
           T (rank b + 1) x a b 
    
      else 
          T (rank a + 1) x b a 
    
    ----"@" read as ----
    merge' h E = h 
    merge' E h = h 
    merge' h1@(T _ x a1 b1) h2@(T _ y a2 b2) = 
        if x >= y then mk x a1 (merge' b1 h2) else mk y a2 (merge' h1 b2) 
    
    
    toList xs E = xs 
    toList xs (T _ x a b) = toList (x:xs) $ merge' a b 
    
    
    heapSort xs = toList [] (foldr (\x -> \h -> merge' (mk x E E) h) E xs)
    
    ----------------------------------------------------------------------------------------------------------------------


    -------------------------------------------------------------------------------------------------------------------------
     
    beadSort :: [Int] -> [Int] 
    beadSort = reverse.map sum. transpose. transpose. map (flip replicate 1)
    
    -------------------------------------------------------------------------------------------------

    bubbleSort :: Ord a => [a]->[a]
    traverseForward :: Ord a => [a]->[a]
    helper1Bubble :: Ord a => [a]-> Int -> [a]
    
    traverseForward [] = []
    traverseForward [x] = [x]
    traverseForward (x:y:xs)
     |x > y = y:(traverseForward (x:xs)) 
     |otherwise = x:(traverseForward (y:xs))
     
    helper1Bubble xs 0 = xs
    helper1Bubble xs n = helper1Bubble (traverseForward xs) (n - 1)
    
    bubbleSort xs = helper1Bubble xs (length xs)
    

    cocktailSort :: Ord a => [a]->[a]
    traverseBackward :: Ord a => [a]->[a]
    traverseForwardSmaller :: Ord a => [a]->[a]
    helperCocktail :: Ord a => [a]-> Int -> [a]

    traverseForwardSmaller [] = []
    traverseForwardSmaller [x] = [x]
    traverseForwardSmaller (x:y:xs)
      |x < y = y:(traverseForwardSmaller (x:xs)) 
      |otherwise = x:(traverseForwardSmaller (y:xs))
 
    traverseBackward xs = reverse (traverseForwardSmaller (reverse xs))

    helperCocktail xs 0 = xs
    helperCocktail xs n 
     |mod n 2 == 1 = helperCocktail (traverseForward xs) (n - 1)
     |otherwise = helperCocktail (traverseBackward xs) (n - 1)

    cocktailSort xs = helperCocktail xs (length xs)


    


    introSort :: Ord a => [a]->[a]
    introSort (x:xs)
     |(length xs) < 16 = insertionSort xs
     |(2 * log (fromIntegral (length xs)) == 0) = heapSort xs
     |otherwise = introSort [y| y<-xs, y <= x] ++ [x] ++ introSort[y| y <- xs , y > x]
 


    half::[Int]->Int
    half xs = length xs

    merge_sort::[Int]->[Int]
    merge_sort [] = []
    merge_sort [x] = [x]
    merge_sort xs = merge (merge_sort ys) (merge_sort zs)
                  where ys = take (div (half xs) 2) xs
                        zs = drop (div (half xs) 2) xs
                          
    merge''::[Int]->[Int]->[Int]
    merge'' [] [] = []
    merge'' xs [] = xs
    merge'' [] ys = ys
    merge'' (x:xs) (y:ys) 
     | x >= y = y:(merge'' (x:xs) ys)
     | y >= x = x:(merge'' xs (y:ys)) 



    selection_sort'::[Int]->[Int]

    selection_sort' [] = []
    selection_sort' [x] = [x]
    selection_sort' xs = x:(selection_sort' ys)
                    where x = minimum xs
                          ys = delete x xs
     



    gnome_sort::[Int]->[Int]
    gnome_sort [] = []
    gnome_sort [x] = [x]
    gnome_sort (x:y:xs) 
     | x >= y = merge'' [y] (gnome_sort (x:xs))
     | y >= x = merge'' [x] (gnome_sort (y:xs))
     | x > y  = merge'' [y] (gnome_sort (x:xs))
                          




    tree_sort::[Int]->[Int]

    tree_sort [] = []
    tree_sort (x:xs) = tree_sort[y|y<-xs,y<x] ++ [x] ++ tree_sort[y|y<-xs,y>=x]
     



    strandSort :: Ord a => [a] -> [a]
    helpStrand :: Ord a => [a] -> [a] -> [a] -> [a] -> Int ->  [a]
    
    helpStrand [] xs ys  [] _ = (xs ++ ys)
    helpStrand (x:xs) [] [] [] _ = helpStrand xs [x] [] [] (length xs) 
    helpStrand (x:xs) [] ys [] _ = helpStrand xs [x] ys [] (length xs) 
    helpStrand xs [] ys (x:ignored) n = helpStrand (xs ++ ignored)[x] ys [] (length(ignored))
    helpStrand xs ys zs ignored 0  = helpStrand (xs ++ ignored) [] (ys ++ zs) [] (length ignored)
    helpStrand (x:xs) (ys) (zs) ignored n 
     |x >= last (ys) = helpStrand xs (reverse(x:reverse(ys))) zs ignored (n - 1)
     |otherwise =  helpStrand xs ys zs (x:ignored) (n-1)
     
    strandSort xs = helpStrand (xs) [] [] [] (length(xs))    





    quicksort :: Ord a => [a] -> [a]
    quicksort []     = []
    quicksort (x:xs) = quicksort pre ++ [x] ++ quicksort post
        where
            pre  = filter (< x) xs
            post = filter (>= x) xs




        
    mergesort ::[Int]->[Int]
    mergesort [] = []
    mergesort [a] = [a]
    mergesort a = merge(mergesort firsthalf)(mergesort secondhalf)
                   where firsthalf = take ((length a) `div` 2) a
                         secondhalf = drop ((length a) `div` 2) a




    insert:: Int->[Int]->[Int]
    insert x [] =[x]
    insert x (y:ys)
     |x<y = x:y:ys
     |otherwise = y:Sorting.insert x ys







    timesort :: [Int]->[Int]
    timesort [] = []
    timesort [a] = [a]
    timesort a = merge (insertionSort firsthalf) (insertionSort secondhalf)
                  where firsthalf = take ((length a) `div` 2) a
                        secondhalf = drop ((length a) `div` 2) a




    merge''' :: [Int] -> [Int] -> [Int]
    merge''' a [] = a
    merge''' [] b = b
    merge''' (a:as) (b:bs)
     | a < b     = a:(merge''' as (b:bs))
     | otherwise = b:(merge''' (a:as) bs)