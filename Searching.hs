module Searching
(
    printGraph
    ,linearsearch
    ,interSearch
    ,binarySearch
    ,jumpSearch
)
where

    


   


    --import Proj.Sorting
    data Vertex = Vertex {
                          vertexLabel :: [Char]
                        , vertexNeighbors :: [[Char]]
                        , vertexDistance :: Int
                        , vertexPredecessor :: [Char]
                      } deriving (Show)

    data Graph = Graph [Vertex] deriving (Show)

    vertexInVertexes :: Vertex -> [Vertex] -> Bool
    vertexInVertexes _ [] = False
    vertexInVertexes Vertex {vertexLabel = label} (x:y) = foldl (\ acc x -> vertexLabel x == label || acc) False (x:y)

    graphVertexes :: Graph -> [[Char]]-> [Vertex]
    graphVertexes (Graph []) _ = []
    graphVertexes (Graph (x:y)) [] = x : y
    graphVertexes (Graph (x:y)) keys = filter (\ z -> vertexLabel z `elem` keys) (x:y)


    bfs :: Graph -> Graph -> [Vertex] -> [Vertex] -> Graph
    bfs (Graph []) _ _ _ = Graph []
    bfs _ outGraph [] _ = outGraph
    bfs (Graph (a:b)) (Graph (c:d)) (e:f) (g:h) = bfs inGraph outGraph queue seen'
      where inGraph = Graph (a:b)
            eLabel= vertexLabel e
            eNeighbors = vertexNeighbors e
            eVertexNeighbors = graphVertexes inGraph eNeighbors
            dist = vertexDistance e + 1
            seen = g : h
          
            filteredNeighbors = filterVertexNeighbors seen eVertexNeighbors
            enqueue = updateDistPred filteredNeighbors dist eLabel
            outGraph = Graph $ (c:d) ++ enqueue
            queue = f ++ enqueue
            seen' = seen ++ enqueue


        
    filterVertexNeighbors :: [Vertex] -> [Vertex] -> [Vertex]
    filterVertexNeighbors _ [] = []
    filterVertexNeighbors [] _ = []        
    filterVertexNeighbors s vn = filter (\ x -> not $ vertexInVertexes x s) vn


    updateDistPred :: [Vertex] -> Int -> [Char] -> [Vertex]
    updateDistPred [] _ _ = []
    updateDistPred (x:y) dist predLabel = map (\ (Vertex label n _ _) -> Vertex label n dist predLabel) (x:y)


    main :: IO ()
    main = do
        let inGraph = Graph [
                      Vertex "a" ["b", "c"          ] 0 ""
                    , Vertex "b" ["a", "d", "e"     ] 0 ""
                    , Vertex "c" ["a", "d"          ] 0 ""
                    , Vertex "d" ["b", "c", "e"     ] 0 ""
                    , Vertex "e" ["b", "d", "f", "g"] 0 ""
                    , Vertex "f" ["e", "g", "h"     ] 0 ""
                    , Vertex "g" ["e", "f", "i"     ] 0 ""
                    , Vertex "h" ["f", "i"          ] 0 ""
                    , Vertex "i" ["g", "h"          ] 0 ""
                          ]
        let queue = graphVertexes inGraph ["e"]
        let outGraph = Graph queue
        let seen = queue
        printGraph $ bfs inGraph outGraph queue seen
        return ()



    printGraph :: Graph -> IO ()
    printGraph (Graph []) = putStrLn ""
    printGraph (Graph (x:y)) = do
        print x
        printGraph (Graph y)
        return ()






  
    linearsearch ::Eq a=> a -> [a] -> Bool
    linearsearch _ [] = False
    linearsearch y (x:xs)= y==x || linearsearch y xs
    



    interSearch ::[Int] -> Int -> Bool
    helpInter :: [Int] -> Int -> Int -> Int -> Bool
    helpInter xs key low high 
      |low > high = False
      |xs !! low > key = False
      |xs !! high < key = False  
      |xs !! index == key = True
      |xs !! index > key = helpInter xs key (low)(index - 1)
      |xs !! index < key = helpInter xs key (index + 1)(high)
        where index = low + (div (high - low) ((xs !! high) - (xs !! low))) * (key - xs !! low)

    interSearch xs key = helpInter xs key 0 ((length xs) - 1)




    binarySearch :: Ord a => [a] -> a -> Bool
    binarySearch [] _ = False
    binarySearch xs key
     |(xs !! index) == key = True
     |(xs !! index) < key = binarySearch ([y | y <- xs , y > xs !! index]) key
     |(xs !! index) > key = binarySearch ([y | y <- xs , y < xs !! index]) key
       where index = div (length xs) 2



    jumpSearch :: Ord a => [a] -> a -> Bool
    helperJump :: Ord a => [a] -> a -> Int -> Int -> Bool
    intSquareRoot :: Int -> Int
    intSquareRoot n = aux n
      where
        aux x
         | x*x > n = aux (x - 1)
         | otherwise = x
             
    linearSearch' :: Ord a => [a] -> a -> Int -> Int -> Bool
    linearSearch' xs key begin ending 
      |begin > ending = False
      |xs !! begin == key = True
      |xs !! begin < key = linearSearch' xs key (begin + 1) ending
      |xs !! begin > key = False
        
       
    helperJump xs key index jumpValue
     |length xs < index = False
     |xs !! 0 > key = False
     |xs !! (length xs - 1) < key = False
     |xs !! index == key = True
     |xs !! index < key = helperJump xs key (index + jumpValue - 1) jumpValue
     |xs !! index > key && index == 0 = linearSearch' xs key index jumpValue
     |xs !! index > key && index > 0= linearSearch' xs key (index - jumpValue + 1) (index)
       
    jumpSearch xs key = helperJump xs key 0 (intSquareRoot (length xs))
       