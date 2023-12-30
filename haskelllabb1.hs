--Vi vet att vi kan importera funktioner från exempelvis Data.List så man kunde ha kortat ner en del grejer, Men vi valde att försöka göra allt på egen hand.

-- Uppgift 1
type Board = [[Integer]]
type Position = (Int,Int)
type State = (Board, [Action])

-- Uppgift 2
--Definera en ny typklass namngiven "Action", Där L, U, R, D tillhör den. Tilldela all funktionalitet från EQ klassen, Så exempelvis L U R D kan jämföras (==). 
-- Instansen är det specifika som tillhör den nya typklassen Action, Där specificeras att show för respektive L U R D, kommer returnera ngt som definerat nedan.
data Action = L | U | R | D deriving Eq
instance Show Action where
    show L = "Left"
    show U = "Up"
    show R = "Right"
    show D = "Down"


-- Uppgift 3
--Representerade som rader
solution :: [[Integer]]
solution = [[1,2,3],[4,5,6],[7,8,-1]] --Upp<-->Ner

isSolved :: Board -> Bool
isSolved xs = xs == solution

--Uppgift 4
findEmpty :: Board -> Position 
findEmpty xys = head $ concat $ zipWith (\ xs y -> ([(x, y) | (a, x) <- zip xs [1..], a == (-1)])) xys [1..]

--Uppgift 5
replace :: Position -> Integer -> Board -> Board 
replace (x,y) a xys = take (y-1) xys ++ [xs] ++ drop y xys
                      where xs = take (x-1) (xys!!(y-1)) ++ [a] ++ drop x (xys!!(y-1))

--Uppgift 6
swap :: Position -> Position -> Board -> Board
swap (x1,y1) (x2,y2) xys = replace (x2,y2) a $ replace (x1,y1) b xys
                           where
                               b = xys!!(y2-1)!!(x2-1)  -- Värde från (x2,y2) ska ersätta (x1,y1)
                               a = xys!!(y1-1)!!(x1-1)  -- Värde från (x1,y1) ska ersätta (x2,y2)

--Uppgift 7
makeMove :: Board -> Action -> Board
makeMove xys mv
    | null xys = []
    | isSolved xys = []
    | mv == R && x /= 3 = swap (x,y) (x+1,y) xys
    | mv == L && x /= 1 = swap (x,y) (x-1,y) xys
    | mv == U && y /= 1 = swap (x,y) (x,y-1) xys
    | mv == D && y /= 3 = swap (x,y) (x,y+1) xys
    | otherwise = []
    where (x,y) = findEmpty xys

--Uppgift 8
allFutures :: State -> [State]
allFutures (xys, ab) = filter (\(xs, a) -> not $ null xs) [(makeMove xys L, ab++[L]),(makeMove xys R, ab++[R]),(makeMove xys U, ab++[U]),(makeMove xys D, ab++[D])]

--Uppgift 9
possibleSolution :: Board -> [[State]]
possibleSolution xys = iterate (concatMap allFutures) [(xys, [])]


--Uppgift 10 
--Tar för lång tid att hitta lösningar som kräver mer än typ 10 handlingar... 
solve :: Board -> [Action]
solve board = snd $ head $ filter (\(xs,_) -> isSolved xs) (concat $ take 10 $ possibleSolution board) 


--Uppgift 11
--Lösning finns i körningsexemplet längst ner
testBoard = [[1,5,8],[7,6,-1],[4,2,3]]
testBoard1 = [[2,5,3],[4,1,6],[-1,7,8]] 
testBoard2 = [[1,2,3],[7,6,8],[5,4,-1]]
