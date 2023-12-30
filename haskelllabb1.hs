-- ////////////////////////////////////////////////////////
-- ASSIGNMENT 1

type Cplex = (String, Float, Float)

-- (Type, Real, Imaginary) 
makeRec :: Float -> Float -> Cplex
makeRec a b = ("R", a, b)

makePol :: Float -> Float -> Cplex
makePol r v
    | v > pi*2 && v < 0 = error "Angle needs to be between 0 and 2*pi"
    | otherwise = ("P", r, v)


-- ////////////////////////////////////////////////////////


-- ////////////////////////////////////////////////////////
-- ASSIGNMENT 2

--For polarform it's the same as converting r from r(cos(v) + isin(v)) to a in rectangularform Z = a + bi
getRe :: Cplex -> Float
getRe ("R",a,b) = a
getRe ("P",r,v) = r * cos v
getRe _ = error "Not complex"

--For polarform it's the same as converting v from r(cos(v) + isin(v)) to b in rectangularform Z = a + bi
getIm :: Cplex -> Float
getIm ("R",a,b) = b
getIm ("P",r,v) = r * sin v
getIm _ = error "Not complex"

-- r = |Z|, The hypotenuse from the coords a&b
getDist :: Cplex -> Float
getDist ("P",r,v) = r
getDist ("R",a,b) = sqrt(a^2 + b^2)
getDist _ = error "Not complex"

-- v, All quadrants accounted for
getAngle :: Cplex -> Float
getAngle ("P",r,v) = v
getAngle ("R",a,b) --("R", -1, -2)
    | a > 0 && b > 0 = v
    | a > 0 && b < 0 = v + (2*pi)
    | a < 0 = v + pi  --(atan(b/a)) + pi
    | b >= 0 && a == 0 = pi/2
    | b < 0 && a == 0 = (3*pi)/2
    | otherwise = 0
    where v = atan (b/a)
getAngle _ = error "Not complex"
-- ////////////////////////////////////////////////////////


-- ////////////////////////////////////////////////////////
-- ASSIGNMENT 3 
-- Two converters

-- ("R",a,b) | Z = a + bi

toRec :: Cplex -> Cplex
toRec ("P", r, v) = makeRec (getRe ("P",r,v)) (getIm ("P",r,v))


-- ("P",r,v) | Z = r*(cos(v)+isin(v))

toPol :: Cplex -> Cplex
toPol ("R", a, b) = makePol (getDist ("R",a,b)) (getAngle ("R",a,b))


-- ////////////////////////////////////////////////////////

-- ////////////////////////////////////////////////////////
-- ASSIGNMENT 4 

-- Addition and Subtractions is prefered to be done in rectangular form, Convert any polar form before calculations are done. 
-- (𝑎+𝑏𝑖)+(𝑐+𝑑𝑖)=(𝑎+𝑐)+(𝑏+𝑑)𝑖, Namely new a = a+c & new b = b+d
compAdd :: Cplex -> Cplex -> Cplex
compAdd (f1, a, b) (f2 , c, d) = makeRec (getRe (f1, a, b) + getRe (f2 , c, d)) (getIm (f1, a, b) + getIm (f2 , c, d))


-- (𝑎+𝑏𝑖)−(𝑐+𝑑𝑖)=(𝑎−𝑐)+(𝑏−𝑑)𝑖, Namely new a = a-c & new b = b-d
compSub :: Cplex -> Cplex -> Cplex
compSub (f1, a, b) (f2 , c, d) = makeRec (getRe (f1, a, b) - getRe (f2 , c, d)) (getIm (f1, a, b) - getIm (f2 , c, d))

-- Multiplication and Division is prefered to be done in polar form, Convert any rectangular form before calculations are done.
-- z1*z2 = r1*r2(cos(v1 + v2) + isin(v1 + v2)), Namely new r = r1*r2 & new v = v1 + v2
compMult :: Cplex -> Cplex -> Cplex
compMult (f1, a, b) (f2 , c, d) = makePol (getRe (f1, a, b) * getRe (f2 , c, d)) (getIm (f1, a, b) + getIm (f2 , c, d))

-- (z1/z2) = (r1/r2)*(cos(v1-v2) + isin(v1-v2)), Namely new r = r1/r2 & new v = v1-v2   
compDiv :: Cplex -> Cplex -> Cplex
compDiv (f1, a, b) (f2 , c, d) = makePol (getRe (f1, a, b) / getRe (f2 , c, d)) (getIm (f1, a, b) - getIm (f2 , c, d))
-- ////////////////////////////////////////////////////////
-- ASSIGNMENT 5

genCompList :: (Float, Float) -> (Float, Float) -> [Cplex]
genCompList (x, y) (z, w) = [makeRec a b | a <- [x..y], b <- [z..w]]

listToPol :: [Cplex] -> [Cplex]
listToPol xs = [toPol z | z <- xs]

filterLengths :: Float -> [Cplex] -> [Cplex]
filterLengths k xs = [z | z <- xs, getDist z <= k]



filterQuadrant :: Int -> [Cplex] -> [Cplex]
filterQuadrant m xs = [(f,a,b) | (f,a,b) <- xs, predicate (f,a,b)] 
    where predicate (f,a,b)
            | m == 1 = v > 0 && v < pi/2 
            | m == 2 = v > pi/2 && v < pi 
            | m == 3 = v > pi && v < (3*pi)/2
            | m == 4 = v > (3*pi)/2 && v < (2*pi)
            | otherwise = error "Invalid quadrant" 
            where v = getAngle (f,a,b)


-- ////////////////////////////////////////////////////////

