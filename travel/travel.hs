-- A possible position of the car on the map
import Text.Printf

type Distance   = Double
type Degree     = Double
type Rad        = Double

type CarPos     = (Distance, Distance)
type TPath      = (Degree, [CarPos])

type KmH        = Double
type Minutes    = Double

data Direction  = Straight | TurnLeft | TurnRight deriving (Show, Eq)
type Leg        = (Direction, KmH, Minutes)

-- the entrie trip
type Trip = [Leg]

-- our trip
journey = 
    [ (Straight,  50, 2.0)
    , (TurnRight, 20, 4.0)
    , (TurnRight, 20, 1.0)
    , (TurnLeft,  50, 1.0)
    , (TurnRight, 20, 2.0)
    , (TurnLeft,  50, 0.5) ] :: Trip

deg2rad ::Degree -> Rad
deg2rad = (*) (pi/180) 

adj :: Degree -> Distance -> (Double -> Double) -> Distance
adj deg dist func = dist * ( func $ deg2rad deg  ) :: Distance

-- calculate new car position
newPos :: Degree -> Distance -> CarPos -> CarPos
newPos deg dist (x, y) = 
    let f = adj deg dist
    in (x + f sin, y + f cos)

-- d = t * v
getDist :: KmH -> Minutes -> Distance
getDist kmh mins = (kmh / 3.6) * (mins * 60)

stops :: TPath -> Leg -> TPath
stops (deg, a@(x:xs) ) (TurnRight, kmh, mins) = let nDeg = deg +   90  ; dist = getDist kmh mins in (nDeg, newPos nDeg dist x : a )
stops (deg, a@(x:xs) ) (TurnLeft,  kmh, mins) = let nDeg = deg + (-90) ; dist = getDist kmh mins in (nDeg, newPos nDeg dist x : a )
stops (deg, a@(x:xs) ) (_,         kmh, mins) = let nDeg = deg +    0  ; dist = getDist kmh mins in (nDeg, newPos nDeg dist x : a )

printArr :: CarPos -> IO ()
printArr (x, y) = printf "    {'x':%.2f, 'y':%.2f},\n" x y

main = do
    let trip = reverse.snd $ foldl stops (90,[(0,0)]) journey 
    putStrLn "["
    mapM_ printArr trip
    putStrLn "]"

