import System.Environment
import System.Random

data Prize = Car | Goat deriving (Eq, Show)
data Door = One | Two | Three deriving (Eq, Show, Read)


main = do 
    argList <- getArgs
    dispatch argList

dispatch :: [String] -> IO ()
dispatch ("play":_) = playGame
dispatch ("simulate":n:"keep":_) = runSimulation (read n) keepDoor
dispatch ("simulate":n:"switch":_) = runSimulation (read n) switchDoor
dispatch _ = help

playGame = do
    gen <- getStdGen
    let (door, gen') = pickDoor gen
    putStrLn "Pick your door! (1, 2, or 3)"
    contestantDoorRaw <- getLine
    let contestentDoor = toDoor contestantDoorRaw
        (eliminatedDoor, _) = eliminateDoor gen' door contestentDoor
    putStrLn $ "Monty opens door " ++ show eliminatedDoor ++ " revealing a Goat!"
    putStrLn $ "Do you want to switch doors? (Y or N)"
    choice <- getLine
    let prize = executeChoice choice door contestentDoor
    putStrLn $ "Congratulatons! You have won a " ++ show prize

-- todo: change return type to Either String Door
toDoor :: [Char] -> Door
toDoor "1" = One 
toDoor "2" = Two 
toDoor "3" = Three
toDoor bad = error $ "You must pick 1, 2, or 3... not " ++ bad

-- todo: change return type to Either String Door
executeChoice :: [Char] -> (Door -> Door -> Prize)
executeChoice "N" = keepDoor
executeChoice "Y" = switchDoor
executeChoice bad = error $ "You must pick either Y or N... not " ++ bad

runSimulation :: Int -> (Door -> Door -> Prize) -> IO ()
runSimulation n strategy = do
    gen <- getStdGen
    let wins = length $ filter (\x -> x == Car) $ take n $ simulate gen strategy
    putStrLn $ "won " ++ show wins ++ " out of " ++ show n ++ " games."

help = do
    putStrLn "TODO - create help message."

simulate :: StdGen -> (Door -> Door -> Prize) -> [Prize]
simulate gen strategy = 
    let (prize, gen') = simulateGame gen strategy
    in prize : simulate gen' strategy

simulateGame :: StdGen -> (Door -> Door -> Prize) -> (Prize, StdGen)
simulateGame gen strategy = 
    let (door, gen') = pickDoor gen
        (choice, gen'') = pickDoor gen'
        (remainingDoor, gen''') = eliminateDoor gen'' door choice
        prize = strategy door choice
    in (prize, gen''')


pickDoor :: StdGen ->  (Door, StdGen)
pickDoor gen = chooseOne gen [One, Two, Three]

eliminateDoor :: StdGen -> Door -> Door -> (Door, StdGen)
eliminateDoor gen One One = chooseOne gen [Two,Three]
eliminateDoor gen Two Two = chooseOne gen [One,Three]
eliminateDoor gen Three Three = chooseOne gen [One,Two]
eliminateDoor gen One Two = (One, gen)
eliminateDoor gen One Three = (One, gen)
eliminateDoor gen Two One = (Two, gen)
eliminateDoor gen Two Three = (Two, gen)
eliminateDoor gen Three One = (Three, gen)
eliminateDoor gen Three Two = (Three, gen)

chooseOne :: StdGen -> [Door] -> (Door, StdGen)
chooseOne gen xs =
    let n = (length xs) - 1
        (i, gen') = randomR (0,n) gen :: (Int, StdGen)
    in (xs !! i, gen')

switchDoor :: Door -> Door -> Prize
switchDoor car door 
    | car /= door = Car
    | otherwise   = Goat

keepDoor :: Door -> Door -> Prize
keepDoor car door 
    | car == door = Car
    | otherwise   = Goat
