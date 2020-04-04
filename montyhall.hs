import System.Environment
import System.Random

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
    let (door, gen') = setupGame gen
        (choice, gen'') = contestantPickDoor gen'
        (remainingDoor, gen''') = eliminateDoor gen'' door choice
    putStrLn $ "The car is behind door number " ++ show door
    putStrLn $ "The contestent chose door number " ++ show choice
    putStrLn $ "The remaining choice is " ++ show remainingDoor
    let prize = keepDoor door choice
    putStrLn $ "You win a " ++ prize
    let (sim, _) = simulateGame gen switchDoor
    putStrLn $ "simulation result = " ++ sim

runSimulation :: Int -> (Int -> Int -> String) -> IO ()
runSimulation n strategy = do
    gen <- getStdGen
    let wins = length $ filter (\x -> x == "car!!!") $ take n $ simulate gen strategy
    putStrLn $ "won " ++ show wins ++ " out of " ++ show n ++ " games."

help = do
    putStrLn "TODO - create help message."

simulate :: StdGen -> (Int -> Int -> String) -> [String]
simulate gen strategy = 
    let (prize, gen') = simulateGame gen strategy
    in prize : simulate gen' strategy

simulateGame :: StdGen -> (Int -> Int -> String) -> (String, StdGen)
simulateGame gen strategy = 
    let (door, gen') = setupGame gen
        (choice, gen'') = contestantPickDoor gen'
        (remainingDoor, gen''') = eliminateDoor gen'' door choice
        prize = strategy door choice
    in (prize, gen''')


setupGame :: StdGen ->  (Int, StdGen)
setupGame gen = randomR (1,3) gen :: (Int, StdGen)

contestantPickDoor :: StdGen -> (Int, StdGen)
contestantPickDoor gen = randomR (1,3) gen :: (Int, StdGen)

eliminateDoor :: StdGen -> Int -> Int -> (Int, StdGen)
eliminateDoor gen 1 1 = chooseOne gen [2,3]
eliminateDoor gen 2 2 = chooseOne gen [1,3]
eliminateDoor gen 3 3 = chooseOne gen [1,2]
eliminateDoor gen 1 2 = (1, gen)
eliminateDoor gen 1 3 = (1, gen)
eliminateDoor gen 2 1 = (2, gen)
eliminateDoor gen 2 3 = (2, gen)
eliminateDoor gen 3 1 = (3, gen)
eliminateDoor gen 3 2 = (3, gen)

chooseOne :: StdGen -> [Int] -> (Int, StdGen)
chooseOne gen xs =
    let n = (length xs) - 1
        (i, gen') = randomR (0,n) gen :: (Int, StdGen)
    in (xs !! i, gen')

switchDoor :: Int -> Int -> String
switchDoor car door 
    | car /= door = "car!!!"
    | otherwise   = "goat :("

keepDoor :: Int -> Int -> String
keepDoor car door 
    | car == door = "car!!!"
    | otherwise   = "goat :("
