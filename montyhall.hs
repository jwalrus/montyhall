import System.Random

main = do
    gen <- getStdGen
    let (door, gen') = setupGame gen
        (choice, gen'') = contestantPickDoor gen'
        remainingDoor = eliminateDoor gen'' door choice
    putStrLn $ "The car is behind door number " ++ show door
    putStrLn $ "The contestent chose door number " ++ show choice
    putStrLn $ "The remaining choice is " ++ show remainingDoor
    putStrLn $ "You win a " ++ (keepDoor door choice)


setupGame :: StdGen -> (Int, StdGen)
setupGame gen = randomR (1,3) gen :: (Int, StdGen)

contestantPickDoor :: StdGen -> (Int, StdGen)
contestantPickDoor gen = randomR (1,3) gen :: (Int, StdGen)

eliminateDoor :: StdGen -> Int -> Int -> Int
eliminateDoor gen 1 1 = chooseOne gen [2,3]
eliminateDoor gen 2 2 = chooseOne gen [1,3]
eliminateDoor gen 3 3 = chooseOne gen [1,2]
eliminateDoor _ 1 2 = 1
eliminateDoor _ 1 3 = 1
eliminateDoor _ 2 1 = 2
eliminateDoor _ 2 3 = 2
eliminateDoor _ 3 1 = 3
eliminateDoor _ 3 2 = 3

chooseOne :: StdGen -> [Int] -> Int
chooseOne gen xs =
    let n = (length xs) - 1
        (i, _) = randomR (0,n) gen :: (Int, StdGen)
    in xs !! i

switchDoor :: Int -> Int -> String
switchDoor car door 
    | car == door = "car!!!"
    | otherwise   = "goat :("

keepDoor :: Int -> Int -> String
keepDoor car door 
    | car == door = "car!!!"
    | otherwise   = "goat :("
