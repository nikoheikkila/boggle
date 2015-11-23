module Main where
import qualified Data.Vector as Vec
import qualified Data.Set as Set
import System.Random
import Data.Char
import Data.List
import Data.Maybe
import System.Console.ANSI
import System.Exit
import Text.Read

offsets :: [(Int, Int)]
offsets = [(1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1)]

data Board = Board {gameGrid :: Vec.Vector Char
                   ,foundWords :: Set.Set String
                   ,allWords :: Set.Set String
                   ,players :: Vec.Vector Player
                   ,wordFinder :: WordFinder
                   ,width :: Int
                   ,height :: Int}
                   deriving (Eq, Show)

newtype WordFinder = WordFinder {knownWords :: Set.Set String}
            deriving(Eq, Show)

newtype Player = Player {points :: Int}
            deriving(Eq, Show)

instance Ord Player where
  (Player p1) <= (Player p2) = p1 <= p2

newBasicBoard :: Int -> Int -> IO Board
newBasicBoard size playerNum = do
    g <- newStdGen
    let grid = take (size * size) (randomRs ('A', 'Z') g)
    wf <- newWordFinder
    let board = findAllWords (Board (Vec.fromList grid) Set.empty Set.empty (Vec.replicate playerNum (Player 0)) wf size size)
    if Set.size (allWords board) == 0 then
      newBasicBoard size playerNum
     else
      return board

newWordBoard :: String -> Int -> Int -> IO Board
newWordBoard grid size playerNum = do
    g <- newStdGen
    wf <- newWordFinder
    let board = findAllWords (Board (Vec.fromList grid) Set.empty Set.empty (Vec.replicate playerNum (Player 0)) wf size size)
    if Set.size (allWords board) == 0 then
      newBasicBoard size playerNum
     else
      return board


newWordFinder :: IO WordFinder
newWordFinder = do
 file <- readFile "word_list.txt"
 return (WordFinder (Set.fromList (words file)))

findAllWords :: Board -> Board
findAllWords board = board {allWords = Set.fromList (map fromJust . filter (/=Nothing) $ [findWord (x, y) board | x <- [0..(width board - 1)], y <- [0..(height board - 1)]])}

findWord :: (Int, Int) -> Board -> Maybe String
findWord (sx, sy) board = foundString
  where
    (foundString, _) = traverse (sx, sy) "" Set.empty
    traverse :: (Int, Int) -> String -> Set.Set (Int, Int) -> (Maybe String, Set.Set (Int, Int))
    traverse (x, y) word traversed
     | x < 0 || x >= width board || y < 0 || y >= height board = (Nothing, traversed)
     | Set.member (x, y) traversed = (Nothing, traversed)
     | not (isGoodWordPart (Set.lookupGE newWord (knownWords wf))) = (Nothing, traversed)
     | isAWord && isNotFound && len >= 4 = (Just newWord, newPath)
     | otherwise = checkNeighbor offsets
      where
       wf = wordFinder board
       c = (gameGrid board) Vec.! (x + y * (width board)) 
       newWord = word ++ [toLower c]
       len = length newWord
       isAWord = Set.member newWord (knownWords wf)
       isNotFound = not (Set.member newWord (foundWords board))

       newPath = Set.insert (x, y) traversed

       isGoodWordPart (Just s) = isPrefixOf newWord s
       isGoodWordPart Nothing = False

       checkNeighbor :: [(Int, Int)] -> (Maybe String, Set.Set (Int, Int))
       checkNeighbor [] = (Nothing, traversed)
       checkNeighbor ((ox, oy):offs) = 
        case traverse (x + ox, y + oy) newWord newPath of
          (Just w, t) -> (Just w, t)
          (Nothing, t) -> checkNeighbor offs

computePoints :: String -> Int
computePoints word
 | 4 <= len && len <= 6 = len - 3
 | len == 7 = 5
 | len == 8 = 11
 | len >= 9 = len * 2
 | otherwise = 0
  where
    len = length word

toLowStr :: String -> String
toLowStr [] = []
toLowStr (c:str) = toLower c : str

main = do
    setTitle "I AM BOGGLE"
    setCursorPosition 0 0
    clearScreen
    putStrLn "########################"
    putStrLn "######I AM BOGGLE!######"
    putStrLn "########################\n"
    putStrLn "General commands:"
    putStrLn ":q -- quit the game"
    putStrLn ":end -- end player's turn\n"
    putStrLn "How big a grid do you want to create?"
    let prompt :: String -> Int -> IO Int
        prompt msg lBound = do
               putStr msg
               val <- getLine
               case val of
                ":q" -> exitSuccess
                _ -> case readMaybe val of
                      Just v -> if v < lBound then putStrLn "That's a tad too small" >> (prompt msg lBound) else return v
                      _ -> putStrLn "Invalid number." >> prompt msg lBound
    gridSize <- prompt "Enter the size of the grid (at least 2): " 2
    playerNumber <- prompt "Enter player number (at least 1): " 1 
    putStrLn "Loading a board with some words in it"
    board <- newBasicBoard gridSize playerNumber

    let printBoard :: Board -> IO ()
        printBoard board = do
                           let lines = [spacefy (Vec.toList (Vec.take w s)) | i <- [0..(w - 1)], let s = Vec.drop (i * h) grid]
                                where
                                  spacefy [] = ' ' : []
                                  spacefy (c:str) = ' ' : c : spacefy str
                                  w = width board
                                  h = height board
                                  grid = gameGrid board
                           putStrLn $ unlines lines
    let gameLoop :: Board -> String -> Int -> IO (Board)
        gameLoop board status currentPlayer = do
                    setCursorPosition 0 0
                    clearScreen
                    printBoard board
                    let player = (players board) Vec.! currentPlayer
                    putStrLn ("Player " ++ show (currentPlayer))
                    putStrLn ("Points: " ++ show (points player))
                    putStrLn status
                    putStrLn "Enter a word you find on the grid (:end to end turn or :q to quit): "
                    let newStatus = ""
                    word <- getLine
                    case word of
                      ":end" -> if currentPlayer == (Vec.length (players board) - 1) then return board
                                 else do
                                  gameLoop board "# Round ended! Next player #" (currentPlayer + 1)
                      ":q" -> putStrLn "Exiting" >> exitSuccess
                      _ -> if length word <= 4 then gameLoop board "# The word must be at least 4 characters long in order to award points. #" currentPlayer
                            else if Set.member (toLowStr word) (allWords board) then
                             if Set.member (toLowStr word) (foundWords board) then
                              gameLoop board "# This word has already been found and thus gives no points. #" currentPlayer
                             else do
                              let dp = computePoints word
                              let plr = player {points = (points player) + dp}
                              let brd = board { players = (players board) Vec.// [(currentPlayer, plr)], foundWords = Set.insert (toLowStr word) (foundWords board)}
                              gameLoop brd ("# Found \"" ++ word ++ "\"! Awarded " ++ show dp ++ " point(s). #") currentPlayer
                            else gameLoop board "# I don't see this word in the grid #" currentPlayer

    endBoard <- gameLoop board "" 0

    setCursorPosition 0 0
    clearScreen
    putStrLn "#########################"
    putStrLn "#########RESULTS#########"
    putStrLn "#########################\n"
    putStrLn "All words in this grid:"
    putStrLn (unlines (Set.toList (allWords endBoard)))
    putStrLn "\nPlayer points:"
    putStrLn (concat (map (\(i, Player pts) -> "Player " ++ show i ++ " -- " ++ show pts ++ " pts\n") (Vec.toList (Vec.indexed (players endBoard)))))
    let winnerIndex = Vec.maxIndex (players endBoard)
    let winnerPoints = points ((players endBoard) Vec.! winnerIndex)
    if  Vec.all (\(Player pts) -> pts == winnerPoints) (players endBoard) && Vec.length (players endBoard) /= 1 then
      putStrLn "Draw"
    else
      putStrLn ("Player " ++ show winnerIndex ++ " wins")
    putStrLn "Press any key to quit"
    l <- getChar
    return ()
