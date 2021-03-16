module Mastermind where

import Control.Monad.Trans.State.Strict
  ( State,
    evalState,
    gets,
    modify,
  )
import Data.HashSet (HashSet)
import qualified Data.HashSet as S

-- | Combination is a numeric string, e.g. "132"
type Combination = String

-- | Guess contains the guessed combination, as well as how many fully and
-- partially correct digits it contains
data Guess = Guess {guessValue :: Combination, guessFulls :: Int, guessPartials :: Int}

instance Show Guess where
  show (Guess c 0 0) = "In the number " ++ c ++ " there are no correct digits"
  show (Guess c f 0) = "In the number " ++ c ++ " there are " ++ show f ++ " fully correct digits, and no partially correct digits."
  show (Guess c 0 p) = "In the number " ++ c ++ " there are no fully correct digits, but there are " ++ show p ++ " partially correct digits."
  show (Guess c f p) = "In the number " ++ c ++ " there are " ++ show f ++ " fully correct digits, and " ++ show p ++ " partially correct digits."

-- | The GameState keeps track of possible answers given the guesses in guesses.
-- As well as the correct answer
data GameState = GameState {possibles :: HashSet Combination, correct :: Combination, guesses :: [Guess]}
  deriving (Show)

-- | Extracts one element out of the HashSet
someElement :: HashSet a -> a
someElement = head . S.toList

-- | Solve and print the answer
prettySolve :: Combination -> IO ()
prettySolve corr = putStrLn . unlines . map show $ solve corr

-- | Given a combination the computer will make guesses until it can say that
--  number without any ambiguety. Gets real slow, real fast for longer
--  combinations. Keep them under 6 digits.
solve :: Combination -> [Guess]
solve corr = evalState solve' (initState corr)

-- | To solve the game, play a round and see if there is any ambiguity left
--  if there is, do it again.
solve' :: State GameState [Guess]
solve' = do
  playRound
  ps <- gets possibles
  if S.null ps then gets guesses else solve'

-- | To play a round get some possible combination out of possibles,
--  make that guess and update possibles according to the answer.
playRound :: State GameState ()
playRound = do
  next <- gets (someElement . possibles)
  g <- makeGuess next
  updatePossibles g
  return ()

-- | initPossibles for a string "___" is every number between "00 ... 0" to "99
-- .. 9"
initPossibles :: Combination -> HashSet Combination
initPossibles corr = S.delete corr $ S.fromList $ helper corr
  where
    helper :: Combination -> [Combination]
    helper [] = []
    helper [_] = [[c] | c <- ['0' .. '9']]
    helper (_ : rest) = [next : curr | curr <- helper rest, next <- ['0' .. '9']]

-- | Inital Game state
initState :: Combination -> GameState
initState corr = GameState (initPossibles corr) corr []

-- | Given a combination, guess that combination and update the state
makeGuess :: Combination -> State GameState Guess
makeGuess comb = do
  corr <- gets correct
  let g = guess comb corr
  modify (\s -> s {guesses = g : guesses s})
  return g

-- | Update the possible answers given a guess
updatePossibles :: Guess -> State GameState (HashSet Combination)
updatePossibles g = do
  modify (\s -> s {possibles = S.filter (g <=|=>) $ possibles s})
  gets possibles

-- | 'Compatible with' operator, returns True if Combination could still be the
-- answer, given the Guess
(<=|=>) :: Guess -> Combination -> Bool
(<=|=>) (Guess comb1 fa pa) comb2 = fa == fb && pa == pb
  where
    (Guess _ fb pb) = guess comb2 comb1

-- | Given the guessed combination, as well as the answer create a Guess
guess :: Combination -> Combination -> Guess
guess comb answer = Guess comb (fulls comb answer) (partials comb answer)

-- | Calculate how many fully matching digits there are
fulls :: Combination -> Combination -> Int
fulls a b = length . filter id $ zipWith (==) a b

-- | Calculate how many partially matching digits there are
partials :: Combination -> Combination -> Int
partials a b = S.size $ aCandidates `S.intersection` bCandidates
  where
    candidateTest :: [Bool]
    candidateTest = zipWith (/=) a b
    aCandidates :: HashSet Char
    aCandidates = S.fromList . map snd . filter fst . zip candidateTest $ a
    bCandidates = S.fromList . map snd . filter fst . zip candidateTest $ b
