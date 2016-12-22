module Automaton
  (
  -- Class
    State (accept, nextStates, finalStates)
  -- Functions
  , canAccept
  ) where

maybeHead :: [b] -> Maybe b
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

class (Eq a) => State a where
    -- | Returns the next possible states for a given state
    nextStates :: a -> [a]

    -- | Returns whehter a given state accepts
    -- the given character as input.
    accept :: a -> Char -> Bool

    -- | Represents the final states for this automaton.
    finalStates :: [a]

move :: (Show a, State a) => a -> String -> (String, String)
move a s
  | null s      = if (a `elem` finalStates) -- (fin a `elem` nextStates a)
                      then ("fin", s)
                      else (show a, s)
  | otherwise   = let x = head s
                      xs = tail s
                      m = nextState x a
                      in case m of
                            Nothing -> ("sink", x:xs)
                            Just m' -> move m' xs

-- | Returns the first state which accepts the given character as input.
nextState :: (State a) => Char -> a -> Maybe a
nextState c s = maybeHead $ filter ((flip accept) c) (nextStates s)

-- | Returns whether this automaton can accept the given String.
canAccept :: (State a) => a -> String -> Bool
canAccept a [] = a `elem` finalStates
canAccept a (x:xs) = case (nextState x a) of
                        Nothing -> False
                        Just m -> canAccept m xs
