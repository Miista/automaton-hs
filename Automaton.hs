module Automaton
  (
  -- Class
    State (accept, nextStates, finalStates)

  -- Functions
  , canAccept
  , step
  ) where

-- | A safer version of the standard 'head' function.
maybeHead :: [b] -> Maybe b
maybeHead (x:_) = Just x
maybeHead _ = Nothing

class (Eq a) => State a where
    -- | Returns the next possible states for a given state
    nextStates :: a -> [a]

    -- | Returns whether a given state accepts
    -- the given character as input.
    accept :: a -> Char -> Bool

    -- | Represents the final states for this automaton.
    finalStates :: [a]

    -- | Returns the first state which accepts the given character as input.
    nextState :: Char -> a -> Maybe a
    nextState c a = maybeHead $ filter ((flip accept) c) (nextStates a)

-- | Returns whether this automaton can accept the given String.
canAccept :: (State a) => a -> String -> Bool
canAccept a [] = a `elem` finalStates
canAccept a (x:xs) = case (nextState x a) of
                        Nothing -> False
                        Just m -> canAccept m xs

-- | Returns the next state for any possible state and input.
-- If no next state exists, 'Nothing' is returned instead.
step :: (State a) => a -> String -> (Maybe a, String)
step a (x:xs) = ((nextState x a), xs)

move :: (Show a, State a) => a -> String -> (String, String)
move a s
  | null s      = if (a `elem` finalStates)
                      then ("fin", s)
                      else (show a, s)
  | otherwise   = let x = head s
                      xs = tail s
                      m = nextState x a
                      in case m of
                            Nothing -> ("sink", x:xs)
                            Just m' -> move m' xs
