import Data.Char (isLetter)

class (Eq a) => State a where
    -- | Returns the next possible states for a given state
    nextStates :: a -> [a]

    -- | Returns whehter a given state accepts
    -- the given character as input.
    accept :: a -> Char -> Bool

    -- | Represents the final states for this automaton.
    finalStates :: a -> [a]

    -- | Returns whether this automaton can accept the given String.
    canAccept :: a -> String -> Bool
    canAccept a [] = a `elem` finalStates a
    canAccept a (x:xs) = case (nextState x a) of
                            Nothing -> False
                            Just m -> canAccept m xs

    sink :: a -> a

    -- move :: a -> String -> (a, String)
    -- move a s
    --   | a == sink a = (sink a, s)
    --   | null s      = if (a `elem` finalStates a) -- (fin a `elem` nextStates a)
    --                       then (fin a, s)
    --                       else (a, s)
    --   | otherwise   = let x = head s
    --                       xs = tail s
    --                       m = nextState a x
    --                       in move m xs

    -- | Returns the first state which accepts the given character as input.
    nextState :: Char -> a -> Maybe a
    nextState c s = maybeHead $ filter ((flip accept) c) (nextStates s)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

firstMatch :: (State a) => [a] -> Char -> Maybe a
firstMatch [] _ = Nothing
firstMatch (m:ms) c = if (accept m) c
                         then Just m
                         else firstMatch ms c

data States = Keyword
            | AnyLetter
            | Si
            deriving (Show, Eq)

instance State States where
    accept Keyword = isLetter
    nextStates Keyword = [AnyLetter]
    finalStates _ = []
    sink _ = Si

type Unread = String
type Stack = [String]
type Configuration = (Unread, Stack)

moveNext :: (String, String) -> (String, String)
moveNext (c,(i:is)) = (c++[i],is)

data P = P0
       | P1
       | P2
       | P3
       | Sink
       deriving (Show, Eq)

instance State P where
    accept P0 = (==) 'a'
    accept P1 = (==) 'b'
    accept P2 = (==) 'a'
    accept P3 = (==) 'b'

    nextStates P0 = [P0,P1]
    nextStates P1 = [P2,P3]
    nextStates P2 = [P0,P1]
    nextStates P3 = [P3,P2]

    finalStates _ = [P2,P3]

    sink _ = Sink


-- move :: (Eq a, State a) => a -> String -> (a, String)
-- move a [] = if (elem (fin a) (nextStates a))
--                 then (fin a, "")
--                 else (a,"")
-- move a (x:xs) = case (firstMatch (nextStates a) x) of
--                     Just m -> move m xs
--                     Nothing -> (a, [x])

-- firstMatch :: (State a) => [a] -> Char -> Maybe a
-- firstMatch [] _ = Nothing
-- firstMatch (m:ms) c = if (accept m) c
--                          then Just m
--                          else firstMatch ms c

-- data State = State { entry :: Char -> Bool
--                    , states :: [State]
--                    , name :: String
--                    }
--
-- instance Show State where
--     show s = name s

-- keywordState :: State
-- keywordState = State (isLetter) [anyLetter] "keyword"
--
-- anyLetter :: State
-- anyLetter = State (isLetter) [anyLetter] "any"

-- firstMatch :: [State] -> Char -> Maybe State
-- firstMatch [] _ = Nothing
-- firstMatch (m:ms) c = if (accept m) c
--                          then Just m
--                          else firstMatch ms c
--
-- type Automaton = (String, String, [String])
--
-- lexx :: [State] -> Automaton -> Automaton
-- lexx [] (c,_,_) = (c,"",[])
-- lexx matchers (c,[],t) = ("","",t)
-- lexx matchers (c,(i:is),t) = case (firstMatch matchers i) of
--                               Nothing -> lexx matchers (c++[i], is, t)
--                               Just m -> lexx (nextStates m) (c++[i], is, t++[c])
--
-- str = "int a = 1"
