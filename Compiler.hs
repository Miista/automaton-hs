import Data.Char
import Automaton

data States = Keyword
            | AnyLetter
            | Whitespace
            | Punctuation
            deriving (Show, Eq)

instance State States where
    accept AnyLetter = isLetter
    accept Whitespace = isSpace
    accept Punctuation = isPunctuation

    nextStates AnyLetter = [AnyLetter, Whitespace, Punctuation]
    nextStates Whitespace = [AnyLetter]
    nextStates Punctuation = [Whitespace, AnyLetter]

    finalStates = [AnyLetter, Punctuation]

type Unread = String
type Stack = [String]
type Configuration = (Unread, Stack)

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

    finalStates = [P2,P3]
