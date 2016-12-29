import Data.Char
import Automaton

data Math = Root
          | Number
          | BinaryOperator
          deriving (Eq)

instance State Math where
    accept Root = isNumber
    accept Number = isNumber
    accept BinaryOperator = (flip elem) ['+','-','*','^','/']

    nextStates Root = [Number]
    nextStates Number = [Number, BinaryOperator]
    nextStates BinaryOperator = [Number]

    finalStates = [Number]

data PL = Init
        | Var
        | Neg
        | And
        | Or
        | Dash
        | LeftArrowHead
        | MiddleDash
        | RightArrowHead
        deriving (Eq)

instance State PL where
    accept Init  = isLetter
    accept Var   = isLetter
    accept Neg = (==) '!'
    accept And = (==) '&'
    accept Or = (==) '|'
    accept LeftArrowHead = (==) '<'
    accept Dash  = (==) '-'
    accept RightArrowHead = (==) '>'

    nextStates Init = [Var, Neg]
    nextStates Var = [Var, Dash, LeftArrowHead, And, Or]
    nextStates Neg = [Var]
    nextStates And = [Var, Neg]
    nextStates Or = [Var, Neg]
    nextStates Dash = [RightArrowHead]
    nextStates LeftArrowHead = [Dash]
    nextStates RightArrowHead = [Var, Neg]
    
    finalStates = [Var]
