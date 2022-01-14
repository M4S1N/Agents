module Structures where

data Agent = Robot{pos::(Int,Int), carrie_up::Bool} |
             IRobot{pos::(Int,Int), carrie_up::Bool, iobjective::(Int,Int)}
             deriving (Show)

data Enviroment = Enviroment{
    t::Int,
    board::[[String]],
    robots::[Agent]
} deriving (Show)

type Cell = [Char]

type Perception = Enviroment