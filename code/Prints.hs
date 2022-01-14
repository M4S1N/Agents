module Prints where

import Structures

print_h_division::Int -> IO()
print_h_division l | l == 1 = putStrLn " ___ " | otherwise = do
    putStr " ___"
    print_h_division (l-1)

fill_cell::String -> String
fill_cell cell | (length cell) ==  3 = cell | otherwise =
    let
        _length = length cell
        _fill = \k -> if k == 0 then "" else (" " ++ (_fill (k-1)))
    in cell ++ (_fill (3 - _length))

print_cell::String -> IO()
print_cell x = putStr (fill_cell x)

print_row1::[String] -> IO()
print_row1 (x:xs) | xs == [] = do
    print_cell x
    putStrLn "|" | otherwise = do
    print_cell x
    putStr "|"
    print_row1 xs

print_row2::[String] -> IO()
print_row2 (x:xs) | xs == [] = putStrLn "___|" | otherwise = do
    putStr "___|"
    print_row2 xs

print_board::[[String]] -> IO()
print_board (xs:xxs) | xxs == [] = do
    putStr "|"
    print_row1 xs
    putStr "|"
    print_row2 xs    | otherwise = do
    putStr "|"
    print_row1 xs
    putStr "|"
    print_row2 xs
    print_board xxs

print_enviroment::Enviroment -> IO()
print_enviroment env = 
    let _board = board env
    in do
        print_h_division (length (_board !! 0))
        print_board _board