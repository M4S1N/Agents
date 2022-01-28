module Utils where

import System.Random
import System.IO.Unsafe

remove_item::Eq a => [a] -> a -> [a]
remove_item [] y = []
remove_item (x:xs) y | x == y = xs | otherwise = (x:(remove_item xs y))

select_first::[a] -> (a -> Bool) -> a
select_first (x:xs) func | func x = x | otherwise = select_first xs func

append::Eq a => [a] -> a -> [a]
append xs x | xs == [] = [x] | otherwise = let (h:r) = xs in (h:(append r x))

set_list::[a] -> Int -> a -> [a]
set_list (x:xs) i value | i == 0 = (value:xs) | otherwise = (x:(set_list xs (i-1) value))

set_matrix::[[a]] -> Int -> Int -> a -> [[a]]
set_matrix (xs:xxs) i j value | i == 0 = ((set_list xs j value):xxs) | otherwise = (xs:(set_matrix xxs (i-1) j value))

list::Int -> a -> [a]
list n v | n == 0 = [] | otherwise = (v:(list (n-1) v))

matrix::Int -> Int -> a -> [[a]]
matrix n m v | n == 0 = [] | otherwise = ((list m v):(matrix (n-1) m v))

adjacents::Int -> Int -> Int -> Int -> [(Int, Int)]
adjacents i j n m = [(x,y) | (x,y) <- [(i,j+1),(i,j-1),(i-1,j),(i+1,j)], 0 <= x && x < n && 0 <= y && y < m]

maximum_list::[a] -> a -> (a -> a -> a) -> a
maximum_list [] y func = y
maximum_list (x:xs) y func = maximum_list xs (func x y) func

bfs::[[String]] -> Int -> [[Int]] -> Bool -> [[Int]]
bfs grid k routes carries =
    let
        n = length grid
        m = length (grid !! 0)
        
        k_dist = [(i-1,j-1) | i <- [1..n], j <- [1..m], ((routes !! (i-1)) !! (j-1)) == k]
        _ch = if carries then "" else "N"
        valid = [(x,y) | (i,j) <- k_dist, (x,y) <- (adjacents i j n m),
                (elem ((grid !! x) !! y) ["","C","S",_ch]) && ((routes !! x) !! y) == (-1)]
        
        set_ = \m -> \l -> \v ->
            if l == [] then m
            else
                let ((i,j):_l) = l
                in set_ (set_matrix m i j v) _l v
    in if valid /= [] then bfs grid (k+1) (set_ routes valid (k+1)) carries else routes


paths::[[String]] -> (Int, Int) -> (Int, Int) -> Bool -> [[(Int, Int)]]
paths grid (i,j) (x,y) carries =
    let
        n = length grid
        m = length (grid !! 0)
        routes = bfs grid 0 (set_matrix (matrix n m (-1)) i j 0) carries

        get_paths = \_i -> \_j -> \rout ->
            if ((rout !! _i) !! _j) == 0 then [[(_i, _j)]]
            else
                let
                    _n = length rout
                    _m = length (rout !! 0)
                    _k = (rout !! _i) !! _j
                    _valid = [(_x,_y) | (_x,_y) <- (adjacents _i _j _n _m), ((rout !! _x) !! _y) == (_k-1)]
                in [append path (_i, _j) | (_x,_y) <- _valid, path <- (get_paths _x _y rout)]
    in if ((routes !! x) !! y) /= (-1) then get_paths x y routes else []


select_short_path::[[(Int,Int)]] -> [[(Int,Int)]] -> [[(Int, Int)]]
select_short_path [] l = l
select_short_path (_path:_paths_rest) best_paths =
    let
        new_best_paths =
            if (length _path) < (length (best_paths !! 0)) then
                [_path]
            else if (length _path) > (length (best_paths !! 0)) then
                best_paths
            else (_path:best_paths)

        _best_paths = select_short_path _paths_rest new_best_paths

    in _best_paths

get_3_x_3_cells::(Int, Int) -> Int -> Int -> [(Int, Int)]
get_3_x_3_cells (i, j) n m =
    let
        (x,y) = head [(s1,s2) | s1 <- [0..(n-3)], s2 <- [0..(m-3)], s1 <= i && i <= s1+2 && s2 <= j && j <= s2+2]
    in
        [(u,v) | u <- [x..(x+2)], v <- [y..(y+2)]]