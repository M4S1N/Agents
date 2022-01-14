module Enviroments where

import System.Random
import System.IO.Unsafe
import ReactiveRobot
import SmartRobot
import Utils
import Prints
import Structures

-- ======================================================================================================================================
-- Corral
-----------------------------------------------------------------------------------------------------------------------------------------
generate_corral::[[String]] -> Int -> StdGen -> ([[String]], StdGen)
generate_corral xxs k gen =
    if k == 0 then (xxs, gen)
    else if k == 1 then
        let 
            (i, gen0) = randomR (0,(length xxs)-1) gen::(Int, StdGen)
            (j, gen1) = randomR (0,(length (xxs !! 0))-1) gen0::(Int, StdGen)
        in (set_matrix xxs i j "C", gen1)
    else
    let 
        _r = length xxs
        _c = length (xxs !! 0)
        (new_board, gen0) = generate_corral xxs (k-1) gen
        
        cells_in_corral = [(i-1,j-1) | i <- [1.._r], j <- [1.._c], ((new_board !! (i-1)) !! (j-1)) == "C"]
        valid_cells = [(c, d) | (a,b) <- cells_in_corral, (c,d) <- (adjacents a b _r _c), ((new_board !! c) !! d) == ""]

        (indx, gen1) = randomR (0, (length valid_cells)-1) gen0::(Int, StdGen)
        (_i, _j) = valid_cells !! indx
    in 
        (set_matrix new_board _i _j "C", gen1)


-- ======================================================================================================================================
-- Obstaculos
-----------------------------------------------------------------------------------------------------------------------------------------
generate_obstacles::[[String]] -> Int -> StdGen -> ([[String]], StdGen)
generate_obstacles xxs k gen | k == 0 = (xxs, gen) | otherwise =
    let
        n = length xxs
        m = length (xxs !! 0)
        (_board, gen1) = generate_obstacles xxs (k-1) gen
        valid_cells = [(i-1,j-1) | i<-[1..n], j<-[1..m], ((_board !! (i-1)) !! (j-1)) == ""]
        (indx, gen2) = randomR (0, (length valid_cells)-1) gen1::(Int, StdGen)
        (_i, _j) = valid_cells !! indx
    in
        (set_matrix _board _i _j "O", gen2)

-- ======================================================================================================================================
-- Niños
-----------------------------------------------------------------------------------------------------------------------------------------
generate_children::[[String]] -> Int -> StdGen -> ([[String]], StdGen)
generate_children xxs k gen | k == 0 = (xxs, gen) | otherwise =
    let
        n = length xxs
        m = length (xxs !! 0)
        (_board, gen1) = generate_children xxs (k-1) gen
        valid_cells = [(i-1,j-1) | i <- [1..n], j <- [1..m], elem ((_board !! (i-1)) !! (j-1)) ["","C"]]
        (indx, gen2) = randomR (0, (length valid_cells)-1) gen1::(Int, StdGen)
        (_i, _j) = valid_cells !! indx
    in
        (set_matrix _board _i _j (((_board !! _i) !! _j) ++ "N"), gen2)

-- movimiento de los obstaculos al ser empujados por los niños
move_from_to::[[String]] -> Int -> Int -> Int -> Int -> [[String]]
move_from_to grid x1 y1 x2 y2 | ((grid !! x2) !! y2) == "" =
    set_matrix (set_matrix grid x2 y2 "O") x1 y1 "" | otherwise =
    let
        n = length grid
        m = length (grid !! 0)
        x3 = x2 - (x1 - x2)
        y3 = y2 - (y1 - y2)
    in
        if 0 <= x3 && x3 < n && 0 <= y3 && y3 < m && (elem ((grid !! x3) !! y3) ["","O"]) then
            let new_grid = move_from_to grid x2 y2 x3 y3
            in
                if ((new_grid !! x2) !! y2) == "" then
                    set_matrix (set_matrix new_grid x2 y2 ((new_grid !! x1) !! y1)) x1 y1 ""
                else new_grid
        else grid


-- Dada una lista de niños, selecciona un subconjunto de esta para moverlos aleatoriamente
move_children::[[String]] -> [(Int,Int)] -> StdGen -> ([[String]], StdGen)
move_children grid xs gen | xs == [] = (grid, gen) | otherwise =
    let
        ((i,j):rest) = xs
        n = length grid
        m = length (grid !! 0)
        (new_grid0, gen0) = move_children grid rest gen
        
        -- suciedad generada por los niños
        (will_dirt, gen1) = random gen0::(Bool, StdGen)
        (new_grid1, gen2) = unsafePerformIO $
            if will_dirt then
                let 
                    cells_clean = [(_i-1,_j-1) | _i<-[1..n], _j<-[1..m], ((new_grid0 !! (_i-1)) !! (_j-1)) == ""]
                    (_i, gen1_0) = randomR (0, n-1) gen1::(Int, StdGen)
                    (_j, gen1_1) = randomR (0, m-1) gen1_0::(Int, StdGen)
                in
                    if elem (_i,_j) cells_clean then do
                        putStrLn ("--> Niño de " ++ (show (i+1,j+1)) ++ " ensucia " ++ (show (_i+1,_j+1)) ++ ". ")
                        return (set_matrix new_grid0 _i _j "S", gen1_1)
                    else return (new_grid0, gen1_1)
            else return (new_grid0, gen1)

        -- movimiento de los niños
        (will_move, gen3) = random gen2::(Bool, StdGen)
        (new_grid2, gen4) =
            if will_move then 
                let
                    directions = adjacents i j n m
                    valid_direction = [(_i,_j) | (_i,_j) <- directions,
                                    all (\bool -> bool == True) [not (elem obj ((new_grid1 !! _i) !! _j)) | obj <- ['N','S','R']]]

                    (grid_ans, gen3_0) = unsafePerformIO $
                        if valid_direction /= [] then 
                            let
                                (d_indx, gen3_1) = randomR (0, (length valid_direction)-1) gen3::(Int, StdGen)
                                (x, y) = valid_direction !! d_indx
                            in 
                                if elem 'O' ((new_grid1 !! x) !! y) then do
                                    putStrLn ("--> Niño de " ++ (show (i+1,j+1)) ++ " empuja obstaculo de " ++ (show (x+1,y+1)) ++ ". ")
                                    return (move_from_to new_grid1 i j x y, gen3_1)
                                else 
                                    let v = ((new_grid1 !! x) !! y) ++ "N"
                                    in do
                                        putStrLn ("--> Niño de " ++ (show (i+1,j+1)) ++ " se mueve hacia " ++ (show (x+1,y+1)) ++ ". ")
                                        return (set_matrix (set_matrix new_grid1 x y v) i j "", gen3_1)
                        else return (new_grid1, gen3)
                in (grid_ans, gen3_0)
            else (new_grid1, gen3)
    in (new_grid2, gen4)


children_action::Enviroment -> StdGen -> (Enviroment, StdGen)
children_action env gen =
    let
        grid = board env
        n = length grid
        m = length (grid !! 0)
        children_cells = [(i-1,j-1) | i<-[1..n], j<-[1..m], ((grid !! (i-1)) !! (j-1)) == "N"]
        (new_grid, new_gen) = move_children grid children_cells gen
        new_env = Enviroment (t env) new_grid (robots env)
    in (new_env, new_gen)

-- ======================================================================================================================================
-- Suciedad
-----------------------------------------------------------------------------------------------------------------------------------------
generate_dirt::[[String]] -> Int -> StdGen -> ([[String]], StdGen)
generate_dirt xxs k gen | k == 0 = (xxs, gen) | otherwise =
    let
        n = length xxs
        m = length (xxs !! 0)
        (_board, gen1) = generate_dirt xxs (k-1) gen
        valid_cells = [(i-1,j-1) | i<-[1..n], j<-[1..m], ((_board !! (i-1)) !! (j-1)) == ""]
        (indx, gen2) = randomR (0, (length valid_cells)-1) gen1::(Int, StdGen)
        (_i, _j) = valid_cells !! indx
    in
        (set_matrix _board _i _j "S", gen2)

-- ======================================================================================================================================
-- Simulacion del Ambiente
-----------------------------------------------------------------------------------------------------------------------------------------
randomize_enviroment::Enviroment -> StdGen -> Int -> (Enviroment, StdGen)
randomize_enviroment env gen agent_model =
    let
        n = length (board env)
        m = length ((board env) !! 0)
        list_pos_robots = [pos rbt | rbt <- (robots env)]
        list_robots =
            if agent_model == 1 then
                [IRobot _pos False (-1,-1) | _pos <- list_pos_robots]
            else [Robot _pos False | _pos <- list_pos_robots]

        (tot_corral_cells, gen1) = randomR (1, (n*m) `div` 4) gen::(Int, StdGen)
        (board0, gen2) = generate_corral (matrix n m "") tot_corral_cells gen1

        place_robots = \_matrix -> \_list_pos ->
            let
                ((rx,ry): _rest) = _list_pos
                _new_matrix = place_robots (set_matrix _matrix rx ry (((_matrix !! rx) !! ry) ++ "R")) _rest
            in
                if _list_pos /= [] then
                    _new_matrix
                else _matrix
        grid = place_robots board0 list_pos_robots
        (board1, gen3) = generate_children grid tot_corral_cells gen2

        cells_clean = length [(i-1,j-1) | i<-[1..n], j<-[1..m], ((board1 !! (i-1)) !! (j-1)) == ""]

        (tot_obstacles, gen4) = randomR (0, cells_clean `div` 4) gen3::(Int, StdGen)
        (board2, gen5) = generate_obstacles board1 tot_obstacles gen4

        (tot_dirt, gen6) = randomR (0, cells_clean `div` 4) gen5::(Int, StdGen)
        (board3, gen7) = generate_dirt board2 tot_dirt gen6
    in (Enviroment (t env) board3 list_robots, gen7)


-- Ciclo de la simulacion
run_simlutation::Enviroment -> Int -> StdGen -> Int -> IO()
run_simlutation env time gen agent_model = 
    let 
        run_agents = \_env -> \_num ->
            if _num < length (robots _env) then
                if agent_model == 1 then
                    run_agents (run_irobot _env _num) (_num + 1)
                else run_agents (run_robot _env _num) (_num + 1)
            else _env

        agent_env = run_agents env 0

        new_time = time + 1
        (natural_change, gen1) = children_action agent_env gen

        (new_env, new_gen) =
            if new_time `mod` (t env) == 0 then
                randomize_enviroment natural_change gen1 agent_model
            else (natural_change, gen1)
    in do
        putStr "---------------------------------------------------- "
        putStr "proximo turno"
        putStrLn " ----------------------------------------------------"
        l <- getLine
        print_enviroment agent_env
        putStrLn " "
        print_enviroment natural_change
        if new_time `mod` (t env) == 0 then do
            putStrLn " "
            putStrLn "==> El Ambiente cambia de forma aleatoria!!! <=="
            print_enviroment new_env
        else do
            putStr ""
        run_simlutation new_env new_time new_gen agent_model


-- Crear el ambiente inicial
init_enviroment::Int -> Int -> Int -> Int -> Int -> Int -> IO()
init_enviroment n m t k_r agent_model seed =
    let 
        (_,gen) = random (mkStdGen seed)::(Int, StdGen)

        -- Generar las posiciones de los robots
        generate_pos_robot = \actual_list -> \k -> \_gen ->
            let
                possibles = [(x-1,y-1) | x <- [1..n], y <- [1..m], not (elem (x-1,y-1) actual_list)]
                (indx, new_gen) = randomR (0, (length possibles)-1) _gen::(Int, StdGen)
                (i,j) = possibles !! indx
                (rest, new_gen1) = generate_pos_robot ((i,j):actual_list) (k-1) new_gen
            in
                if k > 0 && possibles /= [] then
                    (rest, new_gen1)
                else (actual_list, _gen)
        
        (list_pos, gen1) = generate_pos_robot [] k_r gen
        
        -- A partir de las posiciones crear la lista de robots
        create_robots = \(first:_list_pos) ->
            if _list_pos /= [] then
                if agent_model == 1 then
                    ((IRobot first False (-1,-1)): (create_robots _list_pos))
                else ((Robot first False): (create_robots _list_pos))
            else if agent_model == 1 then
                [IRobot first False (-1,-1)]
            else [Robot first False]

        env = Enviroment t (matrix n m "") (create_robots list_pos)
        (new_env, gen2) = randomize_enviroment env gen1 agent_model
    in do
        print_enviroment new_env
        run_simlutation new_env 0 gen2 agent_model
        
-- ======================================================================================================================================
-- Inicio de la Simulacion
-----------------------------------------------------------------------------------------------------------------------------------------
main::Int -> Int -> Int -> Int -> Bool -> IO()
main n m t robot_cant intelligent =
    let
        seed = 0
    in
        if intelligent then
            init_enviroment n m t robot_cant 1 seed
        else init_enviroment n m t robot_cant 0 seed
-- ======================================================================================================================================