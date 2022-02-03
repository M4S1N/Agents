module ReactiveRobot where

import System.IO.Unsafe
import Structures
import Utils

-- ======================================================================================================================================
-- Simulacion del Funcionamiento del Agente reactivo
-----------------------------------------------------------------------------------------------------------------------------------------
-- Retorna la celda hacia la cual se dirige el robot (la primera y ultima celda del camino) y el numero de la accion a realizar:
{-
    0 - Nada
    1 - Moverse
    2 - Moverse y Cargar un Niño
    3 - (Mantenerse en la misma celda) Dejar el Niño
    4 - (Mantenerse en la misma celda) Cargar el Niño
-}
action::[[String]] -> Agent -> Bool -> ((Int, Int), (Int,Int), Int)
action grid robot _show =
    let
        (rx, ry) = pos robot
        n = length grid
        m = length (grid !! 0)
        _corral = [(i-1,j-1) | i <- [1..n], j <- [1..m], elem ((grid !! (i-1)) !! (j-1)) ["C", "CR"]]
        _dirt = [(i-1,j-1) | i <- [1..n], j <- [1..m], ((grid !! (i-1)) !! (j-1)) == "S"]
        _child = [(i-1,j-1) | i <- [1..n], j <- [1..m], ((grid !! (i-1)) !! (j-1)) == "N"]
        _nothing = [(i-1,j-1) | i <- [1..n], j <- [1..m], ((grid !! (i-1)) !! (j-1)) == ""]

        -- Del total de caminos a analizar, selecciona aquellos que maximicen la cantidad de "point"
        max_repeat = \all_paths -> \_grid -> \point -> \list_paths ->
            let
                (pth:rest_paths) = all_paths
                count1 = length (filter (\x -> elem point x) [((_grid !! x) !! y) | (x,y) <- pth])
                count2 = length (filter (\x -> elem point x) [((_grid !! x) !! y) | (x,y) <- (list_paths !! 0)])
                new_list_paths =
                    if count1 > count2 then [pth]
                    else if count1 < count2 then list_paths
                    else (pth:list_paths)

                _list_paths = max_repeat rest_paths _grid point new_list_paths
            in
                if all_paths /= [] then _list_paths
                else list_paths

        -- Objetivos del robot
        objectives =
            if carrie_up robot then
                -- Dejarlo en la celda o Trasladarlo a otra
                ["CNR", "C", "S", "NR"]
            else
                -- Buscar un celda sucia, un niño
                -- o recoger un niño que se encuentre en la misma celda
                ["NR", "N", "S"]


        -- Condiciones y acciones para cumplir el objetivo si se carga un niño
        conditions_actions_case1 =
            let
                _paths_to_corral = [pths | (x,y) <- _corral, pths <- (paths grid (rx,ry) (x,y) True), pths /= []]
                _paths_to_S = [pth | (x,y) <- _dirt, pth <- (paths grid (rx,ry) (x,y) True), pth /= []]

                -- Calcula la puntuacion de una celda del corral
                _calc_score = \_grid -> \_i -> \_j ->
                    let
                        _adj = adjacents _i _j (length _grid) (length (_grid !! 0))
                        _cells_bloq = [(x,y) | (x,y) <- _adj, elem ((_grid !! x) !! y) ["CN", "O"]]
                        _cells_free = [(x,y) | (x,y) <- _adj, not (elem (x,y) _cells_bloq)]
                        _tot_bloq = (4 - (length _adj)) + (length _cells_bloq)

                        _score =
                            if _tot_bloq == 0 then 0
                            else if _tot_bloq == 3 then 5
                            else if _tot_bloq == 1 then 1
                            else
                                let
                                    (x1,y1) = _cells_free !! 0
                                    (x2,y2) = _cells_free !! 1
                                in
                                    if x1 == x2 || y1 == y2 then 2
                                    else 3
                    in _score

                maximum = \(a1, (a2,a3)) -> \(b1, (b2,b3)) ->
                    if a1 > b1 then (a1, (a2,a3))
                    else if a1 < b1 then (b1, (b2, b3))
                    else if a2 > b2 then (a1, (a2, a3))
                    else if a2 < b2 then (b1, (b2, b3))
                    else if a3 > b3 then (a1, (a2, a3))
                    else (b1, (b2, b3))
                            
                _score_corral = [(_calc_score grid i j, (i,j)) | (i,j) <- _corral]
                _score_accessible_corral = [(s,(x,y)) | (s,(x,y)) <- _score_corral, (paths grid (rx,ry) (x,y) True) /= []]

                --(_max, (_,_)) = maximum_list _score_corral (_score_corral !! 0) maximum
                (_accessible_max, (_,_)) = maximum_list _score_accessible_corral (head _score_accessible_corral) maximum

                --_best_corral = filter (\(x,(_,_)) -> x == _max) _score_corral
                _best_accessible_corral = filter (\(x,(_,_)) -> x == _accessible_max) _score_accessible_corral

                (_score_cnr, (_,_)) = (_calc_score grid rx ry, (rx, ry))
                -------------------------------------------------------------------------------------------------------------------------------------

                -- Que la celda del corral en la que se encuentra sea la mejor para dejar al niño
                cond_CNR = elem 'C' ((grid !! rx) !! ry) && (_best_accessible_corral == [] || _accessible_max <= _score_cnr)
                act_CNR = ((rx,ry), (rx, ry), 3)


                -- Que exista un camino hacia "C"
                cond_C = _paths_to_corral /= []
                act_C =
                    let
                        _candidates = [(x,y) | (_, (x,y)) <- _best_accessible_corral]
                        _candidates_paths = [pth | (x,y) <- _candidates, pth <- (paths grid (rx,ry) (x,y) True), pth /= []]
                        valid_paths = select_short_path _candidates_paths [_candidates_paths !! 0]
                        best_paths = max_repeat valid_paths grid 'S' [valid_paths !! 0]
                    in (((head best_paths) !! 1), last (head best_paths), 1)


                -- Que haya suciedad y que no exista un camino hacia el corral ni siquiera pasando por algun otro niño
                cond_S = _paths_to_S /= [] && [pths | (x,y) <- _corral, pths <- (paths grid (rx,ry) (x,y) False), pths /= []] == []
                act_S = 
                    let _short_paths_to_S = select_short_path _paths_to_S [_paths_to_S !! 0]
                    in ((head _short_paths_to_S) !! 1, last (head _short_paths_to_S), 1)


                -- Que no se cumplan las condiciones anteriores
                cond_NR = not cond_C && not cond_CNR && not cond_S
                act_NR = ((rx,ry), (rx,ry), 3)
            
            in [(cond_CNR, act_CNR), (cond_C, act_C), (cond_S, act_S), (cond_NR, act_NR)]
        
        -- Condiciones y operaciones para cumplir el objetivo si no se carga un niño
        conditions_actions_case2 =
            let
                _paths_to_S = [pth | (x,y) <- _dirt, pth <- (paths grid (rx,ry) (x,y) False), pth /= []]
                _paths_to_child = [pth | (x,y) <- _child, pth <- (paths grid (rx,ry) (x,y) False), pth /= []]

                -- Que se encuentre en la misma celda de un niño y que exista un camino a "C" que no contenga a otro niño
                cond_NR = ((grid !! rx) !! ry) == "NR" && [pth | (x,y) <- _corral, pth <- (paths grid (rx,ry) (x,y) True), pth /= []] /= []
                act_NR = ((rx,ry), (rx,ry), 4)

                -- Que exista un camino hacia "N" y otro hacia "C" (tras haberlo cargado)
                without_robot = set_matrix grid rx ry (remove_item ((grid !! rx) !! ry) 'R')
                valid_children = [(x1,y1) | (x1,y1)<-_child, (x2,y2)<-_corral,
                                            (paths grid (rx,ry) (x1,y1) False) /= [] &&
                                            (paths without_robot (x1,y1) (x2,y2) True) /= []]
                cond_N = valid_children /= []
                act_N =
                    let
                        _valid_paths_to_child = [pth | (x,y) <- valid_children, pth <- (paths grid (rx,ry) (x,y) False), pth /= []]
                        _short_paths_to_ch = select_short_path _valid_paths_to_child [head _valid_paths_to_child]
                        best_path_to_ch = max_repeat _short_paths_to_ch grid 'S' [head _short_paths_to_ch]
                        (x,y) = (head best_path_to_ch) !! 1
                        (v,w) = last (head best_path_to_ch)
                        act_do = if ((grid !! x) !! y) == "N" then 2 else 1
                    in ((x,y), (v,w), act_do)

                -- Que no se cumplan las anteriores y que hayan celdas sucias accesibles por el robot
                -- En este caso, la mejor opcion es cargar un niño y limpiar las celdas sucias
                cond_S = _paths_to_S /= [] && not cond_N && not cond_NR
                act_S  = 
                    let
                        _valid = [(x1,y1) | (x1,y1)<-_child, (x2,y2)<-_dirt,
                                                    (paths grid (rx,ry) (x1,y1) False) /= [] &&
                                                    (paths without_robot (x1,y1) (x2,y2) True) /= []]
                        _valid_paths = [pth | (x,y) <- _valid, pth <- (paths grid (rx,ry) (x,y) False), pth /= []]
                        _short_path_to_child = select_short_path _valid_paths [head _valid_paths]
                        _best_short_path = max_repeat _short_path_to_child grid 'S' [head _short_path_to_child]
                        (x,y) = (head _best_short_path) !! 1
                        (v,w) = last (head _best_short_path)
                        _short_paths_to_S = select_short_path _paths_to_S [head _paths_to_S]
                        act_do = if ((grid !! x) !! y) == "N" then 2 else 1
                    in
                        if _paths_to_child /= [] then
                            ((x,y), (v,w), act_do)
                        else ((head _short_paths_to_S) !! 1, last (head _short_paths_to_S), 1)

                -- Que no se cumplan las anteriores
                cond_nothing = not cond_N && not cond_NR && not cond_S
                act_nothing = 
                    if ((grid !! rx) !! ry) == "NR" then
                        ((rx,ry), (-1,-1), 4)
                    else if ((grid !! rx) !! ry) /= "R" then  -- moverse a una celda vacia
                        let
                            valid_paths = [pth | (x,y) <- _nothing, pth <- (paths grid (rx,ry) (x,y) False), pth /= []]
                            short_paths = select_short_path valid_paths [head valid_paths]
                        in 
                            if valid_paths /= [] then
                                ((head short_paths) !! 1, last (head short_paths), 1)
                            else ((rx,ry), (-1,-1), 0)
                    else ((rx,ry), (-1,-1), 0)
            
            in [(cond_NR, act_NR), (cond_N, act_N), (cond_S, act_S), (cond_nothing, act_nothing)]

        
        -- Accion a realizar dadas las condiciones del ambiente y objetivo del robot
        ((_X, _Y), (_V, _W), _Op) =
            let
                conditions_actions = if carrie_up robot then conditions_actions_case1 else conditions_actions_case2
                (_, act) = select_first conditions_actions (\(x,_) -> x == True)
            in act

    in unsafePerformIO $
        if _Op == 0 && _show then do
            putStrLn ("--> El robot de la celda " ++ (show (rx+1,ry+1)) ++ " se mantiene en su celda")
            return ((_X, _Y), (_V, _W), _Op)
        else if _Op == 0 then return ((_X, _Y), (_V, _W), _Op)
        else if _Op == 1 then do
            putStrLn ("--> El robot de la celda " ++ (show (rx+1,ry+1)) ++ " se mueve hacia la celda " ++ (show (_X+1,_Y+1)) ++ " con objetivo " ++ (show (_V+1,_W+1)))
            return ((_X, _Y), (_V, _W), _Op)
        else if _Op == 2 && _show then do
            putStrLn ("--> El robot de la celda " ++ (show (rx+1,ry+1)) ++ " se mueve hacia la celda " ++ (show (_X+1,_Y+1)) ++ " y carga al niño que se encuentra en la celda")
            return ((_X, _Y), (_V, _W), _Op)
        else if _Op == 2 then return ((_X, _Y), (_V, _W), _Op)
        else if _Op == 3 && _show then do
            putStrLn ("--> El robot de la celda " ++ (show (rx+1,ry+1)) ++ " deja al niño en su celda")
            return ((_X, _Y), (_V, _W), _Op)
        else if _Op == 3 then return ((_X, _Y), (_V, _W), _Op)
        else if _show then do
            putStrLn ("--> El robot de la celda " ++ (show (rx+1,ry+1)) ++ " carga al niño de su celda")
            return ((_X, _Y), (_V, _W), _Op)
        else return ((_X, _Y), (_V, _W), _Op)


clean_cell::[[String]] -> Int -> Int -> ([[String]], Int)
clean_cell grid rx ry = unsafePerformIO $
    if elem 'S' ((grid !! rx) !! ry) then do
        putStrLn ("--> El robot de la celda " ++ (show (rx+1,ry+1)) ++ " limpia su celda")
        return (set_matrix grid rx ry (remove_item ((grid !! rx) !! ry) 'S'), 1)
    else return (grid, 0)


run_robot::Perception -> Int -> Enviroment
run_robot perception rnum =
    let
        (rx, ry) = pos ((robots perception) !! rnum)
        grid = board perception
        n = length grid
        m = length (grid !! 0)
        new_robot0 = (robots perception) !! rnum

        -- Comprueba si puede limpiar la celda
        (new_grid0, action1) = clean_cell grid rx ry
        
        -- Comprueba si se puede mover hacia una celda objetivo
        (new_grid1, new_robot1, action2) =
            let
                ((x, y), (_,_), _action) = action grid new_robot0 True
                _grid0 = set_matrix new_grid0 rx ry (remove_item ((new_grid0 !! rx) !! ry) 'R')
                _grid1 =
                    if (carrie_up new_robot0) then
                        set_matrix _grid0 rx ry (remove_item ((_grid0 !! rx) !! ry) 'N')
                    else _grid0
                _grid2 =
                    if (carrie_up new_robot0) then
                        set_matrix _grid1 x y (((_grid1 !! x) !! y) ++ "N")
                    else _grid1
                _grid3 = set_matrix _grid2 x y (((_grid2 !! x) !! y) ++ "R")

                _carrie_up =
                    if elem _action [2,4] then True
                    else if _action == 3 then False
                    else carrie_up new_robot0
                _robot = Robot (x,y) _carrie_up

            in 
                if action1 == 0 then
                    (_grid3, _robot, _action)
                else (new_grid0, new_robot0, 0)
            
        (_rx, _ry) = pos new_robot1
        
        -- En caso de que cargue un niño, segundo movimiento
        (new_grid2, new_robot2) =
            let
                ((x, y), (_,_), _action) = action new_grid1 new_robot1 False
                (_grid, _robot) =
                    let
                        _grid0 = set_matrix new_grid1 _rx _ry (remove_item ((new_grid1 !! _rx) !! _ry) 'R')
                        _grid1 =
                            if (carrie_up new_robot1) then
                                set_matrix _grid0 _rx _ry (remove_item ((_grid0 !! _rx) !! _ry) 'N')
                            else _grid0
                        _grid2 =
                            if (carrie_up new_robot1) then
                                set_matrix _grid1 x y (((_grid1 !! x) !! y) ++ "N")
                            else _grid1
                        _grid3 = set_matrix _grid2 x y (((_grid2 !! x) !! y) ++ "R")
                        _new_robot = Robot (x,y) (carrie_up new_robot1)
                    in
                        if _action == 1 then 
                            (_grid3, _new_robot)
                        else (new_grid1, new_robot1)
            in
                if action2 == 1 && elem ((new_grid1 !! _rx) !! _ry) ["CNR","NR"] && carrie_up new_robot0 then
                    (_grid, _robot)
                else (new_grid1, new_robot1)

    in Enviroment (t perception) new_grid2 (set_list (robots perception) rnum new_robot2)
-- ======================================================================================================================================