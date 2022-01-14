module SmartRobot where

import System.IO.Unsafe
import ReactiveRobot
import Structures
import Utils

-- ======================================================================================================================================
-- Simulacion del Funcionamiento del Agente inteligente
-----------------------------------------------------------------------------------------------------------------------------------------
run_irobot::Perception -> Int -> Enviroment
run_irobot perception rnum =
    let
        robot = (robots perception) !! rnum
        (rx,ry) = pos robot
        grid = board perception

        robots_with_obj = take rnum (robots perception)
        list_obj = [iobjective rbt | rbt <- robots_with_obj]

        erase_objectives = \_grid -> \_list_obj ->
            let
                ((x,y): rest) = _list_obj
                _new_grid =
                    if x >= 0 && y >= 0 then
                        set_matrix _grid x y "O"
                    else _grid
            in
                if _list_obj /= [] then
                    erase_objectives _new_grid rest
                else _grid

        -- Comprueba si puede limpiar la celda
        (new_grid0, action1) = clean_cell grid rx ry
        r_new_grid0 = erase_objectives new_grid0 list_obj

        new_robot0 = if action1 == 0 then robot else IRobot (rx,ry) (carrie_up robot) (rx,ry)

        -- Comprueba si se puede mover hacia una celda objetivo
        (new_grid1, new_robot1, action2) =
            let
                ((x1, y1), (x2, y2), _action) = action r_new_grid0 new_robot0 True
                _grid0 = set_matrix new_grid0 rx ry (remove_item ((new_grid0 !! rx) !! ry) 'R')
                _grid1 =
                    if (carrie_up new_robot0) then
                        set_matrix _grid0 rx ry (remove_item ((_grid0 !! rx) !! ry) 'N')
                    else _grid0
                _grid2 =
                    if (carrie_up new_robot0) then
                        set_matrix _grid1 x1 y1 (((_grid1 !! x1) !! y1) ++ "N")
                    else _grid1
                _grid3 = set_matrix _grid2 x1 y1 (((_grid2 !! x1) !! y1) ++ "R")

                _carrie_up =
                    if elem _action [2,4] then True
                    else if _action == 3 then False
                    else carrie_up new_robot0
                _robot = IRobot (x1,y1) _carrie_up (x2,y2)

            in 
                if action1 == 0 then
                    (_grid3, _robot, _action)
                else (new_grid0, new_robot0, 0)
        
        (_rx, _ry) = pos new_robot1
        r_new_grid1 = erase_objectives new_grid1 list_obj
        
        -- En caso de que cargue un niño, segundo movimiento
        (new_grid2, new_robot2) =
            let
                ((x1, y1), (x2, y2), _action) = action r_new_grid1 new_robot1 False
                (_grid, _robot) =
                    let
                        _grid0 = set_matrix new_grid1 _rx _ry (remove_item ((new_grid1 !! _rx) !! _ry) 'R')
                        _grid1 =
                            if (carrie_up new_robot1) then
                                set_matrix _grid0 _rx _ry (remove_item ((_grid0 !! _rx) !! _ry) 'N')
                            else _grid0
                        _grid2 =
                            if (carrie_up new_robot1) then
                                set_matrix _grid1 x1 y1 (((_grid1 !! x1) !! y1) ++ "N")
                            else _grid1
                        _grid3 = set_matrix _grid2 x1 y1 (((_grid2 !! x1) !! y1) ++ "R")
                        _new_robot = IRobot (x1,y1) (carrie_up new_robot1) (x2,y2)
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