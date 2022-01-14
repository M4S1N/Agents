# Agentes #


Se presenta el problema de simular el comportamiento de un robot de limpieza en un ambiente en forma de tablero de $n \times m$. Los elementos del ambiente son:</br>

* Corral: En este solo puede haber un niño y/o un robot. Si hay un niño este queda inmovilizado
* Niños: Se pueden mover por el medio una celda a la vez, son los responsables de generar suciedad y son capaces de mover obstáculos.
* Obstáculos: Pueden ser movidos por los niños, pero no por el robot.
* Suciedad: Es generada por los niños y puede ser limpiada por el robot.
* Robot: Se encarga de llevar los niños hacia el corral y limpiar las celdas sucias.

La tarea fundamental del robot es mantener el $60\%$ de las casillas libres, limpias.

</br>

# Algoritmo #

Se implementaron dos modelos distintos de agentes: agentes reactivos y agentes inteligentes. Para entrar en el entorno de ejecución, es necesario tener instalado `hugs` o `ghci` e iniciar con:
```
ghci Enviroments.hs
```
o
```
hugs Enviroments.hs
```

Una vez dentro del entorno de haskell, es posible iniciar la ejecución con:
```
main <filas> <colummnas> <tiempo_cambio> <cantidad_robot> <tipo_agente>
```
en donde:
* `<filas>` indica la cantidad de filas
* `<columnas>` la cantidad de columnas
* `<tiempo_cambio>` tiempo tras el cual el ambiente cambiará de forma aleatoria
* `<cantidad_robot>` la cantidad de robots que funcionaran en el medio
* `<tipo_agente>` será `True` si el agente es inteligente o `False` si es reactivo

ejemplo:
```
main 5 5 60 2 False
```
