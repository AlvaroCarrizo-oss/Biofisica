# Proyecto Semestral Grupo 5 

Trabajo grupal para asignatura de ICH3600 Biofísica Ambiental, Pontificia Universidad Católica de Chile

## Contenido del repositorio

- `Tarea1_v4.R`: script principal de la Tarea1.
- `data_balance.txt`: base de datos de balances.
- `data_textura.csv`: base de datos de texturas.
- `graficos/`: carpeta donde se guardan los gráficos generados.
- `ProyectoBiofisica.Rproj`: archivo de proyecto de RStudio.

## Paquetes necesarios

Antes de ejecutar el script:
- El paquete here permite utilizar las rutas relativas de los archivos. **Importante que los archivos a leer se encuentren siempre en la misma carpeta que el Script**.
- instalar los siguientes paquetes en R si no están disponibles:
```r
install.packages(c("tidyverse", "here"))
