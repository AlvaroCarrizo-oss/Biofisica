# Proyecto Semestral Grupo 5 

Trabajo grupal para asignatura de ICH3600 Biofísica Ambiental, Pontificia Universidad Católica de Chile

## Contenido del repositorio

- `Avance 1`: primer script creado por Simón.
- `avance_1_mod`: script modificado por ACR, falta solo calcular almacenamiento.
- `figuras/`: carpeta donde se guardan los gráficos generados
- `graficos/`: carpeta donde se guardan los gráficos generados.
- `Avance 1.Rproj`: archivo de proyecto de RStudio.

## Cosas importantes

Antes de ejecutar el script:
- El paquete **here** permite utilizar las rutas relativas de los archivos. **Importante que los archivos a leer se encuentren siempre en la misma carpeta que el Script**.
- Para que les corra todo el Rscript, deben colocar en la misma carpeta del .Rproject y script, los archivos de SoilMaps, Rosseta_mean, LandCover y DEM
- instalar los siguientes paquetes en R si no están disponibles:
```r
install.packages(c("tidyverse", "here"))
