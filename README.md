# Proyecto Semestral Grupo 5 

Trabajo grupal para asignatura de ICH3600 Biofísica Ambiental, Pontificia Universidad Católica de Chile

## Contenido del repositorio

- `avance_1_v4`: script final.
- `figuras/`: carpeta donde se guardan los gráficos generados
- `Avance 1.Rproj`: archivo de proyecto de RStudio.
- `ROSETTA_MEAN/`: Carpeta que debe contener los Ráster con propiedades hidráulicas
- `SoilMaps_MEAN/`: Carpeta que debe contener los Ráster con granulometrías
- En la carpeta "maestra" debe estar el script, DEM_Chile_Continental, CLDynamicLandCover_2018_1.0 y carpetas "figuras", "ROSETTA_MEAN" y "SoilMaps_MEAN/"

## Cosas importantes

Antes de ejecutar el script:
- El paquete **here** permite utilizar las rutas relativas de los archivos. **Importante que los archivos a leer se encuentren siempre en la misma carpeta que el Script**.
- Para que les corra todo el Rscript, deben colocar en la misma carpeta del .Rproject y script, los archivos de SoilMaps, Rosseta_mean, LandCover y DEM
- instalar los siguientes paquetes en R si no están disponibles:
```r
install.packages(c("tidyverse", "here"))
