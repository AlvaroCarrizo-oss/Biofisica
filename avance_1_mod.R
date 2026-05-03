#Primera modificación: Simón Martínez (generación polígono, LandCover, DEM, PieChart con la distribución de cobertura)
#Segunda Modificación: Alvaro Carrizo (modificación código simón, generación de función para el polígono para cortar fácilmente y guardar .svg rápido igual, calculo de texturas, cc y pmp por horizontes)


#paquetes
install.packages("leaflet")
install.packages("raster")
install.packages("RColorBrewer")
install.packages("sfheaders")
install.packages("terra")
install.packages("ggplot2")
install.packages("here")
install.packages("paletteer")

#librerias
library(sf)
library("leaflet")
library("RColorBrewer")
library("raster")
library(sfheaders)
library(terra)
library(ggplot2)
library(here)
library(paletteer)

#### Fijar directorio de trabajo

fig_dir <- here("figuras")
soil_dir <- here("SoilMaps_MEAN")
rosetta_dir <- here("ROSETTA_MEAN")
#### Paquete sf y leaflet
map <- leaflet()
addTiles(map = map)
map <- addProviderTiles(map = map, "Esri.WorldImagery", group = "ESRI")
map

#### Punto seleccionado (UTM)             #el código dice sur, pero en el sheet sale como norte 
coordenada <- c(este = 121165.911441601 , sur = 5863133.22043047, label = "mi punto") ##esto es solo un vector con los datos de su punto, aún no es un objeto espacial
coordenada_m <- as.matrix(t(coordenada))
coordenada_df <- as.data.frame.matrix(coordenada_m)
coordenada_df[,1:2] <- apply(coordenada_df[1:2], 2, as.numeric)
##transformarlo a un objeto espacial
punto <- st_as_sf(coordenada_df, ##aqui lee un dataframe por lo tanto puede agregar muchos puntos
                  coords = c("este", "sur"), ##aqui van los nombres de sus columnas
                  crs = 9155) ###este CRS es para UTM
punto

#### Ubicar punto en el map. 
crs_latlong <- crs("+proj=longlat +ellps=GRS80 +datum=WGS84 +units=m +no_defs")#sistema de referencia 
punto_latlng <- st_transform(punto, crs = crs_latlong)
map <- addMarkers(map = map, lat = st_coordinates(punto_latlng)[2], lng = st_coordinates(punto_latlng)[1], popup = c(punto_latlng$label))
map

#### Crear un poligono cuadrado, a partir de un punto (centroide)
# Área deseada: 30 km2
area_m2 <- 30 * 1000000 #30km2 a Xm2
lado_m <- sqrt(area_m2)
mitad <- lado_m / 2

# Coordenadas del centro
xy <- st_coordinates(punto)[1,]
x <- xy[1] 
y <- xy[2]

# Crear cuadrado centrado en punto  #deberían ser 4 puntos/vertices
coords_cuadrado <- matrix(c(
  x - mitad, y - mitad,
  x + mitad, y - mitad,
  x + mitad, y + mitad,
  x - mitad, y + mitad,
  x - mitad, y - mitad
), ncol = 2, byrow = TRUE)

##transformar el poligono a un objeto espacial
cuadrado <- st_sf(
  label = "poligono 30 km2",
  geometry = st_sfc(st_polygon(list(coords_cuadrado)), crs = st_crs(punto))
)
cuadrado

# Area del polígono (m2)
st_area(cuadrado)

# Pasar a lat/long para leaflet
crs_latlong <- crs("+proj=longlat +ellps=GRS80 +datum=WGS84 +units=m +no_defs") #repetición código línea 45
punto_latlng <- st_transform(punto, crs = crs_latlong)
cuadrado_latlng <- st_transform(cuadrado, crs = crs_latlong)

# visualizar polígono en el Mapa
map <- leaflet()
map <- addTiles(map)
map <- addProviderTiles(map, "Esri.WorldImagery", group = "ESRI")
map <- addMarkers(map, data = punto_latlng, popup = ~label) #punto en el mapa
map <- addPolygons(map, data = cuadrado_latlng, color = "red", weight = 2, #poligono en el mapa
                   fillColor = "red", fillOpacity = 0.2, popup = ~label)
#opacidad

map

#### Guardar el poligono como shapefile en la carpeta
st_write(cuadrado, dsn = file.path(fig_dir, "polygon.shp"), driver = "ESRI Shapefile", delete_layer = TRUE) #fig_dir es el directorio general, para que se pueda abrir en cualquier pc sin problema, delete_layer = TRUE es para sobreescribir, el archivo cada vez que se corra el cod

#### Leer el shapefile guardado
polygon <- read_sf(file.path(fig_dir, "polygon.shp"), layer = "polygon")
head(polygon)
crs_latlong <- crs("+proj=longlat +ellps=GRS80 +datum=WGS84 +units=m +no_defs")
crs_UTM <- crs("+init=EPSG:9155")
polygon_latlng <- st_transform(polygon, crs = crs_latlong)

# Mapa con nuevo poligono
map <- leaflet()
map <- addTiles(map)
map <- addProviderTiles(map, "Esri.WorldImagery", group = "ESRI")
map <- addPolygons(map, data = polygon_latlng, color = "red", weight = 2, #poligono en el mapa
                   fillColor = "red", fillOpacity = 0.2, popup = ~label)
map

#se genera código con función para recortar cualquier raster (r, tal como DEM_Chile Continental, LandCover, SoilMap, etc) con el polígono generado anteriormente (polygon)
#se define con funtion(variables independientes, en este caso ráster y polígono), luego de { se comienza a definir los pasos de esta función
poligono_recorte <- function(r,polygon){
  polygon_r <- st_transform(polygon, crs = crs(r)) #esto hace que el polígono (ahora polygon_r) tenga mismo sistema de coordenadas que el raster a cortar 
  polygon_v <- vect(polygon_r) #se transforma el polígono a formato SpatVector del paquete terra, importante para pasos posteriores 
  r_crop <- crop(r, polygon_v) # recorta el ráster por el área rectangular del polígono vectorizado (polygon_v), no específico a la forma
  r_mask <- mask(r_crop, polygon_v) #respeta la forma del polígono y los puntos que estaban en el área, pero no en la forma del polígono se pasan a valores N.A
  return(r_mask) #es el = para una función r_mask = f(r, polygon) como si fuese y = f(x,z)
}

#se define una función para guardar los ráster recortados a archivos .svg, para evitar colocarlo siempre. 
# sigue con la forma de función, en este caso, tiene más archivos de entrada, ráster(r), polígono (polygon)
# Además, define el título que tendrán los gráficos, el nombre del archivo de salida y el método de reproyección para el ráster, en este caso bilinear

guardar_svg_raster <- function(r, polygon, titulo, archivo_salida, metodo = "bilinear",paleta = viridisLite::viridis(100)){
  r_cut <- poligono_recorte(r, polygon) #llama a la función anterior, es decir, para recortar el raster al polígono
  r_plot <- project(r_cut, st_crs(polygon)$wkt, method = metodo) # transforma el raster cortado por el polígono a coordenadas consistentes entre DEM y polígono
   
  svg(file.path(fig_dir, archivo_salida), width = 8, height = 6) #esto indica que se va a hacer un archivo .svg, define donde se va a guardar, con qué nombre y las dimensiones
  plot(r_plot, main = titulo, col = paleta) #indica que se va a graficar
  plot(vect(st_transform(polygon, crs(r_plot))), add = TRUE, border = "black", lwd = 1.5) #nuevamente, transforma los resultados a coordenadas coherentes entre sí
  dev.off()
  
  return(r_plot)
}
#### Recortar Land cover según el polígono
lc<- rast(here("CLDynamicLandCover_2018_1.0.tif"))#elemento raster
lc
#### Recortar Land cover según el polígono
lc <- rast(here("CLDynamicLandCover_2018_1.0.tif"))
lc
plot(lc)

lc_cut <- poligono_recorte(lc, polygon)

plot(lc_cut)

# reproyección final para dejarlo consistente con el polígono
lc.proj <- project(lc_cut, st_crs(polygon)$wkt, method = "near")
plot(lc.proj, main = "LC proyectado")

# tabla completa de clases
clases <- data.frame(
  value = 1:16,
  cobertura = c(
    "Agua",
    "Playas/dunas",
    "Bosque mediterráneo",
    "Bosque templado",
    "Plantación de hoja ancha",
    "Árboles frutales",
    "Glaciar/nieve",
    "Vegetación ripariana",
    "Matorrales",
    "Plantación exótica de hojas aciculadas",
    "Praderas y plantaciones anuales",
    "Praderas siempre verde",
    "Suelo desnudo",
    "Turberas",
    "Urbanización",
    "Plantación cosechada"
  )
)

# clases presentes en el raster
vals_presentes <- freq(lc.proj)[, "value"]
clases_presentes <- clases[clases$value %in% vals_presentes, ] #no hay glaciares ni turberas

# convertir a raster categórico
lc.cat <- as.factor(lc.proj)
levels(lc.cat) <- clases

plot(lc.cat, type = "classes")

RColorBrewer::display.brewer.all() # paletas de colores disponibles
#colores = RColorBrewer::brewer.pal(12, "Paired") #hay 14 clases pero solo 12 colores
#colores <- colorRampPalette(brewer.pal(12, "Paired"))(14) #colores muy similares y sin sentido ambiental

colores <- c(
  "1"  = "#2C7FB8",  # Agua (azul)
  "2"  = "#FDD49E",  # Playas/dunas (arena)
  "3"  = "#66C2A5",  # Bosque mediterráneo (verde-azulado suave)
  "4"  = "#1B7837",  # Bosque templado (verde oscuro intenso)
  "5"  = "#B8E186",  # Plantación hoja ancha (verde-amarillento)
  "6"  = "#E6AB02",  # Árboles frutales (amarillo/naranja distintivo)
  "8"  = "#41B6C4",  # Vegetación ripariana (turquesa)
  "9"  = "#FD8D3C",  # Matorrales (naranjo)
  "10" = "#6BAED6",  # Plantación aciculada (azulado)
  "11" = "#9E9AC8",  # Praderas/anuales (lila)
  "12" = "#C7E9C0",  # Praderas siempre verde (muy claro)
  "13" = "#8C510A",  # Suelo desnudo (café)
  "15" = "#969696",  # Urbanización (gris)
  "16" = "#3182BD"   # Plantación cosechada (azul)
) #jugar con los colores para mejor representación

svg(file.path(fig_dir, "landcover_poligono.svg"), width = 8, height = 6)

plot(lc.cat, main = "LandCover Polígono", col = colores)

dev.off()

#### Grafico de Torta
# frecuencia de pixeles sobre el lc proyectado (raster categórico)
freq_lc <- as.data.frame(freq(lc.proj))
freq_lc <- na.omit(freq_lc)
#preguntar si se pueden eliminar las coberturas con poco porcentaje para mejorar la representación

# unir clases con frecuencias
lut <- freq_lc[, c("value", "count")]# crea un nuevo data frame llamado lut, tomando solo las columnas value y count 
names(lut)[names(lut) == "count"] <- "npix"# Cambia el nombre de la columna "count" a "npix"
i <- match(lut$value, clases$value)# Busca en que posición de clases$value aparece cada valor de lut$value
cols_extra <- setdiff(names(clases), "value")# Identifica qué columnas de "clases" se van a agregar a "lut",
lut[cols_extra] <- clases[i, cols_extra, drop = FALSE]# Agrega a "lut" las columnas de "clases" según la coincidencia de "value"
lut$prop <- round(100 * lut$npix / sum(lut$npix), 2)# Calcula el porcentaje de cada cobertura en polygon
lut$plot_name <- paste0(lut$cobertura, " ", lut$prop, "%")# Crear una nueva columna de texto llamada "plot_name", que ira en la leyenda del gráfico 
lut

names(colores) <- unique(lut$plot_name) #se asignan los nombres de plot names a los colores para poder graficar

# grafico de torta (pie chart)
pie_chart <- lut %>% ggplot(aes(x = '', y = prop, fill = plot_name))+
  geom_bar( 
    stat = 'identity', width = 1,
    color="white"
  )+
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_manual(values = colores)+# remove background, grid, numeric labels
  labs(fill = 'Cobertura', title = 'Porcentaje del área cubierto por cada cobertura de suelo')

svg(file.path(fig_dir, "pie_chart_landcover.svg"), width = 8, height = 6)
print(pie_chart)
dev.off()



######recorte raster DEM
# cargar raster DEM #descargar del drive del anuncio del avance 1
DEM <- rast(here("DEM.Chile.Continental.tif"))

# recortar y enmascarar
DEM_cut <- poligono_recorte(DEM, polygon)


# visualizar resultado
DEM.proj = project(DEM_cut, st_crs(polygon)$wkt, method = "bilinear")

svg(file.path(fig_dir,"DEM_poligono.svg"), width = 8, height = 6)

plot(DEM.proj, main = "DEM proyectado")

dev.off()

#minimo y máximo
valores <- global(DEM.proj, fun = c("min", "max"), na.rm = TRUE)

elev_min <- valores[1,1]
elev_max <- valores[1,2]

cat("Elevación mínima:", elev_min, "m\n")
cat("Elevación máxima:", elev_max, "m\n")

###Mapas usando CLSoilMaps
#artículo https://doi.org/10.1038/s41597-023-02536-x
#database https://zenodo.org/records/7464210?preview_file=FileDesc.txt
#descargar SoilMaps_MEAN y ROSETTA_MEAN 

#crear horizontes

horizontes <- c("0-5","5-15","15-30","30-60","60-100","100-200")

#### Fijar carpeta de mapa de texturas
direct_texture <- here("SoilMaps_MEAN") 


#corte por arcillas

Clay_cut <- list() #creará una lista para guardar los raster que serán llenados por el for siguiente
paleta_arcilla <- hcl.colors(100, "YlOrBr")

for (hz in horizontes) {
  archivo <- paste0("Clay.", hz, "cm.tif") #busca el archivo raster correspondiente al horizonte analizado
  nombre_svg <- paste0("arcilla_", gsub("-", "_", hz), "cm.svg") #le asigna un nombre al .svg que entregará 
  
  clay_r <- rast(file.path(direct_texture, archivo)) #busca en la carpeta de texturas, el archivo "Clay.hz.cm.tif" y lo transforma en raster
  
  Clay_cut[[hz]] <- guardar_svg_raster(
    r = clay_r,
    polygon = polygon,
    titulo = paste("Arcilla (%) -", hz, "cm"),
    paleta = paleta_arcilla,
    archivo_salida = nombre_svg,
    metodo = "bilinear"
    
  )  #guarda en svg, según la función guardar_svg_raster para el raster hz buscado en el directorio 
}

clay_cluster_agrup <- rast(Clay_cut)
min_global <- global(clay_cluster_agrup, "min", na.rm = TRUE)[1,1]
max_global <- global(clay_cluster_agrup, "max", na.rm = TRUE)[1,1]
min_global
max_global

svg(file.path(fig_dir, "porcentaje_arcilla_en_cada_nivel.svg"), width=12, height=8 )
par(mfrow = c(2, 3), mar = c(3, 3, 3, 5))

for (hz in horizontes) {
  plot(
    Clay_cut[[hz]],
    main = paste("Arcilla (%) -", hz, "cm"),
    col = paleta_arcilla,
    range = c(min_global, max_global)
  )
  plot(vect(st_transform(polygon, crs(Clay_cut[[hz]]))), add = TRUE, border = "black", lwd = 1.2)
}

dev.off()

sands_cut <- list()
paleta_arena <- colorRampPalette(c("#fff7bc", "#fee391", "#fec44f", "#fe9929", "#d95f0e"))(100)

for (hz in horizontes) {
  archivo <- paste0("Sand.", hz, "cm.tif")
  nombre_svg <- paste0("arena_", gsub("-", "_", hz), "cm.svg")
  
  sand_r <- rast(file.path(direct_texture, archivo))
  
  sands_cut[[hz]] <- guardar_svg_raster(
    r = sand_r,
    polygon = polygon,
    titulo = paste("Arena (%) -", hz, "cm"),
    archivo_salida = nombre_svg,
    metodo = "bilinear",
    paleta = paleta_arena
  )
}

sand_cluster_agrup <- rast(sands_cut)
min_sand_global <- global(sand_cluster_agrup, "min", na.rm = TRUE)[1,1]
max_sand_global <- global(sand_cluster_agrup, "max", na.rm = TRUE)[1,1]
min_sand_global
max_sand_global

svg(file.path(fig_dir, "porcentaje_arena_en_cada_nivel.svg"), width=12, height=8 )
par(mfrow = c(2, 3), mar = c(3, 3, 3, 5))

for (hz in horizontes) {
  plot(
    sands_cut[[hz]],
    main = paste("Arena (%) -", hz, "cm"),
    col = paleta_arena,
    range = c(min_sand_global, max_sand_global)
  )
  plot(vect(st_transform(polygon, crs(sands_cut[[hz]]))), add = TRUE, border = "black", lwd = 1.2)
}

dev.off()

silts_cut <- list()
paleta_silt <- hcl.colors(100, "PuBuGn")

for (hz in horizontes) {
  archivo <- paste0("Silt.", hz, "cm.tif")
  nombre_svg <- paste0("limo_", gsub("-", "_", hz), "cm.svg")
  
  silt_r <- rast(file.path(direct_texture, archivo))
  
  silts_cut[[hz]] <- guardar_svg_raster(
    r = silt_r,
    polygon = polygon,
    titulo = paste("Limo (%) -", hz, "cm"),
    paleta = paleta_silt,
    archivo_salida = nombre_svg,
    metodo = "bilinear"
  )
}

silt_cluster_agrup <- rast(silts_cut)
min_silt_global <- global(silt_cluster_agrup, "min", na.rm = TRUE)[1,1]
max_silt_global <- global(silt_cluster_agrup, "max", na.rm = TRUE)[1,1]
min_silt_global
max_silt_global

svg(file.path(fig_dir, "porcentaje_limo_en_cada_nivel.svg"), width=12, height=8 )
par(mfrow = c(2, 3), mar = c(3, 3, 3, 5))

for (hz in horizontes) {
  plot(
    silts_cut[[hz]],
    main = paste("Limo (%) -", hz, "cm"),
    col = paleta_silt,
    range = c(min_silt_global, max_silt_global)
  )
  plot(vect(st_transform(polygon, crs(Clay_cut[[hz]]))), add = TRUE, border = "black", lwd = 1.2)
}

dev.off()

direct_prop_hid <- here("ROSETTA_MEAN") 

Capacidad_de_campo_cut <- list()
paleta_cc <- hcl.colors(100, "Plasma")

for (hz in horizontes){
  archivo <- paste0("FC.", hz, "cm.tif")
  nombre_svg <- paste0("capacidad_campo", gsub("-","_",hz), "cm.svg")
  
  capacidad_campo_r <- rast(file.path(direct_prop_hid, archivo))
  
  Capacidad_de_campo_cut[[hz]]<- guardar_svg_raster(
    r = capacidad_campo_r,
    polygon = polygon,
    titulo =  paste("Capacidad de campo - ", hz, "cm"),
    paleta = paleta_cc,
    archivo_salida = nombre_svg,
    metodo = "bilinear"
  )
}


Capacidad_de_campo_cluster <- rast(Capacidad_de_campo_cut)
min_cc_global <- global(Capacidad_de_campo_cluster, "min", na.rm = TRUE)[1,1]
max_cc_global <- global(Capacidad_de_campo_cluster, "max", na.rm = TRUE)[1,1]
min_cc_global
max_cc_global

svg(file.path(fig_dir, "cc_cada_nivel.svg"), width=12, height=8 )
par(mfrow = c(2, 3), mar = c(3, 3, 3, 5))

for (hz in horizontes) {
  plot(
    Capacidad_de_campo_cut[[hz]],
    main = paste("Capacidad de campo -", hz, "cm"),
    col = paleta_cc,
    range = c(min_cc_global, max_cc_global)
  )
  plot(vect(st_transform(polygon, crs(Capacidad_de_campo_cut[[hz]]))), add = TRUE, border = "black", lwd = 1.2)
}

dev.off()

                    
Punto_marchitez_cut <- list()
paleta_pm  <- hcl.colors(100, "Heat")

for (hz in horizontes){
  archivo <- paste0("PWP.", hz, "cm.tif")
  nombre_svg <- paste0("punto de marchitez", gsub("-","_",hz), "cm.svg")
  
  punto_marchitez_r <- rast(file.path(direct_prop_hid, archivo))
  
  Punto_marchitez_cut[[hz]]<- guardar_svg_raster(
    r = punto_marchitez_r,
    polygon = polygon,
    titulo =  paste("Punto de marchitez - ", hz, "cm"),
    archivo_salida = nombre_svg,
    paleta = paleta_pm,
    metodo = "bilinear"
  )
}

Punto_marchitez_cluster <- rast(Punto_marchitez_cut)
min_pmp_global <- global(Punto_marchitez_cluster, "min", na.rm = TRUE)[1,1]
max_pmp_global <- global(Punto_marchitez_cluster, "max", na.rm = TRUE)[1,1]
min_pmp_global
max_pmp_global

svg(file.path(fig_dir, "pmp_cada_nivel.svg"), width=12, height=8 )
par(mfrow = c(2, 3), mar = c(3, 3, 3, 5))

for (hz in horizontes) {
  plot(
    Punto_marchitez_cut[[hz]],
    main = paste("Punto de Marchitez -", hz, "cm"),
    col = paleta_pm,
    range = c(min_pmp_global, max_pmp_global)
  )
  plot(vect(st_transform(polygon, crs(Punto_marchitez_cut[[hz]]))), add = TRUE, border = "black", lwd = 1.2)
}

dev.off()
