# Cargar librerías
library(mapedit)
library(mapview)
library(sf)

# Abrir el visor interactivo para dibujar (polígonos, puntos, líneas, etc.)
ambito <- mapedit::drawFeatures()

# El objeto resultante es de clase sf (simple feature)
print(ambito)

# Ver el mapa dibujado
mapview(ambito)

# Ver sistema de referencia
st_crs(ambito)

# Asignar CRS si no tiene (por ejemplo, WGS84)
st_crs(ambito) <- "+proj=longlat +datum=WGS84 +no_defs"

# Guardar en formato .shp
st_write(ambito, "SHP/Poligono.shp")


# Cargar librerías (pacman facilita instalar/cargar en un solo paso)
require(pacman)                                                          # Carga el paquete pacman (si no está instalado, instálalo antes)
pacman::p_load(terra, fs, sf, tidyverse, ggspatial, RColorBrewer, gtools, glue, geodata)
# ^ Instala/carga, en caso necesario, todos los paquetes listados.

g <- gc(reset = TRUE)                                                    # Limpia/compacta memoria (garbage collector) y devuelve info
rm(list = ls())                                                          # Vacía el entorno de objetos en memoria
options(scipen = 999, warn = -1)                                         # Evita notación científica y suprime mensajes de warning
# ---------------------------------------------------------------
# URL base (una por cada año). glue inserta los años en el patrón de texto.

urls <- glue(
  'https://storage.googleapis.com/mapbiomas-public/initiatives/peru/collection_3/LULC/peru_collection3_integration_v1-classification_{1985:2024}.tif'
) |> as.character()                                                      # Convierte el vector glue a character

## Seleccionar algunos años
year <- seq(1985, 2024, 1)                                               # Vector: 2000, 2005, 2010, 2015, 2020
urls <- urls[grep(paste0(year, collapse = '|'), urls, value = FALSE)]    # Filtra las URLs que contienen esos años
basename(urls)                                                           # Muestra solo el nombre de archivo (sin ruta), a modo de chequeo

# Descargar ------------------------------------------------------
map(.x = 1:length(urls), .f = function(i){                               # Recorre cada índice de URL con purrr::map
  
  ## Construir la URL y la ruta de salida
  cat('Descargando: ', basename(urls[i]), '\n')                          # Mensaje en consola con el nombre del archivo
  urle <- urls[i]                                                         # URL específica para el índice i
  dout <- glue('./tif/mapbiomas_per-raw')                                # Carpeta de salida para los .tif
  dir_create(dout)                                                       # Crea la carpeta si no existe (fs::dir_create)
  dnme <- basename(urle)                                                 # Nombre del archivo .tif
  fout <- paste0(dout, '/', dnme)                                        # Ruta completa de salida
  
  ## (Re)crear el directorio por seguridad
  dir_create(dout)                                                       # Idempotente: no falla si ya existe
  
  ## Descargar si no existe
  if(!file.exists(fout)){                                                # Solo descarga si el archivo no está ya en disco
    cat('-> Bajando archivo...\n')
    download.file(url = urle, destfile = fout, mode = 'wb')              # Descarga binaria (wb) para archivos .tif
  } else {
    cat('-> Ya existe. Saltando.\n')                                     # Mensaje si ya estaba descargado
  }
  
  ## Cierre/limpieza por iteración
  rm(urle, dout, dnme, fout)                                             # Libera variables temporales
  gc(reset = TRUE)                                                       # Limpia memoria nuevamente
  cat('Listo!\n')                                                        # Mensaje de fin de iteración
})

## Seleccionar Cali (municipio “Santiago de Cali”)
cali <-  st_read("SHP/Poligono.shp")                                                            # Filtra el polígono correspondiente a Cali

## Leer los ráster descargados
fles <- dir_ls('./tif/mapbiomas_per-raw')                                # Lista de archivos .tif en la carpeta
rstr <- rast(fles)                                                       # Crea un SpatRaster multi-capa con terra::rast

## Recortar / enmascarar al límite de Cali
rstr <- terra::crop(rstr, cali)                                          # Recorta el ráster al bounding box de Cali
rstr <- terra::mask(rstr, cali)                                          # Enmascara (deja NA fuera del polígono de Cali)

names(rstr) <- glue('landcover_{seq(1985, 2024, 1)}')                    # Nombra las capas por año (2000..2020 cada 5)

## Vista rápida
plot(rstr)                                                               # Dibuja un plot simple de todas las capas

## Guardar el ráster resultante
dir_create('./tif/mapbiomas_Tamb-raw')                                   # Asegura la carpeta de salida
terra::writeRaster(
  x = rstr,
  filename = './tif/mapbiomas_Tamb-raw/landcover_2000-2024.tif',
  overwrite = FALSE                                                      # No sobreescribir si ya existe
)

# Ráster a tabla -----------------------------------------------------------
vles <- terra::as.data.frame(rstr, xy = TRUE)                            # Convierte a data.frame incluyendo columnas x,y
vles <- vles |> gather(var, value, -c(x, y))                             # Pivota de ancho a largo: var = capa (año), value = código
vles <- as_tibble(vles)                                                  # Convierte al tibble (impresión más amigable)
unique(vles$value)                                                       # Inspecciona los códigos de clase presentes

## Leyenda de MapBiomas Perú - Colección 3
## Basado en los códigos presentes en el ráster
## Fuente: https://peru.mapbiomas.org/wp-content/uploads/sites/14/2024/11/Codigo-de-la-Leyenda-Coleccion3_Peru.pdf

library(tibble)
library(dplyr)

library(dplyr)
library(tibble)
library(ggplot2)
library(readr)  # parse_number

## 1) Leyenda completa (Perú, Colección 3)
lgnd_all <- tribble(
  ~value, ~class,                                        ~color,
  1,    "Formación boscosa",                            "#1f8d49",
  3,    "Bosque",                                       "#1f8d49",
  4,    "Bosque seco",                                  "#7dc975",
  5,    "Manglar",                                      "#04381d",
  6,    "Bosque inundable",                             "#026975",
  10,    "Formación natural no boscosa",                 "#d6bc74",
  11,    "Zona pantanosa o pastizal inundable",          "#519799",
  12,    "Pastizal / herbazal",                          "#d6bc74",
  29,    "Afloramiento rocoso",                          "#ffaa5f",
  66,    "Matorral",                                     "#a89358",
  70,    "Loma costera",                                 "#be9e00",
  13,    "Otra formación no boscosa",                    "#d89f5c",
  14,    "Área agropecuaria",                            "#ffefc3",
  15,    "Pasto",                                        "#edde8e",
  18,    "Agricultura",                                  "#e974ed",
  35,    "Palma aceitera",                               "#9065d0",
  40,    "Arroz",                                        "#c71585",
  72,    "Otros cultivos",                               "#910046",
  9,    "Plantación forestal",                          "#7a5900",
  21,    "Mosaico agropecuario",                         "#ffefc3",
  22,    "Área sin vegetación",                          "#d4271e",
  23,    "Playa",                                        "#ffa07a",
  24,    "Infraestructura urbana",                       "#d4271e",
  30,    "Minería",                                      "#9c0027",
  32,    "Salina costera",                               "#fc8114",
  61,    "Salar",                                        "#f5d5d5",
  68,    "Otra área natural sin vegetación",             "#E97A7A",
  25,    "Otra área sin vegetación",                     "#db4d4f",
  26,    "Cuerpo de agua",                               "#2532e4",
  33,    "Río",                                          "#2532e4",  #, lago u océano
  31,    "Acuicultura",                                  "#091077",
  34,    "Glaciar",                                      "#93dfe6",
  27,    "No observado",                                 "#ffffff"
)

## 2) Filtrar la leyenda solo a los códigos presentes en tu ráster
vals_present <- sort(unique(vles$value))  # tu salida: 33 23 25 21 11 6 3 30 24 15
lgnd <- lgnd_all %>%
  semi_join(tibble(value = vals_present), by = "value") %>%
  arrange(value)

## 3) Preparar 'vles' limpio y unir (garantizando columna 'class')
vles_clean <- vles %>%
  select(x, y, var, value) %>%                  # asegura columnas base
  mutate(year = parse_number(var)) %>%          # año numérico desde el nombre de capa
  left_join(lgnd, by = "value", suffix = c("", ".lgnd"))  # añade class y color SIN cambiar nombres

# Verificación rápida (debe existir 'class')
stopifnot("class" %in% names(vles_clean))

## 4) Vector de colores con nombres (clase -> color)
clrs <- setNames(lgnd$color, lgnd$class)



# 1) Filtrar solo 2000 (opcional: y la variable landcover_2000)
v2000 <- vles_clean %>%
  filter(year == 2000, var == "landcover_2000") %>%   # quita la 2ª condición si no la necesitas
  select(x, y, class, color)

# 2) Paleta a partir de la tabla (class -> color)
pal <- v2000 %>%
  distinct(class, color) %>%
  arrange(class)
pal_vec <- setNames(pal$color, pal$class)

# --- 1) Ordenar la leyenda dejando al final "Zona pantanosa o pastizal inundable"
ord_base <- v2000 %>% distinct(class) %>% arrange(class) %>% pull(class)
ord <- c(setdiff(ord_base, "Zona pantanosa o pastizal inundable"),
         "Zona pantanosa o pastizal inundable")

# reordenar factor de la capa
v2000 <- v2000 %>% mutate(class = fct_relevel(class, !!!ord))

# vector de colores en el mismo orden (si pal_vec es named class -> color)
pal_ord <- pal_vec[ord]

# --- 2) Mapa pulido
g2000 <- ggplot(v2000, aes(x = x, y = y)) +
  geom_raster(aes(fill = class)) +
  scale_fill_manual(
    values = pal_ord,
    breaks = ord,                 # respeta el orden pedido
    drop   = FALSE,
    name   = "Cobertura",
    guide  = guide_legend(
      ncol = 4, byrow = TRUE,     # cambia a 3 o 5 si prefieres
      keyheight = unit(5, "mm"),
      keywidth  = unit(5, "mm"),
      label.hjust = 0
    )
  ) +
  coord_equal(expand = FALSE) +
  labs(
    title = "                                                           2000",
    x = "Longitud", y = "Latitud",
    caption = "Fuente: MapBiomas • Elaboración: GflorezC"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title   = element_text(face = "bold", size = 12),
    axis.text.y  = element_text(angle = 90),
    legend.text  = element_text(size = 8),
    plot.margin  = margin(6, 6, 6, 6)
  )+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")

g2000

ggsave(
  plot = g2000,
  filename = './png/Map 2000.jpg',                         # Ruta/salida
  units = 'in', width = 6, height = 8, dpi = 800,                        # Tamaño y resolución para publicación
  create.dir = TRUE                                                      # Crea la carpeta ./png si no existe
)




# Paquetes
library(dplyr)
library(ggplot2)
library(forcats)
library(ggspatial)   # annotation_scale / annotation_north_arrow
library(grid)        # unit()

# ====== 0) Paleta y orden GLOBAL (idénticos para todos los años) ======
pal_all <- vles_clean %>% distinct(class, color) %>% arrange(class)
pal_vec <- setNames(pal_all$color, pal_all$class)

ord_all <- c(
  setdiff(pal_all$class, "Zona pantanosa o pastizal inundable"),
  "Zona pantanosa o pastizal inundable"
)

# Carpeta de salida
if (!dir.exists("./png")) dir.create("./png", recursive = TRUE)

# ====== 1) Función para un año ======
map_year <- function(yr) {
  var_y <- paste0("landcover_", yr)
  
  df <- vles_clean %>%
    filter(year == yr, var == var_y) %>%
    select(x, y, class)
  
  if (nrow(df) == 0) {
    message("No hay datos para el año ", yr, " (", var_y, "). Se omite.")
    return(invisible(NULL))
  }
  
  df <- df %>% mutate(class = fct_relevel(class, !!!ord_all))
  
  g <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = class)) +
    scale_fill_manual(
      values = pal_vec[ord_all],
      breaks = ord_all,
      drop   = FALSE,
      name   = "Cobertura",
      guide  = guide_legend(
        ncol = 4, byrow = TRUE,
        keyheight = unit(5, "mm"),
        keywidth  = unit(5, "mm"),
        label.hjust = 0
      )
    ) +
    coord_equal(expand = FALSE) +
    labs(
      title   = as.character(yr),
      x = "Longitud", y = "Latitud",
      caption = "Fuente: MapBiomas • Elaboración: GflorezC"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title = element_text(face = "bold"),
      legend.text  = element_text(size = 8),
      plot.title   = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.text.y  = element_text(angle = 90),
      plot.margin  = margin(6, 6, 6, 6)
    ) +
    annotation_north_arrow(
      location = "tr", which_north = "true",
      style = north_arrow_fancy_orienteering()
    ) +
    ggspatial::annotation_scale(
      location = "bl", bar_cols = c("grey60", "white")
    )
  
  # Exportar
  ggsave(
    filename = sprintf("./png/Map_%d.jpg", yr),
    plot = g, units = "in", width = 6, height = 8,
    dpi = 600, device = "jpeg", bg = "white"
  )
  
  message("Mapa exportado: ./png/Map_", yr, ".jpg")
  invisible(g)
}

# ====== 2) Ejecutar para 2000–2004 ======
years <- 1985:2024
invisible(lapply(years, map_year))


library(av)

# === 1) Define el rango de 24 años (ajústalo si quieres 2000–2023) ===
years <- 1985:2024  # 24 imágenes

# Rutas esperadas ./png/Map_YYYY.jpg
files <- sprintf("./png/Map_%d.jpg", years)

# === 2) Comprobación: ¿existen todas? ===
missing <- files[!file.exists(files)]
if (length(missing)) {
  stop("Faltan las siguientes imágenes:\n", paste(missing, collapse = "\n"))
}

# === 3) Codifica el video (0.5 s/imagen => 2 fps) en 1080p ===
av::av_encode_video(
  input     = files,
  output    = sprintf("maps_%d_%d_1080p_2.mp4", min(years), max(years)),
  framerate = 2,  # 0.5 s por imagen
  codec     = "libx264",
  # reescala a 1920 px de ancho manteniendo aspecto; SAR=1 evita “stretch”
  vfilter   = "scale=1920:-2:flags=lanczos,setsar=1"
)

cat("✅ Video creado: ", sprintf("maps_%d_%d_1080p.mp4\n", min(years), max(years)))










