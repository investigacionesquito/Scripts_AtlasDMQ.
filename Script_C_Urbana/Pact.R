# Dimensión: Complejidades Urbanas 

# Nombre del Indicador: Proximidad a actividades comerciales de uso cotidiano

# Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# Cargar librerías ----------------------------------------------

pacman::p_load(openxlsx,sf,dplyr)

# ADM_ZONAL -------------------------------------------------------------------

# Cargar la malla H3 y los puntos de actividades comerciales
ADM_ZONAL_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")

# Directorio donde están almacenados todos los shapefiles
directorio_shapefiles <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/CB_Uso_Cotidiano"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_shapefiles, pattern = "\\.shp$", full.names = TRUE)

# Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
leer_shapefiles <- function(archivo) {
  shapefile <- st_read(archivo)
  
  # Verificar si existe la columna 'CALIFICACI' y convertirla a 'character'
  if ("CALIFICACI" %in% colnames(shapefile)) {
    shapefile <- shapefile %>%
      mutate(CALIFICACI = as.character(CALIFICACI))
  }
  
  return(shapefile)
}

# Leer y combinar todos los shapefiles
comercios_sf <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(ADM_ZONAL_DMQ))             # Combinar todos en un solo objeto

# Asignar actividades comerciales a hexágonos (join espacial)
actividades_por_hex_az <- st_join(ADM_ZONAL_DMQ, comercios_sf)

# Contar cuántas tipologías distintas hay por cada hexágono
tipologias_por_hex_az <- actividades_por_hex_az %>%
  group_by(adm_zonal) %>%  # Agrupar por ID de hexágono
  summarize(Pact_N = n_distinct(na.omit(TIPOLOGIA)))                                                      # Contar las tipologías únicas excluyendo NA

# Unir este conteo con la población en cada hexágono

tipologias_por_hex_tabla_az <- tipologias_por_hex_az %>% st_drop_geometry()

# Realizar el left_join sin geometría
malla_h3_con_tipologias_az <- ADM_ZONAL_DMQ %>%
  left_join(tipologias_por_hex_tabla_az, by = "adm_zonal") 

pact_az <- malla_h3_con_tipologias_az
pact_az <- pact_az[, c("adm_zonal","Pact_N","geometry" )]

# Identificar hexágonos con 6 o más tipologías de actividades
hex_con_6_tipologias_az <- malla_h3_con_tipologias_az %>%
  filter(Pact_N >= 6)

# Calcular la población total y la población cubierta por 6 o más tipologías
poblacion_total_az <- sum(malla_h3_con_tipologias_az$SUM_pobT, na.rm = TRUE)
poblacion_con_cobertura_az <- sum(hex_con_6_tipologias_az$SUM_pobT, na.rm = TRUE)

# Calcular Pact (%)
Pact_az_P <- (poblacion_con_cobertura_az / poblacion_total_az) * 100

# Resultado
Pact_az_P

# PARROQUIA  -----------------------------------------------------------------

# Cargar la malla H3 y los puntos de actividades comerciales
PARROQUIA_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")

# Directorio donde están almacenados todos los shapefiles
directorio_shapefiles <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/CB_Uso_Cotidiano"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_shapefiles, pattern = "\\.shp$", full.names = TRUE)

# Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
leer_shapefiles <- function(archivo) {
  shapefile <- st_read(archivo)
  
  # Verificar si existe la columna 'CALIFICACI' y convertirla a 'character'
  if ("CALIFICACI" %in% colnames(shapefile)) {
    shapefile <- shapefile %>%
      mutate(CALIFICACI = as.character(CALIFICACI))
  }
  
  return(shapefile)
}

# Leer y combinar todos los shapefiles
comercios_sf <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(PARROQUIA_DMQ))             # Combinar todos en un solo objeto

# Asignar actividades comerciales a hexágonos (join espacial)
actividades_por_hex_pr <- st_join(PARROQUIA_DMQ, comercios_sf)

# Contar cuántas tipologías distintas hay por cada hexágono
tipologias_por_hex_pr <- actividades_por_hex_pr %>%
  group_by(parroquia) %>%  # Agrupar por ID de hexágono
  summarize(Pact_N = n_distinct(na.omit(TIPOLOGIA)))                                                      # Contar las tipologías únicas excluyendo NA

# Unir este conteo con la población en cada hexágono

tipologias_por_hex_tabla_pr <- tipologias_por_hex_pr %>% st_drop_geometry()

# Realizar el left_join sin geometría
malla_h3_con_tipologias_pr <- PARROQUIA_DMQ %>%
  left_join(tipologias_por_hex_tabla_pr, by = "parroquia") 

pact_pr <- malla_h3_con_tipologias_pr
pact_pr <- pact_pr[, c("parroquia","Pact_N","geometry" )]

# Identificar hexágonos con 6 o más tipologías de actividades
hex_con_6_tipologias_pr <- malla_h3_con_tipologias_pr %>%
  filter(Pact_N >= 6)

# Calcular la población total y la población cubierta por 6 o más tipologías
poblacion_total_pr <- sum(malla_h3_con_tipologias_pr$SUM_pobT, na.rm = TRUE)
poblacion_con_cobertura_pr <- sum(hex_con_6_tipologias_pr$SUM_pobT, na.rm = TRUE)

# Calcular Pact (%)
Pact_pr_P <- (poblacion_con_cobertura_pr / poblacion_total_pr) * 100

# Resultado
Pact_pr_P

# SECTOR --------------------------------------------------------

# Cargar la malla H3 y los puntos de actividades comerciales
SECTOR_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\SECTOR_DMQ.shp")

# Directorio donde están almacenados todos los shapefiles
directorio_shapefiles <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/CB_Uso_Cotidiano"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_shapefiles, pattern = "\\.shp$", full.names = TRUE)

# Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
leer_shapefiles <- function(archivo) {
  shapefile <- st_read(archivo)
  
  # Verificar si existe la columna 'CALIFICACI' y convertirla a 'character'
  if ("CALIFICACI" %in% colnames(shapefile)) {
    shapefile <- shapefile %>%
      mutate(CALIFICACI = as.character(CALIFICACI))
  }
  
  return(shapefile)
}

# Leer y combinar todos los shapefiles
comercios_sf <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(SECTOR_DMQ))             # Combinar todos en un solo objeto

# Asignar actividades comerciales a hexágonos (join espacial)
actividades_por_hex_s <- st_join(SECTOR_DMQ, comercios_sf)

# Contar cuántas tipologías distintas hay por cada hexágono
tipologias_por_hex_s <- actividades_por_hex_s %>%
  group_by(Sector_DMQ) %>%  # Agrupar por ID de hexágono
  summarize(Pact_N = n_distinct(na.omit(TIPOLOGIA)))                                                      # Contar las tipologías únicas excluyendo NA

# Unir este conteo con la población en cada hexágono

tipologias_por_hex_tabla_s <- tipologias_por_hex_s %>% st_drop_geometry()

# Realizar el left_join sin geometría
malla_h3_con_tipologias_s <- SECTOR_DMQ %>%
  left_join(tipologias_por_hex_tabla_s, by = "Sector_DMQ") 

pact_s <- malla_h3_con_tipologias_s
pact_s <- pact_s[, c("Sector_DMQ","Pact_N","geometry" )]

# Identificar hexágonos con 6 o más tipologías de actividades
hex_con_6_tipologias_s <- malla_h3_con_tipologias_s %>%
  filter(Pact_N >= 6)

# Calcular la población total y la población cubierta por 6 o más tipologías
poblacion_total_s <- sum(malla_h3_con_tipologias_s$pobT, na.rm = TRUE)
poblacion_con_cobertura_s <- sum(hex_con_6_tipologias_s$pobT, na.rm = TRUE) 

# Calcular Pact (%)
Pact_s_P <- (poblacion_con_cobertura_s / poblacion_total_s) * 100

# Resultado
Pact_s_P

# MALLA H3 NIVEL 8 -------------------------------------------------------------------

# Cargar la malla H3 y los puntos de actividades comerciales
MALLA_H3_N8_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")

# Directorio donde están almacenados todos los shapefiles
directorio_shapefiles <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/CB_Uso_Cotidiano"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_shapefiles, pattern = "\\.shp$", full.names = TRUE)

# Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
leer_shapefiles <- function(archivo) {
  shapefile <- st_read(archivo)
  
  # Verificar si existe la columna 'CALIFICACI' y convertirla a 'character'
  if ("CALIFICACI" %in% colnames(shapefile)) {
    shapefile <- shapefile %>%
      mutate(CALIFICACI = as.character(CALIFICACI))
  }
  
  return(shapefile)
}

# Leer y combinar todos los shapefiles
comercios_sf <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_H3_N8_DMQ))             # Combinar todos en un solo objeto

# Asignar actividades comerciales a hexágonos (join espacial)
actividades_por_hex_N8 <- st_join(MALLA_H3_N8_DMQ, comercios_sf)

# Contar cuántas tipologías distintas hay por cada hexágono
tipologias_por_hex_N8 <- actividades_por_hex_N8 %>%
  group_by(H3HASH) %>%  # Agrupar por ID de hexágono
  summarize(Pact_N = n_distinct(na.omit(TIPOLOGIA)))                                                      # Contar las tipologías únicas excluyendo NA

# Unir este conteo con la población en cada hexágono

tipologias_por_hex_tabla_N8 <- tipologias_por_hex_N8 %>% st_drop_geometry()

# Realizar el left_join sin geometría
malla_h3_con_tipologias_N8 <- MALLA_H3_N8_DMQ %>%
  left_join(tipologias_por_hex_tabla_N8, by = "H3HASH") 

pact_N8 <- malla_h3_con_tipologias_N8 
pact_N8 <- pact_N8[, c("H3HASH","Pact_N","geometry" )]

# Identificar hexágonos con 6 o más tipologías de actividades
hex_con_6_tipologias_N8 <- malla_h3_con_tipologias_N8 %>%
  filter(Pact_N >= 6)

# Calcular la población total y la población cubierta por 6 o más tipologías
poblacion_total_N8 <- sum(malla_h3_con_tipologias_N8$SUM_pob_t, na.rm = TRUE)
poblacion_con_cobertura_N8 <- sum(hex_con_6_tipologias_N8$SUM_pob_t, na.rm = TRUE)

# Calcular Pact (%)
Pact_N8_P <- (poblacion_con_cobertura_N8 / poblacion_total_N8) * 100

# Resultado
Pact_N8_P

# MALLA H3 NIVEL 9 -------------------------------------------------------------------

# Cargar la malla H3 y los puntos de actividades comerciales
MALLA_H3_N9_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# Directorio donde están almacenados todos los shapefiles
directorio_shapefiles <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/CB_Uso_Cotidiano"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_shapefiles, pattern = "\\.shp$", full.names = TRUE)

# Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
leer_shapefiles <- function(archivo) {
  shapefile <- st_read(archivo)
  
  # Verificar si existe la columna 'CALIFICACI' y convertirla a 'character'
  if ("CALIFICACI" %in% colnames(shapefile)) {
    shapefile <- shapefile %>%
      mutate(CALIFICACI = as.character(CALIFICACI))
  }
  
  return(shapefile)
}

# Leer y combinar todos los shapefiles
comercios_sf <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_H3_N9_DMQ))             # Combinar todos en un solo objeto

# Asignar actividades comerciales a hexágonos (join espacial)
actividades_por_hex_N9 <- st_join(MALLA_H3_N9_DMQ, comercios_sf)

# Contar cuántas tipologías distintas hay por cada hexágono
tipologias_por_hex_N9 <- actividades_por_hex_N9 %>%
  group_by(H3HASH) %>%  # Agrupar por ID de hexágono
  summarize(Pact_N = n_distinct(na.omit(TIPOLOGIA)))                                                      # Contar las tipologías únicas excluyendo NA

# Unir este conteo con la población en cada hexágono

tipologias_por_hex_tabla_N9 <- tipologias_por_hex_N9 %>% st_drop_geometry()

# Realizar el left_join sin geometría
malla_h3_con_tipologias_N9 <- MALLA_H3_N9_DMQ %>%
  left_join(tipologias_por_hex_tabla_N9, by = "H3HASH") 

pact_N9 <- malla_h3_con_tipologias_N9
pact_N9 <- pact_N9[, c("H3HASH","Pact_N","geometry" )]

# Identificar hexágonos con 6 o más tipologías de actividades
hex_con_6_tipologias_N9 <- malla_h3_con_tipologias_N9 %>%
  filter(Pact_N >= 6)

# Calcular la población total y la población cubierta por 6 o más tipologías
poblacion_total_N9 <- sum(malla_h3_con_tipologias_N9$SUM_pob_t, na.rm = TRUE)
poblacion_con_cobertura_N9 <- sum(hex_con_6_tipologias_N9$SUM_pob_t, na.rm = TRUE)

# Calcular Pact (%)
Pact_N9_P <- (poblacion_con_cobertura_N9 / poblacion_total_N9) * 100

# Resultado
Pact_N9_P

# COD1000 -----------------------------------------------------

# Cargar la malla H3 y los puntos de actividades comerciales
MALLA_1000_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")

# Directorio donde están almacenados todos los shapefiles
directorio_shapefiles <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/CB_Uso_Cotidiano"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_shapefiles, pattern = "\\.shp$", full.names = TRUE)

# Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
leer_shapefiles <- function(archivo) {
  shapefile <- st_read(archivo)
  
  # Verificar si existe la columna 'CALIFICACI' y convertirla a 'character'
  if ("CALIFICACI" %in% colnames(shapefile)) {
    shapefile <- shapefile %>%
      mutate(CALIFICACI = as.character(CALIFICACI))
  }
  
  return(shapefile)
}

# Leer y combinar todos los shapefiles
comercios_sf <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_1000_DMQ))             # Combinar todos en un solo objeto

# Asignar actividades comerciales a hexágonos (join espacial)
actividades_por_hex_1000 <- st_join(MALLA_1000_DMQ, comercios_sf)

# Contar cuántas tipologías distintas hay por cada hexágono
tipologias_por_hex_1000 <- actividades_por_hex_1000 %>%
  group_by(COD1000) %>%  # Agrupar por ID de hexágono
  summarize(Pact_N = n_distinct(na.omit(TIPOLOGIA)))                                                      # Contar las tipologías únicas excluyendo NA

# Unir este conteo con la población en cada hexágono

tipologias_por_hex_tabla_1000 <- tipologias_por_hex_1000 %>% st_drop_geometry()

# Realizar el left_join sin geometría
malla_h3_con_tipologias_1000 <- MALLA_1000_DMQ %>%
  left_join(tipologias_por_hex_tabla_1000, by = "COD1000") 

pact_1000 <- malla_h3_con_tipologias_1000
pact_1000 <- pact_1000[, c("COD1000","Pact_N","geometry" )]

# Identificar hexágonos con 6 o más tipologías de actividades
hex_con_6_tipologias_1000 <- malla_h3_con_tipologias_1000 %>%
  filter(Pact_N >= 6)

# Calcular la población total y la población cubierta por 6 o más tipologías
poblacion_total_1000 <- sum(malla_h3_con_tipologias_1000$SUM_pob_t, na.rm = TRUE)
poblacion_con_cobertura_1000 <- sum(hex_con_6_tipologias_1000$SUM_pob_t, na.rm = TRUE)

# Calcular Pact (%)
Pact_1000_P <- (poblacion_con_cobertura_1000 / poblacion_total_1000) * 100

# Resultado
Pact_1000_P
# COD500 -----------------------------------------------------

# Cargar la malla H3 y los puntos de actividades comerciales
MALLA_500_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_500_DMQ.shp")

# Directorio donde están almacenados todos los shapefiles
directorio_shapefiles <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/CB_Uso_Cotidiano"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_shapefiles, pattern = "\\.shp$", full.names = TRUE)

# Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
leer_shapefiles <- function(archivo) {
  shapefile <- st_read(archivo)
  
  # Verificar si existe la columna 'CALIFICACI' y convertirla a 'character'
  if ("CALIFICACI" %in% colnames(shapefile)) {
    shapefile <- shapefile %>%
      mutate(CALIFICACI = as.character(CALIFICACI))
  }
  
  return(shapefile)
}

# Leer y combinar todos los shapefiles
comercios_sf <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_500_DMQ))             # Combinar todos en un solo objeto

# Asignar actividades comerciales a hexágonos (join espacial)
actividades_por_hex_500 <- st_join(MALLA_500_DMQ, comercios_sf)

# Contar cuántas tipologías distintas hay por cada hexágono
tipologias_por_hex_500 <- actividades_por_hex_500 %>%
  group_by(COD500) %>%  # Agrupar por ID de hexágono
  summarize(Pact_N = n_distinct(na.omit(TIPOLOGIA)))                                                      # Contar las tipologías únicas excluyendo NA

# Unir este conteo con la población en cada hexágono

tipologias_por_hex_tabla_500 <- tipologias_por_hex_500 %>% st_drop_geometry()

# Realizar el left_join sin geometría
malla_h3_con_tipologias_500 <- MALLA_500_DMQ %>%
  left_join(tipologias_por_hex_tabla_500, by = "COD500") 

pact_500 <- malla_h3_con_tipologias_500
pact_500 <- pact_500[, c("COD500","Pact_N","geometry" )]

# Identificar hexágonos con 6 o más tipologías de actividades
hex_con_6_tipologias_500 <- malla_h3_con_tipologias_500 %>%
  filter(Pact_N >= 6)

# Calcular la población total y la población cubierta por 6 o más tipologías
poblacion_total_500 <- sum(malla_h3_con_tipologias_500$SUM_pob_t, na.rm = TRUE)
poblacion_con_cobertura_500 <- sum(hex_con_6_tipologias_500$SUM_pob_t, na.rm = TRUE)

# Calcular Pact (%)
Pact_500_P <- (poblacion_con_cobertura_500 / poblacion_total_500) * 100

# Resultado
Pact_500_P


# 6. Guardar los resultados------------------------------------------

pact_az2 <- as.data.frame(pact_az)[, !names(pact_az) %in% "geometry"]
pact_s2 <- as.data.frame(pact_s)[, !names(pact_s) %in% "geometry"]
pact_N8_2 <- as.data.frame(pact_N8)[, !names(pact_N8) %in% "geometry"]
pact_N9_2 <- as.data.frame(pact_N9)[, !names(pact_N9) %in% "geometry"]
pact_1000_2 <- as.data.frame(pact_1000)[, !names(pact_1000) %in% "geometry"]
pact_500_2 <- as.data.frame(pact_500)[, !names(pact_500) %in% "geometry"]
pact_pr2 <- as.data.frame(pact_pr)[, !names(pact_pr) %in% "geometry"]

wb <- createWorkbook("Pact")

addWorksheet(wb, "pact_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "pact_az", x=pact_az2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "pact_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "pact_pr", x=pact_pr2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "pact_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "pact_s", x=pact_s2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "pact_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "pact_1000", x=pact_1000_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "pact_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "pact_500", x=pact_500_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "pact_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "pact_N8", x=pact_N8_2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "pact_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "pact_N9", x=pact_N9_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Número_Proximidad a actividades comerciales de uso cotidiano.xlsx")