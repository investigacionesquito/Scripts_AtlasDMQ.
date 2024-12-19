# Dimensión: Complejidades Urbanas 

# Nombre del Indicador: Índice de diversidad urbana

# Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# Cargar librerías ----------------------------------------------

pacman::p_load(openxlsx,sf,dplyr,tidyr,vegan,tidyr,tidytable)

# ADM_ZONAL -------------------------------------------------------------------

# Cargar datos 

# 1. Malla estadística 
ADM_ZONAL_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")

# 2. Comercios 
comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(ADM_ZONAL_DMQ))

# Calcular indicador 

# Unir los comercios combinados con la malla estadistica 
comercios_en_malla_az <- st_join(ADM_ZONAL_DMQ, comercios)

# Contar la cantidad de comercios por celda de la malla
total_comercios_az <- comercios_en_malla_az %>%
  group_by(adm_zonal, TIPOLOGIA) %>%                                                 # Agrupar por celda y tipo de comercio
  summarise(n = n(), .groups = "drop") %>%                                        # Contar la cantidad de comercios de cada tipo
  mutate(n = ifelse(is.na(TIPOLOGIA), NA_integer_, n))

# Crear tabla de frecuencias
tabla_frecuencia_comercios_az <- total_comercios_az %>%
  filter(!is.na(TIPOLOGIA)) %>%
  tidytable() %>%
  group_by(adm_zonal, TIPOLOGIA) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  spread(key = TIPOLOGIA, value = n, fill = 0)                                    # Llenar con 0 donde no hay comercios

# Convertir la tabla a una matriz (sin la columna del adm_zonal)
matriz_frecuencia_az <- as.matrix(tabla_frecuencia_comercios_az[, -1])  

# Calcular la entropía de Shannon
entropia_shannon_az <- diversity(matriz_frecuencia_az, index = "shannon")
mean(entropia_shannon_az)
sd(entropia_shannon_az)

# Agregar la entropía de Shannon a la tabla original
tabla_frecuencia_comercios_az$H <- entropia_shannon_az

# Unir la entropía calculada a la malla original
H_az <- ADM_ZONAL_DMQ %>%
  left_join(tabla_frecuencia_comercios_az, by = "adm_zonal") %>%
  select(adm_zonal,H,geometry)

H_az[is.na(H_az)] <- 0

# PARROQUIA -----------------------------------------------------------------

# Cargar datos 

# 1. Malla estadística 
PARROQUIA_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")

# 2. Comercios 
comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(PARROQUIA_DMQ))

# # Leer y combinar todos los shapefiles de comercios
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# lista <- list.files(path = d_comercios, pattern = "\\.shp$", full.names = TRUE)
# 
# # Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
# leer_shapefiles <- function(archivo) {
#   shapefile <- st_read(archivo)
# 
#   if ("CALIFICACI" %in% colnames(shapefile)) {
#     shapefile <- shapefile %>%
#       mutate(CALIFICACI = as.character(CALIFICACI))
#   }
# 
#   return(shapefile)
# }
# 
# # Leer y combinar todos los shapefiles
# comercios <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>%              # Combinar todos en un solo objeto
#   st_transform(st_crs(PARROQUIA_DMQ))

# Calcular indicador 

# Unir los comercios combinados con la malla estadistica 
comercios_en_malla_pr <- st_join(PARROQUIA_DMQ, comercios)

# Contar la cantidad de comercios por celda de la malla
total_comercios_pr <- comercios_en_malla_pr %>%
  group_by(parroquia, TIPOLOGIA) %>%                                                 # Agrupar por celda y tipo de comercio
  summarise(n = n(), .groups = "drop") %>%                                        # Contar la cantidad de comercios de cada tipo
  mutate(n = ifelse(is.na(TIPOLOGIA), NA_integer_, n))

# Crear tabla de frecuencias
tabla_frecuencia_comercios_pr <- total_comercios_pr %>%
  filter(!is.na(TIPOLOGIA)) %>%
  tidytable() %>%
  group_by(parroquia, TIPOLOGIA) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  spread(key = TIPOLOGIA, value = n, fill = 0)                                    # Llenar con 0 donde no hay comercios

# Convertir la tabla a una matriz (sin la columna del parroquia)
matriz_frecuencia_pr <- as.matrix(tabla_frecuencia_comercios_pr[, -1])  

# Calcular la entropía de Shannon
entropia_shannon_pr <- diversity(matriz_frecuencia_pr, index = "shannon")
mean(entropia_shannon_pr)
sd(entropia_shannon_pr)

# Agregar la entropía de Shannon a la tabla original
tabla_frecuencia_comercios_pr$H <- entropia_shannon_pr

# Unir la entropía calculada a la malla original
H_pr <- PARROQUIA_DMQ %>%
  left_join(tabla_frecuencia_comercios_pr, by = "parroquia") %>%
  select(parroquia,H,geometry)

H_pr[is.na(H_pr)] <- 0

# SECTOR --------------------------------------------------------

# Cargar datos 

# 1. Malla estadística 
SECTOR_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\SECTOR_DMQ.shp")

# 2. Comercios 
comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(SECTOR_DMQ))

# # Leer y combinar todos los shapefiles de comercios
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# lista <- list.files(path = d_comercios, pattern = "\\.shp$", full.names = TRUE)
# 
# # Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
# leer_shapefiles <- function(archivo) {
#   shapefile <- st_read(archivo)
# 
#   if ("CALIFICACI" %in% colnames(shapefile)) {
#     shapefile <- shapefile %>%
#       mutate(CALIFICACI = as.character(CALIFICACI))
#   }
# 
#   return(shapefile)
# }
# 
# # Leer y combinar todos los shapefiles
# comercios <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>%              # Combinar todos en un solo objeto
#   st_transform(st_crs(SECTOR_DMQ))

# Calcular indicador 

# Unir los comercios combinados con la malla estadistica 
comercios_en_malla_s <- st_join(SECTOR_DMQ, comercios)

# Contar la cantidad de comercios por celda de la malla
total_comercios_s <- comercios_en_malla_s %>%
  group_by(Sector_DMQ, TIPOLOGIA) %>%                                                 # Agrupar por celda y tipo de comercio
  summarise(n = n(), .groups = "drop") %>%                                        # Contar la cantidad de comercios de cada tipo
  mutate(n = ifelse(is.na(TIPOLOGIA), NA_integer_, n))

# Crear tabla de frecuencias
tabla_frecuencia_comercios_s <- total_comercios_s %>%
  filter(!is.na(TIPOLOGIA)) %>%
  tidytable() %>%
  group_by(Sector_DMQ, TIPOLOGIA) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  spread(key = TIPOLOGIA, value = n, fill = 0)                                    # Llenar con 0 donde no hay comercios

# Convertir la tabla a una matriz (sin la columna del Sector_DMQ)
matriz_frecuencia_s <- as.matrix(tabla_frecuencia_comercios_s[, -1])  

# Calcular la entropía de Shannon
entropia_shannon_s <- diversity(matriz_frecuencia_s, index = "shannon")
mean(entropia_shannon_s)
sd(entropia_shannon_s)

# Agregar la entropía de Shannon a la tabla original
tabla_frecuencia_comercios_s$H <- entropia_shannon_s

# Unir la entropía calculada a la malla original
H_s <- SECTOR_DMQ %>%
  left_join(tabla_frecuencia_comercios_s, by = "Sector_DMQ") %>%
  select(Sector_DMQ,H,geometry)

H_s[is.na(H_s)] <- 0

# MALLA H3 NIVEL 8 -------------------------------------------------------------------

# Cargar datos 

# 1. Malla estadística 
MALLA_H3_N8_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")

# 2. Comercios 
comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(MALLA_H3_N8_DMQ))

# # Leer y combinar todos los shapefiles de comercios
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# lista <- list.files(path = d_comercios, pattern = "\\.shp$", full.names = TRUE)
# 
# # Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
# leer_shapefiles <- function(archivo) {
#   shapefile <- st_read(archivo)
# 
#   if ("CALIFICACI" %in% colnames(shapefile)) {
#     shapefile <- shapefile %>%
#       mutate(CALIFICACI = as.character(CALIFICACI))
#   }
# 
#   return(shapefile)
# }
# 
# # Leer y combinar todos los shapefiles
# comercios <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>%              # Combinar todos en un solo objeto
#   st_transform(st_crs(MALLA_H3_N8_DMQ))

# Calcular indicador 

# Unir los comercios combinados con la malla estadistica 
comercios_en_malla_N8 <- st_join(MALLA_H3_N8_DMQ, comercios)

# Contar la cantidad de comercios por celda de la malla
total_comercios_N8 <- comercios_en_malla_N8 %>%
  group_by(H3HASH, TIPOLOGIA) %>%                                                 # Agrupar por celda y tipo de comercio
  summarise(n = n(), .groups = "drop") %>%                                        # Contar la cantidad de comercios de cada tipo
  mutate(n = ifelse(is.na(TIPOLOGIA), NA_integer_, n))

# Crear tabla de frecuencias
tabla_frecuencia_comercios_N8 <- total_comercios_N8 %>%
  filter(!is.na(TIPOLOGIA)) %>%
  tidytable() %>%
  group_by(H3HASH, TIPOLOGIA) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  spread(key = TIPOLOGIA, value = n, fill = 0)                                    # Llenar con 0 donde no hay comercios

# Convertir la tabla a una matriz (sin la columna del H3HASH)
matriz_frecuencia_N8 <- as.matrix(tabla_frecuencia_comercios_N8[, -1])  

# Calcular la entropía de Shannon
entropia_shannon_N8 <- diversity(matriz_frecuencia_N8, index = "shannon")
mean(entropia_shannon_N8)
sd(entropia_shannon_N8)

# Agregar la entropía de Shannon a la tabla original
tabla_frecuencia_comercios_N8$H <- entropia_shannon_N8

# Unir la entropía calculada a la malla original
H_N8 <- MALLA_H3_N8_DMQ %>%
  left_join(tabla_frecuencia_comercios_N8, by = "H3HASH") %>%
  select(H3HASH,H,geometry)

H_N8[is.na(H_N8)] <- 0

# MALLA H3 NIVEL 9 -------------------------------------------------------------------

# Cargar datos 

# 1. Malla estadística 
MALLA_H3_N9_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# 2. Comercios 
comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(MALLA_H3_N8_DMQ))

# # Leer y combinar todos los shapefiles de comercios
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# lista <- list.files(path = d_comercios, pattern = "\\.shp$", full.names = TRUE)
# 
# # Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
# leer_shapefiles <- function(archivo) {
#   shapefile <- st_read(archivo)
# 
#   if ("CALIFICACI" %in% colnames(shapefile)) {
#     shapefile <- shapefile %>%
#       mutate(CALIFICACI = as.character(CALIFICACI))
#   }
# 
#   return(shapefile)
# }
# 
# # Leer y combinar todos los shapefiles
# comercios <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>%              # Combinar todos en un solo objeto
#   st_transform(st_crs(MALLA_H3_N9_DMQ))

# Calcular indicador 

# Unir los comercios combinados con la malla estadistica 
comercios_en_malla_N9 <- st_join(MALLA_H3_N9_DMQ, comercios)

# Contar la cantidad de comercios por celda de la malla
total_comercios_N9 <- comercios_en_malla_N9 %>%
  group_by(H3HASH, TIPOLOGIA) %>%                                                 # Agrupar por celda y tipo de comercio
  summarise(n = n(), .groups = "drop") %>%                                        # Contar la cantidad de comercios de cada tipo
  mutate(n = ifelse(is.na(TIPOLOGIA), NA_integer_, n))

# Crear tabla de frecuencias
tabla_frecuencia_comercios_N9 <- total_comercios_N9 %>%
  filter(!is.na(TIPOLOGIA)) %>%
  tidytable() %>%
  group_by(H3HASH, TIPOLOGIA) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  spread(key = TIPOLOGIA, value = n, fill = 0)                                    # Llenar con 0 donde no hay comercios

# Convertir la tabla a una matriz (sin la columna del H3HASH)
matriz_frecuencia_N9 <- as.matrix(tabla_frecuencia_comercios_N9[, -1])  

# Calcular la entropía de Shannon
entropia_shannon_N9 <- diversity(matriz_frecuencia_N9, index = "shannon")
mean(entropia_shannon_N9)
sd(entropia_shannon_N9)

# Agregar la entropía de Shannon a la tabla original
tabla_frecuencia_comercios_N9$H <- entropia_shannon_N9

# Unir la entropía calculada a la malla original
H_N9 <- MALLA_H3_N9_DMQ %>%
  left_join(tabla_frecuencia_comercios_N9, by = "H3HASH") %>%
  select(H3HASH,H,geometry)

H_N9[is.na(H_N9)] <- 0

# COD1000 -----------------------------------------------------

# Cargar datos 

# 1. Malla estadística 
MALLA_1000_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")

# 2. Comercios 
comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(MALLA_1000_DMQ))

# # Leer y combinar todos los shapefiles de comercios
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# lista <- list.files(path = d_comercios, pattern = "\\.shp$", full.names = TRUE)
# 
# # Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
# leer_shapefiles <- function(archivo) {
#   shapefile <- st_read(archivo)
# 
#   if ("CALIFICACI" %in% colnames(shapefile)) {
#     shapefile <- shapefile %>%
#       mutate(CALIFICACI = as.character(CALIFICACI))
#   }
# 
#   return(shapefile)
# }
# 
# # Leer y combinar todos los shapefiles
# comercios <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>%              # Combinar todos en un solo objeto
#   st_transform(st_crs(MALLA_1000_DMQ))

# Calcular indicador 

# Unir los comercios combinados con la malla estadistica 
comercios_en_malla_1000 <- st_join(MALLA_1000_DMQ, comercios)

# Contar la cantidad de comercios por celda de la malla
total_comercios_1000 <- comercios_en_malla_1000 %>%
  group_by(COD1000, TIPOLOGIA) %>%                                                 # Agrupar por celda y tipo de comercio
  summarise(n = n(), .groups = "drop") %>%                                        # Contar la cantidad de comercios de cada tipo
  mutate(n = ifelse(is.na(TIPOLOGIA), NA_integer_, n))

# Crear tabla de frecuencias
tabla_frecuencia_comercios_1000 <- total_comercios_1000 %>%
  filter(!is.na(TIPOLOGIA)) %>%
  tidytable() %>%
  group_by(COD1000, TIPOLOGIA) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  spread(key = TIPOLOGIA, value = n, fill = 0)                                    # Llenar con 0 donde no hay comercios

# Convertir la tabla a una matriz (sin la columna del COD1000)
matriz_frecuencia_1000 <- as.matrix(tabla_frecuencia_comercios_1000[, -1])  

# Calcular la entropía de Shannon
entropia_shannon_1000 <- diversity(matriz_frecuencia_1000, index = "shannon")
mean(entropia_shannon_1000)
sd(entropia_shannon_1000)

# Agregar la entropía de Shannon a la tabla original
tabla_frecuencia_comercios_1000$H <- entropia_shannon_1000

# Unir la entropía calculada a la malla original
H_1000 <- MALLA_1000_DMQ %>%
  left_join(tabla_frecuencia_comercios_1000, by = "COD1000") %>%
  select(COD1000,H,geometry)

H_1000[is.na(H_1000)] <- 0

# COD500 -----------------------------------------------------

# Cargar datos 

# 1. Malla estadística 
MALLA_500_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_500_DMQ.shp")

# 2. Comercios 
comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(MALLA_500_DMQ))

# # Leer y combinar todos los shapefiles de comercios
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# lista <- list.files(path = d_comercios, pattern = "\\.shp$", full.names = TRUE)
# 
# # Función para leer shapefiles y convertir columnas problemáticas al mismo tipo
# leer_shapefiles <- function(archivo) {
#   shapefile <- st_read(archivo)
# 
#   if ("CALIFICACI" %in% colnames(shapefile)) {
#     shapefile <- shapefile %>%
#       mutate(CALIFICACI = as.character(CALIFICACI))
#   }
# 
#   return(shapefile)
# }
# 
# # Leer y combinar todos los shapefiles
# comercios <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>%              # Combinar todos en un solo objeto
#   st_transform(st_crs(MALLA_500_DMQ))

# Calcular indicador 

# Unir los comercios combinados con la malla estadistica 
comercios_en_malla_500 <- st_join(MALLA_500_DMQ, comercios)

# Contar la cantidad de comercios por celda de la malla
total_comercios_500 <- comercios_en_malla_500 %>%
  group_by(COD500, TIPOLOGIA) %>%                                                 # Agrupar por celda y tipo de comercio
  summarise(n = n(), .groups = "drop") %>%                                        # Contar la cantidad de comercios de cada tipo
  mutate(n = ifelse(is.na(TIPOLOGIA), NA_integer_, n))

# Crear tabla de frecuencias
tabla_frecuencia_comercios_500 <- total_comercios_500 %>%
  filter(!is.na(TIPOLOGIA)) %>%
  tidytable() %>%
  group_by(COD500, TIPOLOGIA) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
  spread(key = TIPOLOGIA, value = n, fill = 0)                                    # Llenar con 0 donde no hay comercios

# Convertir la tabla a una matriz (sin la columna del COD500)
matriz_frecuencia_500 <- as.matrix(tabla_frecuencia_comercios_500[, -1])  

# Calcular la entropía de Shannon
entropia_shannon_500 <- diversity(matriz_frecuencia_500, index = "shannon")
mean(entropia_shannon_500)
sd(entropia_shannon_500)

# Agregar la entropía de Shannon a la tabla original
tabla_frecuencia_comercios_500$H <- entropia_shannon_500

# Unir la entropía calculada a la malla original
H_500 <- MALLA_500_DMQ %>%
  left_join(tabla_frecuencia_comercios_500, by = "COD500") %>%
  select(COD500,H,geometry)

H_500[is.na(H_500)] <- 0

# 6. Guardar los resultados------------------------------------------

H_az2 <- as.data.frame(H_az)[, !names(H_az) %in% "geometry"]
H_s2 <- as.data.frame(H_s)[, !names(H_s) %in% "geometry"]
H_N8_2 <- as.data.frame(H_N8)[, !names(H_N8) %in% "geometry"]
H_N9_2 <- as.data.frame(H_N9)[, !names(H_N9) %in% "geometry"]
H_1000_2 <- as.data.frame(H_1000)[, !names(H_1000) %in% "geometry"]
H_500_2 <- as.data.frame(H_500)[, !names(H_500) %in% "geometry"]
H_pr2 <- as.data.frame(H_pr)[, !names(H_pr) %in% "geometry"]

wb <- createWorkbook("H")

addWorksheet(wb, "H_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "H_az", x=H_az2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "H_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "H_pr", x=H_pr2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "H_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "H_s", x=H_s2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "H_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "H_1000", x=H_1000_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "H_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "H_500", x=H_500_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "H_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "H_N8", x=H_N8_2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "H_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "H_N9", x=H_N9_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Índice de diversidad urbana.xlsx")