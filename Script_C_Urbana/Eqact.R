# Dimensión: Complejidades Urbanas 

# Nombre del Indicador: Equilibrio entre la actividad y la residencia

# Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# Cargar librerías ----------------------------------------------

pacman::p_load(openxlsx,sf,dplyr,purrr)

# ADM_ZONAL -------------------------------------------------------------------

# Cargar datos 

# 1. Malla estadística con datos de números de vivienda 
ADM_ZONAL_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")

# 2. Comercios 

# # Definir el directorio donde se encuentran los shapefiles 
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
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
#   st_transform(st_crs(ADM_ZONA_DMQ))      

comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(ADM_ZONAL_DMQ))

# Unir los comercios combinados con la malla H3
comercios_en_malla_az <- st_join(ADM_ZONAL_DMQ, comercios)

# Calcular indicador 

# Contar el número total de comercios en cada celda de la malla
total_comercios_az <- comercios_en_malla_az %>%
  group_by(adm_zonal) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar numero de viviendas y establecimientos 
Eqact_az <- ADM_ZONAL_DMQ %>%
  st_drop_geometry() %>%  # Eliminar geometría para trabajar solo con atributos
  left_join(st_drop_geometry(total_comercios_az), by = "adm_zonal") %>%  # Unir por adm_zonal
  mutate(t_puntos = num_V + t_com,  # Sumar puntos de viviendas y comercios
         Eqact = (t_com / t_puntos) * 100)  

Eqact_az[is.na(Eqact_az)] <- 0 # Reemplazar NA con 0

# PARROQUIA -----------------------------------------------------------------
# Cargar datos 

# 1. Malla estadística con datos de números de vivienda 
PARROQUIA_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")

# 2. Comercios 

# # Definir el directorio donde se encuentran los shapefiles 
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
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

comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(PARROQUIA_DMQ))

# Unir los comercios combinados con la malla H3
comercios_en_malla_pr <- st_join(PARROQUIA_DMQ, comercios)

# Calcular indicador 

# Contar el número total de comercios en cada celda de la malla
total_comercios_pr <- comercios_en_malla_pr %>%
  group_by(parroquia) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar numero de viviendas y establecimientos 
Eqact_pr <- PARROQUIA_DMQ %>%
  st_drop_geometry() %>%  # Eliminar geometría para trabajar solo con atributos
  left_join(st_drop_geometry(total_comercios_pr), by = "parroquia") %>%  # Unir por parroquia
  mutate(t_puntos = num_v + t_com,  # Sumar puntos de viviendas y comercios
         Eqact = (t_com / t_puntos) * 100)  

Eqact_pr[is.na(Eqact_pr)] <- 0 # Reemplazar NA con 0

# SECTOR --------------------------------------------------------

# Cargar datos 

# 1. Malla estadística con datos de números de vivienda 
SECTOR_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\SECTOR_DMQ.shp")

# 2. Comercios 

# # Definir el directorio donde se encuentran los shapefiles 
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
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

comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(SECTOR_DMQ))

# Unir los comercios combinados con la malla H3
comercios_en_malla_s <- st_join(SECTOR_DMQ, comercios)

# Calcular indicador 

# Contar el número total de comercios en cada celda de la malla
total_comercios_s <- comercios_en_malla_s %>%
  group_by(Sector_DMQ) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar numero de viviendas y establecimientos 
Eqact_s <- SECTOR_DMQ %>%
  st_drop_geometry() %>%  # Eliminar geometría para trabajar solo con atributos
  left_join(st_drop_geometry(total_comercios_s), by = "Sector_DMQ") %>%  # Unir por Sector_DMQ
  mutate(t_puntos = num_v + t_com,  # Sumar puntos de viviendas y comercios
         Eqact = (t_com / t_puntos) * 100)  

Eqact_s[is.na(Eqact_s)] <- 0 # Reemplazar NA con 0

# MALLA H3 NIVEL 8 -------------------------------------------------------------------

# Cargar datos 

# 1. Malla estadística con datos de números de vivienda 
MALLA_H3_N8_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")

# 2. Comercios 

# # Definir el directorio donde se encuentran los shapefiles 
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
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

comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(MALLA_H3_N8_DMQ))

# Unir los comercios combinados con la malla H3
comercios_en_malla_N8 <- st_join(MALLA_H3_N8_DMQ, comercios)

# Calcular indicador 

# Contar el número total de comercios en cada celda de la malla
total_comercios_N8 <- comercios_en_malla_N8 %>%
  group_by(H3HASH) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar numero de viviendas y establecimientos 
Eqact_N8 <- MALLA_H3_N8_DMQ %>%
  st_drop_geometry() %>%  # Eliminar geometría para trabajar solo con atributos
  left_join(st_drop_geometry(total_comercios_N8), by = "H3HASH") %>%  # Unir por H3HASH
  mutate(t_puntos = num_v + t_com,  # Sumar puntos de viviendas y comercios
         Eqact = (t_com / t_puntos) * 100)  

Eqact_N8[is.na(Eqact_N8)] <- 0 # Reemplazar NA con 0

# MALLA H3 NIVEL 9 -------------------------------------------------------------------

# Cargar datos 

# 1. Malla estadística con datos de números de vivienda 
MALLA_H3_N9_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# 2. Comercios 

# # Definir el directorio donde se encuentran los shapefiles 
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
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

comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(MALLA_H3_N9_DMQ))

# Unir los comercios combinados con la malla H3
comercios_en_malla_N9 <- st_join(MALLA_H3_N9_DMQ, comercios)

# Calcular indicador 

# Contar el número total de comercios en cada celda de la malla
total_comercios_N9 <- comercios_en_malla_N9 %>%
  group_by(H3HASH) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar numero de viviendas y establecimientos 
Eqact_N9 <- MALLA_H3_N9_DMQ %>%
  st_drop_geometry() %>%  # Eliminar geometría para trabajar solo con atributos
  left_join(st_drop_geometry(total_comercios_N9), by = "H3HASH") %>%  # Unir por H3HASH
  mutate(t_puntos = num_v + t_com,  # Sumar puntos de viviendas y comercios
         Eqact = (t_com / t_puntos) * 100)  

Eqact_N9[is.na(Eqact_N9)] <- 0 # Reemplazar NA con 0

# COD1000 -----------------------------------------------------

# Cargar datos 

# 1. Malla estadística con datos de números de vivienda 
MALLA_1000_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")

# 2. Comercios 

# # Definir el directorio donde se encuentran los shapefiles 
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
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

comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(MALLA_1000_DMQ))

# Unir los comercios combinados con la malla H3
comercios_en_malla_1000 <- st_join(MALLA_1000_DMQ, comercios)

# Calcular indicador 

# Contar el número total de comercios en cada celda de la malla
total_comercios_1000 <- comercios_en_malla_1000 %>%
  group_by(COD1000) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar numero de viviendas y establecimientos 
Eqact_1000 <- MALLA_1000_DMQ %>%
  st_drop_geometry() %>%  # Eliminar geometría para trabajar solo con atributos
  left_join(st_drop_geometry(total_comercios_1000), by = "COD1000") %>%  # Unir por COD1000
  mutate(t_puntos = num_v + t_com,  # Sumar puntos de viviendas y comercios
         Eqact = (t_com / t_puntos) * 100)  

Eqact_1000[is.na(Eqact_1000)] <- 0 # Reemplazar NA con 0
# COD500 -----------------------------------------------------

# Cargar datos 

# 1. Malla estadística con datos de números de vivienda 
MALLA_500_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_500_DMQ.shp")

# 2. Comercios 

# # Definir el directorio donde se encuentran los shapefiles 
# d_comercios <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Comercios"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
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

comercios <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios.shp") %>% 
  st_transform(st_crs(MALLA_500_DMQ))

# Unir los comercios combinados con la malla H3
comercios_en_malla_500 <- st_join(MALLA_500_DMQ, comercios)

# Calcular indicador 

# Contar el número total de comercios en cada celda de la malla
total_comercios_500 <- comercios_en_malla_500 %>%
  group_by(COD500) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar numero de viviendas y establecimientos 
Eqact_500 <- MALLA_500_DMQ %>%
  st_drop_geometry() %>%  # Eliminar geometría para trabajar solo con atributos
  left_join(st_drop_geometry(total_comercios_500), by = "COD500") %>%  # Unir por COD500
  mutate(t_puntos = num_v + t_com,  # Sumar puntos de viviendas y comercios
         Eqact = (t_com / t_puntos) * 100)  

Eqact_500[is.na(Eqact_500)] <- 0 # Reemplazar NA con 0



# 6. Guardar los resultados------------------------------------------

Eqact_az2 <- as.data.frame(Eqact_az)[, !names(Eqact_az) %in% "geometry"]
Eqact_s2 <- as.data.frame(Eqact_s)[, !names(Eqact_s) %in% "geometry"]
Eqact_N8_2 <- as.data.frame(Eqact_N8)[, !names(Eqact_N8) %in% "geometry"]
Eqact_N9_2 <- as.data.frame(Eqact_N9)[, !names(Eqact_N9) %in% "geometry"]
Eqact_1000_2 <- as.data.frame(Eqact_1000)[, !names(Eqact_1000) %in% "geometry"]
Eqact_500_2 <- as.data.frame(Eqact_500)[, !names(Eqact_500) %in% "geometry"]
Eqact_pr2 <- as.data.frame(Eqact_pr)[, !names(Eqact_pr) %in% "geometry"]

wb <- createWorkbook("Eqact")

addWorksheet(wb, "Eqact_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "Eqact_az", x=Eqact_az2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "Eqact_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "Eqact_pr", x=Eqact_pr2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "Eqact_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "Eqact_s", x=Eqact_s2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "Eqact_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "Eqact_1000", x=Eqact_1000_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "Eqact_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "Eqact_500", x=Eqact_500_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "Eqact_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "Eqact_N8", x=Eqact_N8_2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "Eqact_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "Eqact_N9", x=Eqact_N9_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Equilibrio entre la actividad y la residencia.xlsx") 