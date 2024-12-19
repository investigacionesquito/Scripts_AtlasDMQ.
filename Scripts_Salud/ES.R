# Dimensión: Salud

# Nombre del Indicador: Establecimientos de salud

# Cargar paquetes

pacman::p_load(openxlsx,sf,dplyr,purrr)

# ADM_ZONAL -------------------------------------------------------------------
# Cargar datos 

# 1. Unidad de análisis 
ADM_ZONAL_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")

# 2. Establecimientos de salud 

# Definir el directorio donde se encuentran los shapefiles de establecimientos de salud
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Salud"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_actividades, pattern = "\\.shp$", full.names = TRUE)

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
salud <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(ADM_ZONAL_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_az <- st_join(ADM_ZONAL_DMQ, salud)

# 3. Comercios 
establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios_salud.shp") %>% 
  st_transform(st_crs(ADM_ZONAL_DMQ))

# Calcular indicador 

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_az <- st_join(ADM_ZONAL_DMQ, establecimientos)

# Contar el número de establecimientos de salud 
actividades_count_az <- actividades_en_malla_az %>%
  group_by(adm_zonal) %>%  
  summarise(n_salud = sum(!is.na(TIPOLOGIA)), .groups = "drop")                       # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos 
total_establecimientos_az <- establecimientos_en_malla_az %>%
  group_by(adm_zonal) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop")                        # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ES_az <- actividades_count_az %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_az), by = "adm_zonal") %>%
  mutate(ES = (n_salud / t_com) * 100)

# Reemplazar NA con 0
ES_az[is.na(ES_az)] <- 0

# PARROQUIA -----------------------------------------------------------------

# Cargar datos 
# 1. Unidad de análisis 
PARROQUIA_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")

# 2. Establecimientos de salud 

# Definir el directorio donde se encuentran los shapefiles de establecimientos de salud
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Salud"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_actividades, pattern = "\\.shp$", full.names = TRUE)

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

# Leer y combinar todos los shapefiles de establecimientos de salud
salud <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(PARROQUIA_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_pr <- st_join(PARROQUIA_DMQ, salud)

# 3. Comercios 
establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios_salud.shp") %>% 
  st_transform(st_crs(PARROQUIA_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_pr <- st_join(PARROQUIA_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de establecimientos de salud 
actividades_count_pr <- actividades_en_malla_pr %>%
  group_by(parroquia) %>%  
  summarise(n_salud = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos 
total_establecimientos_pr <- establecimientos_en_malla_pr %>%
  group_by(parroquia) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ES_pr <- actividades_count_pr %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_pr), by = "parroquia") %>%
  mutate(ES = (n_salud / t_com) * 100)

# Reemplazar NA con 0
ES_pr[is.na(ES_pr)] <- 0

# SECTOR --------------------------------------------------------

# Cargar datos 
# 1. Unidad de análisis 
SECTOR_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\SECTOR_DMQ.shp")

# 2. Establecimientos de salud 

# Definir el directorio donde se encuentran los shapefiles de establecimientos de salud
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Salud"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_actividades, pattern = "\\.shp$", full.names = TRUE)

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

# Leer y combinar todos los shapefiles de establecimientos de salud
salud <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(SECTOR_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_s <- st_join(SECTOR_DMQ, salud)

# 3. Comercios 
establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios_salud.shp") %>% 
  st_transform(st_crs(SECTOR_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_s <- st_join(SECTOR_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de establecimientos de salud 
actividades_count_s <- actividades_en_malla_s %>%
  group_by(Sector_DMQ) %>%  
  summarise(n_salud = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos 
total_establecimientos_s <- establecimientos_en_malla_s %>%
  group_by(Sector_DMQ) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ES_s <- actividades_count_s %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_s), by = "Sector_DMQ") %>%
  mutate(ES = (n_salud / t_com) * 100)

# Reemplazar NA con 0
ES_s[is.na(ES_s)] <- 0


# MALLA H3 NIVEL 8 -------------------------------------------------------------------

# Cargar datos 
# 1. Unidad de análisis 
MALLA_H3_N8_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")

# 2. Establecimientos de salud 

# Definir el directorio donde se encuentran los shapefiles de establecimientos de salud
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Salud"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_actividades, pattern = "\\.shp$", full.names = TRUE)

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


# Leer y combinar todos los shapefiles de establecimientos de salud
salud <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_H3_N8_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_N8 <- st_join(MALLA_H3_N8_DMQ, salud)

# 3. Comercios 
establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios_salud.shp") %>% 
  st_transform(st_crs(MALLA_H3_N8_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_N8 <- st_join(MALLA_H3_N8_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de establecimientos de salud 
actividades_count_N8 <- actividades_en_malla_N8 %>%
  group_by(H3HASH) %>%  
  summarise(n_salud = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos 
total_establecimientos_N8 <- establecimientos_en_malla_N8 %>%
  group_by(H3HASH) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ES_N8 <- actividades_count_N8 %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_N8), by = "H3HASH") %>%
  mutate(ES = (n_salud / t_com) * 100)

# Reemplazar NA con 0
ES_N8[is.na(ES_N8)] <- 0

# MALLA H3 NIVEL 9 -------------------------------------------------------------------

# Cargar datos 
# 1. Unidad de análisis 
MALLA_H3_N9_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# 2. Establecimientos de salud 

# Definir el directorio donde se encuentran los shapefiles de establecimientos de salud
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Salud"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_actividades, pattern = "\\.shp$", full.names = TRUE)

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


# Leer y combinar todos los shapefiles de establecimientos de salud
salud <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_H3_N9_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_N9 <- st_join(MALLA_H3_N9_DMQ, salud)

# 3. Comercios 
establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios_salud.shp") %>% 
  st_transform(st_crs(MALLA_H3_N9_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_N9 <- st_join(MALLA_H3_N9_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de establecimientos de salud 
actividades_count_N9 <- actividades_en_malla_N9 %>%
  group_by(H3HASH) %>%  
  summarise(n_salud = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos 
total_establecimientos_N9 <- establecimientos_en_malla_N9 %>%
  group_by(H3HASH) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ES_N9 <- actividades_count_N9 %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_N9), by = "H3HASH") %>%
  mutate(ES = (n_salud / t_com) * 100)

# Reemplazar NA con 0
ES_N9[is.na(ES_N9)] <- 0

# COD1000 -----------------------------------------------------

# Cargar datos 
# 1. Unidad de análisis 
MALLA_1000_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")

# 2. Establecimientos de salud 

# Definir el directorio donde se encuentran los shapefiles de establecimientos de salud
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Salud"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_actividades, pattern = "\\.shp$", full.names = TRUE)

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


# Leer y combinar todos los shapefiles de establecimientos de salud
salud <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_1000_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_1000 <- st_join(MALLA_1000_DMQ, salud)

# 3. Comercios 
establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios_salud.shp") %>% 
  st_transform(st_crs(MALLA_1000_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_1000 <- st_join(MALLA_1000_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de establecimientos de salud 
actividades_count_1000 <- actividades_en_malla_1000 %>%
  group_by(COD1000) %>%  
  summarise(n_salud = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos 
total_establecimientos_1000 <- establecimientos_en_malla_1000 %>%
  group_by(COD1000) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ES_1000 <- actividades_count_1000 %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_1000), by = "COD1000") %>%
  mutate(ES = (n_salud / t_com) * 100)

# Reemplazar NA con 0
ES_1000[is.na(ES_1000)] <- 0

# COD500 -----------------------------------------------------

# Cargar datos 
# 1. Unidad de análisis 
MALLA_500_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_500_DMQ.shp")

# 2. Establecimientos de salud 

# Definir el directorio donde se encuentran los shapefiles de establecimientos de salud
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Salud"

# Obtener la lista de todos los archivos .shp en el directorio
lista_shapefiles <- list.files(path = directorio_actividades, pattern = "\\.shp$", full.names = TRUE)

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


# Leer y combinar todos los shapefiles de establecimientos de salud
salud <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_500_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_500 <- st_join(MALLA_500_DMQ, salud)

# 3. Comercios 
establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\comercios_salud.shp") %>% 
  st_transform(st_crs(MALLA_500_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_500 <- st_join(MALLA_500_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de establecimientos de salud 
actividades_count_500 <- actividades_en_malla_500 %>%
  group_by(COD500) %>%  
  summarise(n_salud = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos 
total_establecimientos_500 <- establecimientos_en_malla_500 %>%
  group_by(COD500) %>%  
  summarise(t_com = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ES_500 <- actividades_count_500 %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_500), by = "COD500") %>%
  mutate(ES = (n_salud / t_com) * 100)

# Reemplazar NA con 0
ES_500[is.na(ES_500)] <- 0


# 6. Guardar los resultados------------------------------------------

ES_az2 <- as.data.frame(ES_az)[, !names(ES_az) %in% "geometry"]
ES_s2 <- as.data.frame(ES_s)[, !names(ES_s) %in% "geometry"]
ES_N8_2 <- as.data.frame(ES_N8)[, !names(ES_N8) %in% "geometry"]
ES_N9_2 <- as.data.frame(ES_N9)[, !names(ES_N9) %in% "geometry"]
ES_1000_2 <- as.data.frame(ES_1000)[, !names(ES_1000) %in% "geometry"]
ES_500_2 <- as.data.frame(ES_500)[, !names(ES_500) %in% "geometry"]
ES_pr2 <- as.data.frame(ES_pr)[, !names(ES_pr) %in% "geometry"]

wb <- createWorkbook("ES")

addWorksheet(wb, "ES_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ES_az", x=ES_az2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ES_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ES_pr", x=ES_pr2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ES_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ES_s", x=ES_s2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ES_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ES_1000", x=ES_1000_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ES_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ES_500", x=ES_500_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ES_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ES_N8", x=ES_N8_2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ES_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ES_N9", x=ES_N9_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Establecimientos de salud.xlsx")