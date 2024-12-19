# Dimensión: Complejidades Urbanas 

# Nombre del Indicador: Actividades densas en conocimiento

# Cargar paquetes

pacman::p_load(openxlsx,sf,dplyr,purrr)

# ADM_ZONAL -------------------------------------------------------------------

# Cargar datos 
# 1. Malla estadística 
ADM_ZONAL_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")

# 2. Actividades densas de conocimiento 

# Definir el directorio donde se encuentran los shapefiles de actividades densas
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/A_Conocimiento"

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

# Leer y combinar todos los shapefiles de actividades densas
actividades_densas <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(ADM_ZONAL_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_az <- st_join(ADM_ZONAL_DMQ, actividades_densas)

# 3. Comercios 

# # Definir el directorio donde se encuentran los shapefiles de actividades densas
# d_establecimientos <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Equipamientos"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
# lista <- list.files(path = d_establecimientos, pattern = "\\.shp$", full.names = TRUE)
# 
# # Leer y combinar todos los shapefiles de actividades densas
# establecimientos <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>% st_as_sf() %>% 
#   st_transform(st_crs(ADM_ZONAL_DMQ))     

establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\equipamientos.shp") %>% 
  st_transform(st_crs(ADM_ZONAL_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_az <- st_join(ADM_ZONAL_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de actividades densas en cada celda de la malla
actividades_count_az <- actividades_en_malla_az %>%
  group_by(adm_zonal) %>%  
  summarise(n_act = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos en cada celda de la malla
total_establecimientos_az <- establecimientos_en_malla_az %>%
  group_by(adm_zonal) %>%  
  summarise(t_est = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ACT_az <- actividades_count_az %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_az), by = "adm_zonal") %>%
  mutate(ACT = (n_act / t_est) * 100)

# Reemplazar NA con 0
ACT_az[is.na(ACT_az)] <- 0

# PARROQUIA -----------------------------------------------------------------

# Cargar datos 
# 1. Malla estadística 
PARROQUIA_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")

# 2. Actividades densas de conocimiento 

# Definir el directorio donde se encuentran los shapefiles de actividades densas
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/A_Conocimiento"

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

# Leer y combinar todos los shapefiles de actividades densas
actividades_densas <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(PARROQUIA_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_pr <- st_join(PARROQUIA_DMQ, actividades_densas)

# 3. Comercios 

# # Definir el directorio donde se encuentran los shapefiles de actividades densas
# d_establecimientos <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Equipamientos"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
# lista <- list.files(path = d_establecimientos, pattern = "\\.shp$", full.names = TRUE)
# 
# # Leer y combinar todos los shapefiles de actividades densas
# establecimientos <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>% st_as_sf() %>% 
#   st_transform(st_crs(PARROQUIA_DMQ))     

establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\equipamientos.shp") %>% 
  st_transform(st_crs(PARROQUIA_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_pr <- st_join(PARROQUIA_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de actividades densas en cada celda de la malla
actividades_count_pr <- actividades_en_malla_pr %>%
  group_by(parroquia) %>%  
  summarise(n_act = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos en cada celda de la malla
total_establecimientos_pr <- establecimientos_en_malla_pr %>%
  group_by(parroquia) %>%  
  summarise(t_est = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ACT_pr <- actividades_count_pr %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_pr), by = "parroquia") %>%
  mutate(ACT = (n_act / t_est) * 100)

# Reemplazar NA con 0
ACT_pr[is.na(ACT_pr)] <- 0

# SECTOR --------------------------------------------------------

# Cargar datos 
# 1. Malla estadística 
SECTOR_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\SECTOR_DMQ.shp")

# 2. Actividades densas de conocimiento 

# Definir el directorio donde se encuentran los shapefiles de actividades densas
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/A_Conocimiento"

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

# Leer y combinar todos los shapefiles de actividades densas
actividades_densas <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(SECTOR_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_s <- st_join(SECTOR_DMQ, actividades_densas)

# 3. Comercios 

# # Definir el directorio donde se encuentran los shapefiles de actividades densas
# d_establecimientos <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Equipamientos"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
# lista <- list.files(path = d_establecimientos, pattern = "\\.shp$", full.names = TRUE)
# 
# # Leer y combinar todos los shapefiles de actividades densas
# establecimientos <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>% st_as_sf() %>% 
#   st_transform(st_crs(SECTOR_DMQ))     

establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\equipamientos.shp") %>% 
  st_transform(st_crs(SECTOR_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_s <- st_join(SECTOR_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de actividades densas en cada celda de la malla
actividades_count_s <- actividades_en_malla_s %>%
  group_by(Sector_DMQ) %>%  
  summarise(n_act = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos en cada celda de la malla
total_establecimientos_s <- establecimientos_en_malla_s %>%
  group_by(Sector_DMQ) %>%  
  summarise(t_est = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ACT_s <- actividades_count_s %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_s), by = "Sector_DMQ") %>%
  mutate(ACT = (n_act / t_est) * 100)

# Reemplazar NA con 0
ACT_s[is.na(ACT_s)] <- 0


# MALLA H3 NIVEL 8 -------------------------------------------------------------------

# Cargar datos 
# 1. Malla estadística 
MALLA_H3_N8_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")

# 2. Actividades densas de conocimiento 

# Definir el directorio donde se encuentran los shapefiles de actividades densas
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/A_Conocimiento"

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


# Leer y combinar todos los shapefiles de actividades densas
actividades_densas <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_H3_N8_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_N8 <- st_join(MALLA_H3_N8_DMQ, actividades_densas)

# 3. Comercios 

# # Definir el directorio donde se encuentran los shapefiles de actividades densas
# d_establecimientos <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Equipamientos"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
# lista <- list.files(path = d_establecimientos, pattern = "\\.shp$", full.names = TRUE)
# 
# # Leer y combinar todos los shapefiles de actividades densas
# establecimientos <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>% st_as_sf() %>% 
#   st_transform(st_crs(MALLA_H3_N8_DMQ))     

establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\equipamientos.shp") %>% 
  st_transform(st_crs(MALLA_H3_N8_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_N8 <- st_join(MALLA_H3_N8_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de actividades densas en cada celda de la malla
actividades_count_N8 <- actividades_en_malla_N8 %>%
  group_by(H3HASH) %>%  
  summarise(n_act = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos en cada celda de la malla
total_establecimientos_N8 <- establecimientos_en_malla_N8 %>%
  group_by(H3HASH) %>%  
  summarise(t_est = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ACT_N8 <- actividades_count_N8 %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_N8), by = "H3HASH") %>%
  mutate(ACT = (n_act / t_est) * 100)

# Reemplazar NA con 0
ACT_N8[is.na(ACT_N8)] <- 0

# MALLA H3 NIVEL 9 -------------------------------------------------------------------

# Cargar datos 
# 1. Malla estadística 
MALLA_H3_N9_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# 2. Actividades densas de conocimiento 

# Definir el directorio donde se encuentran los shapefiles de actividades densas
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/A_Conocimiento"

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


# Leer y combinar todos los shapefiles de actividades densas
actividades_densas <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_H3_N9_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_N9 <- st_join(MALLA_H3_N9_DMQ, actividades_densas)

# 3. Comercios 

# # Definir el directorio donde se encuentran los shapefiles de actividades densas
# d_establecimientos <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Equipamientos"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
# lista <- list.files(path = d_establecimientos, pattern = "\\.shp$", full.names = TRUE)
# 
# # Leer y combinar todos los shapefiles de actividades densas
# establecimientos <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>% st_as_sf() %>% 
#   st_transform(st_crs(MALLA_H3_N9_DMQ))     

establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\equipamientos.shp") %>% 
  st_transform(st_crs(MALLA_H3_N9_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_N9 <- st_join(MALLA_H3_N9_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de actividades densas en cada celda de la malla
actividades_count_N9 <- actividades_en_malla_N9 %>%
  group_by(H3HASH) %>%  
  summarise(n_act = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos en cada celda de la malla
total_establecimientos_N9 <- establecimientos_en_malla_N9 %>%
  group_by(H3HASH) %>%  
  summarise(t_est = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ACT_N9 <- actividades_count_N9 %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_N9), by = "H3HASH") %>%
  mutate(ACT = (n_act / t_est) * 100)

# Reemplazar NA con 0
ACT_N9[is.na(ACT_N9)] <- 0

# COD1000 -----------------------------------------------------

# Cargar datos 
# 1. Malla estadística 
MALLA_1000_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")

# 2. Actividades densas de conocimiento 

# Definir el directorio donde se encuentran los shapefiles de actividades densas
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/A_Conocimiento"

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


# Leer y combinar todos los shapefiles de actividades densas
actividades_densas <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_1000_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_1000 <- st_join(MALLA_1000_DMQ, actividades_densas)

# 3. Comercios 

# # Definir el directorio donde se encuentran los shapefiles de actividades densas
# d_establecimientos <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Equipamientos"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
# lista <- list.files(path = d_establecimientos, pattern = "\\.shp$", full.names = TRUE)
# 
# # Leer y combinar todos los shapefiles de actividades densas
# establecimientos <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>% st_as_sf() %>% 
#   st_transform(st_crs(MALLA_1000_DMQ))     

establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\equipamientos.shp") %>% 
  st_transform(st_crs(MALLA_1000_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_1000 <- st_join(MALLA_1000_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de actividades densas en cada celda de la malla
actividades_count_1000 <- actividades_en_malla_1000 %>%
  group_by(COD1000) %>%  
  summarise(n_act = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos en cada celda de la malla
total_establecimientos_1000 <- establecimientos_en_malla_1000 %>%
  group_by(COD1000) %>%  
  summarise(t_est = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ACT_1000 <- actividades_count_1000 %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_1000), by = "COD1000") %>%
  mutate(ACT = (n_act / t_est) * 100)

# Reemplazar NA con 0
ACT_1000[is.na(ACT_1000)] <- 0

# COD500 -----------------------------------------------------

# Cargar datos 
# 1. Malla estadística 
MALLA_500_DMQ <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_500_DMQ.shp")

# 2. Actividades densas de conocimiento 

# Definir el directorio donde se encuentran los shapefiles de actividades densas
directorio_actividades <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/A_Conocimiento"

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


# Leer y combinar todos los shapefiles de actividades densas
actividades_densas <- lista_shapefiles %>%
  lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
  bind_rows() %>% st_as_sf() %>% 
  st_transform(st_crs(MALLA_500_DMQ))                  # Combinar todos en un solo objeto

# Asignar actividades  a hexágonos (join espacial)
actividades_en_malla_500 <- st_join(MALLA_500_DMQ, actividades_densas)

# 3. Comercios 

# # Definir el directorio donde se encuentran los shapefiles de actividades densas
# d_establecimientos <- "C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ/Complejidad_Urb/Equipamientos"
# 
# # Obtener la lista de todos los archivos .shp en el directorio
# lista <- list.files(path = d_establecimientos, pattern = "\\.shp$", full.names = TRUE)
# 
# # Leer y combinar todos los shapefiles de actividades densas
# establecimientos <- lista %>%
#   lapply(leer_shapefiles) %>%  # Leer y transformar cada shapefile
#   bind_rows() %>% st_as_sf() %>% 
#   st_transform(st_crs(MALLA_500_DMQ))     

establecimientos <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Complejidad_Urb\\equipamientos.shp") %>% 
  st_transform(st_crs(MALLA_500_DMQ))

# Unir los establecimientos combinados con la malla H3
establecimientos_en_malla_500 <- st_join(MALLA_500_DMQ, establecimientos)

# Calcular indicador 

# Contar el número de actividades densas en cada celda de la malla
actividades_count_500 <- actividades_en_malla_500 %>%
  group_by(COD500) %>%  
  summarise(n_act = sum(!is.na(TIPOLOGIA)), .groups = "drop")  # Solo cuenta si TIPOLOGIA no es NA

# Contar el número total de establecimientos en cada celda de la malla
total_establecimientos_500 <- establecimientos_en_malla_500 %>%
  group_by(COD500) %>%  
  summarise(t_est = sum(!is.na(TIPOLOGIA)), .groups = "drop") # Solo cuenta si TIPOLOGIA no es NA

# Combinar los conteos de actividades y establecimientos 
ACT_500 <- actividades_count_500 %>%
  st_drop_geometry() %>%  # Eliminar geometría para la unión por atributos
  left_join(st_drop_geometry(total_establecimientos_500), by = "COD500") %>%
  mutate(ACT = (n_act / t_est) * 100)

# Reemplazar NA con 0
ACT_500[is.na(ACT_500)] <- 0


# 6. Guardar los resultados------------------------------------------

ACT_az2 <- as.data.frame(ACT_az)[, !names(ACT_az) %in% "geometry"]
ACT_s2 <- as.data.frame(ACT_s)[, !names(ACT_s) %in% "geometry"]
ACT_N8_2 <- as.data.frame(ACT_N8)[, !names(ACT_N8) %in% "geometry"]
ACT_N9_2 <- as.data.frame(ACT_N9)[, !names(ACT_N9) %in% "geometry"]
ACT_1000_2 <- as.data.frame(ACT_1000)[, !names(ACT_1000) %in% "geometry"]
ACT_500_2 <- as.data.frame(ACT_500)[, !names(ACT_500) %in% "geometry"]
ACT_pr2 <- as.data.frame(ACT_pr)[, !names(ACT_pr) %in% "geometry"]

wb <- createWorkbook("ACT")

addWorksheet(wb, "ACT_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ACT_az", x=ACT_az2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ACT_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ACT_pr", x=ACT_pr2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ACT_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ACT_s", x=ACT_s2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ACT_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ACT_1000", x=ACT_1000_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ACT_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ACT_500", x=ACT_500_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ACT_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ACT_N8", x=ACT_N8_2, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "ACT_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "ACT_N9", x=ACT_N9_2, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Actividades densas en conocimiento.xlsx")