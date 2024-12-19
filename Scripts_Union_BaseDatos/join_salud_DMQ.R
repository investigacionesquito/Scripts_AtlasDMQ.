# Script: Union de indicadores de la dimensión de salud del DMQ. 

# Requerimiento: Antes de correr este código se debera correr los códigos de los indicadores de la dimensión de salud.  

# Cargar librerías ----------------------------------------------

pacman::p_load(raster, rgdal, rgeos, ggrepel, stringr, sf, ggspatial, exactextractr, tidyverse,xlsx, 
               maptools,scales,dplyr,writexl)

# LEER SHAPES --------------------------------------------------------------

ADM_ZONAL <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")
PARROQUIA <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")
SECTOR <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\SECTOR_DMQ.shp")
MALLA_1000 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")
MALLA_500 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_500_DMQ.shp")
MALLA_H3_N8 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")
MALLA_H3_N9 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# ADMINISTRACION ZONAL -----------------------------------------------------------
SL_ADM_ZONAL <-  ADM_ZONAL %>% 
  left_join(DFP_az, by = "adm_zonal") %>% 
  left_join(ES_az, by = "adm_zonal") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SL_ADM_ZONAL, "SL_ADM_ZONAL.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------
SL_PARROQUIA <-  PARROQUIA %>% 
  left_join(DFP_pr, by = "parroquia") %>% 
  left_join(ES_pr, by = "parroquia") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SL_PARROQUIA, "SL_PARROQUIA.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
SL_SECTOR <-  SECTOR %>% 
  left_join(DFP_s, by = "Sector_DMQ") %>% 
  left_join(ES_s, by = "Sector_DMQ") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SL_SECTOR, "SL_SECTOR.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------
SL_MALLA_1000 <-  MALLA_1000 %>% 
  left_join(DFP_1000, by = "COD1000") %>% 
  left_join(ES_1000, by = "COD1000") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SL_MALLA_1000, "SL_MALLA_1000.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
SL_MALLA_500 <-  MALLA_500 %>% 
  left_join(DFP_500, by = "COD500") %>% 
  left_join(ES_500, by = "COD500") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SL_MALLA_500, "SL_MALLA_500.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
MALLA_H3_N8 <- MALLA_H3_N8 %>%
  rename(H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

ES_N8 <- ES_N8 %>%
  rename( H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

SL_MALLA_H3_N8 <-  MALLA_H3_N8 %>% 
  left_join(DFP_N8, by = "H3_N8") %>% 
  left_join(ES_N8, by = "H3_N8") %>% 
  mutate_all(~replace(., is.na(.), 0))

SL_MALLA_H3_N8 <- SL_MALLA_H3_N8 %>%
  rename(H3HASH = H3_N8 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(SL_MALLA_H3_N8, "SL_MALLA_H3_N8.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
MALLA_H3_N9 <- MALLA_H3_N9 %>%
  rename(H3_N9 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

ES_N9 <- ES_N9 %>%
  rename( H3_N9 = H3HASH)  

SL_MALLA_H3_N9 <-  MALLA_H3_N9 %>% 
  left_join(DFP_N9, by = "H3_N9") %>% 
  left_join(ES_N9, by = "H3_N9") %>% 
  mutate_all(~replace(., is.na(.), 0))

SL_MALLA_H3_N9 <- SL_MALLA_H3_N9 %>%
  rename(H3HASH = H3_N9 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(SL_MALLA_H3_N9, "SL_MALLA_H3_N9_3C.shp", delete_layer = TRUE)