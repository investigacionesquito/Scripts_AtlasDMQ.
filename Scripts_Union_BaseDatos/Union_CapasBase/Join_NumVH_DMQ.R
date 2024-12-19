# Script: Union de número de viviendas, número de hogares  del DMQ en el año 2022. 

# Requerimiento: Antes de correr este código se debera correr los códigos de num_v,total_hog 

# Cargar librerías ----------------------------------------------

pacman::p_load(raster, rgdal, rgeos, ggrepel, stringr, sf, ggspatial, exactextractr, tidyverse,xlsx, 
               maptools,scales,dplyr,writexl)

# LEER SHAPES --------------------------------------------------------------

ADM_ZONAL <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\RESULTADOS\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")
PARROQUIA <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\RESULTADOS\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")
SECTOR <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\RESULTADOS\\CAPAS_DMQ\\SECTOR_DMQ.shp")
MALLA_1000 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\RESULTADOS\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")
MALLA_500 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\RESULTADOS\\CAPAS_DMQ\\MALLA_500_DMQ.shp")
MALLA_H3_N8 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\RESULTADOS\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")
MALLA_H3_N9 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\RESULTADOS\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# ADMINISTRACION ZONAL -----------------------------------------------------------
ADM_ZONAL_DMQ <-  ADM_ZONAL %>% 
  left_join(num_v_az, by = "adm_zonal") %>% 
  left_join(total_hog_az, by = "adm_zonal") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(ADM_ZONAL_DMQ, "ADM_ZONAL_DMQ.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------
PARROQUIA_DMQ <-  PARROQUIA %>% 
  left_join(num_v_pr, by = "parroquia") %>% 
  left_join(total_hog_pr, by = "parroquia") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(PARROQUIA_DMQ, "PARROQUIA_DMQ.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
SECTOR_DMQ <-  SECTOR %>% 
  left_join(num_v_s, by = "Sector_DMQ") %>% 
  left_join(total_hog_s, by = "Sector_DMQ") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SECTOR_DMQ, "SECTOR_DMQ.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------

MALLA_1000_DMQ <-  MALLA_1000 %>% 
  left_join(num_v_1000, by = "COD1000") %>% 
  left_join(total_hog_1000, by = "COD1000") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(MALLA_1000_DMQ, "MALLA_1000_DMQ.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
MALLA_500_DMQ <-  MALLA_500 %>% 
  left_join(num_v_500, by = "COD500") %>% 
  left_join(total_hog_500, by = "COD500") %>%  
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(MALLA_500_DMQ, "MALLA_500_DMQ.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
MALLA_H3_N8 <- MALLA_H3_N8 %>%
  rename(H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N8_DMQ <-  MALLA_H3_N8  %>% 
  left_join(num_v_N8, by = "H3_N8") %>% 
  left_join(total_hog_N8, by = "H3_N8") %>% 
  mutate_all(~replace(., is.na(.), 0))

MALLA_H3_N8_DMQ <- MALLA_H3_N8_DMQ %>%
  rename(H3HASH = H3_N8 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(MALLA_H3_N8_DMQ, "MALLA_H3_N8_DMQ.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
MALLA_H3_N9 <- MALLA_H3_N9 %>%
  rename(H3_N9 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N9_DMQ <-  MALLA_H3_N9 %>% 
  left_join(num_v_N9, by = "H3_N9") %>% 
  left_join(total_hog_N9, by = "H3_N9") %>% 
  mutate_all(~replace(., is.na(.), 0))

MALLA_H3_N9_DMQ <- MALLA_H3_N9_DMQ %>%
  rename(H3HASH = H3_N9 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(MALLA_H3_N9_DMQ, "MALLA_H3_N9_DMQ.shp", delete_layer = TRUE)
