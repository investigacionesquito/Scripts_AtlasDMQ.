
# Script: Union de número de viviendas, número de hogares y poblacion del DMQ en el año 2010. 

# Requerimiento: Antes de correr este código se debera correr los códigos de num_v2010,total_hog2010 y CPA2010.  

# Cargar librerías ----------------------------------------------

pacman::p_load(raster, rgdal, rgeos, ggrepel, stringr, sf, ggspatial, exactextractr, tidyverse,xlsx, 
               maptools,scales,dplyr,writexl)

# LEER SHAPES --------------------------------------------------------------
ADM_ZONAL <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ2010\\ADM_ZONAL_DMQ2010.shp")
PARROQUIA <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ2010\\PARROQUIA_DMQ2010.shp")
SECTOR <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ2010\\SECTOR_DMQ2010.shp")
MALLA_1000 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ2010\\MALLA_1000_DMQ2010.shp")
MALLA_500 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ2010\\MALLA_500_DMQ2010.shp")
MALLA_H3_N8 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ2010\\MALLA_H3_N8_DMQ2010.shp")
MALLA_H3_N9 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ2010\\MALLA_H3_N9_DMQ2010.shp")

# ADMINISTRACION ZONAL -----------------------------------------------------------
ADM_ZONAL_DMQ <-  ADM_ZONAL %>% 
  left_join(num_v2010_az, by = "adm_zonal") %>% 
  left_join(t_hog2010_az, by = "adm_zonal") %>% 
  left_join(CPA2010_az, by = "adm_zonal") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(ADM_ZONAL_DMQ, "ADM_ZONAL_DMQ2010.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------
PARROQUIA_DMQ <-  PARROQUIA %>% 
  left_join(num_v2010_pr, by = "parroquia") %>% 
  left_join(t_hog2010_pr, by = "parroquia") %>% 
  left_join(CPA2010_pr, by = "parroquia") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(PARROQUIA_DMQ, "PARROQUIA_DMQ2010.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
SECTOR_DMQ <-  SECTOR %>% 
  left_join(num_v2010_s, by = "Sector_DMQ") %>% 
  left_join(t_hog2010_s, by = "Sector_DMQ") %>% 
  left_join(CPA2010_s, by = "Sector_DMQ") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SECTOR_DMQ, "SECTOR_DMQ2010.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------
MALLA_1000_DMQ <-  MALLA_1000 %>% 
  left_join(num_v2010_1000, by = "COD1000") %>% 
  left_join(t_hog2010_1000, by = "COD1000") %>% 
  left_join(CPA2010_1000, by = "COD1000") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(MALLA_1000_DMQ, "MALLA_1000_DMQ2010.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
MALLA_500_DMQ <-  MALLA_500 %>% 
  left_join(num_v2010_500, by = "COD500") %>% 
  left_join(t_hog2010_500, by = "COD500") %>%  
  left_join(CPA2010_500, by = "COD500") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(MALLA_500_DMQ, "MALLA_500_DMQ2010.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
MALLA_H3_N8 <- MALLA_H3_N8 %>%
  rename(H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N8_DMQ <-  MALLA_H3_N8  %>% 
  left_join(num_v2010_N8, by = "H3_N8") %>% 
  left_join(t_hog2010_N8, by = "H3_N8") %>% 
  left_join(CPA2010_N8, by = "H3_N8") %>% 
  mutate_all(~replace(., is.na(.), 0))

MALLA_H3_N8_DMQ <- MALLA_H3_N8_DMQ %>%
  rename(H3HASH = H3_N8 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(MALLA_H3_N8_DMQ, "MALLA_H3_N8_DMQ2010.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
MALLA_H3_N9 <- MALLA_H3_N9 %>%
  rename(H3_N9 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N9_DMQ <-  MALLA_H3_N9 %>% 
  left_join(num_v2010_N9, by = "H3_N9") %>% 
  left_join(t_hog2010_N9, by = "H3_N9") %>% 
  left_join(CPA2010_N9, by = "H3_N9") %>% 
  mutate_all(~replace(., is.na(.), 0))

MALLA_H3_N9_DMQ <- MALLA_H3_N9_DMQ %>%
  rename(H3HASH = H3_N9 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(MALLA_H3_N9_DMQ, "MALLA_H3_N9_DMQ2010.shp", delete_layer = TRUE)
