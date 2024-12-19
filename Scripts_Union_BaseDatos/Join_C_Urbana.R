# Script: Union de indicadores de la dimensión de Complejidades Urbanas del DMQ. 

# Requerimiento: Antes de correr este código se debera correr los códigos de los indicadores de la dimensión de complejidades urbanas  

# Cargar librerías ----------------------------------------------

pacman::p_load(sf,dplyr)

# LEER SHAPES --------------------------------------------------------------

ADM_ZONAL <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")
PARROQUIA <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")
SECTOR <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\SECTOR_DMQ.shp")
MALLA_1000 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")
MALLA_500 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_500_DMQ.shp")
MALLA_H3_N8 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")
MALLA_H3_N9 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# ADMINISTRACION ZONAL -----------------------------------------------------------
SCU_ADM_ZONAL <-  Eqact_az  %>% 
  left_join(ACT_az, by = "adm_zonal") %>% 
  left_join(pact_az, by = "adm_zonal") %>% 
  left_join(H_az, by = "adm_zonal") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  st_as_sf()
  
# Guardar 
st_write(SCU_ADM_ZONAL, "SCU_ADM_ZONAL.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------
SCU_PARROQUIA <-  Eqact_pr  %>% 
  left_join(ACT_pr, by = "parroquia") %>% 
  left_join(pact_pr, by = "parroquia") %>% 
  left_join(H_pr, by = "parroquia") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  st_as_sf()

# Guardar 
st_write(SCU_PARROQUIA, "SCU_PARROQUIA.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
SCU_SECTOR <-  Eqact_s  %>% 
  left_join(ACT_s, by = "Sector_DMQ") %>% 
  left_join(pact_s, by = "Sector_DMQ") %>% 
  left_join(H_s, by = "Sector_DMQ") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  st_as_sf()

# Guardar 
st_write(SCU_SECTOR, "SCU_SECTOR.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------
SCU_MALLA_1000 <-  Eqact_1000  %>% 
  left_join(ACT_1000, by = "COD1000") %>% 
  left_join(pact_1000, by = "COD1000") %>% 
  left_join(H_1000, by = "COD1000") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  st_as_sf() 

# Guardar 
st_write(SCU_MALLA_1000, "SCU_MALLA_1000.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
SCU_MALLA_500 <-  Eqact_500  %>% 
  left_join(ACT_500, by = "COD500") %>% 
  left_join(pact_500, by = "COD500") %>% 
  left_join(H_500, by = "COD500") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  st_as_sf() 

# Guardar 
st_write(SCU_MALLA_500, "SCU_MALLA_500.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
SCU_MALLA_H3_N8 <-  Eqact_N8  %>% 
  left_join(ACT_N8, by = "H3HASH") %>% 
  left_join(pact_N8, by = "H3HASH") %>% 
  left_join(H_N8, by = "H3HASH") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  st_as_sf() 

# Guardar 
st_write(SCU_MALLA_H3_N8, "SCU_MALLA_H3_N8.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
SCU_MALLA_H3_N9 <-  Eqact_N9  %>% 
  left_join(ACT_N9, by = "H3HASH") %>% 
  left_join(pact_N9, by = "H3HASH") %>% 
  left_join(H_N9, by = "H3HASH") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  st_as_sf() 

# Guardar 
st_write(SCU_MALLA_H3_N9, "SCU_MALLA_H3_N9.shp", delete_layer = TRUE)