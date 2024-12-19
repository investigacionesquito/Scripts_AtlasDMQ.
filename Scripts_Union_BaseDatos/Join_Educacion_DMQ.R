# Script: Union de indicadores de la dimensión de Educación del DMQ. 

# Requerimiento: Antes de correr este código se debera correr los códigos de los indicadores de la dimensión de educación.  

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
HSR_az <- HSR_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna

E_ADM_ZONAL <-  ADM_ZONAL %>% 
  left_join(APE_az, by = "adm_zonal") %>% 
  left_join(APE_az_s, by = "adm_zonal") %>% 
  left_join(PTS_az, by = "adm_zonal") %>% 
  left_join(PTS_az_s, by = "adm_zonal") %>% 
  left_join(PTS_az_a, by = "adm_zonal") %>% 
  left_join(PTT_az, by = "adm_zonal") %>% 
  left_join(PTT_az_s, by = "adm_zonal") %>% 
  left_join(PTT_az_a, by = "adm_zonal") %>% 
  left_join(NEI_az, by = "adm_zonal") %>% 
  left_join(NEI_az_s, by = "adm_zonal") %>% 
  left_join(TA_az, by = "adm_zonal") %>% 
  left_join(TA_az_s, by = "adm_zonal") %>% 
  left_join(TA_az_a, by = "adm_zonal") %>% 
  left_join(TAD_az, by = "adm_zonal") %>% 
  left_join(TAD_az_s, by = "adm_zonal") %>% 
  left_join(TAD_az_a, by = "adm_zonal") %>% 
  left_join(HSR_az, by = "adm_zonal") %>% 
  left_join(HSR_az_a, by = "adm_zonal") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(E_ADM_ZONAL, "E_ADM_ZONAL.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------
HSR_pr <- HSR_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna

E_PARROQUIA <-  PARROQUIA %>% 
  left_join(APE_pr, by = "parroquia") %>% 
  left_join(APE_pr_s, by = "parroquia") %>% 
  left_join(PTS_pr, by = "parroquia") %>% 
  left_join(PTS_pr_s, by = "parroquia") %>% 
  left_join(PTS_pr_a, by = "parroquia") %>% 
  left_join(PTT_pr, by = "parroquia") %>% 
  left_join(PTT_pr_s, by = "parroquia") %>% 
  left_join(PTT_pr_a, by = "parroquia") %>% 
  left_join(NEI_pr, by = "parroquia") %>% 
  left_join(NEI_pr_s, by = "parroquia") %>% 
  left_join(TA_pr, by = "parroquia") %>% 
  left_join(TA_pr_s, by = "parroquia") %>% 
  left_join(TA_pr_a, by = "parroquia") %>% 
  left_join(TAD_pr, by = "parroquia") %>% 
  left_join(TAD_pr_s, by = "parroquia") %>% 
  left_join(TAD_pr_a, by = "parroquia") %>% 
  left_join(HSR_pr, by = "parroquia") %>% 
  left_join(HSR_pr_a, by = "parroquia") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(E_PARROQUIA, "E_PARROQUIA.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
HSR_s <- HSR_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna

E_SECTOR <-  SECTOR %>% 
  left_join(APE_s, by = "Sector_DMQ") %>% 
  left_join(APE_s_s, by = "Sector_DMQ") %>% 
  left_join(PTS_s, by = "Sector_DMQ") %>% 
  left_join(PTS_s_s, by = "Sector_DMQ") %>% 
  left_join(PTS_s_a, by = "Sector_DMQ") %>% 
  left_join(PTT_s, by = "Sector_DMQ") %>% 
  left_join(PTT_s_s, by = "Sector_DMQ") %>% 
  left_join(PTT_s_a, by = "Sector_DMQ") %>% 
  left_join(NEI_s, by = "Sector_DMQ") %>% 
  left_join(NEI_s_s, by = "Sector_DMQ") %>% 
  left_join(TA_s, by = "Sector_DMQ") %>% 
  left_join(TA_s_s, by = "Sector_DMQ") %>% 
  left_join(TA_s_a, by = "Sector_DMQ") %>% 
  left_join(TAD_s, by = "Sector_DMQ") %>% 
  left_join(TAD_s_s, by = "Sector_DMQ") %>% 
  left_join(TAD_s_a, by = "Sector_DMQ") %>% 
  left_join(HSR_s, by = "Sector_DMQ") %>% 
  left_join(HSR_s_a, by = "Sector_DMQ") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(E_SECTOR, "E_SECTOR.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------
HSR_1000 <- HSR_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna

E_MALLA_1000 <-  MALLA_1000 %>% 
  left_join(APE_1000, by = "COD1000") %>% 
  left_join(APE_1000_s, by = "COD1000") %>% 
  left_join(PTS_1000, by = "COD1000") %>% 
  left_join(PTS_1000_s, by = "COD1000") %>% 
  left_join(PTS_1000_a, by = "COD1000") %>% 
  left_join(PTT_1000, by = "COD1000") %>% 
  left_join(PTT_1000_s, by = "COD1000") %>% 
  left_join(PTT_1000_a, by = "COD1000") %>% 
  left_join(NEI_1000, by = "COD1000") %>% 
  left_join(NEI_1000_s, by = "COD1000") %>% 
  left_join(TA_1000, by = "COD1000") %>% 
  left_join(TA_1000_s, by = "COD1000") %>% 
  left_join(TA_1000_a, by = "COD1000") %>% 
  left_join(TAD_1000, by = "COD1000") %>% 
  left_join(TAD_1000_s, by = "COD1000") %>% 
  left_join(TAD_1000_a, by = "COD1000") %>% 
  left_join(HSR_1000, by = "COD1000") %>% 
  left_join(HSR_1000_a, by = "COD1000") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(E_MALLA_1000, "E_MALLA_1000.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
HSR_500 <- HSR_500 %>%
  rename(COD500 = COD500.x)  

E_MALLA_500 <-  MALLA_500 %>% 
  left_join(APE_500, by = "COD500") %>% 
  left_join(APE_500_s, by = "COD500") %>% 
  left_join(PTS_500, by = "COD500") %>% 
  left_join(PTS_500_s, by = "COD500") %>% 
  left_join(PTS_500_a, by = "COD500") %>% 
  left_join(PTT_500, by = "COD500") %>% 
  left_join(PTT_500_s, by = "COD500") %>% 
  left_join(PTT_500_a, by = "COD500") %>% 
  left_join(NEI_500, by = "COD500") %>% 
  left_join(NEI_500_s, by = "COD500") %>% 
  left_join(TA_500, by = "COD500") %>% 
  left_join(TA_500_s, by = "COD500") %>% 
  left_join(TA_500_a, by = "COD500") %>% 
  left_join(TAD_500, by = "COD500") %>% 
  left_join(TAD_500_s, by = "COD500") %>% 
  left_join(TAD_500_a, by = "COD500") %>% 
  left_join(HSR_500, by = "COD500") %>% 
  left_join(HSR_500_a, by = "COD500") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(E_MALLA_500, "E_MALLA_500.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
HSR_N8 <- HSR_N8 %>%
  rename(H3_N8 = H3_N8.x)  

MALLA_H3_N8 <- MALLA_H3_N8 %>%
  rename(H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

E_MALLA_H3_N8 <-  MALLA_H3_N8 %>% 
  left_join(APE_N8, by = "H3_N8") %>% 
  left_join(APE_N8_s, by = "H3_N8") %>% 
  left_join(PTS_N8, by = "H3_N8") %>% 
  left_join(PTS_N8_s, by = "H3_N8") %>% 
  left_join(PTS_N8_a, by = "H3_N8") %>% 
  left_join(PTT_N8, by = "H3_N8") %>% 
  left_join(PTT_N8_s, by = "H3_N8") %>% 
  left_join(PTT_N8_a, by = "H3_N8") %>% 
  left_join(NEI_N8, by = "H3_N8") %>% 
  left_join(NEI_N8_s, by = "H3_N8") %>% 
  left_join(TA_N8, by = "H3_N8") %>% 
  left_join(TA_N8_s, by = "H3_N8") %>% 
  left_join(TA_N8_a, by = "H3_N8") %>% 
  left_join(TAD_N8, by = "H3_N8") %>% 
  left_join(TAD_N8_s, by = "H3_N8") %>% 
  left_join(TAD_N8_a, by = "H3_N8") %>% 
  left_join(HSR_N8, by = "H3_N8") %>% 
  left_join(HSR_N8_a, by = "H3_N8") %>% 
  mutate_all(~replace(., is.na(.), 0))

E_MALLA_H3_N8 <- E_MALLA_H3_N8 %>%
  rename(H3HASH = H3_N8 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(E_MALLA_H3_N8, "E_MALLA_H3_N8.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
HSR_N9 <- HSR_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N9 <- MALLA_H3_N9 %>%
  rename(H3_N9 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

E_MALLA_H3_N9 <-  MALLA_H3_N9 %>% 
  left_join(APE_N9, by = "H3_N9") %>% 
  left_join(APE_N9_s, by = "H3_N9") %>% 
  left_join(PTS_N9, by = "H3_N9") %>% 
  left_join(PTS_N9_s, by = "H3_N9") %>% 
  left_join(PTS_N9_a, by = "H3_N9") %>% 
  left_join(PTT_N9, by = "H3_N9") %>% 
  left_join(PTT_N9_s, by = "H3_N9") %>% 
  left_join(PTT_N9_a, by = "H3_N9") %>% 
  left_join(NEI_N9, by = "H3_N9") %>% 
  left_join(NEI_N9_s, by = "H3_N9") %>% 
  left_join(TA_N9, by = "H3_N9") %>% 
  left_join(TA_N9_s, by = "H3_N9") %>% 
  left_join(TA_N9_a, by = "H3_N9") %>% 
  left_join(TAD_N9, by = "H3_N9") %>% 
  left_join(TAD_N9_s, by = "H3_N9") %>% 
  left_join(TAD_N9_a, by = "H3_N9") %>% 
  left_join(HSR_N9, by = "H3_N9") %>% 
  left_join(HSR_N9_a, by = "H3_N9") %>% 
  mutate_all(~replace(., is.na(.), 0))

E_MALLA_H3_N9 <- E_MALLA_H3_N9 %>%
  rename(H3HASH = H3_N9 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(E_MALLA_H3_N9, "E_MALLA_H3_N9.shp", delete_layer = TRUE)