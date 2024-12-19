# Script: Union de indicadores de la dimensión de Vivienda del DMQ. 

# Requerimiento: Antes de correr este código se debera correr los códigos de los indicadores de la dimensión de vivienda.  

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
PVPD_az <- PVPD_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_az <- HCL_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_az <- HSI_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSC_az <- HSC_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTV_az <- HTV_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_az <- PVRT_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna

V_ADM_ZONAL <-  ADM_ZONAL %>% 
  left_join(VAA_az, by = "adm_zonal") %>% 
  left_join(VEE_az, by = "adm_zonal") %>% 
  left_join(VRPA_az, by = "adm_zonal") %>% 
  left_join(VRB_az, by = "adm_zonal") %>%   
  left_join(PVPT_az, by = "adm_zonal") %>% 
  left_join(PVPO_az, by = "adm_zonal") %>% 
  left_join(PVTV_az, by = "adm_zonal") %>% 
  left_join(PVPD_az, by = "adm_zonal") %>%   
  left_join(PVH_az, by = "adm_zonal") %>% 
  left_join(PVCE_az, by = "adm_zonal")  %>% 
  left_join(HCL_az, by = "adm_zonal") %>% 
  left_join(HSI_az, by = "adm_zonal") %>%   
  left_join(HSC_az, by = "adm_zonal") %>% 
  left_join(HTV_az, by = "adm_zonal")  %>% 
  left_join(PVRT_az, by = "adm_zonal") %>% 
  left_join(HAC_az, by = "adm_zonal") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(V_ADM_ZONAL, "V_ADM_ZONAL.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------
PVPD_pr <- PVPD_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_pr <- HCL_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_pr <- HSI_pr %>%
  rename(parroquia  = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSC_pr <- HSC_pr %>%
  rename(parroquia  = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTV_pr <- HTV_pr %>%
  rename(parroquia  = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_pr <- PVRT_pr %>%
  rename(parroquia  = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna

V_PARROQUIA <-  PARROQUIA %>% 
  left_join(VAA_pr, by = "parroquia") %>% 
  left_join(VEE_pr, by = "parroquia") %>% 
  left_join(VRPA_pr, by = "parroquia") %>% 
  left_join(VRB_pr, by = "parroquia") %>%   
  left_join(PVPT_pr, by = "parroquia") %>% 
  left_join(PVPO_pr, by = "parroquia") %>% 
  left_join(PVTV_pr, by = "parroquia") %>% 
  left_join(PVPD_pr, by = "parroquia") %>%   
  left_join(PVH_pr, by = "parroquia") %>% 
  left_join(PVCE_pr, by = "parroquia")  %>% 
  left_join(HCL_pr, by = "parroquia") %>% 
  left_join(HSI_pr, by = "parroquia") %>%   
  left_join(HSC_pr, by = "parroquia") %>% 
  left_join(HTV_pr, by = "parroquia")  %>% 
  left_join(PVRT_pr, by = "parroquia") %>% 
  left_join(HAC_pr, by = "parroquia") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(V_PARROQUIA, "V_PARROQUIA.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
PVPD_s <- PVPD_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_s <- HCL_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_s <- HSI_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSC_s <- HSC_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTV_s <- HTV_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_s <- PVRT_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)

V_SECTOR <-  SECTOR %>% 
  left_join(VAA_s, by = "Sector_DMQ") %>% 
  left_join(VEE_s, by = "Sector_DMQ") %>% 
  left_join(VRPA_s, by = "Sector_DMQ") %>% 
  left_join(VRB_s, by = "Sector_DMQ") %>%   
  left_join(PVPT_s, by = "Sector_DMQ") %>% 
  left_join(PVPO_s, by = "Sector_DMQ") %>% 
  left_join(PVTV_s, by = "Sector_DMQ") %>% 
  left_join(PVPD_s, by = "Sector_DMQ") %>%   
  left_join(PVH_s, by = "Sector_DMQ") %>% 
  left_join(PVCE_s, by = "Sector_DMQ")  %>% 
  left_join(HCL_s, by = "Sector_DMQ") %>% 
  left_join(HSI_s, by = "Sector_DMQ") %>%   
  left_join(HSC_s, by = "Sector_DMQ") %>% 
  left_join(HTV_s, by = "Sector_DMQ")  %>% 
  left_join(PVRT_s, by = "Sector_DMQ") %>% 
  left_join(HAC_s, by = "Sector_DMQ") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(V_SECTOR, "V_SECTOR.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------
PVPD_1000 <- PVPD_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_1000 <- HCL_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_1000 <- HSI_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSC_1000 <- HSC_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTV_1000 <- HTV_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_1000 <- PVRT_1000 %>%
  rename(COD1000 = COD1000.x)

V_MALLA_1000 <-  MALLA_1000 %>% 
  left_join(VAA_1000, by = "COD1000") %>% 
  left_join(VEE_1000, by = "COD1000") %>% 
  left_join(VRPA_1000, by = "COD1000") %>% 
  left_join(VRB_1000, by = "COD1000") %>%   
  left_join(PVPT_1000, by = "COD1000") %>% 
  left_join(PVPO_1000, by = "COD1000") %>% 
  left_join(PVTV_1000, by = "COD1000") %>% 
  left_join(PVPD_1000, by = "COD1000") %>%   
  left_join(PVH_1000, by = "COD1000") %>% 
  left_join(PVCE_1000, by = "COD1000")  %>% 
  left_join(HCL_1000, by = "COD1000") %>% 
  left_join(HSI_1000, by = "COD1000") %>%   
  left_join(HSC_1000, by = "COD1000") %>% 
  left_join(HTV_1000, by = "COD1000")  %>% 
  left_join(PVRT_1000, by = "COD1000") %>% 
  left_join(HAC_1000, by = "COD1000") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(V_MALLA_1000, "V_MALLA_1000.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
PVPD_500 <- PVPD_500 %>%
  rename(COD500 = COD500.x)  
HCL_500 <- HCL_500 %>%
  rename(COD500 = COD500.x)   
HSI_500 <- HSI_500 %>%
  rename(COD500 = COD500.x)    
HSC_500 <- HSC_500 %>%
  rename(COD500 = COD500.x)    
HTV_500 <- HTV_500 %>%
  rename(COD500 = COD500.x)    
PVRT_500 <- PVRT_500 %>%
  rename(COD500 = COD500.x)  

V_MALLA_500 <-  MALLA_500 %>% 
  left_join(VAA_500, by = "COD500") %>% 
  left_join(VEE_500, by = "COD500") %>% 
  left_join(VRPA_500, by = "COD500") %>% 
  left_join(VRB_500, by = "COD500") %>%   
  left_join(PVPT_500, by = "COD500") %>% 
  left_join(PVPO_500, by = "COD500") %>% 
  left_join(PVTV_500, by = "COD500") %>% 
  left_join(PVPD_500, by = "COD500") %>%   
  left_join(PVH_500, by = "COD500") %>% 
  left_join(PVCE_500, by = "COD500")  %>% 
  left_join(HCL_500, by = "COD500") %>% 
  left_join(HSI_500, by = "COD500") %>%   
  left_join(HSC_500, by = "COD500") %>% 
  left_join(HTV_500, by = "COD500")  %>% 
  left_join(PVRT_500, by = "COD500") %>% 
  left_join(HAC_500, by = "COD500") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(V_MALLA_500, "V_MALLA_500.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
PVPD_N8 <- PVPD_N8 %>%
  rename(H3_N8 = H3_N8.x)  
HCL_N8 <- HCL_N8 %>%
  rename(H3_N8 = H3_N8.x)    # Reemplazar old_column_name con el nombre actual de la columna
HSI_N8 <- HSI_N8 %>%
  rename(H3_N8 = H3_N8.x)   # Reemplazar old_column_name con el nombre actual de la columna
HSC_N8 <- HSC_N8 %>%
  rename(H3_N8 = H3_N8.x)    # Reemplazar old_column_name con el nombre actual de la columna
HTV_N8 <- HTV_N8 %>%
  rename(H3_N8 = H3_N8.x)    # Reemplazar old_column_name con el nombre actual de la columna
PVRT_N8 <- PVRT_N8 %>%
  rename(H3_N8 = H3_N8.x)    # Reemplazar old_column_name con el nombre actual de la columna


MALLA_H3_N8 <- MALLA_H3_N8 %>%
  rename(H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

V_MALLA_H3_N8 <-  MALLA_H3_N8  %>% 
  left_join(VAA_N8, by = "H3_N8") %>% 
  left_join(VEE_N8, by = "H3_N8") %>% 
  left_join(VRPA_N8, by = "H3_N8") %>% 
  left_join(VRB_N8, by = "H3_N8") %>%   
  left_join(PVPT_N8, by = "H3_N8") %>% 
  left_join(PVPO_N8, by = "H3_N8") %>% 
  left_join(PVTV_N8, by = "H3_N8") %>% 
  left_join(PVPD_N8, by = "H3_N8") %>%   
  left_join(PVH_N8, by = "H3_N8") %>% 
  left_join(PVCE_N8, by = "H3_N8")  %>% 
  left_join(HCL_N8, by = "H3_N8") %>% 
  left_join(HSI_N8, by = "H3_N8") %>%   
  left_join(HSC_N8, by = "H3_N8") %>% 
  left_join(HTV_N8, by = "H3_N8")  %>% 
  left_join(PVRT_N8, by = "H3_N8") %>% 
  left_join(HAC_N8, by = "H3_N8") %>% 
  mutate_all(~replace(., is.na(.), 0))

V_MALLA_H3_N8 <- V_MALLA_H3_N8 %>%
  rename(H3HASH = H3_N8 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(V_MALLA_H3_N8, "V_MALLA_H3_N8.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
PVPD_N9 <- PVPD_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_N9 <- HCL_N9 %>%
  rename(H3_N9 = H3_N9.x)   # Reemplazar old_column_name con el nombre actual de la columna
HSI_N9 <- HSI_N9 %>%
  rename(H3_N9 = H3_N9.x)   # Reemplazar old_column_name con el nombre actual de la columna
HSC_N9 <- HSC_N9 %>%
  rename(H3_N9 = H3_N9.x)   # Reemplazar old_column_name con el nombre actual de la columna
HTV_N9 <- HTV_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_N9 <- PVRT_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N9 <- MALLA_H3_N9 %>%
  rename(H3_N9 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

V_MALLA_H3_N9 <-  MALLA_H3_N9 %>% 
  left_join(VAA_N9, by = "H3_N9") %>% 
  left_join(VEE_N9, by = "H3_N9") %>% 
  left_join(VRPA_N9, by = "H3_N9") %>% 
  left_join(VRB_N9, by = "H3_N9") %>%   
  left_join(PVPT_N9, by = "H3_N9") %>% 
  left_join(PVPO_N9, by = "H3_N9") %>% 
  left_join(PVTV_N9, by = "H3_N9") %>% 
  left_join(PVPD_N9, by = "H3_N9") %>%   
  left_join(PVH_N9, by = "H3_N9") %>% 
  left_join(PVCE_N9, by = "H3_N9")  %>% 
  left_join(HCL_N9, by = "H3_N9") %>% 
  left_join(HSI_N9, by = "H3_N9") %>%   
  left_join(HSC_N9, by = "H3_N9") %>% 
  left_join(HTV_N9, by = "H3_N9")  %>% 
  left_join(PVRT_N9, by = "H3_N9") %>% 
  left_join(HAC_N9, by = "H3_N9") %>% 
  mutate_all(~replace(., is.na(.), 0))

V_MALLA_H3_N9 <- V_MALLA_H3_N9 %>%
  rename(H3HASH = H3_N9 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(V_MALLA_H3_N9, "V_MALLA_H3_N9.shp", delete_layer = TRUE)
