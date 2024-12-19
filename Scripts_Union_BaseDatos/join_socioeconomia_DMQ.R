# Script: Union de indicadores de la dimensión de Socioeconomía del DMQ. 

# Requerimiento: Antes de correr este código se debera correr los códigos de los indicadores de la dimensión de Socioeconomía.  

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
SS_ADM_ZONAL <-  ADM_ZONAL %>% 
  left_join(PSS_az, by = "adm_zonal") %>% 
  left_join(PSS_az_s, by = "adm_zonal") %>% 
  left_join(PSS_az_e, by = "adm_zonal") %>% 
  left_join(PSS_az_eq, by = "adm_zonal") %>% 
  left_join(PJ_az, by = "adm_zonal") %>% 
  left_join(PJ_az_s, by = "adm_zonal") %>% 
  left_join(PJ_az_e, by = "adm_zonal") %>% 
  mutate_all(~replace(., is.na(.), 0))

STP_ADM_ZONAL <-  ADM_ZONAL %>% 
  left_join(NINI_az, by = "adm_zonal") %>% 
  left_join(NINI_az_s, by = "adm_zonal") %>% 
  left_join(NINI_az_a, by = "adm_zonal") %>% 
  left_join(PCA_az, by = "adm_zonal") %>% 
  left_join(PCA_az_s, by = "adm_zonal") %>% 
  left_join(PCA_az_a, by = "adm_zonal") %>% 
  left_join(PSC_az, by = "adm_zonal") %>% 
  left_join(PSC_az_s, by = "adm_zonal") %>% 
  left_join(PSC_az_a, by = "adm_zonal") %>% 
  left_join(PNAE_az, by = "adm_zonal") %>% 
  left_join(PNAE_az_s, by = "adm_zonal") %>% 
  left_join(NBIP_az, by = "adm_zonal") %>% 
  left_join(NBIH_az, by = "adm_zonal") %>% 
  left_join(XNBIP_az, by = "adm_zonal") %>% 
  left_join(XNBIH_az, by = "adm_zonal") %>% 
  left_join(AER_az, by = "adm_zonal") %>% 
  left_join(POH_az, by = "adm_zonal") %>% 
  left_join(NEE_az, by = "adm_zonal") %>% 
  left_join(MPV_az, by = "adm_zonal") %>% 
  left_join(MPEV_az, by = "adm_zonal") %>% 
  left_join(SH_az, by = "adm_zonal") %>% 
  left_join(FSA_az, by = "adm_zonal") %>% 
  left_join(HAC_az, by = "adm_zonal") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SS_ADM_ZONAL, "SS_ADM_ZONAL.shp", delete_layer = TRUE)
st_write(STP_ADM_ZONAL, "STP_ADM_ZONAL.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------
SS_PARROQUIA <-  PARROQUIA %>% 
  left_join(PSS_pr, by = "parroquia") %>% 
  left_join(PSS_pr_s, by = "parroquia") %>% 
  left_join(PSS_pr_e, by = "parroquia") %>% 
  left_join(PSS_pr_eq, by = "parroquia") %>% 
  left_join(PJ_pr, by = "parroquia") %>% 
  left_join(PJ_pr_s, by = "parroquia") %>% 
  left_join(PJ_pr_e, by = "parroquia") %>% 
  mutate_all(~replace(., is.na(.), 0))

STP_PARROQUIA <-  PARROQUIA %>% 
  left_join(NINI_pr, by = "parroquia") %>% 
  left_join(NINI_pr_s, by = "parroquia") %>% 
  left_join(NINI_pr_a, by = "parroquia") %>% 
  left_join(PCA_pr, by = "parroquia") %>% 
  left_join(PCA_pr_s, by = "parroquia") %>% 
  left_join(PCA_pr_a, by = "parroquia") %>% 
  left_join(PSC_pr, by = "parroquia") %>% 
  left_join(PSC_pr_s, by = "parroquia") %>% 
  left_join(PSC_pr_a, by = "parroquia") %>% 
  left_join(PNAE_pr, by = "parroquia") %>% 
  left_join(PNAE_pr_s, by = "parroquia") %>% 
  left_join(NBIP_pr, by = "parroquia") %>% 
  left_join(NBIH_pr, by = "parroquia") %>% 
  left_join(XNBIP_pr, by = "parroquia") %>% 
  left_join(XNBIH_pr, by = "parroquia") %>% 
  left_join(AER_pr, by = "parroquia") %>% 
  left_join(POH_pr, by = "parroquia") %>% 
  left_join(NEE_pr, by = "parroquia") %>% 
  left_join(MPV_pr, by = "parroquia") %>% 
  left_join(MPEV_pr, by = "parroquia") %>% 
  left_join(SH_pr, by = "parroquia") %>% 
  left_join(FSA_pr, by = "parroquia") %>% 
  left_join(HAC_pr, by = "parroquia") %>% 
  mutate_all(~replace(., is.na(.), 0))


# Guardar 
st_write(SS_PARROQUIA, "SS_PARROQUIA.shp", delete_layer = TRUE)
st_write(STP_PARROQUIA, "STP_PARROQUIA.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
SS_SECTOR <-  SECTOR %>% 
  left_join(PSS_s, by = "Sector_DMQ") %>% 
  left_join(PSS_s_s, by = "Sector_DMQ") %>% 
  left_join(PSS_s_e, by = "Sector_DMQ") %>% 
  left_join(PSS_s_eq, by = "Sector_DMQ") %>% 
  left_join(PJ_s, by = "Sector_DMQ") %>% 
  left_join(PJ_s_s, by = "Sector_DMQ") %>% 
  left_join(PJ_s_e, by = "Sector_DMQ") %>% 
  mutate_all(~replace(., is.na(.), 0))

STP_SECTOR <-  SECTOR %>% 
  left_join(NINI_s, by = "Sector_DMQ") %>% 
  left_join(NINI_s_s, by = "Sector_DMQ") %>% 
  left_join(NINI_s_a, by = "Sector_DMQ") %>% 
  left_join(PCA_s, by = "Sector_DMQ") %>% 
  left_join(PCA_s_s, by = "Sector_DMQ") %>% 
  left_join(PCA_s_a, by = "Sector_DMQ") %>% 
  left_join(PSC_s, by = "Sector_DMQ") %>% 
  left_join(PSC_s_s, by = "Sector_DMQ") %>% 
  left_join(PSC_s_a, by = "Sector_DMQ") %>% 
  left_join(PNAE_s, by = "Sector_DMQ") %>% 
  left_join(PNAE_s_s, by = "Sector_DMQ") %>% 
  left_join(NBIP_s, by = "Sector_DMQ") %>% 
  left_join(NBIH_s, by = "Sector_DMQ") %>% 
  left_join(XNBIP_s, by = "Sector_DMQ") %>% 
  left_join(XNBIH_s, by = "Sector_DMQ") %>% 
  left_join(AER_s, by = "Sector_DMQ") %>% 
  left_join(POH_s, by = "Sector_DMQ") %>% 
  left_join(NEE_s, by = "Sector_DMQ") %>% 
  left_join(MPV_s, by = "Sector_DMQ") %>% 
  left_join(MPEV_s, by = "Sector_DMQ") %>% 
  left_join(SH_s, by = "Sector_DMQ") %>% 
  left_join(FSA_s, by = "Sector_DMQ") %>% 
  left_join(HAC_s, by = "Sector_DMQ") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SS_SECTOR, "SS_SECTOR.shp", delete_layer = TRUE)
st_write(STP_SECTOR, "STP_SECTOR.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------
SS_MALLA_1000 <-  MALLA_1000 %>% 
  left_join(PSS_1000, by = "COD1000") %>% 
  left_join(PSS_1000_s, by = "COD1000") %>% 
  left_join(PSS_1000_e, by = "COD1000") %>% 
  left_join(PSS_1000_eq, by = "COD1000") %>% 
  left_join(PJ_1000, by = "COD1000") %>% 
  left_join(PJ_1000_s, by = "COD1000") %>% 
  left_join(PJ_1000_e, by = "COD1000") %>% 
  mutate_all(~replace(., is.na(.), 0))

STP_MALLA_1000 <-  MALLA_1000 %>% 
  left_join(NINI_1000, by = "COD1000") %>% 
  left_join(NINI_1000_s, by = "COD1000") %>% 
  left_join(NINI_1000_a, by = "COD1000") %>% 
  left_join(PCA_1000, by = "COD1000") %>% 
  left_join(PCA_1000_s, by = "COD1000") %>% 
  left_join(PCA_1000_a, by = "COD1000") %>% 
  left_join(PSC_1000, by = "COD1000") %>% 
  left_join(PSC_1000_s, by = "COD1000") %>% 
  left_join(PSC_1000_a, by = "COD1000") %>% 
  left_join(PNAE_1000, by = "COD1000") %>% 
  left_join(PNAE_1000_s, by = "COD1000") %>% 
  left_join(NBIP_1000, by = "COD1000") %>% 
  left_join(NBIH_1000, by = "COD1000") %>% 
  left_join(XNBIP_1000, by = "COD1000") %>% 
  left_join(XNBIH_1000, by = "COD1000") %>% 
  left_join(AER_1000, by = "COD1000") %>% 
  left_join(POH_1000, by = "COD1000") %>% 
  left_join(NEE_1000, by = "COD1000") %>% 
  left_join(MPV_1000, by = "COD1000") %>% 
  left_join(MPEV_1000, by = "COD1000") %>% 
  left_join(SH_1000, by = "COD1000") %>% 
  left_join(FSA_1000, by = "COD1000") %>% 
  left_join(HAC_1000, by = "COD1000") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SS_MALLA_1000, "SS_MALLA_1000.shp", delete_layer = TRUE)
st_write(STP_MALLA_1000, "STP_MALLA_1000.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
SS_MALLA_500 <-  MALLA_500 %>% 
  left_join(PSS_500, by = "COD500") %>% 
  left_join(PSS_500_s, by = "COD500") %>% 
  left_join(PSS_500_e, by = "COD500") %>% 
  left_join(PSS_500_eq, by = "COD500") %>% 
  left_join(PJ_500, by = "COD500") %>% 
  left_join(PJ_500_s, by = "COD500") %>% 
  left_join(PJ_500_e, by = "COD500") %>% 
  mutate_all(~replace(., is.na(.), 0))

STP_MALLA_500 <-  MALLA_500 %>% 
  left_join(NINI_500, by = "COD500") %>% 
  left_join(NINI_500_s, by = "COD500") %>% 
  left_join(NINI_500_a, by = "COD500") %>% 
  left_join(PCA_500, by = "COD500") %>% 
  left_join(PCA_500_s, by = "COD500") %>% 
  left_join(PCA_500_a, by = "COD500") %>% 
  left_join(PSC_500, by = "COD500") %>% 
  left_join(PSC_500_s, by = "COD500") %>% 
  left_join(PSC_500_a, by = "COD500") %>% 
  left_join(PNAE_500, by = "COD500") %>% 
  left_join(PNAE_500_s, by = "COD500") %>% 
  left_join(NBIP_500, by = "COD500") %>% 
  left_join(NBIH_500, by = "COD500") %>% 
  left_join(XNBIP_500, by = "COD500") %>% 
  left_join(XNBIH_500, by = "COD500") %>% 
  left_join(AER_500, by = "COD500") %>% 
  left_join(POH_500, by = "COD500") %>% 
  left_join(NEE_500, by = "COD500") %>% 
  left_join(MPV_500, by = "COD500") %>% 
  left_join(MPEV_500, by = "COD500") %>% 
  left_join(SH_500, by = "COD500") %>% 
  left_join(FSA_500, by = "COD500") %>% 
  left_join(HAC_500, by = "COD500") %>% 
  mutate_all(~replace(., is.na(.), 0))

# Guardar 
st_write(SS_MALLA_500, "SS_MALLA_500.shp", delete_layer = TRUE)
st_write(STP_MALLA_500, "STP_MALLA_500.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
MALLA_H3_N8 <- MALLA_H3_N8 %>%
  rename(H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

SS_MALLA_H3_N8 <-  MALLA_H3_N8 %>% 
  left_join(PSS_N8, by = "H3_N8") %>% 
  left_join(PSS_N8_s, by = "H3_N8") %>% 
  left_join(PSS_N8_e, by = "H3_N8") %>% 
  left_join(PSS_N8_eq, by = "H3_N8") %>% 
  left_join(PJ_N8, by = "H3_N8") %>% 
  left_join(PJ_N8_s, by = "H3_N8") %>% 
  left_join(PJ_N8_e, by = "H3_N8") %>% 
  mutate_all(~replace(., is.na(.), 0))

STP_MALLA_H3_N8 <-  MALLA_H3_N8 %>% 
  left_join(NINI_N8, by = "H3_N8") %>% 
  left_join(NINI_N8_s, by = "H3_N8") %>% 
  left_join(NINI_N8_a, by = "H3_N8") %>% 
  left_join(PCA_N8, by = "H3_N8") %>% 
  left_join(PCA_N8_s, by = "H3_N8") %>% 
  left_join(PCA_N8_a, by = "H3_N8") %>% 
  left_join(PSC_N8, by = "H3_N8") %>% 
  left_join(PSC_N8_s, by = "H3_N8") %>% 
  left_join(PSC_N8_a, by = "H3_N8") %>% 
  left_join(PNAE_N8, by = "H3_N8") %>% 
  left_join(PNAE_N8_s, by = "H3_N8") %>% 
  left_join(NBIP_N8, by = "H3_N8") %>% 
  left_join(NBIH_N8, by = "H3_N8") %>% 
  left_join(XNBIP_N8, by = "H3_N8") %>% 
  left_join(XNBIH_N8, by = "H3_N8") %>% 
  left_join(AER_N8, by = "H3_N8") %>% 
  left_join(POH_N8, by = "H3_N8") %>% 
  left_join(NEE_N8, by = "H3_N8") %>% 
  left_join(MPV_N8, by = "H3_N8") %>% 
  left_join(MPEV_N8, by = "H3_N8") %>% 
  left_join(SH_N8, by = "H3_N8") %>% 
  left_join(FSA_N8, by = "H3_N8") %>% 
  left_join(HAC_N8, by = "H3_N8") %>% 
  mutate_all(~replace(., is.na(.), 0))

SS_MALLA_H3_N8 <- SS_MALLA_H3_N8 %>%
  rename(H3HASH = H3_N8 )    # Reemplazar old_column_name con el nombre actual de la columna
STP_MALLA_H3_N8 <- STP_MALLA_H3_N8 %>%
  rename(H3HASH = H3_N8 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(SS_MALLA_H3_N8, "SS_MALLA_H3_N8.shp", delete_layer = TRUE)
st_write(STP_MALLA_H3_N8, "STP_MALLA_H3_N8.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
MALLA_H3_N9 <- MALLA_H3_N9 %>%
  rename(H3_N9 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

SS_MALLA_H3_N9 <-  MALLA_H3_N9 %>% 
  left_join(PSS_N9, by = "H3_N9") %>% 
  left_join(PSS_N9_s, by = "H3_N9") %>% 
  left_join(PSS_N9_e, by = "H3_N9") %>% 
  left_join(PSS_N9_eq, by = "H3_N9") %>% 
  left_join(PJ_N9, by = "H3_N9") %>% 
  left_join(PJ_N9_s, by = "H3_N9") %>% 
  left_join(PJ_N9_e, by = "H3_N9") %>% 
  mutate_all(~replace(., is.na(.), 0))

STP_MALLA_H3_N9 <-  MALLA_H3_N9 %>% 
  left_join(NINI_N9, by = "H3_N9") %>% 
  left_join(NINI_N9_s, by = "H3_N9") %>% 
  left_join(NINI_N9_a, by = "H3_N9") %>% 
  left_join(PCA_N9, by = "H3_N9") %>% 
  left_join(PCA_N9_s, by = "H3_N9") %>% 
  left_join(PCA_N9_a, by = "H3_N9") %>% 
  left_join(PSC_N9, by = "H3_N9") %>% 
  left_join(PSC_N9_s, by = "H3_N9") %>% 
  left_join(PSC_N9_a, by = "H3_N9") %>% 
  left_join(PNAE_N9, by = "H3_N9") %>% 
  left_join(PNAE_N9_s, by = "H3_N9") %>% 
  left_join(NBIP_N9, by = "H3_N9") %>% 
  left_join(NBIH_N9, by = "H3_N9") %>% 
  left_join(XNBIP_N9, by = "H3_N9") %>% 
  left_join(XNBIH_N9, by = "H3_N9") %>% 
  left_join(AER_N9, by = "H3_N9") %>% 
  left_join(POH_N9, by = "H3_N9") %>% 
  left_join(NEE_N9, by = "H3_N9") %>% 
  left_join(MPV_N9, by = "H3_N9") %>% 
  left_join(MPEV_N9, by = "H3_N9") %>% 
  left_join(SH_N9, by = "H3_N9") %>% 
  left_join(FSA_N9, by = "H3_N9") %>% 
  left_join(HAC_N9, by = "H3_N9") %>% 
  mutate_all(~replace(., is.na(.), 0))

SS_MALLA_H3_N9 <- SS_MALLA_H3_N9 %>%
  rename(H3HASH = H3_N9 )    # Reemplazar old_column_name con el nombre actual de la columna
STP_MALLA_H3_N9 <- STP_MALLA_H3_N9 %>%
  rename(H3HASH = H3_N9 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(SS_MALLA_H3_N9, "SS_MALLA_H3_N9.shp", delete_layer = TRUE)
st_write(STP_MALLA_H3_N9, "STP_MALLA_H3_N9.shp", delete_layer = TRUE)
