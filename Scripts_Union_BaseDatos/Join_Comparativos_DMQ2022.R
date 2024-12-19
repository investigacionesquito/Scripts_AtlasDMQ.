# Script: Union de indicadores comparativos del DMQ en el año 2022. 

# Requerimiento: Antes de correr este código se debera correr los códigos de los indicadores comparativos 2022.   

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
HTH_az <- HTH_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_az <- HSI_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_az <- HCL_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_az <- PVRT_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN_az <- HTHN_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN_az <- HSIN_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN_az <- HCLN_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN_az <- PVRTN_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna

ADM_ZONAL_DMQ <-  ADM_ZONAL %>% 
  left_join(CPA_az, by = "adm_zonal") %>% 
  left_join(HTH_az, by = "adm_zonal") %>% 
  left_join(HTHN_az, by = "adm_zonal") %>% 
  left_join(EM_az, by = "adm_zonal") %>% 
  left_join(PPC_az, by = "adm_zonal") %>%
  left_join(PPCN_az, by = "adm_zonal") %>% 
  left_join(IE_az, by = "adm_zonal") %>% 
  left_join(RD_az, by = "adm_zonal") %>% 
  left_join(HSI_az, by = "adm_zonal") %>% 
  left_join(HSIN_az, by = "adm_zonal") %>% 
  left_join(HCL_az, by = "adm_zonal") %>% 
  left_join(HCLN_az, by = "adm_zonal") %>% 
  left_join(PVPT_az, by = "adm_zonal") %>% 
  left_join(PVPTN_az, by = "adm_zonal") %>% 
  left_join(PVRT_az, by = "adm_zonal") %>% 
  left_join(PVRTN_az, by = "adm_zonal") %>% 
  left_join(APE_az, by = "adm_zonal") %>% 
  left_join(TA_az, by = "adm_zonal") %>% 
  left_join(TA_az_s, by = "adm_zonal") %>%
  left_join(PTS_az, by = "adm_zonal") %>% 
  left_join(PTSN_az, by = "adm_zonal") %>% 
  left_join(DFP_az, by = "adm_zonal") %>% 
  left_join(DFPN_az, by = "adm_zonal") %>% 
  left_join(PSS_az, by = "adm_zonal") %>% 
  left_join(PSSN_az, by = "adm_zonal") %>% 
  left_join(PSC_az, by = "adm_zonal") %>% 
  left_join(PSCN_az, by = "adm_zonal") %>% 
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
st_write(ADM_ZONAL_DMQ, "ADM_ZONAL_DMQ2022.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------
HTH_pr <- HTH_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_pr <- HSI_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_pr <- HCL_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_pr <- PVRT_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN_pr <- HTHN_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN_pr <- HSIN_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN_pr <- HCLN_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN_pr <- PVRTN_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna

PARROQUIA_DMQ <-  PARROQUIA %>% 
  left_join(CPA_pr, by = "parroquia") %>% 
  left_join(HTH_pr, by = "parroquia") %>% 
  left_join(HTHN_pr, by = "parroquia") %>% 
  left_join(EM_pr, by = "parroquia") %>% 
  left_join(PPC_pr, by = "parroquia") %>%
  left_join(PPCN_pr, by = "parroquia") %>% 
  left_join(IE_pr, by = "parroquia") %>% 
  left_join(RD_pr, by = "parroquia") %>% 
  left_join(HSI_pr, by = "parroquia") %>% 
  left_join(HSIN_pr, by = "parroquia") %>% 
  left_join(HCL_pr, by = "parroquia") %>% 
  left_join(HCLN_pr, by = "parroquia") %>% 
  left_join(PVPT_pr, by = "parroquia") %>% 
  left_join(PVPTN_pr, by = "parroquia") %>% 
  left_join(PVRT_pr, by = "parroquia") %>% 
  left_join(PVRTN_pr, by = "parroquia") %>% 
  left_join(APE_pr, by = "parroquia") %>% 
  left_join(TA_pr, by = "parroquia") %>% 
  left_join(PTS_pr, by = "parroquia") %>% 
  left_join(PTSN_pr, by = "parroquia") %>% 
  left_join(DFP_pr, by = "parroquia") %>% 
  left_join(DFPN_pr, by = "parroquia") %>% 
  left_join(PSS_pr, by = "parroquia") %>% 
  left_join(PSSN_pr, by = "parroquia") %>% 
  left_join(PSC_pr, by = "parroquia") %>% 
  left_join(PSCN_pr, by = "parroquia") %>% 
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
st_write(PARROQUIA_DMQ, "PARROQUIA_DMQ2022.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
HTH_s <- HTH_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_s <- HSI_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_s <- HCL_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_s <- PVRT_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN_s <- HTHN_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN_s <- HSIN_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN_s <- HCLN_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN_s <- PVRTN_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna

SECTOR_DMQ <-  SECTOR %>% 
  left_join(CPA_s2, by = "Sector_DMQ") %>% 
  left_join(HTH_s, by = "Sector_DMQ") %>% 
  left_join(HTHN_s, by = "Sector_DMQ") %>% 
  left_join(EM_s, by = "Sector_DMQ") %>% 
  left_join(PPC_s, by = "Sector_DMQ") %>%
  left_join(PPCN_s, by = "Sector_DMQ") %>% 
  left_join(IE_s, by = "Sector_DMQ") %>% 
  left_join(RD_s, by = "Sector_DMQ") %>% 
  left_join(HSI_s, by = "Sector_DMQ") %>% 
  left_join(HSIN_s, by = "Sector_DMQ") %>% 
  left_join(HCL_s, by = "Sector_DMQ") %>% 
  left_join(HCLN_s, by = "Sector_DMQ") %>% 
  left_join(PVPT_s, by = "Sector_DMQ") %>% 
  left_join(PVPTN_s, by = "Sector_DMQ") %>% 
  left_join(PVRT_s, by = "Sector_DMQ") %>% 
  left_join(PVRTN_s, by = "Sector_DMQ") %>% 
  left_join(APE_s, by = "Sector_DMQ") %>% 
  left_join(TA_s, by = "Sector_DMQ") %>% 
  left_join(PTS_s, by = "Sector_DMQ") %>% 
  left_join(PTSN_s, by = "Sector_DMQ") %>% 
  left_join(DFP_s, by = "Sector_DMQ") %>% 
  left_join(DFPN_s, by = "Sector_DMQ") %>% 
  left_join(PSS_s, by = "Sector_DMQ") %>% 
  left_join(PSSN_s, by = "Sector_DMQ") %>% 
  left_join(PSC_s, by = "Sector_DMQ") %>% 
  left_join(PSCN_s, by = "Sector_DMQ") %>% 
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
st_write(SECTOR_DMQ, "SECTOR_DMQ2022.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------
HTH_1000 <- HTH_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_1000 <- HSI_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_1000 <- HCL_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_1000 <- PVRT_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN_1000 <- HTHN_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN_1000 <- HSIN_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN_1000 <- HCLN_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN_1000 <- PVRTN_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_1000_DMQ <-  MALLA_1000 %>% 
  left_join(CPA_1000, by = "COD1000") %>% 
  left_join(HTH_1000, by = "COD1000") %>% 
  left_join(HTHN_1000, by = "COD1000") %>% 
  left_join(EM_1000, by = "COD1000") %>% 
  left_join(PPC_1000, by = "COD1000") %>%
  left_join(PPCN_1000, by = "COD1000") %>% 
  left_join(IE_1000, by = "COD1000") %>% 
  left_join(RD_1000, by = "COD1000") %>% 
  left_join(HSI_1000, by = "COD1000") %>% 
  left_join(HSIN_1000, by = "COD1000") %>% 
  left_join(HCL_1000, by = "COD1000") %>% 
  left_join(HCLN_1000, by = "COD1000") %>% 
  left_join(PVPT_1000, by = "COD1000") %>% 
  left_join(PVPTN_1000, by = "COD1000") %>% 
  left_join(PVRT_1000, by = "COD1000") %>% 
  left_join(PVRTN_1000, by = "COD1000") %>% 
  left_join(APE_1000, by = "COD1000") %>% 
  left_join(TA_1000, by = "COD1000") %>% 
  left_join(PTS_1000, by = "COD1000") %>% 
  left_join(PTSN_1000, by = "COD1000") %>% 
  left_join(DFP_1000, by = "COD1000") %>% 
  left_join(DFPN_1000, by = "COD1000") %>% 
  left_join(PSS_1000, by = "COD1000") %>% 
  left_join(PSSN_1000, by = "COD1000") %>% 
  left_join(PSC_1000, by = "COD1000") %>% 
  left_join(PSCN_1000, by = "COD1000") %>% 
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
st_write(MALLA_1000_DMQ, "MALLA_1000_DMQ2022.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
HTH_500 <- HTH_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_500 <- HSI_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_500 <- HCL_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_500 <- PVRT_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN_500 <- HTHN_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN_500 <- HSIN_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN_500 <- HCLN_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN_500 <- PVRTN_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_500_DMQ <-  MALLA_500 %>% 
  left_join(CPA_500, by = "COD500") %>% 
  left_join(HTH_500, by = "COD500") %>% 
  left_join(HTHN_500, by = "COD500") %>% 
  left_join(EM_500, by = "COD500") %>% 
  left_join(PPC_500, by = "COD500") %>%
  left_join(PPCN_500, by = "COD500") %>% 
  left_join(IE_500, by = "COD500") %>% 
  left_join(RD_500, by = "COD500") %>% 
  left_join(HSI_500, by = "COD500") %>% 
  left_join(HSIN_500, by = "COD500") %>% 
  left_join(HCL_500, by = "COD500") %>% 
  left_join(HCLN_500, by = "COD500") %>% 
  left_join(PVPT_500, by = "COD500") %>% 
  left_join(PVPTN_500, by = "COD500") %>% 
  left_join(PVRT_500, by = "COD500") %>% 
  left_join(PVRTN_500, by = "COD500") %>% 
  left_join(APE_500, by = "COD500") %>% 
  left_join(TA_500, by = "COD500") %>% 
  left_join(PTS_500, by = "COD500") %>% 
  left_join(PTSN_500, by = "COD500") %>% 
  left_join(DFP_500, by = "COD500") %>% 
  left_join(DFPN_500, by = "COD500") %>% 
  left_join(PSS_500, by = "COD500") %>% 
  left_join(PSSN_500, by = "COD500") %>% 
  left_join(PSC_500, by = "COD500") %>% 
  left_join(PSCN_500, by = "COD500") %>% 
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
st_write(MALLA_500_DMQ, "MALLA_500_DMQ2022.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
MALLA_H3_N8 <- MALLA_H3_N8 %>%
  rename(H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

HTH_N8 <- HTH_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_N8 <- HSI_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_N8 <- HCL_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_N8 <- PVRT_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN_N8 <- HTHN_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN_N8 <- HSIN_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN_N8 <- HCLN_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN_N8 <- PVRTN_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N8_DMQ <-  MALLA_H3_N8 %>% 
  left_join(CPA_N8, by = "H3_N8") %>% 
  left_join(HTH_N8, by = "H3_N8") %>% 
  left_join(HTHN_N8, by = "H3_N8") %>% 
  left_join(EM_N8, by = "H3_N8") %>% 
  left_join(PPC_N8, by = "H3_N8") %>%
  left_join(PPCN_N8, by = "H3_N8") %>% 
  left_join(IE_N8, by = "H3_N8") %>% 
  left_join(RD_N8, by = "H3_N8") %>% 
  left_join(HSI_N8, by = "H3_N8") %>% 
  left_join(HSIN_N8, by = "H3_N8") %>% 
  left_join(HCL_N8, by = "H3_N8") %>% 
  left_join(HCLN_N8, by = "H3_N8") %>% 
  left_join(PVPT_N8, by = "H3_N8") %>% 
  left_join(PVPTN_N8, by = "H3_N8") %>% 
  left_join(PVRT_N8, by = "H3_N8") %>% 
  left_join(PVRTN_N8, by = "H3_N8") %>% 
  left_join(APE_N8, by = "H3_N8") %>% 
  left_join(TA_N8, by = "H3_N8") %>% 
  left_join(PTS_N8, by = "H3_N8") %>% 
  left_join(PTSN_N8, by = "H3_N8") %>% 
  left_join(DFP_N8, by = "H3_N8") %>% 
  left_join(DFPN_N8, by = "H3_N8") %>% 
  left_join(PSS_N8, by = "H3_N8") %>% 
  left_join(PSSN_N8, by = "H3_N8") %>% 
  left_join(PSC_N8, by = "H3_N8") %>% 
  left_join(PSCN_N8, by = "H3_N8") %>% 
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

MALLA_H3_N8_DMQ <- MALLA_H3_N8_DMQ %>%
  rename(H3HASH = H3_N8 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(MALLA_H3_N8_DMQ, "MALLA_H3_N8_DMQ2022.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
MALLA_H3_N9 <- MALLA_H3_N9 %>%
  rename(H3_N9 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

HTH_N9 <- HTH_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI_N9 <- HSI_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL_N9 <- HCL_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT_N9 <- PVRT_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN_N9 <- HTHN_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN_N9 <- HSIN_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN_N9 <- HCLN_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN_N9 <- PVRTN_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N9_DMQ <-  MALLA_H3_N9 %>% 
  left_join(CPA_N9, by = "H3_N9") %>% 
  left_join(HTH_N9, by = "H3_N9") %>% 
  left_join(HTHN_N9, by = "H3_N9") %>% 
  left_join(EM_N9, by = "H3_N9") %>% 
  left_join(PPC_N9, by = "H3_N9") %>%
  left_join(PPCN_N9, by = "H3_N9") %>% 
  left_join(IE_N9, by = "H3_N9") %>% 
  left_join(RD_N9, by = "H3_N9") %>% 
  left_join(HSI_N9, by = "H3_N9") %>% 
  left_join(HSIN_N9, by = "H3_N9") %>% 
  left_join(HCL_N9, by = "H3_N9") %>% 
  left_join(HCLN_N9, by = "H3_N9") %>% 
  left_join(PVPT_N9, by = "H3_N9") %>% 
  left_join(PVPTN_N9, by = "H3_N9") %>% 
  left_join(PVRT_N9, by = "H3_N9") %>% 
  left_join(PVRTN_N9, by = "H3_N9") %>% 
  left_join(APE_N9, by = "H3_N9") %>% 
  left_join(TA_N9, by = "H3_N9") %>% 
  left_join(PTS_N9, by = "H3_N9") %>% 
  left_join(PTSN_N9, by = "H3_N9") %>% 
  left_join(DFP_N9, by = "H3_N9") %>% 
  left_join(DFPN_N9, by = "H3_N9") %>% 
  left_join(PSS_N9, by = "H3_N9") %>% 
  left_join(PSSN_N9, by = "H3_N9") %>% 
  left_join(PSC_N9, by = "H3_N9") %>% 
  left_join(PSCN_N9, by = "H3_N9") %>% 
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

MALLA_H3_N9_DMQ <- MALLA_H3_N9_DMQ %>%
  rename(H3HASH = H3_N9 )    # Reemplazar old_column_name con el nombre actual de la columna

# Guardar 
st_write(MALLA_H3_N9_DMQ, "MALLA_H3_N9_DMQ2022.shp", delete_layer = TRUE)
