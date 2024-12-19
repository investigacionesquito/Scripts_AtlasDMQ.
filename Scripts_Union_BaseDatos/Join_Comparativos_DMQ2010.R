# Script: Union de indicadores comparativos del DMQ en el año 2010. 

# Requerimiento: Antes de correr este código se debera correr los códigos de los indicadores comparativos 2010.   

# Cargar librerías ----------------------------------------------

# Union de número de viviendas,hogares y poblacion total 10 de los 3 cantones

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
HTH10_az <- HTH10_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI10_az <- HSI10_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL10_az <- HCL10_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT10_az <- PVRT10_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN10_az <- HTHN10_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN10_az <- HSIN10_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN10_az <- HCLN10_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN10_az <- PVRTN10_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna

ADM_ZONAL_DMQ <-  ADM_ZONAL %>% 
  left_join(HTH10_az, by = "adm_zonal") %>% 
  left_join(HTHN10_az, by = "adm_zonal") %>% 
  left_join(EM10_az, by = "adm_zonal") %>% 
  left_join(EM10_az_s, by = "adm_zonal") %>% 
  left_join(PPC10_az, by = "adm_zonal") %>%
  left_join(PPC10_az_s, by = "adm_zonal") %>%
  left_join(PPCN10_az, by = "adm_zonal") %>% 
  left_join(IE10_az, by = "adm_zonal") %>% 
  left_join(RD10_az, by = "adm_zonal") %>% 
  left_join(HSI10_az, by = "adm_zonal") %>% 
  left_join(HSIN10_az, by = "adm_zonal") %>% 
  left_join(HCL10_az, by = "adm_zonal") %>% 
  left_join(HCLN10_az, by = "adm_zonal") %>% 
  left_join(PVPT10_az, by = "adm_zonal") %>% 
  left_join(PVPTN10_az, by = "adm_zonal") %>% 
  left_join(PVRT10_az, by = "adm_zonal") %>% 
  left_join(PVRTN10_az, by = "adm_zonal") %>% 
  left_join(APE10_az, by = "adm_zonal") %>% 
  left_join(APE10_az_s, by = "adm_zonal") %>% 
  left_join(TA10_az, by = "adm_zonal") %>% 
  left_join(TA10_az_s, by = "adm_zonal") %>%
  left_join(PTS10_az, by = "adm_zonal") %>% 
  left_join(PTS10_az_s, by = "adm_zonal") %>%
  left_join(PTSN10_az, by = "adm_zonal") %>% 
  left_join(DFP10_az, by = "adm_zonal") %>% 
  left_join(DFP10_az_s, by = "adm_zonal") %>% 
  left_join(DFPN10_az, by = "adm_zonal") %>% 
  left_join(PSS10_az, by = "adm_zonal") %>% 
  left_join(PSS10_az_s, by = "adm_zonal") %>% 
  left_join(PSSN10_az, by = "adm_zonal") %>% 
  left_join(PSC10_az, by = "adm_zonal") %>% 
  left_join(PSC10_az_s, by = "adm_zonal") %>% 
  left_join(PSCN10_az, by = "adm_zonal") %>% 
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
st_write(ADM_ZONAL_DMQ, "ADM_ZONAL_DMQ10.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------
HTH10_pr <- HTH10_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI10_pr <- HSI10_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL10_pr <- HCL10_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT10_pr <- PVRT10_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN10_pr <- HTHN10_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN10_pr <- HSIN10_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN10_pr <- HCLN10_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN10_pr <- PVRTN10_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna

PARROQUIA_DMQ <-  PARROQUIA %>% 
  left_join(HTH10_pr, by = "parroquia") %>% 
  left_join(HTHN10_pr, by = "parroquia") %>% 
  left_join(EM10_pr, by = "parroquia") %>% 
  left_join(EM10_pr_s, by = "parroquia") %>% 
  left_join(PPC10_pr, by = "parroquia") %>%
  left_join(PPC10_pr_s, by = "parroquia") %>%
  left_join(PPCN10_pr, by = "parroquia") %>% 
  left_join(IE10_pr, by = "parroquia") %>% 
  left_join(RD10_pr, by = "parroquia") %>% 
  left_join(HSI10_pr, by = "parroquia") %>% 
  left_join(HSIN10_pr, by = "parroquia") %>% 
  left_join(HCL10_pr, by = "parroquia") %>% 
  left_join(HCLN10_pr, by = "parroquia") %>% 
  left_join(PVPT10_pr, by = "parroquia") %>% 
  left_join(PVPTN10_pr, by = "parroquia") %>% 
  left_join(PVRT10_pr, by = "parroquia") %>% 
  left_join(PVRTN10_pr, by = "parroquia") %>% 
  left_join(APE10_pr, by = "parroquia") %>% 
  left_join(APE10_pr_s, by = "parroquia") %>% 
  left_join(TA10_pr, by = "parroquia") %>% 
  left_join(TA10_pr_s, by = "parroquia") %>%
  left_join(PTS10_pr, by = "parroquia") %>% 
  left_join(PTS10_pr_s, by = "parroquia") %>%
  left_join(PTSN10_pr, by = "parroquia") %>% 
  left_join(DFP10_pr, by = "parroquia") %>% 
  left_join(DFP10_pr_s, by = "parroquia") %>% 
  left_join(DFPN10_pr, by = "parroquia") %>% 
  left_join(PSS10_pr, by = "parroquia") %>% 
  left_join(PSS10_pr_s, by = "parroquia") %>% 
  left_join(PSSN10_pr, by = "parroquia") %>% 
  left_join(PSC10_pr, by = "parroquia") %>% 
  left_join(PSC10_pr_s, by = "parroquia") %>% 
  left_join(PSCN10_pr, by = "parroquia") %>% 
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
st_write(PARROQUIA_DMQ, "PARROQUIA_DMQ10.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------
HTH10_s <- HTH10_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI10_s <- HSI10_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL10_s <- HCL10_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT10_s <- PVRT10_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN10_s <- HTHN10_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN10_s <- HSIN10_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN10_s <- HCLN10_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN10_s <- PVRTN10_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna

SECTOR_DMQ <-  SECTOR %>% 
  left_join(HTH10_s, by = "Sector_DMQ") %>% 
  left_join(HTHN10_s, by = "Sector_DMQ") %>% 
  left_join(EM10_s, by = "Sector_DMQ") %>% 
  left_join(EM10_s_s, by = "Sector_DMQ") %>% 
  left_join(PPC10_s, by = "Sector_DMQ") %>%
  left_join(PPC10_s_s, by = "Sector_DMQ") %>%
  left_join(PPCN10_s, by = "Sector_DMQ") %>% 
  left_join(IE10_s, by = "Sector_DMQ") %>% 
  left_join(RD10_s, by = "Sector_DMQ") %>% 
  left_join(HSI10_s, by = "Sector_DMQ") %>% 
  left_join(HSIN10_s, by = "Sector_DMQ") %>% 
  left_join(HCL10_s, by = "Sector_DMQ") %>% 
  left_join(HCLN10_s, by = "Sector_DMQ") %>% 
  left_join(PVPT10_s, by = "Sector_DMQ") %>% 
  left_join(PVPTN10_s, by = "Sector_DMQ") %>% 
  left_join(PVRT10_s, by = "Sector_DMQ") %>% 
  left_join(PVRTN10_s, by = "Sector_DMQ") %>% 
  left_join(APE10_s, by = "Sector_DMQ") %>% 
  left_join(APE10_s_s, by = "Sector_DMQ") %>% 
  left_join(TA10_s, by = "Sector_DMQ") %>% 
  left_join(TA10_s_s, by = "Sector_DMQ") %>%
  left_join(PTS10_s, by = "Sector_DMQ") %>% 
  left_join(PTS10_s_s, by = "Sector_DMQ") %>%
  left_join(PTSN10_s, by = "Sector_DMQ") %>% 
  left_join(DFP10_s, by = "Sector_DMQ") %>% 
  left_join(DFP10_s_s, by = "Sector_DMQ") %>% 
  left_join(DFPN10_s, by = "Sector_DMQ") %>% 
  left_join(PSS10_s, by = "Sector_DMQ") %>% 
  left_join(PSS10_s_s, by = "Sector_DMQ") %>% 
  left_join(PSSN10_s, by = "Sector_DMQ") %>% 
  left_join(PSC10_s, by = "Sector_DMQ") %>% 
  left_join(PSC10_s_s, by = "Sector_DMQ") %>% 
  left_join(PSCN10_s, by = "Sector_DMQ") %>% 
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
st_write(SECTOR_DMQ, "SECTOR_DMQ10.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------
HTH10_1000 <- HTH10_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI10_1000 <- HSI10_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL10_1000 <- HCL10_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT10_1000 <- PVRT10_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN10_1000 <- HTHN10_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN10_1000 <- HSIN10_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN10_1000 <- HCLN10_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN10_1000 <- PVRTN10_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_1000_DMQ <-  MALLA_1000 %>% 
  left_join(HTH10_1000, by = "COD1000") %>% 
  left_join(HTHN10_1000, by = "COD1000") %>% 
  left_join(EM10_1000, by = "COD1000") %>% 
  left_join(EM10_1000_s, by = "COD1000") %>% 
  left_join(PPC10_1000, by = "COD1000") %>%
  left_join(PPC10_1000_s, by = "COD1000") %>%
  left_join(PPCN10_1000, by = "COD1000") %>% 
  left_join(IE10_1000, by = "COD1000") %>% 
  left_join(RD10_1000, by = "COD1000") %>% 
  left_join(HSI10_1000, by = "COD1000") %>% 
  left_join(HSIN10_1000, by = "COD1000") %>% 
  left_join(HCL10_1000, by = "COD1000") %>% 
  left_join(HCLN10_1000, by = "COD1000") %>% 
  left_join(PVPT10_1000, by = "COD1000") %>% 
  left_join(PVPTN10_1000, by = "COD1000") %>% 
  left_join(PVRT10_1000, by = "COD1000") %>% 
  left_join(PVRTN10_1000, by = "COD1000") %>% 
  left_join(APE10_1000, by = "COD1000") %>% 
  left_join(APE10_1000_s, by = "COD1000") %>% 
  left_join(TA10_1000, by = "COD1000") %>% 
  left_join(TA10_1000_s, by = "COD1000") %>%
  left_join(PTS10_1000, by = "COD1000") %>% 
  left_join(PTS10_1000_s, by = "COD1000") %>%
  left_join(PTSN10_1000, by = "COD1000") %>% 
  left_join(DFP10_1000, by = "COD1000") %>% 
  left_join(DFP10_1000_s, by = "COD1000") %>% 
  left_join(DFPN10_1000, by = "COD1000") %>% 
  left_join(PSS10_1000, by = "COD1000") %>% 
  left_join(PSS10_1000_s, by = "COD1000") %>% 
  left_join(PSSN10_1000, by = "COD1000") %>% 
  left_join(PSC10_1000, by = "COD1000") %>% 
  left_join(PSC10_1000_s, by = "COD1000") %>% 
  left_join(PSCN10_1000, by = "COD1000") %>% 
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
st_write(MALLA_1000_DMQ, "MALLA_1000_DMQ10.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------
HTH10_500 <- HTH10_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI10_500 <- HSI10_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL10_500 <- HCL10_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT10_500 <- PVRT10_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN10_500 <- HTHN10_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN10_500 <- HSIN10_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN10_500 <- HCLN10_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN10_500 <- PVRTN10_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_500_DMQ <-  MALLA_500 %>% 
  left_join(HTH10_500, by = "COD500") %>% 
  left_join(HTHN10_500, by = "COD500") %>% 
  left_join(EM10_500, by = "COD500") %>% 
  left_join(EM10_500_s, by = "COD500") %>% 
  left_join(PPC10_500, by = "COD500") %>%
  left_join(PPC10_500_s, by = "COD500") %>%
  left_join(PPCN10_500, by = "COD500") %>% 
  left_join(IE10_500, by = "COD500") %>% 
  left_join(RD10_500, by = "COD500") %>% 
  left_join(HSI10_500, by = "COD500") %>% 
  left_join(HSIN10_500, by = "COD500") %>% 
  left_join(HCL10_500, by = "COD500") %>% 
  left_join(HCLN10_500, by = "COD500") %>% 
  left_join(PVPT10_500, by = "COD500") %>% 
  left_join(PVPTN10_500, by = "COD500") %>% 
  left_join(PVRT10_500, by = "COD500") %>% 
  left_join(PVRTN10_500, by = "COD500") %>% 
  left_join(APE10_500, by = "COD500") %>% 
  left_join(APE10_500_s, by = "COD500") %>% 
  left_join(TA10_500, by = "COD500") %>% 
  left_join(TA10_500_s, by = "COD500") %>%
  left_join(PTS10_500, by = "COD500") %>% 
  left_join(PTS10_500_s, by = "COD500") %>%
  left_join(PTSN10_500, by = "COD500") %>% 
  left_join(DFP10_500, by = "COD500") %>% 
  left_join(DFP10_500_s, by = "COD500") %>% 
  left_join(DFPN10_500, by = "COD500") %>% 
  left_join(PSS10_500, by = "COD500") %>% 
  left_join(PSS10_500_s, by = "COD500") %>% 
  left_join(PSSN10_500, by = "COD500") %>% 
  left_join(PSC10_500, by = "COD500") %>% 
  left_join(PSC10_500_s, by = "COD500") %>% 
  left_join(PSCN10_500, by = "COD500") %>% 
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
st_write(MALLA_500_DMQ, "MALLA_500_DMQ10.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------
MALLA_H3_N8 <- MALLA_H3_N8 %>%
  rename(H3_N8 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

HTH10_N8 <- HTH10_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI10_N8 <- HSI10_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL10_N8 <- HCL10_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT10_N8 <- PVRT10_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN10_N8 <- HTHN10_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN10_N8 <- HSIN10_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN10_N8 <- HCLN10_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN10_N8 <- PVRTN10_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N8_DMQ <-  MALLA_H3_N8 %>% 
  left_join(HTH10_N8, by = "H3_N8") %>% 
  left_join(HTHN10_N8, by = "H3_N8") %>% 
  left_join(EM10_N8, by = "H3_N8") %>% 
  left_join(EM10_N8_s, by = "H3_N8") %>% 
  left_join(PPC10_N8, by = "H3_N8") %>%
  left_join(PPC10_N8_s, by = "H3_N8") %>%
  left_join(PPCN10_N8, by = "H3_N8") %>% 
  left_join(IE10_N8, by = "H3_N8") %>% 
  left_join(RD10_N8, by = "H3_N8") %>% 
  left_join(HSI10_N8, by = "H3_N8") %>% 
  left_join(HSIN10_N8, by = "H3_N8") %>% 
  left_join(HCL10_N8, by = "H3_N8") %>% 
  left_join(HCLN10_N8, by = "H3_N8") %>% 
  left_join(PVPT10_N8, by = "H3_N8") %>% 
  left_join(PVPTN10_N8, by = "H3_N8") %>% 
  left_join(PVRT10_N8, by = "H3_N8") %>% 
  left_join(PVRTN10_N8, by = "H3_N8") %>% 
  left_join(APE10_N8, by = "H3_N8") %>% 
  left_join(APE10_N8_s, by = "H3_N8") %>% 
  left_join(TA10_N8, by = "H3_N8") %>% 
  left_join(TA10_N8_s, by = "H3_N8") %>%
  left_join(PTS10_N8, by = "H3_N8") %>% 
  left_join(PTS10_N8_s, by = "H3_N8") %>%
  left_join(PTSN10_N8, by = "H3_N8") %>% 
  left_join(DFP10_N8, by = "H3_N8") %>% 
  left_join(DFP10_N8_s, by = "H3_N8") %>% 
  left_join(DFPN10_N8, by = "H3_N8") %>% 
  left_join(PSS10_N8, by = "H3_N8") %>% 
  left_join(PSS10_N8_s, by = "H3_N8") %>% 
  left_join(PSSN10_N8, by = "H3_N8") %>% 
  left_join(PSC10_N8, by = "H3_N8") %>% 
  left_join(PSC10_N8_s, by = "H3_N8") %>% 
  left_join(PSCN10_N8, by = "H3_N8") %>% 
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
st_write(MALLA_H3_N8_DMQ, "MALLA_H3_N8_DMQ10.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------
MALLA_H3_N9 <- MALLA_H3_N9 %>%
  rename(H3_N9 = H3HASH)    # Reemplazar old_column_name con el nombre actual de la columna

HTH10_N9 <- HTH10_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSI10_N9 <- HSI10_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCL10_N9 <- HCL10_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRT10_N9 <- PVRT10_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna

HTHN10_N9 <- HTHN10_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HSIN10_N9 <- HSIN10_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HCLN10_N9 <- HCLN10_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
PVRTN10_N9 <- PVRTN10_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna

MALLA_H3_N9_DMQ <-  MALLA_H3_N9 %>% 
  left_join(HTH10_N9, by = "H3_N9") %>% 
  left_join(HTHN10_N9, by = "H3_N9") %>% 
  left_join(EM10_N9, by = "H3_N9") %>% 
  left_join(EM10_N9_s, by = "H3_N9") %>% 
  left_join(PPC10_N9, by = "H3_N9") %>%
  left_join(PPC10_N9_s, by = "H3_N9") %>%
  left_join(PPCN10_N9, by = "H3_N9") %>% 
  left_join(IE10_N9, by = "H3_N9") %>% 
  left_join(RD10_N9, by = "H3_N9") %>% 
  left_join(HSI10_N9, by = "H3_N9") %>% 
  left_join(HSIN10_N9, by = "H3_N9") %>% 
  left_join(HCL10_N9, by = "H3_N9") %>% 
  left_join(HCLN10_N9, by = "H3_N9") %>% 
  left_join(PVPT10_N9, by = "H3_N9") %>% 
  left_join(PVPTN10_N9, by = "H3_N9") %>% 
  left_join(PVRT10_N9, by = "H3_N9") %>% 
  left_join(PVRTN10_N9, by = "H3_N9") %>% 
  left_join(APE10_N9, by = "H3_N9") %>% 
  left_join(APE10_N9_s, by = "H3_N9") %>% 
  left_join(TA10_N9, by = "H3_N9") %>% 
  left_join(TA10_N9_s, by = "H3_N9") %>%
  left_join(PTS10_N9, by = "H3_N9") %>% 
  left_join(PTS10_N9_s, by = "H3_N9") %>%
  left_join(PTSN10_N9, by = "H3_N9") %>% 
  left_join(DFP10_N9, by = "H3_N9") %>% 
  left_join(DFP10_N9_s, by = "H3_N9") %>% 
  left_join(DFPN10_N9, by = "H3_N9") %>% 
  left_join(PSS10_N9, by = "H3_N9") %>% 
  left_join(PSS10_N9_s, by = "H3_N9") %>% 
  left_join(PSSN10_N9, by = "H3_N9") %>% 
  left_join(PSC10_N9, by = "H3_N9") %>% 
  left_join(PSC10_N9_s, by = "H3_N9") %>% 
  left_join(PSCN10_N9, by = "H3_N9") %>% 
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
st_write(MALLA_H3_N9_DMQ, "MALLA_H3_N9_DMQ10.shp", delete_layer = TRUE)
