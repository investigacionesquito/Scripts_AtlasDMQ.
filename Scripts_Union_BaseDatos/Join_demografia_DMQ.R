# Script: Union de indicadores de la dimensión de Demografía del DMQ. 

# Requerimiento: Antes de correr este código se debera correr los códigos de los indicadores de la dimensión de demografía.   

# Cargar librerías ----------------------------------------------

pacman::p_load(raster, rgdal, rgeos, ggrepel, stringr, sf, ggspatial, exactextractr, tidyverse,xlsx, 
               maptools,scales,dplyr,writexl,dplyr)
# LEER SHAPES --------------------------------------------------------------

# SHAPES BASE 
ADM_ZONAL <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\ADM_ZONAL_DMQ.shp")
PARROQUIA <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\PARROQUIA_DMQ.shp")
SECTOR <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\SECTOR_DMQ.shp")
MALLA_1000 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_1000_DMQ.shp")
MALLA_500 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_500_DMQ.shp")
MALLA_H3_N8 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N8_DMQ.shp")
MALLA_H3_N9 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\CAPAS_DMQ\\MALLA_H3_N9_DMQ.shp")

# INDICADORES RADATAM
ADM_ZONAL_RD <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Demografia_RADATAM\\ADM_ZONAL_RD.shp")
PARROQUIA_RD <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Demografia_RADATAM\\PARROQUIA_RD.shp") %>% 
  rename(IF = IF_)
SECTOR_RD <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Demografia_RADATAM\\SECTOR_RD.shp")
MALLA_1000_RD <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Demografia_RADATAM\\MALLA_1000_RD.shp")
MALLA_500_RD <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Demografia_RADATAM\\MALLA_500_RD.shp")
MALLA_H3_N8_RD <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Demografia_RADATAM\\MALLA_H3_N8_RD.shp")
MALLA_H3_N9_RD <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Demografia_RADATAM\\MALLA_H3_N9_RD.shp")

# EXTRAER INDICADORES DE RADATAM EN OTRAS TABLAS ---------------------------------------------

columnas_DR <- c("IDPP","IF","H1449","H4549","PHM14_49","PHM45_49","NFPS1","NFPS2","MMF" )

columnas_DMD <- c("PPL1","PPL2","PPL3","PPL4", "PPL5", "PPL6", "PIMP1S1_1", "PIMP1S1_2", "PIMP1S1_3", "PIMP1S1_4", "PIMP1S1_5", "PIMP1S1_6", 
                 "PIMP1S1_7", "PIMP1S1_8", "PIMP1S1_9", "PIMP1S1_10", "PIMP1S1_11", "PIMP1S1_12", "PIMP1S1_13", "PIMP1S1_14", "PIMP1S1_15",
                 "PIMP1S1_16", "PIMP1S1_17", "PIMP1S1_18", "PIMP1S1_19", "PIMP1S1_20", "PIMP1S1_21", "PIMP1S1_22", "PIMP1S1_23", "PIMP1S1_24",
                 "PIMP1S1_25", "PIMP1S1_26", "PIMP2S1_1", "PIMP2S1_2", "PIMP2S1_3", "PIMP2S1_4", "PIMP2S1_5", "PIMP2S1_6", "PIMP2S1_7", 
                 "PIMP2S1_8", "PIMP2S1_9", "PIMP2S1_10", "PIMP2S1_11", "PIMP2S1_12", "PIMP2S1_13", "PIMP2S1_14", "PIMP2S1_15", "PIMP2S1_16",
                 "PIMP2S1_17", "PIMP2S1_18", "PIMP2S1_19", "PIMP2S1_20", "PIMP2S1_21", "PIMP2S1_22", "PIMP2S1_23", "PIMP2S1_24", "PIMP2S1_25",
                 "PIMP2S1_26", "PIMP1S2_1", "PIMP1S2_2", "PIMP1S2_3", "PIMP1S2_4", "PIMP1S2_5", "PIMP1S2_6", "PIMP1S2_7", "PIMP1S2_8", 
                 "PIMP1S2_9", "PIMP1S2_10", "PIMP1S2_11", "PIMP1S2_12", "PIMP1S2_13", "PIMP1S2_14", "PIMP1S2_15", "PIMP1S2_16", "PIMP1S2_17",
                 "PIMP1S2_18", "PIMP1S2_19", "PIMP1S2_20", "PIMP1S2_21", "PIMP1S2_22", "PIMP1S2_23", "PIMP1S2_24", "PIMP1S2_25", "PIMP1S2_26",
                 "PIMP2S2_1", "PIMP2S2_2", "PIMP2S2_3", "PIMP2S2_4", "PIMP2S2_5", "PIMP2S2_6", "PIMP2S2_7", "PIMP2S2_8", "PIMP2S2_9", 
                 "PIMP2S2_10", "PIMP2S2_11", "PIMP2S2_12", "PIMP2S2_13", "PIMP2S2_14", "PIMP2S2_15", "PIMP2S2_16", "PIMP2S2_17", "PIMP2S2_18",
                 "PIMP2S2_19", "PIMP2S2_20", "PIMP2S2_21", "PIMP2S2_22", "PIMP2S2_23", "PIMP2S2_24", "PIMP2S2_25", "PIMP2S2_26")

columnas_DPA <- c("CGE1S1_1","CGE1S2_1","CGE1S1_2",
                 "CGE1S2_2", "CGE1S1_3", "CGE1S2_3", "CGE1S1_4", "CGE1S2_4", "CGE1S1_5", "CGE1S2_5", "CGE1S1_6", "CGE1S2_6", 
                 "CGE1S1_7", "CGE1S2_7", "CGE1S1_8", "CGE1S2_8", "CGE2S1_1", "CGE2S2_1", "CGE2S1_2", "CGE2S2_2", "CGE2S1_3", 
                 "CGE2S2_3", "CGE2S1_4", "CGE2S2_4", "CGE2S1_5", "CGE2S2_5", "CGE2S1_6", "CGE2S2_6", "CGE2S1_7", "CGE2S2_7", 
                 "CGE2S1_8", "CGE2S2_8", "CGE2S1_9", "CGE2S2_9", "CGE2S1_10", "CGE2S2_10", "CGE2S1_11", "CGE2S2_11", "CGE2S1_12", 
                 "CGE2S2_12", "CGE2S1_13", "CGE2S2_13", "CGE2S1_14", "CGE2S2_14", "CGE2S1_15", "CGE2S2_15", "CGE2S1_16", "CGE2S2_16", 
                 "CGE2S1_17", "CGE2S2_17", "CGE2S1_18", "CGE2S2_18", "CGE2S1_19", "CGE2S2_19", "CGE2S1_20", "CGE2S2_20", "CGE2S1_21", 
                 "CGE2S2_21", "CGE2S1_22", "CGE2S2_22", "CGE2S1_23", "CGE2S2_23", "CGE2S1_24", "CGE2S2_24", "CGE2S1_25", "CGE2S2_25", 
                 "CGE3S1_1", "CGE3S2_1", "CGE3S1_2", "CGE3S2_2", "CGE3S1_3", "CGE3S2_3", "CGE3S1_4", "CGE3S2_4", "CGE3S1_5", 
                 "CGE3S2_5", "CGE3S1_6", "CGE3S2_6", "CPA", "CPAS1", "CPAS2")

ADM_ZONAL_DMD <- ADM_ZONAL_RD[, c("adm_zonal",columnas_DMD)] %>% st_drop_geometry()
PARROQUIA_DMD <- PARROQUIA_RD[, c("parroquia",columnas_DMD)] %>% st_drop_geometry()
SECTOR_DMD <- SECTOR_RD[, c("Sector_DMQ",columnas_DMD)] %>% st_drop_geometry()
MALLA_1000_DMD <- MALLA_1000_RD[, c("COD1000",columnas_DMD)] %>% st_drop_geometry()
MALLA_500_DMD <- MALLA_500_RD[, c("COD500",columnas_DMD)] %>% st_drop_geometry()
MALLA_H3_N8_DMD <- MALLA_H3_N8_RD[, c("H3HASH",columnas_DMD)] %>% st_drop_geometry()
MALLA_H3_N9_DMD <- MALLA_H3_N9_RD[, c("H3HASH",columnas_DMD)] %>% st_drop_geometry()

ADM_ZONAL_DPA <- ADM_ZONAL_RD[, c("adm_zonal",columnas_DPA)] %>% st_drop_geometry()
PARROQUIA_DPA <- PARROQUIA_RD[, c("parroquia",columnas_DPA)] %>% st_drop_geometry()
SECTOR_DPA <- SECTOR_RD[, c("Sector_DMQ",columnas_DPA)] %>% st_drop_geometry()
MALLA_1000_DPA <- MALLA_1000_RD[, c("COD1000",columnas_DPA)] %>% st_drop_geometry()
MALLA_500_DPA <- MALLA_500_RD[, c("COD500",columnas_DPA)] %>% st_drop_geometry()
MALLA_H3_N8_DPA <- MALLA_H3_N8_RD[, c("H3HASH",columnas_DPA)] %>% st_drop_geometry()
MALLA_H3_N9_DPA <- MALLA_H3_N9_RD[, c("H3HASH",columnas_DPA)] %>% st_drop_geometry()

ADM_ZONAL_DR <- ADM_ZONAL_RD[, c("adm_zonal",columnas_DR)] %>%  st_drop_geometry()
PARROQUIA_DR <- PARROQUIA_RD[, c("parroquia",columnas_DR)] %>% st_drop_geometry()
SECTOR_DR <- SECTOR_RD[, c("Sector_DMQ",columnas_DR)] %>% st_drop_geometry()
MALLA_1000_DR <- MALLA_1000_RD[, c("COD1000",columnas_DR)] %>% st_drop_geometry()
MALLA_500_DR <- MALLA_500_RD[, c("COD500",columnas_DR)] %>% st_drop_geometry()
MALLA_H3_N8_DR <- MALLA_H3_N8_RD[, c("H3HASH",columnas_DR)] %>% st_drop_geometry()
MALLA_H3_N9_DR <- MALLA_H3_N9_RD[, c("H3HASH",columnas_DR)] %>% st_drop_geometry()

# ADMINISTRACION ZONAL -----------------------------------------------------------

# Renombro columnas 

HTH_az <- HTH_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTR_az <- HTR_az %>%
  rename(adm_zonal = adm_zonal.x)  # Reemplazar old_column_name con el nombre actual de la columna

# Creo una tabla de union para limpiar los datos 

DAA_ADM_ZONAL <-  PPC_az %>% 
  left_join(PPC_az_e, by = "adm_zonal") %>% 
  left_join(PPC_az_s, by = "adm_zonal") %>%   
  mutate(across(everything(), ~replace_na(. , 0)))  
DAA_ADM_ZONAL <- ADM_ZONAL %>% 
  left_join(DAA_ADM_ZONAL, by = "adm_zonal")

DAB_ADM_ZONAL <- PPN_az %>% 
  left_join(PPN_az_e, by = "adm_zonal") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DAB_ADM_ZONAL <- ADM_ZONAL %>% 
  left_join(DAB_ADM_ZONAL, by = "adm_zonal")

DAC_ADM_ZONAL <- PPN_az_s %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0))) # Se agrego esta base 
DAC_ADM_ZONAL <- ADM_ZONAL %>% 
  left_join(DAC_ADM_ZONAL, by = "adm_zonal")

DE_ADM_ZONAL <- EM_az %>%
  left_join(EM_az_s, by = "adm_zonal") %>% 
  left_join(EMM_az, by = "adm_zonal") %>% 
  left_join(EMM_az_s, by = "adm_zonal") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DE_ADM_ZONAL <- ADM_ZONAL %>% 
  left_join(DE_ADM_ZONAL, by = "adm_zonal")

DH_ADM_ZONAL <- HPM_az %>% 
  left_join(HTH_az, by = "adm_zonal") %>% 
  left_join(HTR_az, by = "adm_zonal") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DH_ADM_ZONAL <- ADM_ZONAL %>% 
  left_join(DH_ADM_ZONAL, by = "adm_zonal")

DMA_ADM_ZONAL <- PIM_az  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMA_ADM_ZONAL <- ADM_ZONAL %>% 
  left_join(DMA_ADM_ZONAL, by = "adm_zonal")

DMB_ADM_ZONAL <- PIM_az_s %>%
  mutate(across(everything(), ~replace_na(. , 0))) %>%
  dplyr::select(where(~ any(. != 0)))
DMB_ADM_ZONAL <- ADM_ZONAL %>% 
  left_join(DMB_ADM_ZONAL, by = "adm_zonal")

DMC_ADM_ZONAL <- PEM_az %>%  # Se debe quitar de esta base 
  left_join(PRN_az, by = "adm_zonal") %>% 
  left_join(PRN_az_a, by = "adm_zonal") %>% 
  left_join(PRN_az_s, by = "adm_zonal") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DMC_ADM_ZONAL <- ADM_ZONAL %>% 
  left_join(DMC_ADM_ZONAL, by = "adm_zonal")

DMD_ADM_ZONAL <-  ADM_ZONAL %>% 
  left_join(ADM_ZONAL_DMD, by = "adm_zonal") 

DPA_ADM_ZONAL <- PGE_az %>%
  left_join(PGE_az_s, by = "adm_zonal") %>%
  left_join(PPA_az, by = "adm_zonal") %>%
  left_join(PPA_az_s, by = "adm_zonal") %>%
  left_join(PPE_az, by = "adm_zonal") %>%
  left_join(PPE_az_s, by = "adm_zonal") %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DPA_ADM_ZONAL <- ADM_ZONAL %>%
  left_join(DPA_ADM_ZONAL, by = "adm_zonal") %>%
  left_join(ADM_ZONAL_DPA, by = "adm_zonal") 

DR_ADM_ZONAL <- IE_az %>%
  left_join(IJ_az, by = "adm_zonal") %>%
  left_join(RD_az, by = "adm_zonal") %>%
  left_join(RDAM_az, by = "adm_zonal") %>%
  left_join(RDJ_az, by = "adm_zonal") %>%
  left_join(RHM_az, by = "adm_zonal")  %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DR_ADM_ZONAL <- ADM_ZONAL %>%
  left_join(DR_ADM_ZONAL, by = "adm_zonal") %>% 
  left_join(ADM_ZONAL_DR, by = "adm_zonal")

# Guardar en un archivo Excel

st_write(DAA_ADM_ZONAL,"DAA_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DAB_ADM_ZONAL,"DAB_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DAC_ADM_ZONAL,"DAC_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DE_ADM_ZONAL,"DE_ADM_ZONALL.shp", delete_layer = TRUE)

st_write(DH_ADM_ZONAL,"DH_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DMA_ADM_ZONAL,"DMA_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DMB_ADM_ZONAL,"DMB_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DMC_ADM_ZONAL,"DMC_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DMD_ADM_ZONAL,"DMD_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DPA_ADM_ZONAL,"DPA_ADM_ZONAL.shp", delete_layer = TRUE)

st_write(DR_ADM_ZONAL,"DR_ADM_ZONAL.shp", delete_layer = TRUE)

# PARROQUIA -----------------------------------------------------------------

# Renombro columnas 

HTH_pr <- HTH_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTR_pr <- HTR_pr %>%
  rename(parroquia = parroquia.x)  # Reemplazar old_column_name con el nombre actual de la columna

# Creo una tabla de union para limpiar los datos 

DAA_PARROQUIA<-  PPC_pr %>% 
  left_join(PPC_pr_e, by = "parroquia") %>% 
  left_join(PPC_pr_s, by = "parroquia") %>%   
  mutate(across(everything(), ~replace_na(. , 0)))  
DAA_PARROQUIA <- PARROQUIA %>% 
  left_join(DAA_PARROQUIA, by = "parroquia")

DAB_PARROQUIA <- PPN_pr %>% 
  left_join(PPN_pr_e, by = "parroquia") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DAB_PARROQUIA <- PARROQUIA %>% 
  left_join(DAB_PARROQUIA, by = "parroquia")

DAC_PARROQUIA <- PPN_pr_s %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DAC_PARROQUIA <- PARROQUIA %>% 
  left_join(DAC_PARROQUIA, by = "parroquia")

DE_PARROQUIA <- EM_pr %>%
  left_join(EM_pr_s, by = "parroquia") %>% 
  left_join(EMM_pr, by = "parroquia") %>% 
  left_join(EMM_pr_s, by = "parroquia") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DE_PARROQUIA <- PARROQUIA %>% 
  left_join(DE_PARROQUIA, by = "parroquia")

DH_PARROQUIA <- HPM_pr %>% 
  left_join(HTH_pr, by = "parroquia") %>% 
  left_join(HTR_pr, by = "parroquia") %>% 
  mutate(across(everything(), ~replace_na(. , 0)))
DH_PARROQUIA <- PARROQUIA %>% 
  left_join(DH_PARROQUIA, by = "parroquia")

DMA_PARROQUIA <- PIM_pr  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMA_PARROQUIA <- PARROQUIA %>% 
  left_join(DMA_PARROQUIA, by = "parroquia")

DMB_PARROQUIA <- PIM_pr_s  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMB_PARROQUIA <- PARROQUIA %>% 
  left_join(DMB_PARROQUIA, by = "parroquia")

DMC_PARROQUIA <- PEM_pr %>% #
  left_join(PRN_pr, by = "parroquia") %>% 
  left_join(PRN_pr_a, by = "parroquia") %>% 
  left_join(PRN_pr_s, by = "parroquia") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DMC_PARROQUIA <- PARROQUIA %>% 
  left_join(DMC_PARROQUIA, by = "parroquia")

DMD_PARROQUIA <-  PARROQUIA %>% 
  left_join(PARROQUIA_DMD, by = "parroquia") 

DPA_PARROQUIA <- PGE_pr %>%
  left_join(PGE_pr_s, by = "parroquia") %>%
  left_join(PPA_pr, by = "parroquia") %>%
  left_join(PPA_pr_s, by = "parroquia") %>%
  left_join(PPE_pr, by = "parroquia") %>%
  left_join(PPE_pr_s, by = "parroquia") %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DPA_PARROQUIA <- PARROQUIA %>%
  left_join(DPA_PARROQUIA, by = "parroquia") %>% 
  left_join(PARROQUIA_DPA, by = "parroquia") 

DR_PARROQUIA <- IE_pr %>%
  left_join(IJ_pr, by = "parroquia") %>%
  left_join(RD_pr, by = "parroquia") %>%
  left_join(RDAM_pr, by = "parroquia") %>%
  left_join(RDJ_pr, by = "parroquia") %>%
  left_join(RHM_pr, by = "parroquia")  %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DR_PARROQUIA <- PARROQUIA %>%
  left_join(DR_PARROQUIA, by = "parroquia") %>%
  left_join(PARROQUIA_DR, by = "parroquia")

# Guardar en un archivo Excel

st_write(DAA_PARROQUIA,"DAA_PARROQUIAL.shp", delete_layer = TRUE)

st_write(DAB_PARROQUIA,"DAB_PARROQUIA.shp", delete_layer = TRUE)

st_write(DAC_PARROQUIA,"DAC_PARROQUIA.shp", delete_layer = TRUE)

st_write(DE_PARROQUIA, "DE_PARROQUIA.shp", delete_layer = TRUE)

st_write(DH_PARROQUIA,"DH_PARROQUIA.shp", delete_layer = TRUE)

st_write(DMA_PARROQUIA,"DMA_PARROQUIA.shp", delete_layer = TRUE)

st_write(DMB_PARROQUIA,"DMB_PARROQUIA.shp", delete_layer = TRUE)

st_write(DMC_PARROQUIA,"DMC_PARROQUIA.shp", delete_layer = TRUE)

st_write(DMD_PARROQUIA,"DMD_PARROQUIA.shp", delete_layer = TRUE)

st_write(DPA_PARROQUIA,"DPA_PARROQUIA.shp", delete_layer = TRUE)

st_write(DR_PARROQUIA,"DR_PARROQUIA.shp", delete_layer = TRUE)

# SECTOR --------------------------------------------------------

# Renombro columnas 

HTH_s <- HTH_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTR_s <- HTR_s %>%
  rename(Sector_DMQ = Sector_DMQ.x)  # Reemplazar old_column_name con el nombre actual de la columna

# Creo una tabla de union para limpiar los datos 

DAA_SECTOR <-  PPC_s %>% 
  left_join(PPC_s_e, by = "Sector_DMQ") %>% 
  left_join(PPC_s_s, by = "Sector_DMQ") %>%   
  mutate(across(everything(), ~replace_na(. , 0)))  
DAA_SECTOR <- SECTOR %>% 
  left_join(DAA_SECTOR, by = "Sector_DMQ")

DAB_SECTOR <- PPN_s %>% 
  left_join(PPN_s_e, by = "Sector_DMQ") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DAB_SECTOR <- SECTOR %>% 
  left_join(DAB_SECTOR, by = "Sector_DMQ")

DAC_SECTOR <- PPN_s_s %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DAC_SECTOR <- SECTOR %>% 
  left_join(DAC_SECTOR, by = "Sector_DMQ")

DE_SECTOR <- EM_s %>%
  left_join(EM_s_s, by = "Sector_DMQ") %>% 
  left_join(EMM_s, by = "Sector_DMQ") %>% 
  left_join(EMM_s_s, by = "Sector_DMQ") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DE_SECTOR <- SECTOR %>% 
  left_join(DE_SECTOR, by = "Sector_DMQ")

DH_SECTOR <- HPM_s %>% 
  left_join(HTH_s, by = "Sector_DMQ") %>% 
  left_join(HTR_s, by = "Sector_DMQ") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DH_SECTOR <- SECTOR %>% 
  left_join(DH_SECTOR, by = "Sector_DMQ")

DMA_SECTOR <- PIM_s  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMA_SECTOR <- SECTOR %>% 
  left_join(DMA_SECTOR, by = "Sector_DMQ")

DMB_SECTOR <- PIM_s_s  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMB_SECTOR <- SECTOR %>% 
  left_join(DMB_SECTOR, by = "Sector_DMQ")

DMC_SECTOR <- PEM_s %>% 
  left_join(PRN_s, by = "Sector_DMQ") %>% 
  left_join(PRN_s_a, by = "Sector_DMQ") %>% 
  left_join(PRN_s_s, by = "Sector_DMQ") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DMC_SECTOR <- SECTOR %>% 
  left_join(DMC_SECTOR, by = "Sector_DMQ")

DMD_SECTOR <-  SECTOR %>% 
  left_join(SECTOR_DMD, by = "Sector_DMQ") 

DPA_SECTOR <- PGE_s %>%
  left_join(PGE_s_s, by = "Sector_DMQ") %>%
  left_join(PPA_s, by = "Sector_DMQ") %>%
  left_join(PPA_s_s, by = "Sector_DMQ") %>%
  left_join(PPE_s, by = "Sector_DMQ") %>%
  left_join(PPE_s_s, by = "Sector_DMQ") %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DPA_SECTOR <- SECTOR %>%
  left_join(DPA_SECTOR, by = "Sector_DMQ") %>% 
  left_join(SECTOR_DPA, by = "Sector_DMQ")

DR_SECTOR <- IE_s %>%
  left_join(IJ_s, by = "Sector_DMQ") %>%
  left_join(RD_s, by = "Sector_DMQ") %>%
  left_join(RDAM_s, by = "Sector_DMQ") %>%
  left_join(RDJ_s, by = "Sector_DMQ") %>%
  left_join(RHM_s, by = "Sector_DMQ")  %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DR_SECTOR <- SECTOR %>%
  left_join(DR_SECTOR, by = "Sector_DMQ") %>% 
  left_join(SECTOR_DR, by = "Sector_DMQ")

# Guardar en un archivo Excel

st_write(DAA_SECTOR,"DAA_SECTOR.shp", delete_layer = TRUE)

st_write(DAB_SECTOR,"DAB_SECTOR.shp", delete_layer = TRUE)

st_write(DAC_SECTOR,"DAC_SECTOR.shp", delete_layer = TRUE)

st_write(DE_SECTOR, "DE_SECTOR.shp", delete_layer = TRUE)

st_write(DH_SECTOR,"DH_SECTOR.shp", delete_layer = TRUE)

st_write(DMA_SECTOR,"DMA_SECTOR.shp", delete_layer = TRUE)

st_write(DMB_SECTOR,"DMB_SECTOR.shp", delete_layer = TRUE)

st_write(DMC_SECTOR,"DMC_SECTOR.shp", delete_layer = TRUE)

st_write(DMD_SECTOR,"DMD_SECTOR.shp", delete_layer = TRUE)

st_write(DPA_SECTOR,"DPA_SECTOR.shp", delete_layer = TRUE)

st_write(DR_SECTOR,"DR_SECTOR.shp", delete_layer = TRUE)

# COD1000 -----------------------------------------------------

HTH_1000 <- HTH_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTR_1000 <- HTR_1000 %>%
  rename(COD1000 = COD1000.x)  # Reemplazar old_column_name con el nombre actual de la columna

# Creo una tabla de union para limpiar los datos 

DAA_MALLA_1000 <-  PPC_1000 %>% 
  left_join(PPC_1000_e, by = "COD1000") %>% 
  left_join(PPC_1000_s, by = "COD1000") %>%   
  mutate(across(everything(), ~replace_na(. , 0)))  
DAA_MALLA_1000 <- MALLA_1000 %>% 
  left_join(DAA_MALLA_1000, by = "COD1000")

DAB_MALLA_1000 <- PPN_1000 %>% 
  left_join(PPN_1000_e, by = "COD1000") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DAB_MALLA_1000 <- MALLA_1000 %>% 
  left_join(DAB_MALLA_1000, by = "COD1000")

DAC_MALLA_1000 <- PPN_1000_s %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DAC_MALLA_1000 <- MALLA_1000 %>% 
  left_join(DAC_MALLA_1000, by = "COD1000")

DE_MALLA_1000 <- EM_1000 %>%
  left_join(EM_1000_s, by = "COD1000") %>% 
  left_join(EMM_1000, by = "COD1000") %>% 
  left_join(EMM_1000_s, by = "COD1000") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DE_MALLA_1000 <- MALLA_1000 %>% 
  left_join(DE_MALLA_1000, by = "COD1000")

DH_MALLA_1000 <- HPM_1000 %>% 
  left_join(HTH_1000, by = "COD1000") %>% 
  left_join(HTR_1000, by = "COD1000") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DH_MALLA_1000 <- MALLA_1000 %>% 
  left_join(DH_MALLA_1000, by = "COD1000")

DMA_MALLA_1000 <- PIM_1000  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMA_MALLA_1000 <- MALLA_1000 %>% 
  left_join(DMA_MALLA_1000, by = "COD1000")

DMB_MALLA_1000 <- PIM_1000_s  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMB_MALLA_1000 <- MALLA_1000 %>% 
  left_join(DMB_MALLA_1000, by = "COD1000")

DMC_MALLA_1000 <- PEM_1000 %>% 
  left_join(PRN_1000, by = "COD1000") %>% 
  left_join(PRN_1000_a, by = "COD1000") %>% 
  left_join(PRN_1000_s, by = "COD1000") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DMC_MALLA_1000 <- MALLA_1000 %>% 
  left_join(DMC_MALLA_1000, by = "COD1000")

DMD_MALLA_1000 <-  MALLA_1000 %>% 
  left_join(MALLA_1000_DMD, by = "COD1000") 

DPA_MALLA_1000 <- PGE_1000 %>%
  left_join(PGE_1000_s, by = "COD1000") %>%
  left_join(PPA_1000, by = "COD1000") %>%
  left_join(PPA_1000_s, by = "COD1000") %>%
  left_join(PPE_1000, by = "COD1000") %>%
  left_join(PPE_1000_s, by = "COD1000") %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DPA_MALLA_1000 <- MALLA_1000 %>%
  left_join(DPA_MALLA_1000, by = "COD1000") %>%
  left_join(MALLA_1000_DPA, by = "COD1000")

DR_MALLA_1000 <- IE_1000 %>%
  left_join(IJ_1000, by = "COD1000") %>%
  left_join(RD_1000, by = "COD1000") %>%
  left_join(RDAM_1000, by = "COD1000") %>%
  left_join(RDJ_1000, by = "COD1000") %>%
  left_join(RHM_1000, by = "COD1000")  %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DR_MALLA_1000 <- MALLA_1000 %>%
  left_join(DR_MALLA_1000, by = "COD1000") %>% 
  left_join(MALLA_1000_DR, by = "COD1000")

# Guardar en un archivo Excel

st_write(DAA_MALLA_1000,"DAA_MALLA_1000.shp", delete_layer = TRUE)

st_write(DAB_MALLA_1000,"DAB_MALLA_1000.shp", delete_layer = TRUE)

st_write(DAC_MALLA_1000,"DAC_MALLA_1000.shp", delete_layer = TRUE)

st_write(DE_MALLA_1000, "DE_MALLA_1000.shp", delete_layer = TRUE)

st_write(DH_MALLA_1000,"DH_MALLA_1000.shp", delete_layer = TRUE)

st_write(DMA_MALLA_1000,"DMA_MALLA_1000.shp", delete_layer = TRUE)

st_write(DMB_MALLA_1000,"DMB_MALLA_1000.shp", delete_layer = TRUE)

st_write(DMC_MALLA_1000,"DMC_MALLA_1000.shp", delete_layer = TRUE)

st_write(DMD_MALLA_1000,"DMD_MALLA_1000.shp", delete_layer = TRUE)

st_write(DPA_MALLA_1000,"DPA_MALLA_1000.shp", delete_layer = TRUE)

st_write(DR_MALLA_1000,"DR_MALLA_1000.shp", delete_layer = TRUE)

# COD500 ------------------------------------------------------

HTH_500 <- HTH_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTR_500 <- HTR_500 %>%
  rename(COD500 = COD500.x)  # Reemplazar old_column_name con el nombre actual de la columna

# Creo una tabla de union para limpiar los datos 

DAA_MALLA_500 <-  PPC_500 %>% 
  left_join(PPC_500_e, by = "COD500") %>% 
  left_join(PPC_500_s, by = "COD500") %>%   
  mutate(across(everything(), ~replace_na(. , 0)))  
DAA_MALLA_500 <- MALLA_500 %>% 
  left_join(DAA_MALLA_500, by = "COD500")

DAB_MALLA_500 <- PPN_500 %>% 
  left_join(PPN_500_e, by = "COD500") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DAB_MALLA_500 <- MALLA_500 %>% 
  left_join(DAB_MALLA_500, by = "COD500")

DAC_MALLA_500 <- PPN_500_s %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0))) # Se agrego esta base 
DAC_MALLA_500 <- MALLA_500 %>% 
  left_join(DAC_MALLA_500, by = "COD500")

DE_MALLA_500 <- EM_500 %>%
  left_join(EM_500_s, by = "COD500") %>% 
  left_join(EMM_500, by = "COD500") %>% 
  left_join(EMM_500_s, by = "COD500") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DE_MALLA_500 <- MALLA_500 %>% 
  left_join(DE_MALLA_500, by = "COD500")

DH_MALLA_500 <- HPM_500 %>% 
  left_join(HTH_500, by = "COD500") %>% 
  left_join(HTR_500, by = "COD500") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DH_MALLA_500 <- MALLA_500 %>% 
  left_join(DH_MALLA_500, by = "COD500")

DMA_MALLA_500 <- PIM_500  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMA_MALLA_500 <- MALLA_500 %>% 
  left_join(DMA_MALLA_500, by = "COD500")

DMB_MALLA_500<- PIM_500_s  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))
DMB_MALLA_500 <- MALLA_500 %>% 
  left_join(DMB_MALLA_500, by = "COD500")

DMC_MALLA_500 <- PEM_500 %>% 
  left_join(PRN_500, by = "COD500") %>% 
  left_join(PRN_500_a, by = "COD500") %>% 
  left_join(PRN_500_s, by = "COD500") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DMC_MALLA_500 <- MALLA_500 %>% 
  left_join(DMC_MALLA_500, by = "COD500")

DMD_MALLA_500 <-  MALLA_500 %>% 
  left_join(MALLA_500_DMD, by = "COD500") 

DPA_MALLA_500 <- PGE_500 %>%
  left_join(PGE_500_s, by = "COD500") %>%
  left_join(PPA_500, by = "COD500") %>%
  left_join(PPA_500_s, by = "COD500") %>%
  left_join(PPE_500, by = "COD500") %>%
  left_join(PPE_500_s, by = "COD500") %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DPA_MALLA_500 <- MALLA_500 %>%
  left_join(DPA_MALLA_500, by = "COD500") %>%
  left_join(MALLA_500_DPA, by = "COD500")

DR_MALLA_500 <- IE_500 %>%
  left_join(IJ_500, by = "COD500") %>%
  left_join(RD_500, by = "COD500") %>%
  left_join(RDAM_500, by = "COD500") %>%
  left_join(RDJ_500, by = "COD500") %>%
  left_join(RHM_500, by = "COD500")  %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DR_MALLA_500 <- MALLA_500 %>%
  left_join(DR_MALLA_500, by = "COD500") %>%
  left_join(MALLA_500_DR, by = "COD500")

# Guardar en un archivo Excel

st_write(DAA_MALLA_500,"DAA_MALLA_500.shp", delete_layer = TRUE)

st_write(DAB_MALLA_500,"DAB_MALLA_500.shp", delete_layer = TRUE)

st_write(DAC_MALLA_500,"DAC_MALLA_500.shp", delete_layer = TRUE)

st_write(DE_MALLA_500, "DE_MALLA_500.shp", delete_layer = TRUE)

st_write(DH_MALLA_500,"DH_MALLA_500.shp", delete_layer = TRUE)

st_write(DMA_MALLA_500,"DMA_MALLA_500.shp", delete_layer = TRUE)

st_write(DMB_MALLA_500,"DMB_MALLA_500.shp", delete_layer = TRUE)

st_write(DMC_MALLA_500,"DMC_MALLA_500.shp", delete_layer = TRUE)

st_write(DMD_MALLA_500,"DMD_MALLA_500.shp", delete_layer = TRUE)

st_write(DPA_MALLA_500,"DPA_MALLA_500.shp", delete_layer = TRUE)

st_write(DR_MALLA_500,"DR_MALLA_500.shp", delete_layer = TRUE)

# H3_N8 -------------------------------------------------------

HTH_N8 <- HTH_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTR_N8 <- HTR_N8 %>%
  rename(H3_N8 = H3_N8.x)  # Reemplazar old_column_name con el nombre actual de la columna

# Creo una tabla de union para limpiar los datos 

DAA_MALLA_H3_N8 <-  PPC_N8 %>% 
  left_join(PPC_N8_e, by = "H3_N8") %>% 
  left_join(PPC_N8_s, by = "H3_N8") %>%   
  mutate(across(everything(), ~replace_na(. , 0)))  

DAA_MALLA_H3_N8 <- DAA_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DAA_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DAA_MALLA_H3_N8, by = "H3HASH")

DAB_MALLA_H3_N8 <- PPN_N8 %>% 
  left_join(PPN_N8_e, by = "H3_N8") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 

DAB_MALLA_H3_N8 <- DAB_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DAB_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DAB_MALLA_H3_N8, by = "H3HASH")

DAC_MALLA_H3_N8 <- PPN_N8 %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0))) # Se agrego esta base 
DAC_MALLA_H3_N8 <- DAC_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DAC_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DAC_MALLA_H3_N8, by = "H3HASH")

DE_MALLA_H3_N8 <- EM_N8 %>%
  left_join(EM_N8_s, by = "H3_N8") %>% 
  left_join(EMM_N8, by = "H3_N8") %>% 
  left_join(EMM_N8_s, by = "H3_N8") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 

DE_MALLA_H3_N8 <- DE_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DE_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DE_MALLA_H3_N8, by = "H3HASH")

DH_MALLA_H3_N8 <- HPM_N8 %>% 
  left_join(HTH_N8, by = "H3_N8") %>% 
  left_join(HTR_N8, by = "H3_N8") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 

DH_MALLA_H3_N8 <- DH_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DH_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DH_MALLA_H3_N8, by = "H3HASH")

DMA_MALLA_H3_N8<- PIM_N8  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))

DMA_MALLA_H3_N8 <- DMA_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DMA_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DMA_MALLA_H3_N8, by = "H3HASH")

DMB_MALLA_H3_N8 <- PIM_N8_s  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))

DMB_MALLA_H3_N8 <- DMB_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DMB_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DMB_MALLA_H3_N8, by = "H3HASH")

DMC_MALLA_H3_N8 <- PEM_N8 %>% 
  left_join(PRN_N8, by = "H3_N8") %>% 
  left_join(PRN_N8_a, by = "H3_N8") %>% 
  left_join(PRN_N8_s, by = "H3_N8") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 
DMC_MALLA_H3_N8 <- DMC_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DMC_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DMC_MALLA_H3_N8, by = "H3HASH")

DMD_MALLA_H3_N8 <-  MALLA_H3_N8 %>% 
  left_join(MALLA_H3_N8_DMD, by = "H3HASH") 

DPA_MALLA_H3_N8 <- PGE_N8 %>%
  left_join(PGE_N8_s, by = "H3_N8") %>%
  left_join(PPA_N8, by = "H3_N8") %>%
  left_join(PPA_N8_s, by = "H3_N8") %>%
  left_join(PPE_N8, by = "H3_N8") %>%
  left_join(PPE_N8_s, by = "H3_N8") %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DPA_MALLA_H3_N8 <- DPA_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)  
DPA_MALLA_H3_N8 <- MALLA_H3_N8 %>% 
  left_join(DPA_MALLA_H3_N8, by = "H3HASH") %>%
  left_join(MALLA_H3_N8_DPA, by = "H3HASH")

DR_MALLA_H3_N8 <- IE_N8 %>%
  left_join(IJ_N8, by = "H3_N8") %>%
  left_join(RD_N8, by = "H3_N8") %>%
  left_join(RDAM_N8, by = "H3_N8") %>%
  left_join(RDJ_N8, by = "H3_N8") %>%
  left_join(RHM_N8, by = "H3_N8")  %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DR_MALLA_H3_N8 <- DR_MALLA_H3_N8 %>%
  rename( H3HASH = H3_N8)
DR_MALLA_H3_N8 <- MALLA_H3_N8 %>%
  left_join(DR_MALLA_H3_N8, by = "H3HASH") %>% 
  left_join(MALLA_H3_N8_DR, by = "H3HASH")

# Guardar en un archivo Excel

st_write(DAA_MALLA_H3_N8,"DAA_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DAB_MALLA_H3_N8, "DAB_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DAC_MALLA_H3_N8, "DAC_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DE_MALLA_H3_N8, "DE_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DH_MALLA_H3_N8,"DH_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DMA_MALLA_H3_N8,"DMA_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DMB_MALLA_H3_N8,"DMB_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DMC_MALLA_H3_N8,"DMC_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DMD_MALLA_H3_N8,"DMD_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DPA_MALLA_H3_N8,"DPA_MALLA_H3_N8.shp", delete_layer = TRUE)

st_write(DR_MALLA_H3_N8,"DR_MALLA_H3_N8.shp", delete_layer = TRUE)

# H3_N9 -------------------------------------------------------

HTH_N9 <- HTH_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna
HTR_N9 <- HTR_N9 %>%
  rename(H3_N9 = H3_N9.x)  # Reemplazar old_column_name con el nombre actual de la columna

# Creo una tabla de union para limpiar los datos 

DAA_MALLA_H3_N9 <-  PPC_N9 %>% 
  left_join(PPC_N9_e, by = "H3_N9") %>% 
  left_join(PPC_N9_s, by = "H3_N9") %>%   
  mutate(across(everything(), ~replace_na(. , 0)))  

DAA_MALLA_H3_N9 <- DAA_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)  
DAA_MALLA_H3_N9 <- MALLA_H3_N9 %>% 
  left_join(DAA_MALLA_H3_N9, by = "H3HASH")

DAB_MALLA_H3_N9 <- PPN_N9 %>% 
  left_join(PPN_N9_e, by = "H3_N9") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 

DAB_MALLA_H3_N9 <- DAB_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)  
DAB_MALLA_H3_N9 <- MALLA_H3_N9 %>% 
  left_join(DAB_MALLA_H3_N9, by = "H3HASH")

DAC_MALLA_H3_N9 <- PPN_N9_s %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0))) # Se agrego esta base 
DAC_MALLA_H3_N9 <- DAC_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)  
DAC_MALLA_H3_N9 <- MALLA_H3_N9 %>% 
  left_join(DAC_MALLA_H3_N9, by = "H3HASH")

DE_MALLA_H3_N9 <- EM_N9 %>%
  left_join(EM_N9_s, by = "H3_N9") %>% 
  left_join(EMM_N9, by = "H3_N9") %>% 
  left_join(EMM_N9_s, by = "H3_N9") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 

DE_MALLA_H3_N9 <- DE_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)  
DE_MALLA_H3_N9 <- MALLA_H3_N9 %>% 
  left_join(DE_MALLA_H3_N9, by = "H3HASH")

DH_MALLA_H3_N9 <- HPM_N9 %>% 
  left_join(HTH_N9, by = "H3_N9") %>% 
  left_join(HTR_N9, by = "H3_N9") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 

DH_MALLA_H3_N9 <- DH_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)  
DH_MALLA_H3_N9 <- MALLA_H3_N9 %>% 
  left_join(DH_MALLA_H3_N9, by = "H3HASH")

DMA_MALLA_H3_N9<- PIM_N9 %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))

DMA_MALLA_H3_N9 <- DMA_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)  
DMA_MALLA_H3_N9 <- MALLA_H3_N9 %>% 
  left_join(DMA_MALLA_H3_N9, by = "H3HASH")

DMB_MALLA_H3_N9 <- PIM_N9_s  %>% 
  mutate(across(everything(), ~replace_na(. , 0))) %>% 
  dplyr::select(where(~ any(. != 0)))

DMB_MALLA_H3_N9 <- DMB_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)  
DMB_MALLA_H3_N9 <- MALLA_H3_N9 %>% 
  left_join(DMB_MALLA_H3_N9, by = "H3HASH")

DMC_MALLA_H3_N9 <- PEM_N9 %>% 
  left_join(PRN_N9, by = "H3_N9") %>% 
  left_join(PRN_N9_a, by = "H3_N9") %>% 
  left_join(PRN_N9_s, by = "H3_N9") %>% 
  mutate(across(everything(), ~replace_na(. , 0))) 

DMC_MALLA_H3_N9 <- DMC_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)  
DMC_MALLA_H3_N9 <- MALLA_H3_N9 %>% 
  left_join(DMC_MALLA_H3_N9, by = "H3HASH")

DMD_MALLA_H3_N9 <-  MALLA_H3_N9 %>% 
  left_join(MALLA_H3_N9_DMD, by = "H3HASH") 

DPA_MALLA_H3_N9 <- PGE_N9 %>%
  left_join(PGE_N9_s, by = "H3_N9") %>%
  left_join(PPA_N9, by = "H3_N9") %>%
  left_join(PPA_N9_s, by = "H3_N9") %>%
  left_join(PPE_N9, by = "H3_N9") %>%
  left_join(PPE_N9_s, by = "H3_N9") %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DPA_MALLA_H3_N9 <- DPA_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)
DPA_MALLA_H3_N9 <- MALLA_H3_N9 %>%
  left_join(DPA_MALLA_H3_N9, by = "H3HASH") %>%
  left_join(MALLA_H3_N9_DPA, by = "H3HASH")

DR_MALLA_H3_N9 <- IE_N9 %>%
  left_join(IJ_N9, by = "H3_N9") %>%
  left_join(RD_N9, by = "H3_N9") %>%
  left_join(RDAM_N9, by = "H3_N9") %>%
  left_join(RDJ_N9, by = "H3_N9") %>%
  left_join(RHM_N9, by = "H3_N9")  %>%
  mutate(across(everything(), ~replace_na(. , 0)))
DR_MALLA_H3_N9 <- DR_MALLA_H3_N9 %>%
  rename( H3HASH = H3_N9)
DR_MALLA_H3_N9 <- MALLA_H3_N9 %>%
  left_join(DR_MALLA_H3_N9, by = "H3HASH")%>% 
  left_join(MALLA_H3_N9_DR, by = "H3HASH")

# Guardar en un archivo Excel

st_write(DAA_MALLA_H3_N9,"DAA_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DAB_MALLA_H3_N9, "DAB_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DAC_MALLA_H3_N9, "DAC_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DE_MALLA_H3_N9, "DE_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DH_MALLA_H3_N9,"DH_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DMA_MALLA_H3_N9,"DMA_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DMB_MALLA_H3_N9,"DMB_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DMC_MALLA_H3_N9,"DMC_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DMD_MALLA_H3_N9,"DMD_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DPA_MALLA_H3_N9,"DPA_MALLA_H3_N9.shp", delete_layer = TRUE)

st_write(DR_MALLA_H3_N9,"DR_MALLA_H3_N9.shp", delete_layer = TRUE)
