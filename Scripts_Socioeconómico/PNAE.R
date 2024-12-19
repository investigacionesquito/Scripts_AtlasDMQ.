# Dimensión: Socioeconomía

# Nombre del Indicador: 
# Población en viviendas particulares de 14 años y más nacida en otro país, por condición de actividad económica

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5 . Calcular indicadores
# 9 . Guardar los resultados

# 1. Descargar bases de datos ---------------------------------------

# 2. Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ----------------------------------------------

pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr, tidyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()
vivienda2022 <- read_sav("vivienda2022_Atlas.sav") %>% as.data.table()

VP2022 <- merge(vivienda2022,poblacion2022, by="ID_VIV")

VP2022 <- VP2022 %>% rename(canton = canton.x)
VP2022 <- VP2022 %>% rename(adm_zonal = adm_zonal.x)
VP2022 <- VP2022 %>% rename(parroquia = parroquia.x)
VP2022 <- VP2022 %>% rename(Sector_DMQ = Sector_DMQ.x)
VP2022 <- VP2022 %>% rename(COD1000 = COD1000.x)
VP2022 <- VP2022 %>% rename(COD500 = COD500.x)
VP2022 <- VP2022 %>% rename(H3_N8 = H3_N8.x)
VP2022 <- VP2022 %>% rename(H3_N9 = H3_N9.x)

# 5. Calcular indicadores --------------------------------------------------

#P08 = En dónde nació
# 1 En esta ciudad o parroquia rural
# 2 En otro lugar del país
# 3 En otro país

# CONDACT1 = Condicion de actividad (desagregada)
# 1 Menor de 5 años
# 2 Ocupado
# 3 Desocupado
# 4 Fuera de la fuerza de trabajo

# VO1 = Tipo de vivienda 

# Desagregaciones

## Geográfico territorial

# Cantón
PNAE_c <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(canton~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PNAE_", .x), .x))  

# Administración zonal 
PNAE_az <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PNAE_", .x), .x))  

# Parroquial
PNAE_pr <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(parroquia~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PNAE_", .x), .x))  

# Sector
PNAE_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast( Sector_DMQ ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PNAE_", .x), .x))  

# Grilla COD1000
PNAE_1000 <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PNAE_", .x), .x))  

# Grilla COD500
PNAE_500 <- VP2022[P03 >= 14 & P08==3 & V01<=8& V01<=8, .(freq=.N), by = .(CONDACT1, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD500 ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PNAE_", .x), .x))  

# Grilla H3_N8
PNAE_N8 <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N8 ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PNAE_", .x), .x))  

# Grilla H3_N9
PNAE_N9 <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PNAE_", .x), .x))  

## Socio Demográfico/Económico

# Cantón - sexo
PNAE_c_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, P02,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(canton + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(2:4), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PNAEs", .x), .x))

# Administración zonal - sexo
PNAE_az_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, P02,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PNAEs", .x), .x))  

# Parroquial - sexo
PNAE_pr_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, P02,parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(parroquia + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PNAEs", .x), .x))  

# Sector - sexo 
PNAE_s_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, P02,Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P02)][,
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P02~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PNAEs", .x), .x)) 

# Grilla COD1000 - sexo
PNAE_1000_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, P02,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD1000 + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PNAEs", .x), .x))  

# Grilla COD500 - sexo
PNAE_500_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, P02,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD500 + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PNAEs", .x), .x))  

# Grilla H3_N8 - sexo
PNAE_N8_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, P02,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N8 + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PNAEs", .x), .x))  

# Grilla H3_N9 - sexo
PNAE_N9_s <- VP2022[P03 >= 14 & P08==3 & V01<=8, .(freq=.N), by = .(CONDACT1, P02,H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N9 + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PNAEs", .x), .x))  

# 9. Guardar los resultados

wb <-  createWorkbook("PNAE")

addWorksheet(wb, "PNAE_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_c", x= PNAE_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_az", x= PNAE_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_pr", x= PNAE_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_s", x= PNAE_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_1000", x= PNAE_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_500", x= PNAE_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_N8", x= PNAE_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_N9", x= PNAE_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PNAE_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_c_s", x= PNAE_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_az_s", x= PNAE_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_pr_s", x= PNAE_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_s_s", x= PNAE_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_1000_s", x= PNAE_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_500_s", x= PNAE_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_N8_s", x= PNAE_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PNAE_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PNAE_N9_s", x= PNAE_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Población en viviendas particulares de 14 años y más nacida en otro país, por condición de actividad económica.xlsx")