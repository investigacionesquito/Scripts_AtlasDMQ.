# Dimensión: Socioeconomía

# Nombre del Indicador: 
# Cantidad de la población, según aporte de seguridad social

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

# 5. Calcular indicadores --------------------------------------------------

# P30 = Aporta actualmente
# 1 IESS Seguro General
# 2 IESS Seguro Voluntario
# 3 IESS Seguro Campesino
# 4 Seguro ISSFA
# 5 Seguro ISSPOL
# 6 No aporta, es jubilada/o IESS/ ISSFA/ ISSPOL.
# 7 No aporta
# 9 Se ignora

# Desagregaciones

## Geográfico territorial

# Cantón
PSSN_c <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, canton)] %>% 
  dcast(canton~ P30, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN_", .x), .x))  

# Administración zonal
PSSN_az <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, adm_zonal)] %>% 
  dcast(adm_zonal ~ P30, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN_", .x), .x))  

# Parroquial
PSSN_pr <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, parroquia)] %>% 
  dcast(parroquia~ P30, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN_", .x), .x))  

# Sector
PSSN_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, Sector_DMQ)] %>% 
  dcast( Sector_DMQ ~ P30, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN_", .x), .x))  

# Grilla COD1000
PSSN_1000 <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, COD1000)] %>% 
  dcast(COD1000 ~ P30, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN_", .x), .x))  

# Grilla COD500
PSSN_500 <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, COD500)] %>% 
  dcast(COD500 ~ P30, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN_", .x), .x))  

# Grilla H3_N8
PSSN_N8 <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, H3_N8)] %>% 
  dcast(H3_N8 ~ P30, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN_", .x), .x))  

# Grilla H3_N9
PSSN_N9 <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, H3_N9)]%>% 
  dcast(H3_N9 ~ P30, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN_", .x), .x))  

# 9. Guardar los resultados

wb <-  createWorkbook("PSSN")

addWorksheet(wb, "PSSN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN_c", x= PSSN_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN_az", x= PSSN_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN_pr", x= PSSN_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN_s", x= PSSN_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN_1000", x= PSSN_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN_500", x= PSSN_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN_N8", x= PSSN_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN_N9", x= PSSN_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de la población, según aporte de seguridad social.xlsx")