# Dimensión: Demografía 

# Nombre del Indicador: 
# Cantidad de oblación de acuerdo a la identificación según cultura y costumbres (Número)

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5 . Calcular indicadores
# 6 . Guardar los resultados

# 1. Descargar bases de datos ---------------------------------------

# 2. Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ----------------------------------------------

pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr, tidyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

# Desagregaciones

## Geográfico territorial

# Cantón
PPCN_c <- poblacion2022[, .(freq=.N), by = .(P11R, canton)] %>% 
  dcast(canton~ P11R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN_", .x), .x))  

# Administración zonal 
PPCN_az <- poblacion2022[, .(freq=.N), by = .(P11R, adm_zonal)] %>% 
  dcast(adm_zonal ~ P11R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN_", .x), .x))  

# Parroquial
PPCN_pr <- poblacion2022[, .(freq=.N), by = .(P11R, parroquia)] %>% 
  dcast(parroquia~ P11R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN_", .x), .x))  

# Sector
PPCN_s <- poblacion2022[, .(freq=.N), by = .(P11R, Sector_DMQ)]%>% 
  dcast( Sector_DMQ ~ P11R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN_", .x), .x))  

# Grilla COD1000
PPCN_1000 <- poblacion2022[, .(freq=.N), by = .(P11R, COD1000)]%>% 
  dcast(COD1000 ~ P11R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN_", .x), .x))  

# Grilla COD500
PPCN_500 <- poblacion2022[, .(freq=.N), by = .(P11R, COD500)]%>% 
  dcast(COD500 ~ P11R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN_", .x), .x))  

# Grilla H3_N8
PPCN_N8 <- poblacion2022[, .(freq=.N), by = .(P11R, H3_N8)]%>% 
  dcast(H3_N8 ~ P11R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN_", .x), .x))  

# Grilla H3_N9
PPCN_N9 <- poblacion2022[, .(freq=.N), by = .(P11R, H3_N9)] %>% 
  dcast(H3_N9 ~ P11R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN_", .x), .x))  


# 6. Guardar los resultados

wb <-  createWorkbook("PPCN")

addWorksheet(wb, "PPCN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN_c", x= PPCN_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN_az", x= PPCN_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN_pr", x= PPCN_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN_s", x= PPCN_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN_1000", x= PPCN_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN_500", x= PPCN_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN_N8", x= PPCN_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN_N9", x= PPCN_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de población de acuerdo a la identificación según cultura y costumbres.xlsx")