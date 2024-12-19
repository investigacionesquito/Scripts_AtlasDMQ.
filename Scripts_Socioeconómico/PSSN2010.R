# Dimensión: Socioeconomía

# Nombre del Indicador: 
# Cantidad de la población, según aporte de seguridad social 2010

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

poblacion2010 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

# P35 = Aporte o afiliación a la Seguridad Social
# 1 Seguro ISSFA
# 2 Seguro ISSPOL
# 3 IESS Seguro general
# 4 IESS Seguro voluntario
# 5 IESS Seguro campesino
# 6 Es jubilado del IESS/ISSFA/ISSPOL
# 7 No aporta
# 9 Se ignora


# Recodifico para poder comparar con 2022 

# 1 IESS Seguro General
# 2 IESS Seguro Voluntario
# 3 IESS Seguro Campesino
# 4 Seguro ISSFA
# 5 Seguro ISSPOL
# 6 No aporta, es jubilada/o IESS/ ISSFA/ ISSPOL.
# 7 No aporta
# 9 Se ignora

poblacion2010[P35==1, P35R:=4]
poblacion2010[P35==2, P35R:=5]
poblacion2010[P35==3, P35R:=1]
poblacion2010[P35==4, P35R:=2]
poblacion2010[P35==5, P35R:=3]
poblacion2010[P35==6, P35R:=6]
poblacion2010[P35==7, P35R:=7]
poblacion2010[P35==9, P35R:=9]

# Desagregaciones

## Geográfico territorial

# Cantón
PSSN10_c <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, canton)] %>% 
  dcast(canton~ P35R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN10_", .x), .x))  

# Administración zonal
PSSN10_az <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, adm_zonal)] %>% 
  dcast(adm_zonal ~ P35R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN10_", .x), .x))  

# Parroquial
PSSN10_pr <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, parroquia)] %>% 
  dcast(parroquia~ P35R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN10_", .x), .x))  

# Sector
PSSN10_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, Sector_DMQ)] %>% 
  dcast( Sector_DMQ ~ P35R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN10_", .x), .x))  

# Grilla COD1000
PSSN10_1000 <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, COD1000)] %>% 
  dcast(COD1000 ~ P35R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN10_", .x), .x))  

# Grilla COD500
PSSN10_500 <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, COD500)] %>% 
  dcast(COD500 ~ P35R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN10_", .x), .x))  

# Grilla H3_N8
PSSN10_N8 <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, H3_N8)] %>% 
  dcast(H3_N8 ~ P35R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN10_", .x), .x))  

# Grilla H3_N9
PSSN10_N9 <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, H3_N9)]%>% 
  dcast(H3_N9 ~ P35R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSSN10_", .x), .x))  

# 9. Guardar los resultados

wb <-  createWorkbook("PSSN10")

addWorksheet(wb, "PSSN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN10_c", x= PSSN10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN10_az", x= PSSN10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN10_pr", x= PSSN10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN10_s", x= PSSN10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN10_1000", x= PSSN10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN10_500", x= PSSN10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN10_N8", x= PSSN10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSSN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSSN10_N9", x= PSSN10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de la población, según aporte de seguridad social 2010.xlsx")