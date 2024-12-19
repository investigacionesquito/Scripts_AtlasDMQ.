# Dimensión: Salud

# Nombre del Indicador: 
# Cantidad de la poblacion con dificultad funcional permanente

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

# DFUNC = Dificultad funcional permanente
# 1 Con dificultad funcional
# 2 Sin dificultad funcional
# 9 Se ignora

poblacion2022[,DFP:=0]
poblacion2022[DFUNC==1, DFP:=1] 

# Desagregaciones

## Geográfico territorial

# Cantón
DFPN_c <- poblacion2022[DFP==1, sum(DFP), by = .(canton)] %>% 
  setnames("V1", "DFPN") 

# Administración zonal 
DFPN_az <- poblacion2022[DFP==1, sum(DFP), by = .(adm_zonal)] %>% 
  setnames("V1", "DFPN") 

# Parroquial
DFPN_pr <- poblacion2022[DFP==1, sum(DFP), by = .(parroquia)] %>% 
  setnames("V1", "DFPN")

# Sector
DFPN_s <- poblacion2022[DFP==1, sum(DFP), by = .(Sector_DMQ)] %>% 
  setnames("V1", "DFPN")

# Grilla COD1000
DFPN_1000 <- poblacion2022[DFP==1, sum(DFP), by = .(COD1000)] %>% 
  setnames("V1", "DFPN")

# Grilla COD500
DFPN_500 <- poblacion2022[DFP==1, sum(DFP), by = .(COD500)] %>% 
  setnames("V1", "DFPN")

# Grilla H3_N8
DFPN_N8 <- poblacion2022[DFP==1, sum(DFP), by = .(H3_N8)] %>% 
  setnames("V1", "DFPN")

# Grilla H3_N9
DFPN_N9 <- poblacion2022[DFP==1, sum(DFP), by = .(H3_N9)] %>% 
  setnames("V1", "DFPN")


# 6. Guardar los resultados

wb <-  createWorkbook("DFPN")

addWorksheet(wb, "DFPN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN_c", x= DFPN_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN_az", x= DFPN_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN_pr", x= DFPN_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN_s", x= DFPN_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN_1000", x= DFPN_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN_500", x= DFPN_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN_N8", x= DFPN_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN_N9", x= DFPN_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de la poblacion con dificultad funcional permanente.xlsx")

