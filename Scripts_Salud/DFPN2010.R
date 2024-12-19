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

poblacion2010 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

# P08 = Discapacidad permanente por más de un año
# 1 Si
# 2 No
# 9 No responde

poblacion2010[,DFP10:=0]
poblacion2010[P08==1, DFP10:=1] 
# Desagregaciones

## Geográfico territorial

# Cantón
DFPN10_c <- poblacion2010[DFP2010==1, sum(DFP2010), by = .(canton)] %>% 
  setnames("V1", "DFPN10") 

# Administración zonal 
DFPN10_az <- poblacion2010[DFP2010==1, sum(DFP2010), by = .(adm_zonal)] %>% 
  setnames("V1", "DFPN10") 

# Parroquial
DFPN10_pr <- poblacion2010[DFP2010==1, sum(DFP2010), by = .(parroquia)] %>% 
  setnames("V1", "DFPN10")

# Sector
DFPN10_s <- poblacion2010[DFP2010==1, sum(DFP2010), by = .(Sector_DMQ)] %>% 
  setnames("V1", "DFPN10")

# Grilla COD1000
DFPN10_1000 <- poblacion2010[DFP2010==1, sum(DFP2010), by = .(COD1000)] %>% 
  setnames("V1", "DFPN10")

# Grilla COD500
DFPN10_500 <- poblacion2010[DFP2010==1, sum(DFP2010), by = .(COD500)] %>% 
  setnames("V1", "DFPN10")

# Grilla H3_N8
DFPN10_N8 <- poblacion2010[DFP2010==1, sum(DFP2010), by = .(H3_N8)] %>% 
  setnames("V1", "DFPN10")

# Grilla H3_N9
DFPN10_N9 <- poblacion2010[DFP2010==1, sum(DFP2010), by = .(H3_N9)] %>% 
  setnames("V1", "DFPN10")


# 6. Guardar los resultados

wb <-  createWorkbook("DFPN10")

addWorksheet(wb, "DFPN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN10_c", x= DFPN10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN10_az", x= DFPN10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN10_pr", x= DFPN10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN10_s", x= DFPN10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN10_1000", x= DFPN10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN10_500", x= DFPN10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN10_N8", x= DFPN10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFPN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFPN10_N9", x= DFPN10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de la poblacion con dificultad funcional permanente 2010.xlsx")

