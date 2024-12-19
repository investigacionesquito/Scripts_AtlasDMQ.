# Dimensión: Demografía 

# Nombre del Indicador: 
#Índice de envejecimiento 2010

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

# P03 = EDAD 

poblacion2010[P03>=65, ImE:=1]
poblacion2010[P03 %in% c(0:14), PrE:=1]

## Geográfico/Territorial

# Cantón 
IE10_c <-  poblacion2010[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(canton)] %>% 
  setnames("V1", "IE10") 

# Administracion zonal
IE10_az = poblacion2010[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(adm_zonal)] %>% 
  setnames("V1", "IE10")

# Parroquial
IE10_pr <-  poblacion2010[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(parroquia)] %>% 
  setnames("V1", "IE10")  

# Sector 
IE10_s = poblacion2010[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(Sector_DMQ)] %>% 
  setnames("V1", "IE10")

# Grilla COD1000
IE10_1000 = poblacion2010[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(COD1000)] %>% 
  setnames("V1", "IE10")

# Grilla COD500
IE10_500 = poblacion2010[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(COD500)] %>% 
  setnames("V1", "IE10")

# Grilla H3_N8
IE10_N8 = poblacion2010[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(H3_N8)] %>% 
  setnames("V1", "IE10")

# Grilla H3_N9
IE10_N9 = poblacion2010[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(H3_N9)] %>% 
  setnames("V1", "IE10")


# 6. Guardar los resultados ----------------------------------------------------

wb <-  createWorkbook("IE10")

addWorksheet(wb, "IE10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE10_c", x=IE10_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE10_az", x=IE10_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE10_pr", x=IE10_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE10_s", x=IE10_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "IE10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE10_1000", x=IE10_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE10_500", x=IE10_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE10_N8", x=IE10_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE10_N9", x=IE10_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Índice de envejecimiento 2010.xlsx")