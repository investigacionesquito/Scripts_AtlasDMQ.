# Dimensión: Demografía 

# Nombre del Indicador: 
#Índice de envejecimiento

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

poblacion2022[P03>=65, ImE:=1]
poblacion2022[P03 %in% c(0:14), PrE:=1]

## Geográfico/Territorial

# Cantón 
IE_c <-  poblacion2022[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(canton)] %>% 
  setnames("V1", "IE") 

# Administracion zonal
IE_az = poblacion2022[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(adm_zonal)] %>% 
  setnames("V1", "IE")

# Parroquial
IE_pr <-  poblacion2022[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(parroquia)] %>% 
  setnames("V1", "IE")  

# Sector 
IE_s = poblacion2022[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(Sector_DMQ)] %>% 
  setnames("V1", "IE")

# Grilla COD1000
IE_1000 = poblacion2022[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(COD1000)] %>% 
  setnames("V1", "IE")

# Grilla COD500
IE_500 = poblacion2022[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(COD500)] %>% 
  setnames("V1", "IE")

# Grilla H3_N8
IE_N8 = poblacion2022[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(H3_N8)] %>% 
  setnames("V1", "IE")

# Grilla H3_N9
IE_N9 = poblacion2022[,
  round((freq=sum(ImE, na.rm = T)/sum(PrE, na.rm = T)*100),1), by=.(H3_N9)] %>% 
  setnames("V1", "IE")


# 6. Guardar los resultados ----------------------------------------------------

wb <-  createWorkbook("IE")

addWorksheet(wb, "IE_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE_c", x=IE_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE_az", x=IE_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE_pr", x=IE_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE_s", x=IE_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "IE_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE_1000", x=IE_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE_500", x=IE_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE_N8", x=IE_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IE_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IE_N9", x=IE_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Índice de envejecimiento.xlsx")