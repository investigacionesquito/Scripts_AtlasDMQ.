# Dimensión: Demografía 

# Nombre del Indicador: 
#Relación de dependencia de adultas/os mayores

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

poblacion2022[P03>=65, Impa:=1]
poblacion2022[P03 %in% c(15:64), Proa:=1]

## Geográfico/Territorial

# Cantón 
RDAM_c <-  poblacion2022[,round((freq=sum(Impa, na.rm = T)/sum(Proa, na.rm = T)*100),1), by=.(canton)] %>% 
  setnames("V1", "RDAM") 

# Administración zonal 
RDAM_az <-  poblacion2022[,round((freq=sum(Impa, na.rm = T)/sum(Proa, na.rm = T)*100),1), by=.(adm_zonal)] %>% 
  setnames("V1", "RDAM") 

# Parroquial
RDAM_pr <-  poblacion2022[,round((freq=sum(Impa, na.rm = T)/sum(Proa, na.rm = T)*100),1), by=.(parroquia)] %>% 
  setnames("V1", "RDAM") 

# Sector 
RDAM_s = poblacion2022[,round((freq=sum(Impa, na.rm = T)/sum(Proa, na.rm = T)*100),1), by=.(Sector_DMQ)] %>% 
  setnames("V1", "RDAM") 

# Grilla COD1000
RDAM_1000 <-  poblacion2022[,round((freq=sum(Impa, na.rm = T)/sum(Proa, na.rm = T)*100),1), by=.(COD1000)] %>% 
  setnames("V1", "RDAM") 

# Grilla COD500
RDAM_500 <-  poblacion2022[,round((freq=sum(Impa, na.rm = T)/sum(Proa, na.rm = T)*100),1), by=.(COD500)] %>% 
  setnames("V1", "RDAM") 

# Grilla H3_N8
RDAM_N8 <-  poblacion2022[,round((freq=sum(Impa, na.rm = T)/sum(Proa, na.rm = T)*100),1), by=.(H3_N8)] %>% 
  setnames("V1", "RDAM") 

# Grilla H3_N9
RDAM_N9 <-  poblacion2022[,round((freq=sum(Impa, na.rm = T)/sum(Proa, na.rm = T)*100),1), by=.(H3_N9)] %>% 
  setnames("V1", "RDAM") 

# 6. Guardar los resultados ----------------------------------------------------

wb <-  createWorkbook("RDAM")

addWorksheet(wb, "RDAM_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDAM_c", x=RDAM_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDAM_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDAM_az", x=RDAM_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDAM_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDAM_pr", x=RDAM_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDAM_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDAM_s", x=RDAM_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "RDAM_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDAM_1000", x=RDAM_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDAM_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDAM_500", x=RDAM_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDAM_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDAM_N8", x=RDAM_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDAM_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDAM_N9", x=RDAM_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Relación de dependencia de adultas_os mayores.xlsx")