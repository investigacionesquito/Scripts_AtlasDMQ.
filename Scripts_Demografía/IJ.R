# Dimensión: Demografía 

# Nombre del Indicador: 
#Índice de juventud 2010

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

poblacion2022[P03 %in% c(0:14), Im:=1]
poblacion2022[P03>=65, Pr:=1]

## Geográfico/Territorial

# Cantón 
IJ_c <-  poblacion2022[,
  round((freq=sum(Im, na.rm = T)/sum(Pr, na.rm = T)*100),1), by=.(canton)] %>% 
  setnames("V1", "IJ") 

# Administracion zonal
IJ_az <-  poblacion2022[,
         round((freq=sum(Im, na.rm = T)/sum(Pr, na.rm = T)*100),1), by=.(adm_zonal)] %>% 
  setnames("V1", "IJ") 

# Parroquial
IJ_pr <-  poblacion2022[,
          round((freq=sum(Im, na.rm = T)/sum(Pr, na.rm = T)*100),1), by=.(parroquia)] %>% 
  setnames("V1", "IJ") 
# Sector 
IJ_s = poblacion2022[,
         round((freq=sum(Im, na.rm = T)/sum(Pr, na.rm = T)*100),1), by=.(Sector_DMQ)] %>% 
  setnames("V1", "IJ")

# Grilla COD1000
IJ_1000 = poblacion2022[,
       round((freq=sum(Im, na.rm = T)/sum(Pr, na.rm = T)*100),1), by=.(COD1000)] %>% 
  setnames("V1", "IJ")

# Grilla COD500
IJ_500 = poblacion2022[,
       round((freq=sum(Im, na.rm = T)/sum(Pr, na.rm = T)*100),1), by=.(COD500)] %>% 
  setnames("V1", "IJ")

# Grilla H3_N8
IJ_N8 = poblacion2022[,
       round((freq=sum(Im, na.rm = T)/sum(Pr, na.rm = T)*100),1), by=.(H3_N8)] %>% 
  setnames("V1", "IJ")

# Grilla H3_N9
IJ_N9 = poblacion2022[,
       round((freq=sum(Im, na.rm = T)/sum(Pr, na.rm = T)*100),1), by=.(H3_N9)] %>% 
  setnames("V1", "IJ")

# 6. Guardar los resultados ----------------------------------------------------

wb <-  createWorkbook("IJ")

addWorksheet(wb, "IJ_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IJ_c", x=IJ_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IJ_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IJ_az", x=IJ_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IJ_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IJ_pr", x=IJ_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IJ_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IJ_s", x=IJ_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "IJ_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IJ_1000", x=IJ_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IJ_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IJ_500", x=IJ_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IJ_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IJ_N9", x=IJ_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "IJ_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "IJ_N8", x=IJ_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Índice de juventud 2010.xlsx")
