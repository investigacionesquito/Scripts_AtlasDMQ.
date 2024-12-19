# Dimensión: Demografía 

# Nombre del Indicador: 
# Relación de dependencia jovenes

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

poblacion2022[P03<=14, Impj:=1]
poblacion2022[P03 %in% c(15:64), Proj:=1]

## Geográfico/Territorial

# Cantón 
RDJ_c <-  poblacion2022[,round((freq=sum(Impj, na.rm = T)/sum(Proj, na.rm = T)*100),1), by=.(canton)] %>% 
  setnames("V1", "RDJ") 

# Administración zonal 
RDJ_az = poblacion2022[,round((freq=sum(Impj, na.rm = T)/sum(Proj, na.rm = T)*100),1), by=.(adm_zonal)] %>% 
  setnames("V1", "RDJ") 

# Parroquial
RDJ_pr <-  poblacion2022[,round((freq=sum(Impj, na.rm = T)/sum(Proj, na.rm = T)*100),1), by=.(parroquia)] %>% 
  setnames("V1", "RDJ") 

# Sector 
RDJ_s = poblacion2022[,round((freq=sum(Impj, na.rm = T)/sum(Proj, na.rm = T)*100),1), by=.(Sector_DMQ)] %>% 
  setnames("V1", "RDJ") 

# Grilla COD1000
RDJ_1000 = poblacion2022[,round((freq=sum(Impj, na.rm = T)/sum(Proj, na.rm = T)*100),1), by=.(COD1000)] %>% 
  setnames("V1", "RDJ") 

# Grilla COD500
RDJ_500 = poblacion2022[,round((freq=sum(Impj, na.rm = T)/sum(Proj, na.rm = T)*100),1), by=.(COD500)] %>% 
  setnames("V1", "RDJ") 

# Grilla H3_N8
RDJ_N8 = poblacion2022[,round((freq=sum(Impj, na.rm = T)/sum(Proj, na.rm = T)*100),1), by=.(H3_N8)] %>% 
  setnames("V1", "RDJ") 

# Grilla H3_N9
RDJ_N9 = poblacion2022[,round((freq=sum(Impj, na.rm = T)/sum(Proj, na.rm = T)*100),1), by=.(H3_N9)] %>% 
  setnames("V1", "RDJ") 


# 6. Guardar los resultados ----------------------------------------------------

wb <-  createWorkbook("RDJ")

addWorksheet(wb, "RDJ_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDJ_c", x=RDJ_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDJ_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDJ_az", x=RDJ_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDJ_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDJ_pr", x=RDJ_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDJ_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDJ_s", x=RDJ_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "RDJ_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDJ_1000", x=RDJ_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDJ_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDJ_500", x=RDJ_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDJ_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDJ_N8", x=RDJ_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RDJ_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RDJ_N9", x=RDJ_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Relación de dependencia joven.xlsx")