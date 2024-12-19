# Dimensión: Demografía 

# Nombre del Indicador: Relación de dependencia total

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

poblacion2022[P03<=14 | P03>=65, Imp:=1]
poblacion2022[P03 %in% c(15:64), Pro:=1]

## Geográfico/Territorial

# Cantón 
RD_c<-  poblacion2022[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(canton)] %>% 
  setnames( "V1", "RD") 

# Administración zonal 
RD_az <-  poblacion2022[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(adm_zonal)] %>% 
  setnames( "V1", "RD") 

# Parroquial
RD_pr <-  poblacion2022[,
        round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(parroquia)] %>% 
  setnames( "V1", "RD") 

# Sector 
RD_s = poblacion2022[,
       round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(Sector_DMQ)] %>% 
  setnames("V1", "RD") 

# Grilla COD1000
RD_1000 = poblacion2022[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(COD1000)] %>% 
  setnames("V1", "RD")

# Grilla COD500
RD_500 = poblacion2022[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(COD500)] %>% 
  setnames("V1", "RD")

# Grilla H3_N8
RD_N8 = poblacion2022[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(H3_N8)] %>% 
  setnames("V1", "RD")

# Grilla H3_N9
RD_N9 = poblacion2022[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(H3_N9)] %>% 
  setnames("V1", "RD")

# 6. Guardar los resultados ----------------------------------------------------

wb <- createWorkbook("RD")

addWorksheet(wb, "RD_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD_c", x=RD_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD_az", x=RD_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD_pr", x=RD_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD_s", x=RD_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "RD_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD_1000", x=RD_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD_500", x=RD_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD_N8", x=RD_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD_N9", x=RD_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Relación de dependencia total.xlsx")
