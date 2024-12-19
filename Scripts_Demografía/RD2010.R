# Dimensión: Demografía 

# Nombre del Indicador: Relación de dependencia total 2010

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

poblacion2010[P03<=14 | P03>=65, Imp:=1]
poblacion2010[P03 %in% c(15:64), Pro:=1]

## Geográfico/Territorial

# Cantón 
RD10_c<-  poblacion2010[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(canton)] %>% 
  setnames( "V1", "RD10") 

# Administración zonal 
RD10_az <-  poblacion2010[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(adm_zonal)] %>% 
  setnames( "V1", "RD10") 

# Parroquial
RD10_pr <-  poblacion2010[,
        round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(parroquia)] %>% 
  setnames( "V1", "RD10") 

# Sector 
RD10_s = poblacion2010[,
       round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(Sector_DMQ)] %>% 
  setnames("V1", "RD10") 

# Grilla COD1000
RD10_1000 = poblacion2010[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(COD1000)] %>% 
  setnames("V1", "RD10")

# Grilla COD500
RD10_500 = poblacion2010[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(COD500)] %>% 
  setnames("V1", "RD10")

# Grilla H3_N8
RD10_N8 = poblacion2010[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(H3_N8)] %>% 
  setnames("V1", "RD10")

# Grilla H3_N9
RD10_N9 = poblacion2010[,
  round((freq=sum(Imp, na.rm = T)/sum(Pro, na.rm = T)*100),1), by=.(H3_N9)] %>% 
  setnames("V1", "RD10")

# 6. GuaRD10ar los resultados ----------------------------------------------------

wb <- createWorkbook("RD10")

addWorksheet(wb, "RD10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD10_c", x=RD10_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD10_az", x=RD10_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD10_pr", x=RD10_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD10_s", x=RD10_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "RD10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD10_1000", x=RD10_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD10_500", x=RD10_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD10_N8", x=RD10_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RD10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RD10_N9", x=RD10_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Relación de dependencia total 2010.xlsx")
