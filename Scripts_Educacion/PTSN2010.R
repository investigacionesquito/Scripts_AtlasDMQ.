# Dimensión: Educación

# Nombre del Indicador: 
# Cantidad de la población de 23 años o más tituladas de educación superior en universidades o escuelas politécnicas 2010.
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

#P03 = Años cumplidos 

#P23 = Nivel de instrucción más alto al que asiste o asistió: 

# 1 Ninguno 
# 2 Centro de Alfabetización/(EBA) 
# 3 Preescolar 
# 4 Primario
# 5 Secundario
# 6 Educación Básica 
# 7 Educación Media/ bachillerato 
# 8 Ciclo Postbachillerato 
# 9 Superior 
# 10 Postgrado 
# 99 Se ignora

#P25 = Tiene título de postbachillerato, superior o postgrado
# 1 Que es reconocido por el CONESUP
# 2 Que no es reconocido por el CONESUP
# 3 No tiene
# 9 No sabe

poblacion2010[,TSP:=NULL]
poblacion2010[P03>=23,TSP:=0]
poblacion2010[P03>=23 & ((P23==99 & P25==1) | (P25==9)), TSP:=9]
poblacion2010[P03>=23 & P23==9 & P25==1, TSP:=1]
poblacion2010[P03>=23 & P23==10, TSP:=1]

# Desagregaciones

## Geográfico territorial

# Cantón

PTSN10_c <- poblacion2010[P03 >= 23 & TSP==1, sum(TSP), by = .(canton)] %>% 
  setnames("V1", "PTSN10") 

# Administración zonal 
PTSN10_az <- poblacion2010[P03 >= 23 & TSP==1, sum(TSP), by = .(adm_zonal)] %>% 
  setnames("V1", "PTSN10") 

# Parroquial
PTSN10_pr <- poblacion2010[P03 >= 23 & TSP==1, sum(TSP), by = .(parroquia)] %>% 
  setnames("V1", "PTSN10")

# Sector
PTSN10_s <- poblacion2010[P03 >= 23& TSP==1, sum(TSP), by = .(Sector_DMQ)] %>% 
  setnames("V1", "PTSN10")

# Grilla COD1000
PTSN10_1000 <- poblacion2010[P03 >= 23 & TSP==1, sum(TSP), by = .(COD1000)] %>% 
  setnames("V1", "PTSN10")

# Grilla COD500
PTSN10_500 <- poblacion2010[P03 >= 23 & TSP==1, sum(TSP), by = .(COD500)] %>% 
  setnames("V1", "PTSN10")

# Grilla H3_N8
PTSN10_N8 <- poblacion2010[P03 >= 23 & TSP==1, sum(TSP), by = .(H3_N8)] %>% 
  setnames("V1", "PTSN10")

# Grilla H3_N9
PTSN10_N9 <- poblacion2010[P03 >= 23 & TSP==1, sum(TSP), by = .(H3_N9)] %>% 
  setnames("V1", "PTSN10")

# 6. Guardar los resultados

wb <-  createWorkbook("PTSN10")

addWorksheet(wb, "PTSN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN10_c", x= PTSN10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN10_az", x= PTSN10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN10_pr", x= PTSN10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN10_s", x= PTSN10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN10_1000", x= PTSN10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN10_500", x= PTSN10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN10_N8", x= PTSN10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN10_N9", x= PTSN10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de poblacion de 23 años o más tituladas de educación superior en universidades o escuelas politécnicas 2010.xlsx")