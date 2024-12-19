# Dimensión: Educación

# Nombre del Indicador: 
# Cantidad de la población de 23 años o más tituladas de educación superior en universidades o escuelas politécnicas.
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

#P03 = Años cumplidos 

#P17R = Nivel de instrucción más alto al que asiste o asistió: 
## 1 Ninguno
## 2 Centro de desarrollo infantil, guardería
## 3 Educación inicial, preescolar, SAFPI
## 4 Alfabetización, Post Alfabetización
## 5 Educación General Básica
## 6 Bachillerato
## 7 Ciclo Postbachillerato (No superior)
## 8 Educación Técnica o Tecnológica Superior (institutos superiores técnicos y tecnológicos)
## 9 Educación Superior  (universidades, escuelas politécnicas)
## 10 Maestría/Especialización
## 11 PHD, Doctorado

#P20 = Obtuvo algún título en el nivel que indica
# 1 Sí
# 2 No
# 9 Se ignora

poblacion2022[,TSP:=NULL]
poblacion2022[P03>=23,TSP:=0]
poblacion2022[P03>=23 & ((P17R==99 & P20==1) | (P20==9)), TSP:=9]
poblacion2022[P03>=23 & P17R==9 & P20==1, TSP:=1]
poblacion2022[P03>=23 & P17R==10, TSP:=1]
poblacion2022[P03>=23 & P17R==11, TSP:=1] 

# Desagregaciones

## Geográfico territorial

# Cantón

PTSN_c <- poblacion2022[P03 >= 23 & TSP==1, sum(TSP), by = .(canton)] %>% 
  setnames("V1", "PTSN") 

# Administración zonal 
PTSN_az <- poblacion2022[P03 >= 23 & TSP==1, sum(TSP), by = .(adm_zonal)] %>% 
  setnames("V1", "PTSN") 

# Parroquial
PTSN_pr <- poblacion2022[P03 >= 23 & TSP==1, sum(TSP), by = .(parroquia)] %>% 
  setnames("V1", "PTSN")

# Sector
PTSN_s <- poblacion2022[P03 >= 23& TSP==1, sum(TSP), by = .(Sector_DMQ)] %>% 
  setnames("V1", "PTSN")

# Grilla COD1000
PTSN_1000 <- poblacion2022[P03 >= 23 & TSP==1, sum(TSP), by = .(COD1000)] %>% 
  setnames("V1", "PTSN")

# Grilla COD500
PTSN_500 <- poblacion2022[P03 >= 23 & TSP==1, sum(TSP), by = .(COD500)] %>% 
  setnames("V1", "PTSN")

# Grilla H3_N8
PTSN_N8 <- poblacion2022[P03 >= 23 & TSP==1, sum(TSP), by = .(H3_N8)] %>% 
  setnames("V1", "PTSN")

# Grilla H3_N9
PTSN_N9 <- poblacion2022[P03 >= 23 & TSP==1, sum(TSP), by = .(H3_N9)] %>% 
  setnames("V1", "PTSN")

# 6. Guardar los resultados

wb <-  createWorkbook("PTSN")

addWorksheet(wb, "PTSN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN_c", x= PTSN_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN_az", x= PTSN_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN_pr", x= PTSN_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN_s", x= PTSN_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN_1000", x= PTSN_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN_500", x= PTSN_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN_N8", x= PTSN_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTSN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTSN_N9", x= PTSN_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de 23 años o más tituladas de educación superior en universidades o escuelas politécnicas .xlsx")