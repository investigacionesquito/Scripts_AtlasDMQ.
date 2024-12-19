# Dimensión: Educación

# Nombre del Indicador: 
# Cantidad de personas de 15 años de edad o más que no saben leer ni escribir

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

#P19 = Sabe leer y escribir
# 1 Sí
# 2 No

poblacion2022[P03 >=15, Analf:=0]
poblacion2022[P03 >=15 & P19==9, Analf:=9]
poblacion2022[P03 >=15 & P17R%between%c(1,5) & P19==2, Analf:=1]

# Desagregaciones

## Geográfico territorial

# Cantón
TAN_c <- poblacion2022[P03 >= 15 & Analf==1, sum(Analf), by = .(canton)] %>% 
  setnames("V1", "TAN") 

# Administración zonal 
TAN_az <- poblacion2022[P03 >= 15 & Analf==1, sum(Analf), by = .(adm_zonal)] %>% 
  setnames("V1", "TAN") 

# Parroquial
TAN_pr <- poblacion2022[P03 >= 15 & Analf==1, sum(Analf), by = .(parroquia)] %>% 
  setnames("V1", "TAN")

# Sector
TAN_s <- poblacion2022[P03 >= 15 & Analf==1, sum(Analf), by = .(Sector_DMQ)] %>% 
  setnames("V1", "TAN")

# Grilla COD1000
TAN_1000 <- poblacion2022[P03 >= 15 & Analf==1, sum(Analf), by = .(COD1000)] %>% 
  setnames("V1", "TAN")

# Grilla COD500
TAN_500 <- poblacion2022[P03 >= 15 & Analf==1, sum(Analf), by = .(COD500)] %>% 
  setnames("V1", "TAN")

# Grilla H3_N8
TAN_N8 <- poblacion2022[P03 >= 15 & Analf==1, sum(Analf), by = .(H3_N8)] %>% 
  setnames("V1", "TAN")

# Grilla H3_N9
TAN_N9 <- poblacion2022[P03 >= 15 & Analf==1, sum(Analf), by = .(H3_N9)] %>% 
  setnames("V1", "TAN")

# 6. Guardar los resultados

wb <-  createWorkbook("TAN")

addWorksheet(wb, "TAN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TAN_c", x= TAN_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TAN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TAN_az", x= TAN_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TAN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TAN_pr", x= TAN_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TAN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TAN_s", x= TAN_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TAN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TAN_1000", x= TAN_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TAN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TAN_500", x= TAN_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TAN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TAN_N8", x= TAN_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TAN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TAN_N9", x= TAN_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Cantidad de personas de 15 años de edad o más que no saben leer ni escribir.xlsx")