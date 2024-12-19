# Dimensión: Socioeconomía

# Nombre del Indicador: 
#Cantidad de la población de 25 años y más Ocupada con secundario completo 

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

# CONDACT1 = Condicion de actividad (desagregada)
# 1 Menor de 5 años
# 2 Ocupado
# 3 Desocupado
# 4 Fuera de la fuerza de trabajo

poblacion2022[, PS:=NULL]
poblacion2022[P03>=25, PS:=0]
poblacion2022[P03>=25 & P17R>=6 & CONDACT1==2 & P20==1, PS:=1]

# Desagregaciones

## Geográfico territorial

# Cantón
PSCN_c <- poblacion2022[P03 >= 25 & PS==1, .N, by = .(canton)] %>% 
  setnames("N", "PSCN") 

# Administración zonal 
PSCN_az <- poblacion2022[P03 >= 25 & PS==1, .N, by = .(adm_zonal)] %>% 
  setnames("N", "PSCN") 

# Parroquial
PSCN_pr <- poblacion2022[P03 >= 25 & PS==1, .N, by = .(parroquia)]%>% 
  setnames("N", "PSCN")

# Sector
PSCN_s <- poblacion2022[P03 >= 25 & PS==1, .N, by = .(Sector_DMQ)] %>% 
  setnames("N", "PSCN")

# Grilla COD1000
PSCN_1000 <- poblacion2022[P03 >= 25 & PS==1, .N, by = .(COD1000)]%>% 
  setnames("N", "PSCN")

# Grilla COD500
PSCN_500 <- poblacion2022[P03 >= 25 & PS==1, .N, by = .(COD500)]%>% 
  setnames("N", "PSCN")

# Grilla H3_N8
PSCN_N8 <- poblacion2022[P03 >= 25 & PS==1, .N, by = .(H3_N8)] %>% 
  setnames("N", "PSCN")

# Grilla H3_N9
PSCN_N9 <- poblacion2022[P03 >= 25 & PS==1, .N, by = .(H3_N9)] %>% 
  setnames("N", "PSCN")

# 6. Guardar los resultados

wb <-  createWorkbook("PSCN")

addWorksheet(wb, "PSCN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN_c", x= PSCN_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN_az", x= PSCN_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN_pr", x= PSCN_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN_s", x= PSCN_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN_1000", x= PSCN_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN_500", x= PSCN_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN_N8", x= PSCN_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN_N9", x= PSCN_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de la población de 25 años y más Ocupada con secundario completo .xlsx")