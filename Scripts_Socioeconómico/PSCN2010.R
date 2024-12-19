# Dimensión: Socioeconomía

# Nombre del Indicador: 
#Cantidad de la población de 25 años y más Ocupada con secundario completo 2010

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
# 7 Educación Media / Bachillerato 
# 8 Ciclo Postbachillerato 
# 9 Superior 
# 10 Postgrado 
# 99 Se ignora

#Calculo de años de escolaridad para saber si tiene secundario completo, los años seran igual o mayor que 13 
poblacion2010[is.na(P23),P23:=99]
poblacion2010[is.na(P24) & P23>=2 ,P24:=99]

poblacion2010[,ANIOS:=NULL]
poblacion2010[, ANIOS := fcase(
  P23 == 1, 0,  # Ninguno
  P23 == 2, 0 + P24,  # Centro de alfabetización
  P23 == 2 & P24 %in% c(7:10), 99,  # Error o inconsistencias: 99
  P23 == 3, 1,  # Pre-escolar
  P23 == 4 & P24 != 99, 0 + P24,  # Primaria: años según P24 
  P23 == 5 & P24 != 99, 6 + P24,  # Secundario: 6 años de primaria + P24
  P23 == 6 & P24 != 99, 0 + P24,  # Educacion Básica años según P24 
  P23 == 7 & P24 != 99, 10 + P24,  # Educacion Media/Bachillerato: 10 básicos + P24
  P23 == 8 & P24 != 99, 13 + P24,  # Postbachillerato: 13 básicos + P24 
  P23 == 9 & P24 != 99, 13 + P24,  # Superior: 13 básicos + P24  
  P23 == 10 & P24 != 99, 18 + P24,  # Postgrado: 18 básicos y superior + P24
  P23 == 99 | P24 == 99, 99  # Valores faltantes o inconsistentes
)]

# Se debe crear el indicador de condición de actividad 

# P27 = Qué hizo la semana pasada
# 1 Trabajó al menos una hora
# 2 No trabajó pero si tiene trabajo
# 3 Al menos una hora fabricó algún producto o brindó algún servicio
# 4 Al menos una hora ayudó en algún negocio o trabajo de un familiar
# 5 Al menos una hora realizó labores agrícolas o cuidó animales
# 6 Es Cesante; Buscó trabajo habiendo trabajado antes y está disponible para trabajar
# 7 No trabajó

# P28 = Si no ha trabajado
# 1 Buscó trabajo por primera vez y está disponible para trabajar
# 2 Es rentista
# 3 Es jubilado o pensionista
# 4 Es estudiante
# 5 Realiza quehaceres del hogar
# 6 Le impide su discapacidad
# 7 Otro

# CONDACT1 = Condicion de actividad 
poblacion2010[, CONDACT1:=NULL]
poblacion2010[P03< 5, CONDACT1:=1] # 1 Menor de 5 años
poblacion2010[P27< 7, CONDACT1:=2] # 2 Ocupado
poblacion2010[P27==7 & P28==1, CONDACT1:=3] # 3 Desocupado
poblacion2010[P27==7 & P28>1, CONDACT1:=4] # 4 Fuera de la fuerza de trabajo

# Condiciones para calcular el indicador 
poblacion2010[, PS:=NULL]
poblacion2010[P03>=25, PS:=0]
poblacion2010[P03>=25 & P23>=7 & ANIOS>= 13 & CONDACT1==2, PS:=1]

# Desagregaciones

## Geográfico territorial

# Cantón
PSCN10_c <- poblacion2010[P03 >= 25 & PS==1, .N, by = .(canton)] %>% 
  setnames("N", "PSCN10") 

# Administración zonal 
PSCN10_az <- poblacion2010[P03 >= 25 & PS==1, .N, by = .(adm_zonal)] %>% 
  setnames("N", "PSCN10") 

# Parroquial
PSCN10_pr <- poblacion2010[P03 >= 25 & PS==1, .N, by = .(parroquia)]%>% 
  setnames("N", "PSCN10")

# Sector
PSCN10_s <- poblacion2010[P03 >= 25 & PS==1, .N, by = .(Sector_DMQ)] %>% 
  setnames("N", "PSCN10")

# Grilla COD1000
PSCN10_1000 <- poblacion2010[P03 >= 25 & PS==1, .N, by = .(COD1000)]%>% 
  setnames("N", "PSCN10")

# Grilla COD500
PSCN10_500 <- poblacion2010[P03 >= 25 & PS==1, .N, by = .(COD500)]%>% 
  setnames("N", "PSCN10")

# Grilla H3_N8
PSCN10_N8 <- poblacion2010[P03 >= 25 & PS==1, .N, by = .(H3_N8)] %>% 
  setnames("N", "PSCN10")

# Grilla H3_N9
PSCN10_N9 <- poblacion2010[P03 >= 25 & PS==1, .N, by = .(H3_N9)] %>% 
  setnames("N", "PSCN10")

# 6. Guardar los resultados

wb <-  createWorkbook("PSCN10")

addWorksheet(wb, "PSCN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN10_c", x= PSCN10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN10_az", x= PSCN10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN10_pr", x= PSCN10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN10_s", x= PSCN10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN10_1000", x= PSCN10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN10_500", x= PSCN10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN10_N8", x= PSCN10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSCN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSCN10_N9", x= PSCN10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de la población de 25 años y más Ocupada con secundario completo 2010.xlsx")