# Dimensión: Socioeconomía

# Nombre del Indicador: 
#Población de 25 años y más ocupada con secundario completo 2010

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
PSC10_c <- poblacion2010[P03>= 25, .N, by = .(PS, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(canton)] %>% 
  setnames("freq", "PSC10") 

# Administración zonal 
PSC10_az <- poblacion2010[P03 >= 25, .N, by = .(PS, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "PSC10") 

# Parroquial
PSC10_pr <- poblacion2010[P03 >= 25, .N, by = .(PS, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(parroquia)] %>% 
  setnames("freq", "PSC10")

# Sector
PSC10_s <- poblacion2010[P03 >= 25, .N, by = .(PS, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "PSC10")

# Grilla COD1000
PSC10_1000 <- poblacion2010[P03 >= 25, .N, by = .(PS, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD1000)] %>% 
  setnames("freq", "PSC10")

# Grilla COD500
PSC10_500 <- poblacion2010[P03 >= 25, .N, by = .(PS, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD500)] %>% 
  setnames("freq", "PSC10")

# Grilla H3_N8
PSC10_N8 <- poblacion2010[P03 >= 25, .N, by = .(PS, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N8)] %>% 
  setnames("freq", "PSC10")

# Grilla H3_N9
PSC10_N9 <- poblacion2010[P03 >= 25, .N, by = .(PS, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N9)] %>% 
  setnames("freq", "PSC10")

## Socio Demográfico/Económico

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
PSC10_c_s <- poblacion2010[P03 >= 25, .N, by = .(PS, P01,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P01)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(canton)] %>% 
  dcast(canton~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC10_S1", ifelse(.x == "2", "PSC10_S2", .x)))  

# Administración zonal - sexo
PSC10_az_s <- poblacion2010[P03 >= 25, .N, by = .(PS, P01,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P01)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC10_S1", ifelse(.x == "2", "PSC10_S2", .x)))   

# Parroquia - Sexo 
PSC10_pr_s <- poblacion2010[P03 >= 25, .N, by = .(PS, P01,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P01)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC10_S1", ifelse(.x == "2", "PSC10_S2", .x))) 

# Sector - Sexo
PSC10_s_s <- poblacion2010[P03 >= 25, .N, by = .(PS, P01,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P01)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC10_S1", ifelse(.x == "2", "PSC10_S2", .x))) 

# Grilla COD1000 - Sexo 
PSC10_1000_s <- poblacion2010[P03 >= 25, .N, by = .(PS, P01,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P01)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC10_S1", ifelse(.x == "2", "PSC10_S2", .x))) 


# Grilla COD500 - Sexo 
PSC10_500_s <- poblacion2010[P03 >= 25, .N, by = .(PS, P01,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P01)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC10_S1", ifelse(.x == "2", "PSC10_S2", .x))) 

# Grilla H3_N8 - Sexo 
PSC10_N8_s <- poblacion2010[P03 >= 25, .N, by = .(PS, P01,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P01)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC10_S1", ifelse(.x == "2", "PSC10_S2", .x))) 

# Grilla H3_N9 - Sexo 
PSC10_N9_s <- poblacion2010[P03 >= 25, .N, by = .(PS, P01,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P01)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC10_S1", ifelse(.x == "2", "PSC10_S2", .x))) 

# 6. Guardar los resultados

wb <-  createWorkbook("PSC10")

addWorksheet(wb, "PSC10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_c", x= PSC10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_az", x= PSC10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_pr", x= PSC10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_s", x= PSC10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_1000", x= PSC10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_500", x= PSC10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_N8", x= PSC10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_N9", x= PSC10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PSC10_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_c_s", x= PSC10_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_az_s", x= PSC10_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_pr_s", x= PSC10_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_s_s", x= PSC10_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_1000_s", x= PSC10_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_500_s", x= PSC10_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_N8_s", x= PSC10_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC10_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC10_N9_s", x= PSC10_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Población de 25 años y más Ocupada con secundario completo 2010.xlsx")