# Dimensión: Educación

# Nombre del Indicador: 
# Tasa de analfabetismo de la población de 15 años o más 2010.

# Número de personas que no saben leer y/o escribir de 15 años o más, expresado como porcentaje de la población total de la edad dereferencia. 

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

#P19 = Sabe leer y escribir
# 1 Sí
# 2 No

poblacion2010[P03 >=15, Analf:=0]
poblacion2010[P03 >=15 & P19==9, Analf:=9]
poblacion2010[P03 >=15 & P23%between%c(1,6) & P19==2, Analf:=1]

# Desagregaciones

## Geográfico territorial

# Cantón
TA10_c <- poblacion2010[P03 >= 15, .N, by = .(Analf, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(canton)] %>% 
  setnames("freq", "TA10") 

# Administración zonal 
TA10_az <- poblacion2010[P03 >= 15, .N, by = .(Analf, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "TA10") 

# Parroquial
TA10_pr <- poblacion2010[P03 >= 15, .N, by = .(Analf, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(parroquia)] %>% 
  setnames("freq", "TA10")

# Sector
TA10_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "TA10")

# Grilla COD1000
TA10_1000 <- poblacion2010[P03 >= 15, .N, by = .(Analf, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(COD1000)] %>% 
  setnames("freq", "TA10")

# Grilla COD500
TA10_500 <- poblacion2010[P03 >= 15, .N, by = .(Analf, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(COD500)] %>% 
  setnames("freq", "TA10")

# Grilla H3_N8
TA10_N8 <- poblacion2010[P03 >= 15, .N, by = .(Analf, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(H3_N8)] %>% 
  setnames("freq", "TA10")

# Grilla H3_N9
TA10_N9 <- poblacion2010[P03 >= 15, .N, by = .(Analf, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(H3_N9)] %>% 
  setnames("freq", "TA10")

## Socio Demográfico/Económico

# P01: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
TA10_c_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, P01,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P01)][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(canton)] %>% 
  dcast(canton~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "TA10_S1", ifelse(.x == "2", "TA10_S2", .x)))  

# Administración zonal - sexo
TA10_az_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, P01,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P01)][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "TA10_S1", ifelse(.x == "2", "TA10_S2", .x)))   

# Parroquia - Sexo 
TA10_pr_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, P01,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P01)][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "TA10_S1", ifelse(.x == "2", "TA10_S2", .x))) 

# Sector - Sexo
TA10_s_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, P01,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P01)][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "TA10_S1", ifelse(.x == "2", "TA10_S2", .x))) 

# Grilla COD1000 - Sexo 
TA10_1000_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, P01,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P01)][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "TA10_S1", ifelse(.x == "2", "TA10_S2", .x))) 

# Grilla COD500 - Sexo 
TA10_500_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, P01,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P01)][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "TA10_S1", ifelse(.x == "2", "TA10_S2", .x))) 

# Grilla H3_N8 - Sexo 
TA10_N8_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, P01,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P01)][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "TA10_S1", ifelse(.x == "2", "TA10_S2", .x))) 

# Grilla H3_N9 - Sexo 
TA10_N9_s <- poblacion2010[P03 >= 15, .N, by = .(Analf, P01,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P01)][Analf == 1, ][
    , `:=`(N = NULL, Analf = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "TA10_S1", ifelse(.x == "2", "TA10_S2", .x))) 

# 6. Guardar los resultados

wb <-  createWorkbook("TA")

addWorksheet(wb, "TA10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_c", x= TA10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_az", x= TA10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_pr", x= TA10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_s", x= TA10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_1000", x= TA10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_500", x= TA10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_N8", x= TA10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_N9", x= TA10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "TA10_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_c_s", x= TA10_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_az_s", x= TA10_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_pr_s", x= TA10_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_s_s", x= TA10_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_1000_s", x= TA10_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_500_s", x= TA10_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_N8_s", x= TA10_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "TA10_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "TA10_N9_s", x= TA10_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Tasa de analfabetismo de la población de 15 años o más 2010.xlsx")