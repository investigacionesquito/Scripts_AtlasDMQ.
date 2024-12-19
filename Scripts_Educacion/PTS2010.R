# Dimensión: Educación

# Nombre del Indicador: 
#Porcentaje de la población de 23 años o más tituladas de educación superior en universidades o escuelas politécnicas 2010.

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
PTS10_c <- poblacion2010[P03 >= 23, .N, by = .(TSP, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][TSP == 1, ][
      , `:=`(N = NULL, TSP = NULL)][order(canton)] %>% 
  setnames("freq", "PTS10") 

# Administración zonal 
PTS10_az <- poblacion2010[P03 >= 23, .N, by = .(TSP, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "PTS10") 

# Parroquial
PTS10_pr <- poblacion2010[P03 >= 23, .N, by = .(TSP, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(parroquia)] %>% 
  setnames("freq", "PTS10")

# Sector
PTS10_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "PTS10")

# Grilla COD1000
PTS10_1000 <- poblacion2010[P03 >= 23, .N, by = .(TSP, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD1000)] %>% 
  setnames("freq", "PTS10")

# Grilla COD500
PTS10_500 <- poblacion2010[P03 >= 23, .N, by = .(TSP, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD500)] %>% 
  setnames("freq", "PTS10")

# Grilla H3_N8
PTS10_N8 <- poblacion2010[P03 >= 23, .N, by = .(TSP, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N8)] %>% 
  setnames("freq", "PTS10")

# Grilla H3_N9
PTS10_N9 <- poblacion2010[P03 >= 23, .N, by = .(TSP, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N9)] %>% 
  setnames("freq", "PTS10")

## Socio Demográfico/Económico

# P01: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
PTS10_c_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, P01,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P01)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(canton)] %>% 
  dcast(canton~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS10_S1", ifelse(.x == "2", "PTS10_S2", .x)))  

# Administración zonal - sexo
PTS10_az_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, P01,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P01)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS10_S1", ifelse(.x == "2", "PTS10_S2", .x)))   

# Parroquia - Sexo 
PTS10_pr_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, P01,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P01)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS10_S1", ifelse(.x == "2", "PTS10_S2", .x))) 

# Sector - Sexo
PTS10_s_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, P01,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P01)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS10_S1", ifelse(.x == "2", "PTS10_S2", .x))) 

# Grilla COD1000 - Sexo 
PTS10_1000_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, P01,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P01)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS10_S1", ifelse(.x == "2", "PTS10_S2", .x))) 
 
# Grilla COD500 - Sexo 
PTS10_500_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, P01,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P01)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS10_S1", ifelse(.x == "2", "PTS10_S2", .x))) 
  
# Grilla H3_N8 - Sexo 
PTS10_N8_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, P01,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P01)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS10_S1", ifelse(.x == "2", "PTS10_S2", .x))) 


# Grilla H3_N9 - Sexo 
PTS10_N9_s <- poblacion2010[P03 >= 23, .N, by = .(TSP, P01,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P01)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS10_S1", ifelse(.x == "2", "PTS10_S2", .x))) 


# 6. Guardar los resultados

wb <-  createWorkbook("PTS10")

addWorksheet(wb, "PTS10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_c", x= PTS10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_az", x= PTS10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_pr", x= PTS10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_s", x= PTS10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_1000", x= PTS10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_500", x= PTS10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_N8", x= PTS10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_N9", x= PTS10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PTS10_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_c_s", x= PTS10_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_az_s", x= PTS10_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_pr_s", x= PTS10_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_s_s", x= PTS10_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_1000_s", x= PTS10_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_500_s", x= PTS10_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_N8_s", x= PTS10_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS10_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS10_N9_s", x= PTS10_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Población de 23 años o más tituladas de educación superior en universidades o escuelas politécnicas 2010.xlsx")