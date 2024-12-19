# Dimensión: Educación

# Nombre del Indicador: 
#Porcentaje de niñas/os de 3 y 4 años que asisten a Educación Inicial/ Preescolar o SAFPI.

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

# P15 = Asiste actualmente a educación regular o formal
# 1 Sí
# 2 No

poblacion2022[, AEI:=NULL]
poblacion2022[P03%between%c(3,4), AEI:=0]
poblacion2022[P03%between%c(3,4) & P15==1 & P17R==3 , AEI:=1]
poblacion2022[P03%between%c(3,4) & ((P15==1 & P17R==99) | (P15==9)), AEI:=9]

# Desagregaciones

## Geográfico territorial

# Cantón
NEI_c <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(canton)] %>% 
  setnames("freq", "NEI") 

# Administración zonal 
NEI_az <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "NEI") 

# Parroquial
NEI_pr <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(parroquia)] %>% 
  setnames("freq", "NEI")

# Sector
NEI_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "NEI")

# Grilla COD1000
NEI_1000 <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(COD1000)] %>% 
  setnames("freq", "NEI")

# Grilla COD500
NEI_500 <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(COD500)] %>% 
  setnames("freq", "NEI")

# Grilla H3_N8
NEI_N8 <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(H3_N8)] %>% 
  setnames("freq", "NEI")

# Grilla H3_N9
NEI_N9 <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(H3_N9)] %>% 
  setnames("freq", "NEI")

## Socio Demográfico/Económico

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
NEI_c_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, P02,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P02)][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(canton)] %>% 
  dcast(canton~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NEI_S1", ifelse(.x == "2", "NEI_S2", .x)))  

# Administración zonal - sexo
NEI_az_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, P02,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P02)][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NEI_S1", ifelse(.x == "2", "NEI_S2", .x)))   

# Parroquia - Sexo 
NEI_pr_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, P02,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P02)][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NEI_S1", ifelse(.x == "2", "NEI_S2", .x))) 

# Sector - Sexo
NEI_s_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, P02,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P02)][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NEI_S1", ifelse(.x == "2", "NEI_S2", .x))) 

# Grilla COD1000 - Sexo 
NEI_1000_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, P02,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P02)][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NEI_S1", ifelse(.x == "2", "NEI_S2", .x))) 


# Grilla COD500 - Sexo 
NEI_500_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, P02,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P02)][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NEI_S1", ifelse(.x == "2", "NEI_S2", .x))) 


# Grilla H3_N8 - Sexo 
NEI_N8_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, P02,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P02)][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NEI_S1", ifelse(.x == "2", "NEI_S2", .x))) 


# Grilla H3_N9 - Sexo 
NEI_N9_s <- poblacion2022[P03%between%c(3,4), .N, by = .(AEI, P02,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P02)][AEI == 1, ][
    , `:=`(N = NULL, AEI = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NEI_S1", ifelse(.x == "2", "NEI_S2", .x))) 

# 6. Guardar los resultados

wb <-  createWorkbook("NEI")

addWorksheet(wb, "NEI_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_c", x= NEI_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_az", x= NEI_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_pr", x= NEI_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_s", x= NEI_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_1000", x= NEI_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_500", x= NEI_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_N8", x= NEI_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_N9", x= NEI_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "NEI_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_c_s", x= NEI_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_az_s", x= NEI_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_pr_s", x= NEI_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_s_s", x= NEI_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_1000_s", x= NEI_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_500_s", x= NEI_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_N8_s", x= NEI_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEI_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEI_N9_s", x= NEI_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Niñas-os de 3 y 4 años que asisten a Educación Inicial-Preescolar o SAFPI.xlsx")