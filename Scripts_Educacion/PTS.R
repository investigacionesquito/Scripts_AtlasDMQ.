# Dimensión: Educación

# Nombre del Indicador: 
#Porcentaje de la población de 23 años o más tituladas de educación superior en universidades o escuelas politécnicas.
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
PTS_c <- poblacion2022[P03 >= 23, .N, by = .(TSP, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][TSP == 1, ][
      , `:=`(N = NULL, TSP = NULL)][order(canton)] %>% 
  setnames("freq", "PTS") 

# Administración zonal 
PTS_az <- poblacion2022[P03 >= 23, .N, by = .(TSP, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "PTS") 

# Parroquial
PTS_pr <- poblacion2022[P03 >= 23, .N, by = .(TSP, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(parroquia)] %>% 
  setnames("freq", "PTS")

# Sector
PTS_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "PTS")

# Grilla COD1000
PTS_1000 <- poblacion2022[P03 >= 23, .N, by = .(TSP, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD1000)] %>% 
  setnames("freq", "PTS")

# Grilla COD500
PTS_500 <- poblacion2022[P03 >= 23, .N, by = .(TSP, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD500)] %>% 
  setnames("freq", "PTS")

# Grilla H3_N8
PTS_N8 <- poblacion2022[P03 >= 23, .N, by = .(TSP, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N8)] %>% 
  setnames("freq", "PTS")

# Grilla H3_N9
PTS_N9 <- poblacion2022[P03 >= 23, .N, by = .(TSP, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N9)] %>% 
  setnames("freq", "PTS")

## Socio Demográfico/Económico

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
PTS_c_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, P02,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P02)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(canton)] %>% 
  dcast(canton~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS_S1", ifelse(.x == "2", "PTS_S2", .x)))  

# Administración zonal - sexo
PTS_az_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, P02,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P02)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS_S1", ifelse(.x == "2", "PTS_S2", .x)))   

# Parroquia - Sexo 
PTS_pr_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, P02,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P02)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS_S1", ifelse(.x == "2", "PTS_S2", .x))) 

# Sector - Sexo
PTS_s_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, P02,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P02)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS_S1", ifelse(.x == "2", "PTS_S2", .x))) 

# Grilla COD1000 - Sexo 
PTS_1000_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, P02,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P02)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS_S1", ifelse(.x == "2", "PTS_S2", .x))) 
 
# Grilla COD500 - Sexo 
PTS_500_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, P02,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P02)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS_S1", ifelse(.x == "2", "PTS_S2", .x))) 
  
# Grilla H3_N8 - Sexo 
PTS_N8_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, P02,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P02)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS_S1", ifelse(.x == "2", "PTS_S2", .x))) 


# Grilla H3_N9 - Sexo 
PTS_N9_s <- poblacion2022[P03 >= 23, .N, by = .(TSP, P02,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P02)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTS_S1", ifelse(.x == "2", "PTS_S2", .x))) 


## Autodeterminación cultural 

#P11R; Cómo se identifica según su cultura y costumbres
## 1 Indígena
## 2 Afroecuatoriana/o, Afrodescendiente, Negra/o, Mulata/o
## 3 Montubia/o
## 4 Mestiza/o
## 5 Blanca/o
## 6 Otro

# Cantón - Autodeterminación
PTS_c_a <- poblacion2022[P03 >= 23, .N, by = .(TSP, P11R,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P11R)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(canton)] %>% 
  dcast(canton~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTS_A", 1:6))

# Administración zonal - Autodeterminación
PTS_az_a <- poblacion2022[P03 >= 23, .N, by = .(TSP, P11R,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P11R)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTS_A", 1:6))

# Parroquia - Autodeterminación
PTS_pr_a <- poblacion2022[P03 >= 23, .N, by = .(TSP, P11R,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P11R)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(parroquia)] %>% 
  dcast(parroquia~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTS_A", 1:6)) 

# Sector - Autodeterminación
PTS_s_a <- poblacion2022[P03 >= 23, .N, by = .(TSP, P11R,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P11R)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTS_A", 1:6))

# Grilla COD1000 - Autodeterminación
PTS_1000_a <- poblacion2022[P03 >= 23, .N, by = .(TSP, P11R,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P11R)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD1000)] %>% 
  dcast(COD1000~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTS_A", 1:6))

# Grilla COD500 - Autodeterminación
PTS_500_a <- poblacion2022[P03 >= 23, .N, by = .(TSP, P11R,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P11R)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(COD500)] %>% 
  dcast(COD500~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTS_A", 1:6))

# Grilla H3_N8 - Autodeterminación
PTS_N8_a <- poblacion2022[P03 >= 23, .N, by = .(TSP, P11R,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P11R)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTS_A", 1:6))

# Grilla H3_N9 - Autodeterminación
PTS_N9_a <- poblacion2022[P03 >= 23, .N, by = .(TSP, P11R,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P11R)][TSP == 1, ][
    , `:=`(N = NULL, TSP = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTS_A", 1:6))

# 6. Guardar los resultados

wb <-  createWorkbook("PTS")

addWorksheet(wb, "PTS_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_c", x= PTS_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_az", x= PTS_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_pr", x= PTS_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_s", x= PTS_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_1000", x= PTS_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_500", x= PTS_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_N8", x= PTS_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_N9", x= PTS_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PTS_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_c_s", x= PTS_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_az_s", x= PTS_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_pr_s", x= PTS_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_s_s", x= PTS_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_1000_s", x= PTS_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_500_s", x= PTS_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_N8_s", x= PTS_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_N9_s", x= PTS_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PTS_c_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_c_a", x= PTS_c_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_az_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_az_a", x= PTS_az_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_pr_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_pr_a", x= PTS_pr_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_s_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_s_a", x= PTS_s_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_1000_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_1000_a", x= PTS_1000_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_500_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_500_a", x= PTS_500_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_N8_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_N8_a", x= PTS_N8_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTS_N9_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTS_N9_a", x= PTS_N9_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Población de 23 años o más tituladas de educación superior en universidades o escuelas politécnicas .xlsx")