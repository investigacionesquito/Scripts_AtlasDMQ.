# Dimensión: Educación

# Nombre del Indicador: 
#Porcentaje de población de 21 años o más titulada de educación técnica o tecnológica superior en institutos superiores técnicos y tecnológicos.
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

poblacion2022[, PT:=NULL]
poblacion2022[P03>=21, PT:=0]
poblacion2022[(P03>=21 & ((P17R==99 & P20==1) | (P20==9))), PT:=9]
poblacion2022[P03>=21 & P17R==8 & P20==1, PT:=1]

# Desagregaciones

## Geográfico territorial

# Cantón
PTT_c <- poblacion2022[P03 >= 21, .N, by = .(PT, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(canton)] %>% 
  setnames("freq", "PTT") 

# Administración zonal 
PTT_az <- poblacion2022[P03 >= 21, .N, by = .(PT, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "PTT") 

# Parroquial
PTT_pr <- poblacion2022[P03 >= 21, .N, by = .(PT, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(parroquia)] %>% 
  setnames("freq", "PTT")

# Sector
PTT_s <- poblacion2022[P03 >= 21, .N, by = .(PT, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "PTT")

# Grilla COD1000
PTT_1000 <- poblacion2022[P03 >= 21, .N, by = .(PT, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(COD1000)] %>% 
  setnames("freq", "PTT")

# Grilla COD500
PTT_500 <- poblacion2022[P03 >= 21, .N, by = .(PT, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(COD500)] %>% 
  setnames("freq", "PTT")

# Grilla H3_N8
PTT_N8 <- poblacion2022[P03 >= 21, .N, by = .(PT, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(H3_N8)] %>% 
  setnames("freq", "PTT")

# Grilla H3_N9
PTT_N9 <- poblacion2022[P03 >= 21, .N, by = .(PT, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(H3_N9)] %>% 
  setnames("freq", "PTT")

## Socio Demográfico/Económico

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
PTT_c_s <- poblacion2022[P03 >= 21, .N, by = .(PT, P02,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P02)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(canton)] %>% 
  dcast(canton~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTT_S1", ifelse(.x == "2", "PTT_S2", .x)))  

# Administración zonal - sexo
PTT_az_s <- poblacion2022[P03 >= 21, .N, by = .(PT, P02,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P02)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTT_S1", ifelse(.x == "2", "PTT_S2", .x)))   

# Parroquia - Sexo 
PTT_pr_s <- poblacion2022[P03 >= 21, .N, by = .(PT, P02,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P02)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTT_S1", ifelse(.x == "2", "PTT_S2", .x))) 

# Sector - Sexo
PTT_s_s <- poblacion2022[P03 >= 21, .N, by = .(PT, P02,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P02)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTT_S1", ifelse(.x == "2", "PTT_S2", .x))) 

# Grilla COD1000 - Sexo 
PTT_1000_s <- poblacion2022[P03 >= 21, .N, by = .(PT, P02,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P02)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTT_S1", ifelse(.x == "2", "PTT_S2", .x))) 


# Grilla COD500 - Sexo 
PTT_500_s <- poblacion2022[P03 >= 21, .N, by = .(PT, P02,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P02)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTT_S1", ifelse(.x == "2", "PTT_S2", .x))) 

# Grilla H3_N8 - Sexo 
PTT_N8_s <- poblacion2022[P03 >= 21, .N, by = .(PT, P02,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P02)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTT_S1", ifelse(.x == "2", "PTT_S2", .x))) 

# Grilla H3_N9 - Sexo 
PTT_N9_s <- poblacion2022[P03 >= 21, .N, by = .(PT, P02,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P02)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PTT_S1", ifelse(.x == "2", "PTT_S2", .x))) 

## Autodeterminación cultural 

#P11R; Cómo se identifica según su cultura y costumbres
## 1 Indígena
## 2 Afroecuatoriana/o, Afrodescendiente, Negra/o, Mulata/o
## 3 Montubia/o
## 4 Mestiza/o
## 5 Blanca/o
## 6 Otro

# Cantón - Autodeterminación
PTT_c_a <- poblacion2022[P03 >= 21, .N, by = .(PT, P11R,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P11R)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(canton)] %>% 
  dcast(canton~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTT_A", 1:6))

# Administración zonal - Autodeterminación
PTT_az_a <- poblacion2022[P03 >= 21, .N, by = .(PT, P11R,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P11R)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTT_A", 1:6))

# Parroquia - Autodeterminación
PTT_pr_a <- poblacion2022[P03 >= 21, .N, by = .(PT, P11R,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P11R)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(parroquia)] %>% 
  dcast(parroquia~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTT_A", 1:6)) 

# Sector - Autodeterminación
PTT_s_a <- poblacion2022[P03 >= 21, .N, by = .(PT, P11R,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P11R)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTT_A", 1:6))

# Grilla COD1000 - Autodeterminación
PTT_1000_a <- poblacion2022[P03 >= 21, .N, by = .(PT, P11R,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P11R)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(COD1000)] %>% 
  dcast(COD1000~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTT_A", 1:6))

# Grilla COD500 - Autodeterminación
PTT_500_a <- poblacion2022[P03 >= 21, .N, by = .(PT, P11R,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P11R)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(COD500)] %>% 
  dcast(COD500~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTT_A", 1:6))

# Grilla H3_N8 - Autodeterminación
PTT_N8_a <- poblacion2022[P03 >= 21, .N, by = .(PT, P11R,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P11R)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTT_A", 1:6))

# Grilla H3_N9 - Autodeterminación
PTT_N9_a <- poblacion2022[P03 >= 21, .N, by = .(PT, P11R,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P11R)][PT == 1, ][
    , `:=`(N = NULL, PT = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PTT_A", 1:6))

# 6. Guardar los resultados

wb <-  createWorkbook("PTT")

addWorksheet(wb, "PTT_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_c", x= PTT_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_az", x= PTT_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_pr", x= PTT_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_s", x= PTT_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_1000", x= PTT_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_500", x= PTT_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_N8", x= PTT_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_N9", x= PTT_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PTT_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_c_s", x= PTT_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_az_s", x= PTT_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_pr_s", x= PTT_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_s_s", x= PTT_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_1000_s", x= PTT_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_500_s", x= PTT_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_N8_s", x= PTT_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_N9_s", x= PTT_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PTT_c_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_c_a", x= PTT_c_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_az_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_az_a", x= PTT_az_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_pr_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_pr_a", x= PTT_pr_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_s_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_s_a", x= PTT_s_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_1000_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_1000_a", x= PTT_1000_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_500_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_500_a", x= PTT_500_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_N8_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_N8_a", x= PTT_N8_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PTT_N9_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PTT_N9_a", x= PTT_N9_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Población de 21 años o más titulada de educación técnica o tecnológica superior en institutos superiores técnicos y tecnológicos..xlsx")