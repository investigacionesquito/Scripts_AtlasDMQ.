# Dimensión: Socioeconomía

# Nombre del Indicador: 
#Población de 25 años y más Ocupada con secundario completo 

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
PSC_c <- poblacion2022[P03 >= 25, .N, by = .(PS, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(canton)] %>% 
  setnames("freq", "PSC") 

# Administración zonal 
PSC_az <- poblacion2022[P03 >= 25, .N, by = .(PS, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "PSC") 

# Parroquial
PSC_pr <- poblacion2022[P03 >= 25, .N, by = .(PS, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(parroquia)] %>% 
  setnames("freq", "PSC")

# Sector
PSC_s <- poblacion2022[P03 >= 25, .N, by = .(PS, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "PSC")

# Grilla COD1000
PSC_1000 <- poblacion2022[P03 >= 25, .N, by = .(PS, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD1000)] %>% 
  setnames("freq", "PSC")

# Grilla COD500
PSC_500 <- poblacion2022[P03 >= 25, .N, by = .(PS, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD500)] %>% 
  setnames("freq", "PSC")

# Grilla H3_N8
PSC_N8 <- poblacion2022[P03 >= 25, .N, by = .(PS, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N8)] %>% 
  setnames("freq", "PSC")

# Grilla H3_N9
PSC_N9 <- poblacion2022[P03 >= 25, .N, by = .(PS, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N9)] %>% 
  setnames("freq", "PSC")

## Socio Demográfico/Económico

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
PSC_c_s <- poblacion2022[P03 >= 25, .N, by = .(PS, P02,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P02)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(canton)] %>% 
  dcast(canton~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC_S1", ifelse(.x == "2", "PSC_S2", .x)))  

# Administración zonal - sexo
PSC_az_s <- poblacion2022[P03 >= 25, .N, by = .(PS, P02,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P02)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC_S1", ifelse(.x == "2", "PSC_S2", .x)))   

# Parroquia - Sexo 
PSC_pr_s <- poblacion2022[P03 >= 25, .N, by = .(PS, P02,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P02)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC_S1", ifelse(.x == "2", "PSC_S2", .x))) 

# Sector - Sexo
PSC_s_s <- poblacion2022[P03 >= 25, .N, by = .(PS, P02,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P02)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC_S1", ifelse(.x == "2", "PSC_S2", .x))) 

# Grilla COD1000 - Sexo 
PSC_1000_s <- poblacion2022[P03 >= 25, .N, by = .(PS, P02,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P02)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC_S1", ifelse(.x == "2", "PSC_S2", .x))) 


# Grilla COD500 - Sexo 
PSC_500_s <- poblacion2022[P03 >= 25, .N, by = .(PS, P02,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P02)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC_S1", ifelse(.x == "2", "PSC_S2", .x))) 

# Grilla H3_N8 - Sexo 
PSC_N8_s <- poblacion2022[P03 >= 25, .N, by = .(PS, P02,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P02)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC_S1", ifelse(.x == "2", "PSC_S2", .x))) 

# Grilla H3_N9 - Sexo 
PSC_N9_s <- poblacion2022[P03 >= 25, .N, by = .(PS, P02,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P02)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PSC_S1", ifelse(.x == "2", "PSC_S2", .x))) 

## Autodeterminación cultural 

#P11R; Cómo se identifica según su cultura y costumbres
## 1 Indígena
## 2 Afroecuatoriana/o, Afrodescendiente, Negra/o, Mulata/o
## 3 Montubia/o
## 4 Mestiza/o
## 5 Blanca/o
## 6 Otro

# Cantón - Autodeterminación
PSC_c_a <- poblacion2022[P03 >= 25, .N, by = .(PS, P11R,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P11R)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(canton)] %>% 
  dcast(canton~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PSC_A", 1:6))

# Administración zonal - Autodeterminación
PSC_az_a <- poblacion2022[P03 >= 25, .N, by = .(PS, P11R,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P11R)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PSC_A", 1:6))

# Parroquia - Autodeterminación
PSC_pr_a <- poblacion2022[P03 >= 25, .N, by = .(PS, P11R,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P11R)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(parroquia)] %>% 
  dcast(parroquia~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PSC_A", 1:6)) 

# Sector - Autodeterminación
PSC_s_a <- poblacion2022[P03 >= 25, .N, by = .(PS, P11R,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P11R)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PSC_A", 1:6))

# Grilla COD1000 - Autodeterminación
PSC_1000_a <- poblacion2022[P03 >= 25, .N, by = .(PS, P11R,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P11R)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD1000)] %>% 
  dcast(COD1000~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PSC_A", 1:6))

# Grilla COD500 - Autodeterminación
PSC_500_a <- poblacion2022[P03 >= 25, .N, by = .(PS, P11R,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P11R)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(COD500)] %>% 
  dcast(COD500~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PSC_A", 1:6))

# Grilla H3_N8 - Autodeterminación
PSC_N8_a <- poblacion2022[P03 >= 25, .N, by = .(PS, P11R,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P11R)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PSC_A", 1:6))

# Grilla H3_N9 - Autodeterminación
PSC_N9_a <- poblacion2022[P03 >= 25, .N, by = .(PS, P11R,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P11R)][PS == 1, ][
    , `:=`(N = NULL, PS = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("PSC_A", 1:6))

# 6. Guardar los resultados

wb <-  createWorkbook("PSC")

addWorksheet(wb, "PSC_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_c", x= PSC_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_az", x= PSC_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_pr", x= PSC_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_s", x= PSC_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_1000", x= PSC_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_500", x= PSC_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_N8", x= PSC_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_N9", x= PSC_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PSC_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_c_s", x= PSC_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_az_s", x= PSC_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_pr_s", x= PSC_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_s_s", x= PSC_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_1000_s", x= PSC_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_500_s", x= PSC_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_N8_s", x= PSC_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_N9_s", x= PSC_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PSC_c_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_c_a", x= PSC_c_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_az_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_az_a", x= PSC_az_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_pr_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_pr_a", x= PSC_pr_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_s_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_s_a", x= PSC_s_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_1000_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_1000_a", x= PSC_1000_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_500_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_500_a", x= PSC_500_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_N8_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_N8_a", x= PSC_N8_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSC_N9_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSC_N9_a", x= PSC_N9_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Población de 25 años y más Ocupada con secundario completo .xlsx")