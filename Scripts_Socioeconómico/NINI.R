# Dimensión: Socioeconomía

# Nombre del Indicador: 
#Poblacion de 14 a 24 años que no estudio ni trabaja NINI
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

#P15 = Asiste actualmente a educación regular o formal
# 1 Sí
# 2 No

#P22 = La semana pasada: 
# 1 Trabajó al menos una hora para generar un ingreso
# 2 Realizó algún trabajo ocasional (cachuelo o chaucha) por un pago
# 3 Atendió un negocio propio
# 4 Ayudó en algún negocio o empleo de algún miembro de su hogar
# 5 No trabajó, pero SÍ tiene un trabajo al que seguro va a volver (por vacaciones, enfermedad, etc.)
# 6 Hizo o ayudó en labores agrícolas, cría de animales o pesca
# 7 No trabajó

poblacion2022[, NI:=NULL]
poblacion2022[P03 >= 14 & P03 <= 24, NI := 0]
poblacion2022[P03 >= 14 & P03 <= 24 & P15 == 2 & P22 == 7, NI:=1]

# Desagregaciones

## Geográfico territorial

# Cantón
NINI_c <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(canton)] %>% 
  setnames("freq", "NINI") 

# Administración zonal 
NINI_az <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "NINI") 

# Parroquial
NINI_pr <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(parroquia)] %>% 
  setnames("freq", "NINI")

# Sector
NINI_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "NINI")

# Grilla COD1000
NINI_1000 <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(COD1000)] %>% 
  setnames("freq", "NINI")

# Grilla COD500
NINI_500 <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(COD500)] %>% 
  setnames("freq", "NINI")

# Grilla H3_N8
NINI_N8 <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(H3_N8)] %>% 
  setnames("freq", "NINI")

# Grilla H3_N9
NINI_N9 <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(H3_N9)] %>% 
  setnames("freq", "NINI")

## Socio Demográfico/Económico

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
NINI_c_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P02,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P02)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(canton)] %>% 
  dcast(canton~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NINI_S1", ifelse(.x == "2", "NINI_S2", .x)))  

# Administración zonal - sexo
NINI_az_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P02,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P02)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NINI_S1", ifelse(.x == "2", "NINI_S2", .x)))   

# Parroquia - Sexo 
NINI_pr_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P02,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P02)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NINI_S1", ifelse(.x == "2", "NINI_S2", .x))) 

# Sector - Sexo
NINI_s_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P02,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P02)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NINI_S1", ifelse(.x == "2", "NINI_S2", .x))) 

# Grilla COD1000 - Sexo 
NINI_1000_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P02,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P02)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NINI_S1", ifelse(.x == "2", "NINI_S2", .x))) 


# Grilla COD500 - Sexo 
NINI_500_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P02,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P02)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NINI_S1", ifelse(.x == "2", "NINI_S2", .x))) 

# Grilla H3_N8 - Sexo 
NINI_N8_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P02,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P02)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NINI_S1", ifelse(.x == "2", "NINI_S2", .x))) 

# Grilla H3_N9 - Sexo 
NINI_N9_s <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P02,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P02)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "NINI_S1", ifelse(.x == "2", "NINI_S2", .x))) 

## Autodeterminación cultural 

#P11R; Cómo se identifica según su cultura y costumbres
## 1 Indígena
## 2 Afroecuatoriana/o, Afrodescendiente, Negra/o, Mulata/o
## 3 Montubia/o
## 4 Mestiza/o
## 5 Blanca/o
## 6 Otro

# Cantón - Autodeterminación
NINI_c_a <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P11R,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P11R)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(canton)] %>% 
  dcast(canton~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("NINI_A", 1:6))

# Administración zonal - Autodeterminación
NINI_az_a <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P11R,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P11R)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("NINI_A", 1:6))

# Parroquia - Autodeterminación
NINI_pr_a <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P11R,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P11R)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(parroquia)] %>% 
  dcast(parroquia~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("NINI_A", 1:6)) 

# Sector - Autodeterminación
NINI_s_a <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P11R,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P11R)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("NINI_A", 1:6))

# Grilla COD1000 - Autodeterminación
NINI_1000_a <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P11R,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P11R)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(COD1000)] %>% 
  dcast(COD1000~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("NINI_A", 1:6))

# Grilla COD500 - Autodeterminación
NINI_500_a <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P11R,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P11R)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(COD500)] %>% 
  dcast(COD500~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("NINI_A", 1:6))

# Grilla H3_N8 - Autodeterminación
NINI_N8_a <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P11R,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P11R)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("NINI_A", 1:6))

# Grilla H3_N9 - Autodeterminación
NINI_N9_a <- poblacion2022[P03 >= 14 & P03 <= 24, .N, by = .(NI, P11R,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P11R)][NI == 1, ][
    , `:=`(N = NULL, NI = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("NINI_A", 1:6))

# 6. Guardar los resultados

wb <-  createWorkbook("NINI")

addWorksheet(wb, "NINI_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_c", x= NINI_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_az", x= NINI_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_pr", x= NINI_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_s", x= NINI_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_1000", x= NINI_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_500", x= NINI_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_N8", x= NINI_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_N9", x= NINI_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "NINI_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_c_s", x= NINI_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_az_s", x= NINI_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_pr_s", x= NINI_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_s_s", x= NINI_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_1000_s", x= NINI_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_500_s", x= NINI_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_N8_s", x= NINI_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_N9_s", x= NINI_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "NINI_c_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_c_a", x= NINI_c_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_az_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_az_a", x= NINI_az_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_pr_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_pr_a", x= NINI_pr_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_s_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_s_a", x= NINI_s_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_1000_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_1000_a", x= NINI_1000_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_500_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_500_a", x= NINI_500_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_N8_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_N8_a", x= NINI_N8_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NINI_N9_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NINI_N9_a", x= NINI_N9_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Poblacion de 14 a 24 años que no estudio ni trabaja NINI.xlsx")