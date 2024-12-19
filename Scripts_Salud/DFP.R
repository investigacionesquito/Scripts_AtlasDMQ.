# Dimensión: Salud

# Nombre del Indicador: 
# Porcentaje de la poblacion con dificultad funcional permanente

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

# DFUNC = Dificultad funcional permanente
# 1 Con dificultad funcional
# 2 Sin dificultad funcional
# 9 Se ignora

poblacion2022[,DFP:=0]
poblacion2022[DFUNC==1, DFP:=1] 

# Desagregaciones

## Geográfico territorial

# Cantón
DFP_c <- poblacion2022[, .N, by = .(DFP, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(canton)] %>% 
  setnames("freq", "DFP") 

# Administración zonal 
DFP_az <- poblacion2022[, .N, by = .(DFP, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "DFP") 

# Parroquial
DFP_pr <- poblacion2022[, .N, by = .(DFP, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(parroquia)] %>% 
  setnames("freq", "DFP")

# Sector
DFP_s <- poblacion2022[, .N, by = .(DFP, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "DFP")

# Grilla COD1000
DFP_1000 <- poblacion2022[, .N, by = .(DFP, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(COD1000)] %>% 
  setnames("freq", "DFP")

# Grilla COD500
DFP_500 <- poblacion2022[, .N, by = .(DFP, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(COD500)] %>% 
  setnames("freq", "DFP")

# Grilla H3_N8
DFP_N8 <- poblacion2022[, .N, by = .(DFP, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(H3_N8)] %>% 
  setnames("freq", "DFP")

# Grilla H3_N9
DFP_N9 <- poblacion2022[, .N, by = .(DFP, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(H3_N9)] %>% 
  setnames("freq", "DFP")

## Socio Demográfico/Económico

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
DFP_c_s <- poblacion2022[, .N, by = .(DFP, P02,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P02)][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(canton)] %>% 
  dcast(canton~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP_S1", ifelse(.x == "2", "DFP_S2", .x)))  

# Administración zonal - sexo
DFP_az_s <- poblacion2022[, .N, by = .(DFP, P02,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P02)][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP_S1", ifelse(.x == "2", "DFP_S2", .x)))   

# Parroquia - Sexo 
DFP_pr_s <- poblacion2022[, .N, by = .(DFP, P02,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P02)][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP_S1", ifelse(.x == "2", "DFP_S2", .x))) 

# Sector - Sexo
DFP_s_s <- poblacion2022[, .N, by = .(DFP, P02,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P02)][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP_S1", ifelse(.x == "2", "DFP_S2", .x))) 

# Grilla COD1000 - Sexo 
DFP_1000_s <- poblacion2022[, .N, by = .(DFP, P02,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P02)][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP_S1", ifelse(.x == "2", "DFP_S2", .x))) 


# Grilla COD500 - Sexo 
DFP_500_s <- poblacion2022[, .N, by = .(DFP, P02,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P02)][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP_S1", ifelse(.x == "2", "DFP_S2", .x))) 


# Grilla H3_N8 - Sexo 
DFP_N8_s <- poblacion2022[, .N, by = .(DFP, P02,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P02)][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP_S1", ifelse(.x == "2", "DFP_S2", .x))) 


# Grilla H3_N9 - Sexo 
DFP_N9_s <- poblacion2022[, .N, by = .(DFP, P02,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P02)][DFP == 1, ][
    , `:=`(N = NULL, DFP = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP_S1", ifelse(.x == "2", "DFP_S2", .x))) 

# 6. Guardar los resultados

wb <-  createWorkbook("DFP")

addWorksheet(wb, "DFP_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_c", x= DFP_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_az", x= DFP_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_pr", x= DFP_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_s", x= DFP_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_1000", x= DFP_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_500", x= DFP_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_N8", x= DFP_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_N9", x= DFP_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "DFP_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_c_s", x= DFP_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_az_s", x= DFP_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_pr_s", x= DFP_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_s_s", x= DFP_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_1000_s", x= DFP_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_500_s", x= DFP_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_N8_s", x= DFP_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP_N9_s", x= DFP_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de la poblacion con dificultad funcional permanente.xlsx")

