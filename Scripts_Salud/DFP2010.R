# Dimensión: Salud

# Nombre del Indicador: 
# Porcentaje de la poblacion con dificultad funcional permanente 2010

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

# P08 = Discapacidad permanente por más de un año
# 1 Si
# 2 No
# 9 No responde

poblacion2010[,DFP10:=0]
poblacion2010[P08==1, DFP10:=1] 

# Desagregaciones

## Geográfico territorial

# Cantón
DFP10_c <- poblacion2010[, .N, by = .(DFP10, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(canton)] %>% 
  setnames("freq", "DFP10") 

# Administración zonal 
DFP10_az <- poblacion2010[, .N, by = .(DFP10, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "DFP10") 

# Parroquial
DFP10_pr <- poblacion2010[, .N, by = .(DFP10, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(parroquia)] %>% 
  setnames("freq", "DFP10")

# Sector
DFP10_s <- poblacion2010[, .N, by = .(DFP10, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "DFP10")

# Grilla COD1000
DFP10_1000 <- poblacion2010[, .N, by = .(DFP10, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(COD1000)] %>% 
  setnames("freq", "DFP10")

# Grilla COD500
DFP10_500 <- poblacion2010[, .N, by = .(DFP10, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(COD500)] %>% 
  setnames("freq", "DFP10")

# Grilla H3_N8
DFP10_N8 <- poblacion2010[, .N, by = .(DFP10, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(H3_N8)] %>% 
  setnames("freq", "DFP10")

# Grilla H3_N9
DFP10_N9 <- poblacion2010[, .N, by = .(DFP10, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(H3_N9)] %>% 
  setnames("freq", "DFP10")

## Socio Demográfico/Económico

# PO1: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
DFP10_c_s <- poblacion2010[, .N, by = .(DFP10, P01,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P01)][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(canton)] %>% 
  dcast(canton~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP10_S1", ifelse(.x == "2", "DFP10_S2", .x)))  

# Administración zonal - sexo
DFP10_az_s <- poblacion2010[, .N, by = .(DFP10, P01,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P01)][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP10_S1", ifelse(.x == "2", "DFP10_S2", .x)))   

# Parroquia - Sexo 
DFP10_pr_s <- poblacion2010[, .N, by = .(DFP10, P01,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P01)][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP10_S1", ifelse(.x == "2", "DFP10_S2", .x))) 

# Sector - Sexo
DFP10_s_s <- poblacion2010[, .N, by = .(DFP10, P01,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P01)][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP10_S1", ifelse(.x == "2", "DFP10_S2", .x))) 

# Grilla COD1000 - Sexo 
DFP10_1000_s <- poblacion2010[, .N, by = .(DFP10, P01,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P01)][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP10_S1", ifelse(.x == "2", "DFP10_S2", .x))) 


# Grilla COD500 - Sexo 
DFP10_500_s <- poblacion2010[, .N, by = .(DFP10, P01,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P01)][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP10_S1", ifelse(.x == "2", "DFP10_S2", .x))) 


# Grilla H3_N8 - Sexo 
DFP10_N8_s <- poblacion2010[, .N, by = .(DFP10, P01,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P01)][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP10_S1", ifelse(.x == "2", "DFP10_S2", .x))) 


# Grilla H3_N9 - Sexo 
DFP10_N9_s <- poblacion2010[, .N, by = .(DFP10, P01,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P01)][DFP10 == 1, ][
    , `:=`(N = NULL, DFP10 = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P01, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "DFP10_S1", ifelse(.x == "2", "DFP10_S2", .x))) 

# 6. Guardar los resultados

wb <-  createWorkbook("DFP10")

addWorksheet(wb, "DFP10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_c", x= DFP10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_az", x= DFP10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_pr", x= DFP10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_s", x= DFP10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_1000", x= DFP10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_500", x= DFP10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_N8", x= DFP10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_N9", x= DFP10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "DFP10_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_c_s", x= DFP10_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_az_s", x= DFP10_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_pr_s", x= DFP10_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_s_s", x= DFP10_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_1000_s", x= DFP10_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_500_s", x= DFP10_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_N8_s", x= DFP10_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "DFP10_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "DFP10_N9_s", x= DFP10_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de la poblacion con dificultad funcional permanente.xlsx")

