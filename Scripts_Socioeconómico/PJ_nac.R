# Dimensión: Socioeconomía

# Nombre del Indicador: 
#Previsión Social/Población Jubilada
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

poblacion2022 <- read_sav("CPV_Población_2022_Nacional.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

#P03 = Años cumplidos 

#P26 = Si no trabajó ni ha buscado trabajo
#1 Es rentista
#2 Es jubilada/o o pensionista
#3 Es estudiante
#4 Realiza quehaceres del hogar
#5 Le impide trabjajar su discapacidad
#6 Otro

poblacion2022[,JUB:=NULL]
poblacion2022[P03>=65,JUB:=0]
poblacion2022[P03>= 65 & P26==2, JUB:=1] 

# Desagregaciones

## Geográfico territorial

# Cantón
PJ_n <- poblacion2022[P03 >= 65, .N, by = .(JUB)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)] %>% 
  setnames("freq", "PJ") 

TA = poblacion2022[P03>=15,.N, by = .(Analf)][,freq:=round(100*(N/sum(N, na.rm
                                                                      = T)),1)]
TA = TA[Analf ==1,][order(Analf)][,Analf:=NULL]
setnames(TA, c( "N","freq"), c( "Número de personas analfabetas de 15 años o
más","Tasa de analfabetismo de la población de 15 años o más"))

# Cantón
PJ_c <- poblacion2022[P03 >= 65, .N, by = .(JUB, canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = canton][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(canton)] %>% 
  setnames("freq", "PJ") 

# Administración zonal 
PJ_az <- poblacion2022[P03 >= 65, .N, by = .(JUB, adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = adm_zonal][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(adm_zonal)] %>% 
  setnames("freq", "PJ") 

# Parroquial
PJ_pr <- poblacion2022[P03 >= 65, .N, by = .(JUB, parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = parroquia][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(parroquia)] %>% 
  setnames("freq", "PJ")

# Sector
PJ_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = Sector_DMQ][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(Sector_DMQ)] %>% 
  setnames("freq", "PJ")

# Grilla COD1000
PJ_1000 <- poblacion2022[P03 >= 65, .N, by = .(JUB, COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD1000][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(COD1000)] %>% 
  setnames("freq", "PJ")

# Grilla COD500
PJ_500 <- poblacion2022[P03 >= 65, .N, by = .(JUB, COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = COD500][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(COD500)] %>% 
  setnames("freq", "PJ")

# Grilla H3_N8
PJ_N8 <- poblacion2022[P03 >= 65, .N, by = .(JUB, H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N8][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(H3_N8)] %>% 
  setnames("freq", "PJ")

# Grilla H3_N9
PJ_N9 <- poblacion2022[P03 >= 65, .N, by = .(JUB, H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by = H3_N9][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(H3_N9)] %>% 
  setnames("freq", "PJ")

## Socio Demográfico/Económico

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Cantón - Sexo 
PJ_c_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, P02,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P02)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(canton)] %>% 
  dcast(canton~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PJ_S1", ifelse(.x == "2", "PJ_S2", .x)))  

# Administración zonal - sexo
PJ_az_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, P02,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P02)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PJ_S1", ifelse(.x == "2", "PJ_S2", .x)))   

# Parroquia - Sexo 
PJ_pr_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, P02,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P02)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(parroquia)] %>% 
  dcast(parroquia ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PJ_S1", ifelse(.x == "2", "PJ_S2", .x))) 

# Sector - Sexo
PJ_s_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, P02,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P02)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PJ_S1", ifelse(.x == "2", "PJ_S2", .x))) 

# Grilla COD1000 - Sexo 
PJ_1000_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, P02,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P02)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(COD1000)] %>% 
  dcast(COD1000 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PJ_S1", ifelse(.x == "2", "PJ_S2", .x))) 


# Grilla COD500 - Sexo 
PJ_500_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, P02,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P02)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(COD500)] %>% 
  dcast(COD500 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PJ_S1", ifelse(.x == "2", "PJ_S2", .x))) 


# Grilla H3_N8 - Sexo 
PJ_N8_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, P02,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P02)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PJ_S1", ifelse(.x == "2", "PJ_S2", .x))) 


# Grilla H3_N9 - Sexo 
PJ_N9_s <- poblacion2022[P03 >= 65, .N, by = .(JUB, P02,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P02)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9 ~ P02, value.var = "freq") %>% 
  rename_with(~ ifelse(.x == "1", "PJ_S1", ifelse(.x == "2", "PJ_S2", .x))) 


## Grupos de edad

#GEDAD: Grupos de edad quinquenales

# Cantón - Grupos de edad quinquenales
PJ_c_e <- poblacion2022[P03 >= 65, .N, by = .(JUB, GEDAD,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,GEDAD)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(canton)] %>% 
  dcast(canton ~  GEDAD, value.var = "freq") %>% 
  setnames(old = as.character(15:22), new = paste0("PJ_E", 15:22))

# Administración zonal - Grupos de edad quinquenales
PJ_az_e <- poblacion2022[P03 >= 65, .N, by = .(JUB, GEDAD,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,GEDAD)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal~ GEDAD, value.var = "freq") %>% 
  setnames(old = as.character(15:22), new = paste0("PJ_E", 15:22))

# Parroquia - Grupos de edad quinquenales
PJ_pr_e <- poblacion2022[P03 >= 65, .N, by = .(JUB, GEDAD,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,GEDAD)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(parroquia)] %>% 
  dcast(parroquia~ GEDAD, value.var = "freq") %>% 
  setnames(old = as.character(15:22), new = paste0("PJ_E", 15:22)) 

# Sector - Grupos de edad quinquenales
PJ_s_e <- poblacion2022[P03 >= 65, .N, by = .(JUB, GEDAD,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,GEDAD)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ~ GEDAD, value.var = "freq") %>% 
  setnames(old = as.character(15:22), new = paste0("PJ_E", 15:22))

# Grilla COD1000 - Grupos de edad quinquenales
PJ_1000_e <- poblacion2022[P03 >= 65, .N, by = .(JUB, GEDAD,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,GEDAD)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(COD1000)] %>% 
  dcast(COD1000~ GEDAD, value.var = "freq") %>% 
  setnames(old = as.character(15:22), new = paste0("PJ_E", 15:22))

# Grilla COD500 - Grupos de edad quinquenales
PJ_500_e <- poblacion2022[P03 >= 65, .N, by = .(JUB, GEDAD,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,GEDAD)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(COD500)] %>% 
  dcast(COD500~ GEDAD, value.var = "freq") %>% 
  setnames(old = as.character(15:22), new = paste0("PJ_E", 15:22))

# Grilla H3_N8 - Grupos de edad quinquenales
PJ_N8_e <- poblacion2022[P03 >= 65, .N, by = .(JUB, GEDAD,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,GEDAD)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8~ GEDAD, value.var = "freq") %>% 
  setnames(old = as.character(15:22), new = paste0("PJ_E", 15:22))

# Grilla H3_N9 - Grupos de edad quinquenales
PJ_N9_e <- poblacion2022[P03 >= 65, .N, by = .(JUB, GEDAD,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,GEDAD)][JUB == 1, ][
    , `:=`(N = NULL, JUB = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9~ GEDAD, value.var = "freq") %>% 
  setnames(old = as.character(15:22), new = paste0("PJ_E", 15:22))

# 6. Guardar los resultados

wb <-  createWorkbook("PJ")

addWorksheet(wb, "PJ_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_c", x= PJ_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_az", x= PJ_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_pr", x= PJ_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_s", x= PJ_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_1000", x= PJ_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_500", x= PJ_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_N8", x= PJ_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_N9", x= PJ_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PJ_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_c_s", x= PJ_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_az_s", x= PJ_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_pr_s", x= PJ_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_s_s", x= PJ_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_1000_s", x= PJ_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_500_s", x= PJ_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_N8_s", x= PJ_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_N9_s", x= PJ_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PJ_c_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_c_e", x= PJ_c_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_az_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_az_e", x= PJ_az_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_pr_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_pr_e", x= PJ_pr_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_s_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_s_e", x= PJ_s_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_1000_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_1000_e", x= PJ_1000_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_500_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_500_e", x= PJ_500_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_N8_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_N8_e", x= PJ_N8_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PJ_N9_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PJ_N9_e", x= PJ_N9_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Previsión Social-Población Jubilada.xlsx")