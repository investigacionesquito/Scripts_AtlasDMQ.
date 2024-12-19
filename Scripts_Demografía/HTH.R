# Dimensión: Demografía 

# Nombre del Indicador: Porcentaje de hogares de acuerdo al tipo de hogar

# Proceso

# 1. Descargar bas es de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5. Calcular indicadores
# 6. Guardar los resultados

# 1. Descargar bases de datos -------------------------------------------------

# 2. Establecer el directorio -------------------------------------------------

# 2. Establecer el directorio -------------------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías -------------------------------------------------------

pacman::p_load(data.table,openxlsx, foreign, haven, writexl, sjlabelled, stringr,dplyr)

# 4. Cargar base de datos ----------------------------------------------------
  
hogar2022 <- read_sav("hogar2022_Atlas.sav") %>% as.data.table() 
poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table() 


# 5. Calcular indicadores --------------------------------------------------------

# Para el cálculo del indicador es necesario unir las bases de datos de población y hogar

poblacion2022[, I01_c:=str_pad(I01, 2, pad = "0")]
poblacion2022[, I02_c:=str_pad(I02, 2, pad = "0")]
poblacion2022[, I03_c:=str_pad(I03, 2, pad = "0")]
poblacion2022[, I04_c:=str_pad(I04, 3, pad = "0")]
poblacion2022[, I05_c:=str_pad(I05, 3, pad = "0")]
poblacion2022[, I10_c:=str_pad(I10, 3, pad = "0")]
poblacion2022[, INHP_c:=str_pad(INH, 2, pad = "0")]

poblacion2022[,ID_HOG:= do.call(paste, c(replace(.SD, is.na(.SD),
                   ""), sep = "")),.SDcols = c("I01_c", "I02_c", "I03_c", "I04_c",
                   "I05_c","I10_c", "INHP_c")]                          

HP2022 <- merge(hogar2022,poblacion2022, by="ID_HOG")

# P01 = Parentesco o relación con el representante del hogar

# 1 Representante del hogar
# 2 Cónyuge o conviviente
# 3 Hija o hijo
# 4 Hijastra o hijastro
# 5 Nuera o yerno
# 6 Nieta o nieto
# 7 Madre o padre
# 8 Suegra o suegro
# 9 Otro pariente
# 10 Otro no pariente
# 11 Empleada/o doméstica/o
# 12 Miembro de hogar colectivo
# 13 Persona sin vivienda

HP2022[P01==1, jefe:=1]
HP2022[P01==2, conyugue:=1]
HP2022[P01 %in% c(3,4), hijos:=1]
HP2022[P01 %in% (5:9), OtroPAriente:=1]
HP2022[P01 == 10, OtroNoPAriente:=1]

HP2022[, `:=` (sumjefe = sum(jefe, na.rm = TRUE)), by = ID_HOG]
HP2022[, `:=` (sumconyugue = sum(conyugue, na.rm = TRUE)), by = ID_HOG]
HP2022[, `:=` (sumhijos = sum(hijos, na.rm = TRUE)), by = ID_HOG]
HP2022[, `:=` (sumOtrosPAriente = sum(OtroPAriente, na.rm = TRUE)), by = ID_HOG]
HP2022[, `:=` (sumOtrosNoPAriente = sum(OtroNoPAriente, na.rm = TRUE)), by = ID_HOG]

# Establezco condiciones por tipo 

# Hogares Unipersonales
HP2022[(sumjefe==1 & sumconyugue==0 & sumhijos==0 &  
                sumOtrosPAriente==0 & sumOtrosNoPAriente==0),
             TIPHogar:=1]

#Hogares nucleares
HP2022[(sumjefe==1 & sumconyugue==1 & sumhijos>0 & 
                sumOtrosPAriente==0 & sumOtrosNoPAriente==0) | 
               (sumjefe==1 & sumconyugue==1 & sumhijos==0 & sumOtrosPAriente==0 & sumOtrosNoPAriente==0) |
               (sumjefe==1 & sumconyugue==0 & sumhijos>0 & sumOtrosPAriente==0 & sumOtrosNoPAriente==0),
             TIPHogar:=2]

#Hogares extensos
HP2022[((sumjefe==1 & sumconyugue==1 & sumhijos>0 & sumOtrosPAriente>0 & sumOtrosNoPAriente==0) | 
                (sumjefe==1 & sumconyugue==1 & sumhijos==0 & sumOtrosPAriente>0 & sumOtrosNoPAriente==0) | 
                (sumjefe==1 & sumconyugue==0 & sumhijos>0 & sumOtrosPAriente>0 & sumOtrosNoPAriente==0)), 
             TIPHogar:=3]

# Hogares compuestos
HP2022[((sumjefe==1 & sumconyugue==1 & sumhijos>0 & sumOtrosPAriente>0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==1 & sumhijos>0 & sumOtrosPAriente==0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==1 & sumhijos==0 & sumOtrosPAriente>0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==1 & sumhijos==0 & sumOtrosPAriente==0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==0 & sumhijos>0 & sumOtrosPAriente>0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==0 & sumhijos>0 & sumOtrosPAriente==0 & sumOtrosNoPAriente>0)),
             TIPHogar:=4]

#Hogares sin núcleo conyugal
HP2022[((sumjefe==1 & sumconyugue==0 & sumhijos==0 & sumOtrosPAriente>0 & sumOtrosNoPAriente==0) |
                (sumjefe==1 & sumconyugue==0 & sumhijos==0 & sumOtrosPAriente==0 & sumOtrosNoPAriente>0) |
                (sumjefe==1 & sumconyugue==0 & sumhijos==0 & sumOtrosPAriente>0 & sumOtrosNoPAriente>0)),
             TIPHogar:=5]

# Desegregaciones: 

## Geográfico/Territorial

# Cantón
HTH_c <- HP2022[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (canton.x, TIPHogar)] [,
  prop := round((freq/sum(freq)*100),1), by=.(canton.x)][,':='(freq = NULL)] %>% 
  dcast (canton.x ~ TIPHogar, value.var=c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTH_", .x), .x))  

# Administracion zonal
HTH_az <- HP2022[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (adm_zonal.x, TIPHogar)] [,
  prop := round((freq/sum(freq)*100),1), by=.(adm_zonal.x)][,':='(freq = NULL)] %>% 
  dcast (adm_zonal.x ~ TIPHogar, value.var=c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTH_", .x), .x)) 

# Parroquial
HTH_pr <- HP2022[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (parroquia.x, TIPHogar)] [,
prop := round((freq/sum(freq)*100),1), by=.(parroquia.x)][,':='(freq = NULL)] %>% 
  dcast (parroquia.x ~ TIPHogar, value.var=c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTH_", .x), .x))  

# Sector
HTH_s <- HP2022[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (Sector_DMQ.x, TIPHogar)] [,
  prop := round((freq/sum(freq)*100),1), by=.(Sector_DMQ.x)][,':='(freq = NULL)] %>% 
  dcast (Sector_DMQ.x ~ TIPHogar, value.var=c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTH_", .x), .x)) 

# Grilla COD1000
HTH_1000 <- HP2022[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (COD1000.x, TIPHogar)] [,
  prop := round((freq/sum(freq)*100),1), by=.(COD1000.x)][,':='(freq = NULL)] %>% 
  dcast (COD1000.x ~ TIPHogar,value.var=c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTH_", .x), .x)) 

# Grilla COD500
HTH_500 <- HP2022[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (COD500.x, TIPHogar)] [,
  prop := round((freq/sum(freq)*100),1), by=.(COD500.x)][,':='(freq = NULL)] %>% 
  dcast (COD500.x ~ TIPHogar,value.var=c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTH_", .x), .x)) 

# Grilla H3_N8
HTH_N8 <- HP2022[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (H3_N8.x, TIPHogar)] [,
  prop := round((freq/sum(freq)*100),1), by=.(H3_N8.x)][,':='(freq = NULL)] %>% 
  dcast (H3_N8.x ~ TIPHogar,value.var=c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTH_", .x), .x)) 

# Grilla H3_N9
HTH_N9 <- HP2022[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (H3_N9.x, TIPHogar)] [,
   prop := round((freq/sum(freq)*100),1), by=.(H3_N9.x)][,':='(freq = NULL)] %>% 
  dcast (H3_N9.x ~ TIPHogar,value.var=c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTH_", .x), .x)) 

# 6. Guardar los resultados --------------------------------------------------

wb <- createWorkbook("HTH")

addWorksheet(wb, "HTH_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTH_c", x= HTH_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTH_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTH_az", x= HTH_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTH_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTH_pr", x= HTH_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTH_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTH_s", x= HTH_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HTH_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTH_1000", x= HTH_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTH_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTH_500", x= HTH_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTH_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTH_N8", x= HTH_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTH_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTH_N9", x= HTH_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de hogares de acuerdo al tipo de hogar.xlsx", overwrite = TRUE)
