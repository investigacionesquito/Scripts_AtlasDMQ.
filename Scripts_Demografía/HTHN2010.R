# Dimensión: Demografía 

# Nombre del Indicador: Número de hogares de acuerdo al tipo de hogar 2010

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
  
hogar2022 <- read_sav("hogar2010_Atlas.sav") %>% as.data.table() 
poblacion2022 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table() 


# 5. Calcular indicadores --------------------------------------------------------

# Para el cálculo del indicador es necesario unir las bases de datos de población y hogar

HP2010 <- merge(hogar2010,poblacion2010, by="id_hog")                        

HP2010 <- merge(hogar2022,poblacion2022, by="ID_HOG")

#P02 = Relacion de parentesco con el jefe(a) del hogar
# 1 Jefe o jefa de hogar
# 2 Cónyuge o  conviviente
# 3 Hijo o hija
# 4 Yerno o nuera
# 5 Nieto o nieta
# 6 Padres o suegros
# 7 Otro Pariente
# 8 Otro no pariente
# 9 Empleado(a) doméstico(a)
# 10 Miembro de hogar colectivo
# 11 Sin vivienda

HP2010[P02==1, jefe:=1]
HP2010[P02==2, conyugue:=1]
HP2010[P02==3, hijos:=1]
HP2010[P02 %in% (4:7), OtroPAriente:=1]
HP2010[P02 == 8, OtroNoPAriente:=1]

HP2010[, `:=` (sumjefe = sum(jefe, na.rm = TRUE)), by = id_hog]
HP2010[, `:=` (sumconyugue = sum(conyugue, na.rm = TRUE)), by = id_hog]
HP2010[, `:=` (sumhijos = sum(hijos, na.rm = TRUE)), by = id_hog]
HP2010[, `:=` (sumOtrosPAriente = sum(OtroPAriente, na.rm = TRUE)), by = id_hog]
HP2010[, `:=` (sumOtrosNoPAriente = sum(OtroNoPAriente, na.rm = TRUE)), by = id_hog]

# Establezco condiciones por tipo 

# Hogares Unipersonales
HP2010[(sumjefe==1 & sumconyugue==0 & sumhijos==0 &  
                sumOtrosPAriente==0 & sumOtrosNoPAriente==0),
             TIPHogar:=1]

#Hogares nucleares
HP2010[(sumjefe==1 & sumconyugue==1 & sumhijos>0 & 
                sumOtrosPAriente==0 & sumOtrosNoPAriente==0) | 
               (sumjefe==1 & sumconyugue==1 & sumhijos==0 & sumOtrosPAriente==0 & sumOtrosNoPAriente==0) |
               (sumjefe==1 & sumconyugue==0 & sumhijos>0 & sumOtrosPAriente==0 & sumOtrosNoPAriente==0),
             TIPHogar:=2]

#Hogares extensos
HP2010[((sumjefe==1 & sumconyugue==1 & sumhijos>0 & sumOtrosPAriente>0 & sumOtrosNoPAriente==0) | 
                (sumjefe==1 & sumconyugue==1 & sumhijos==0 & sumOtrosPAriente>0 & sumOtrosNoPAriente==0) | 
                (sumjefe==1 & sumconyugue==0 & sumhijos>0 & sumOtrosPAriente>0 & sumOtrosNoPAriente==0)), 
             TIPHogar:=3]

# Hogares compuestos
HP2010[((sumjefe==1 & sumconyugue==1 & sumhijos>0 & sumOtrosPAriente>0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==1 & sumhijos>0 & sumOtrosPAriente==0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==1 & sumhijos==0 & sumOtrosPAriente>0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==1 & sumhijos==0 & sumOtrosPAriente==0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==0 & sumhijos>0 & sumOtrosPAriente>0 & sumOtrosNoPAriente>0)|
                (sumjefe==1 & sumconyugue==0 & sumhijos>0 & sumOtrosPAriente==0 & sumOtrosNoPAriente>0)),
             TIPHogar:=4]

#Hogares sin núcleo conyugal
HP2010[((sumjefe==1 & sumconyugue==0 & sumhijos==0 & sumOtrosPAriente>0 & sumOtrosNoPAriente==0) |
                (sumjefe==1 & sumconyugue==0 & sumhijos==0 & sumOtrosPAriente==0 & sumOtrosNoPAriente>0) |
                (sumjefe==1 & sumconyugue==0 & sumhijos==0 & sumOtrosPAriente>0 & sumOtrosNoPAriente>0)),
             TIPHogar:=5]

# Desegregaciones: 

## Geográfico/Territorial

# Cantón
HTHN10_c <- HP2010[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (canton.x, TIPHogar)] %>% 
  dcast (canton.x ~ TIPHogar, value.var=c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTHN10_", .x), .x))  

# Administracion zonal
HTHN10_az <- HP2010[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (adm_zonal.x, TIPHogar)] %>% 
  dcast (adm_zonal.x ~ TIPHogar, value.var=c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTHN10_", .x), .x)) 

# Parroquial
HTHN10_pr <- HP2010[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (parroquia.x, TIPHogar)] %>% 
  dcast (parroquia.x ~ TIPHogar, value.var=c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTHN10_", .x), .x))  

# Sector
HTHN10_s <- HP2010[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (Sector_DMQ.x, TIPHogar)] %>% 
  dcast (Sector_DMQ.x ~ TIPHogar, value.var=c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTHN10_", .x), .x)) 

# Grilla COD1000
HTHN10_1000 <- HP2010[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (COD1000.x, TIPHogar)] %>% 
  dcast (COD1000.x ~ TIPHogar,value.var=c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTHN10_", .x), .x)) 

# Grilla COD500
HTHN10_500 <- HP2010[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (COD500.x, TIPHogar)] %>% 
  dcast (COD500.x ~ TIPHogar,value.var=c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTHN10_", .x), .x)) 

# Grilla H3_N8
HTHN10_N8 <- HP2010[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (H3_N8.x, TIPHogar)] %>% 
  dcast (H3_N8.x ~ TIPHogar,value.var=c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTHN10_", .x), .x)) 

# Grilla H3_N9
HTHN10_N9 <- HP2010[P01==1 & !is.na(TIPHogar), .(freq=.N), by=. (H3_N9.x, TIPHogar)] %>% 
  dcast (H3_N9.x ~ TIPHogar,value.var=c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("HTHN10_", .x), .x)) 

# 6. Guardar los resultados --------------------------------------------------

wb <- createWorkbook("HTHN")

addWorksheet(wb, "HTHN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTHN10_c", x= HTHN10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTHN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTHN10_az", x= HTHN10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTHN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTHN10_pr", x= HTHN10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTHN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTHN10_s", x= HTHN10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HTHN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTHN10_1000", x= HTHN10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTHN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTHN10_500", x= HTHN10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTHN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTHN10_N8", x= HTHN10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTHN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTHN10_N9", x= HTHN10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Número de hogares de acuerdo al tipo de hogar 2010.xlsx", overwrite = TRUE)
