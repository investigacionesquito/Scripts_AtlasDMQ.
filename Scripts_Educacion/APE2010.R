# Dimensión: Educación

# Nombre del Indicador: Años promedio de escolaridad 2010.

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3. Cargar librerías
# 4. Importar base de datos
# 5. Calcular indicadores
# 6. Guardar los resultados

# 1. Descargar bases de datos

# 2. Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ----------------------------------------------

pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr,tidyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2010 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores--------------------------------------------

#P23 = Nivel de instrucción más alto al que asiste o asistió: 
# 1 Ninguno 
# 2 Centro de Alfabetización/(EBA) 
# 3 Preescolar 
# 4 Primario
# 5 Secundario
# 6 Educación Básica 
# 7 Educación Media/ bachillerato 
# 8 Ciclo Postbachillerato 
# 9 Superior 
# 10 Postgrado 
# 99 Se ignora

#P24 = Grado, curso o año más alto que asiste o asistio

# dado que todos deben contestar la pregunta 23, recodifico NA con 99
# dado que todas las personas que hayan contestado la pregunta 23 desde el código 2 deben contestar la pregunta 24, recodifico NA con 99
poblacion2010[is.na(P23),P23:=99]
poblacion2010[is.na(P24) & P23>=2 ,P24:=99]

poblacion2010[,ANIOS:=NULL]
poblacion2010[, ANIOS := fcase(
  P23 == 1, 0,  # Ninguno
  P23 == 2, 0 + P24,  # Centro de alfabetización
  P23 == 2 & P24 %in% c(7:10), 99,  # Error o inconsistencias: 99
  P23 == 3, 1,  # Pre-escolar
  P23 == 4 & P24 != 99, 0 + P24,  # Primaria: años según P24 
  P23 == 5 & P24 != 99, 6 + P24,  # Secundario: 6 años de primaria + P24
  P23 == 6 & P24 != 99, 0 + P24,  # Educacion Básica años según P24 
  P23 == 7 & P24 != 99, 10 + P24,  # Educacion Media/Bachillerato: 10 básicos + P24
  P23 == 8 & P24 != 99, 13 + P24,  # Postbachillerato: 13 básicos + P24 
  P23 == 9 & P24 != 99, 13 + P24,  # Superior: 13 básicos + P24  
  P23 == 10 & P24 != 99, 18 + P24,  # Postgrado: 18 básicos y superior + P24
  P23 == 99 | P24 == 99, 99  # Valores faltantes o inconsistentes
)]

poblacion2010[,ES:=NULL]
poblacion2010[((P03>=24) & (P24<=10 | is.na(P24)) & P23!=99 & ANIOS!=99),
              ES:=ANIOS]

## Geográfico/Territorial

# Cantón 
APE10_c <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(canton)] %>% 
  setnames("V1", "APE10") 

# Administracion zonal 
APE10_az <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(adm_zonal)] %>% 
  setnames("V1", "APE10") 

# Parroquial
APE10_pr <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(parroquia)] %>% 
  setnames("V1", "APE10") 

# Sector
APE10_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(Sector_DMQ)] %>% 
  setnames("V1", "APE10") 

# Grilla COD1000
APE10_1000 <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(COD1000)] %>%  
  setnames("V1", "APE10") 

# Grilla COD500
APE10_500 <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(COD500)] %>%  
  setnames("V1", "APE10") 

# Grilla H3_N8
APE10_N8 <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(H3_N8)] %>% 
  setnames("V1", "APE10") 

# Grilla H3_N9

APE10_N9 <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(H3_N9)] %>%  
  setnames("V1", "APE10") 

## Socio Demográfico/Económico

# Cantón - Sexo
APE10_c_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(canton,P01)] %>% 
  dcast(canton~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE10_S1", ifelse(.x == "2", "APE10_S2", .x))) 

# Administración zonal - Sexo 
APE10_az_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(adm_zonal,P01)] %>% 
  dcast(adm_zonal  ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE10_S1", ifelse(.x == "2", "APE10_S2", .x))) 

# Parroquia - Sexo 
APE10_pr_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(parroquia,P01)] %>% 
  dcast(parroquia  ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE10_S1", ifelse(.x == "2", "APE10_S2", .x))) 

# Sector - Sexo
APE10_s_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(Sector_DMQ, P01)] %>% 
  dcast(Sector_DMQ ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE10_S1", ifelse(.x == "2", "APE10_S2", .x))) 

# Grilla COD1000 - Sexo 
APE10_1000_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(COD1000 , P01)] %>% 
  dcast(COD1000  ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE10_S1", ifelse(.x == "2", "APE10_S2", .x))) 

# Grilla COD500 - Sexo 
APE10_500_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(COD500, P01)] %>% 
  dcast(COD500 ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE10_S1", ifelse(.x == "2", "APE10_S2", .x))) 

# Grilla H3_N8 - Sexo 
APE10_N8_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(H3_N8 , P01)] %>% 
  dcast(H3_N8  ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE10_S1", ifelse(.x == "2", "APE10_S2", .x))) 

# Grilla H3_N9 - Sexo 
APE10_N9_s <- poblacion2010[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(H3_N9, P01)] %>% 
  dcast(H3_N9 ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE10_S1", ifelse(.x == "2", "APE10_S2", .x))) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("APE10")

addWorksheet(wb, "APE10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_c", x= APE10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_az", x= APE10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_pr", x= APE10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_s", x= APE10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_1000", x= APE10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_500", x= APE10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_N8", x= APE10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_N9", x= APE10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "APE10_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_c_s", x= APE10_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_az_s", x= APE10_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_pr_s", x= APE10_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_s_s", x= APE10_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_1000_s", x= APE10_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_500_s", x= APE10_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_N8_s", x= APE10_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE10_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE10_N9_s", x= APE10_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Años promedio de escolaridad 2010.xlsx") 