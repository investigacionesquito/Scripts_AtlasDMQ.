# Dimensión: Educación

# Nombre del Indicador: Años promedio de escolaridad.

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

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()


# 5. Calcular indicadores--------------------------------------------

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

#P18R = Grado, curso o año más alto que aprobó

# dado que todos deben contestar la pregunta 17, recodifico NA con 99
# dado que todas las personas que hayan contestado la pregunta 17 desde el código 3 deben contestar la pregunta 18, recodifico NA con 99

poblacion2022[is.na(P17R),P17R:=99]
poblacion2022[is.na(P18R) & P17R>=3 ,P18R:=99]

poblacion2022[,ANIOS:=NULL]
poblacion2022[, ANIOS := fcase(
  P17R == 1, 0,  # Sin instrucción: 0 años
  P17R == 2, 0,  # Guardería o centro de desarrollo infantil: 0 años
  P17R == 3, 0,  # Educación inicial, preescolar: 0 años
  P17R == 4 & P18R == 0, 0,  # Alfabetización sin aprobación: 0 años
  P17R == 4 & P18R %in% c(1, 2), 3,  # #Alfabetización Módulo 1y2,2do–3ro EGB: 3 años
  P17R == 4 & P18R %in% c(3, 4), 5,  # Alfabetización módulo 3 o 4,4to–5to EGB: 5 años
  P17R == 4 & P18R %in% c(5, 6), 7,  # Alfabetización módulo 5 o 6, 6to–7mo EGB: 7 años
  P17R == 4 & P18R %in% c(7:10), 99,  # Error o inconsistencias: 99
  P17R == 5 & P18R != 99, 0 + P18R,  # Educación básica: años según P18R
  P17R == 6 & P18R != 99, 10 + P18R,  # Bachillerato: 10 años + P18R
  P17R == 7 & P18R != 99, 13 + P18R,  # Ciclo postbachillerato: 13 años + P18R
  P17R %in% c(8, 9), 13 + P18R,  # Educación técnica o superior: 13 + P18R
  P17R == 10 & P18R != 99, 18 + P18R,  # Maestría/especialización: 18 + P18R
  P17R == 11 & P18R != 99, 20 + P18R,  # Doctorado: 20 + P18R
  P17R == 99 | P18R == 99, 99  # Valores faltantes o inconsistentes: 99
)]

poblacion2022[,ES:=NULL]
poblacion2022[((P03>=24) & (P18R<=10 | is.na(P18R)) & P17R!=99 & ANIOS!=99),
              ES:=ANIOS]

## Geográfico/Territorial

# Cantón 
APE_c <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(canton)] %>% 
  setnames("V1", "APE") 

# Administracion zonal 
APE_az <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(adm_zonal)] %>% 
  setnames("V1", "APE") 

# Parroquial
APE_pr <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(parroquia)] %>% 
  setnames("V1", "APE") 

# Sector
APE_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(Sector_DMQ)] %>% 
  setnames("V1", "APE") 

# Grilla COD1000
APE_1000 <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(COD1000)] %>%  
  setnames("V1", "APE") 

# Grilla COD500
APE_500 <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(COD500)] %>%  
  setnames("V1", "APE") 

# Grilla H3_N8
APE_N8 <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(H3_N8)] %>% 
  setnames("V1", "APE") 

# Grilla H3_N9

APE_N9 <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(H3_N9)] %>%  
  setnames("V1", "APE") 

## Socio DAPEográfico/Económico

# Cantón - Sexo
APE_c_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(canton,P02)] %>% 
  dcast(canton~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE_S1", ifelse(.x == "2", "APE_S2", .x))) 

# Administración zonal - Sexo 
APE_az_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(adm_zonal,P02)] %>% 
  dcast(adm_zonal  ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE_S1", ifelse(.x == "2", "APE_S2", .x))) 

# Parroquia - Sexo 
APE_pr_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(parroquia,P02)] %>% 
  dcast(parroquia  ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE_S1", ifelse(.x == "2", "APE_S2", .x))) 

# Sector - Sexo
APE_s_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(Sector_DMQ, P02)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE_S1", ifelse(.x == "2", "APE_S2", .x))) 

# Grilla COD1000 - Sexo 
APE_1000_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(COD1000 , P02)] %>% 
  dcast(COD1000  ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE_S1", ifelse(.x == "2", "APE_S2", .x))) 

# Grilla COD500 - Sexo 
APE_500_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(COD500, P02)] %>% 
  dcast(COD500 ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE_S1", ifelse(.x == "2", "APE_S2", .x))) 

# Grilla H3_N8 - Sexo 
APE_N8_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(H3_N8 , P02)] %>% 
  dcast(H3_N8  ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE_S1", ifelse(.x == "2", "APE_S2", .x))) 

# Grilla H3_N9 - Sexo 
APE_N9_s <- poblacion2022[((P03>=24) & ES!=99), round(mean(ES,na.rm=T),1), by = .(H3_N9, P02)] %>% 
  dcast(H3_N9 ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "APE_S1", ifelse(.x == "2", "APE_S2", .x))) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("APE")

addWorksheet(wb, "APE_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_c", x= APE_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_az", x= APE_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_pr", x= APE_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_s", x= APE_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_1000", x= APE_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_500", x= APE_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_N8", x= APE_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_N9", x= APE_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "APE_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_c_s", x= APE_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_az_s", x= APE_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_pr_s", x= APE_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_s_s", x= APE_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_1000_s", x= APE_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_500_s", x= APE_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_N8_s", x= APE_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "APE_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "APE_N9_s", x= APE_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Años promedio de escolaridad..xlsx") 