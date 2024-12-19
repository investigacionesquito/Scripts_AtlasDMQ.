# Dimensión: Demografía 

# Nombre del Indicador: Edad media de la población

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

## Geográfico/Territorial

# Cantón 
EM_c <- poblacion2022[, round(mean(P03),1), by = .(canton)] %>% 
  setnames("V1", "EM") 

# Administracion zonal 
EM_az <- poblacion2022[, round(mean(P03),1), by = .(adm_zonal)] %>% 
  setnames("V1", "EM") 

# Parroquial
EM_pr <- poblacion2022[, round(mean(P03),1), by = .(parroquia)] %>% 
  setnames("V1", "EM") 

# Sector
EM_s <- poblacion2022[, round(mean(P03),1), by = .(Sector_DMQ)] %>%
  setnames("V1", "EM") 

# Grilla COD1000
EM_1000 <- poblacion2022[, round(mean(P03),1), by = .(COD1000)] %>% 
  setnames("V1", "EM") 

# Grilla COD500
EM_500 <- poblacion2022[, round(mean(P03),1), by = .(COD500)] %>% 
  setnames("V1", "EM") 

# Grilla H3_N8
EM_N8 <- poblacion2022[, round(mean(P03),1), by = .(H3_N8)] %>% 
  setnames("V1", "EM") 

# Grilla H3_N9

EM_N9 <- poblacion2022[, round(mean(P03),1), by = .(H3_N9)] %>% 
  setnames("V1", "EM") 

## Socio Demográfico/Económico

# Cantón - Sexo
EM_c_s <- poblacion2022[, round(mean(P03),1), by = .(canton,P02)] %>% 
  dcast(canton~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMs_1", ifelse(.x == "2", "EMs_2", .x))) 

# Administración zonal - Sexo 
EM_az_s <- poblacion2022[, round(mean(P03),1), by = .(adm_zonal,P02)] %>% 
  dcast(adm_zonal  ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMs_1", ifelse(.x == "2", "EMs_2", .x))) 

# Parroquia - Sexo 
EM_pr_s <- poblacion2022[, round(mean(P03),1), by = .(parroquia,P02)] %>% 
  dcast(parroquia  ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMs_1", ifelse(.x == "2", "EMs_2", .x))) 

# Sector - Sexo
EM_s_s <- poblacion2022[, round(mean(P03),1), by = .(Sector_DMQ, P02)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMs_1", ifelse(.x == "2", "EMs_2", .x))) 

# Grilla COD1000 - Sexo 
EM_1000_s <- poblacion2022[, round(mean(P03),1), by = .(COD1000 , P02)] %>% 
  dcast(COD1000  ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMs_1", ifelse(.x == "2", "EMs_2", .x))) 

# Grilla COD500 - Sexo 
EM_500_s <- poblacion2022[, round(mean(P03),1), by = .(COD500, P02)] %>% 
  dcast(COD500 ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMs_1", ifelse(.x == "2", "EMs_2", .x))) 

# Grilla H3_N8 - Sexo 
EM_N8_s <- poblacion2022[, round(mean(P03),1), by = .(H3_N8 , P02)] %>% 
  dcast(H3_N8  ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMs_1", ifelse(.x == "2", "EMs_2", .x))) 

# Grilla H3_N9 - Sexo 
EM_N9_s <- poblacion2022[, round(mean(P03),1), by = .(H3_N9, P02)] %>% 
  dcast(H3_N9 ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMs_1", ifelse(.x == "2", "EMs_2", .x))) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("EM")

addWorksheet(wb, "EM_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_c", x=EM_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_az", x=EM_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_pr", x=EM_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_s", x=EM_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "EM_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_1000", x=EM_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_500", x=EM_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_N8", x=EM_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_N9", x=EM_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


addWorksheet(wb, "EM_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_c_s", x=EM_c_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_az_s", x=EM_az_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_pr_s", x=EM_pr_s, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_s_s", x=EM_s_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "EM_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_1000_s", x=EM_1000_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_500_s", x=EM_500_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_N8_s", x=EM_N8_s, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM_N9_s", x=EM_N9_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Edad media de la población.xlsx") 
