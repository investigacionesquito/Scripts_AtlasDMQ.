# Dimensión: Demografía 

# Nombre del Indicador: Edad mediana de la población

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
               , sjlabelled, stringr,labelled,dplyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores--------------------------------------------

## Geográfico/Territorial

# Cantón 
EMM_c <- poblacion2022[, median(P03), by = .(canton)] %>% 
  setnames("V1", "EMM") 

# Administración zonal 
EMM_az <- poblacion2022[, median(P03), by = .(adm_zonal)] %>% 
  setnames("V1", "EMM") 

# Parroquial
EMM_pr <- poblacion2022[, median(P03), by = .(parroquia)] %>% 
  setnames("V1", "EMM")  

# Sector
EMM_s <- poblacion2022[, median(P03), by = .(Sector_DMQ)] %>% 
  setnames("V1", "EMM")

# Grilla COD1000
EMM_1000 <- poblacion2022[, median(P03), by = .(COD1000)] %>% 
  setnames("V1", "EMM")

# Grilla COD500
EMM_500 <- poblacion2022[, median(P03), by = .(COD500)] %>% 
  setnames("V1", "EMM")

# Grilla H3_N8
EMM_N8 <- poblacion2022[, median(P03), by = .(H3_N8)] %>% 
  setnames("V1", "EMM")

# Grilla H3_N9
EMM_N9 <- poblacion2022[, median(P03), by = .(H3_N9)] %>% 
  setnames("V1", "EMM")

## Socio Demográfico/Económico

# Cantón - Sexo 
EMM_c_s <- poblacion2022[, median(P03), by = .(canton,P02)] %>% 
  dcast(canton ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMMs_1", ifelse(.x == "2", "EMMs_2", .x))) 

# Administracion zonal - Sexo
EMM_az_s <- poblacion2022[, median(P03), by = .(adm_zonal,P02)] %>% 
  dcast(adm_zonal ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMMs_1", ifelse(.x == "2", "EMMs_2", .x))) 

# Parroquia - Sexo 
EMM_pr_s <- poblacion2022[, median(P03), by = .(parroquia,P02)] %>% 
  dcast(parroquia ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMMs_1", ifelse(.x == "2", "EMMs_2", .x))) 

# Sector - Sexo
EMM_s_s <- poblacion2022[, median(P03), by = .(Sector_DMQ , P02)] %>% 
  dcast(Sector_DMQ ~ P02, value.var = "V1")  %>% 
  rename_with(~ ifelse(.x == "1", "EMMs_1", ifelse(.x == "2", "EMMs_2", .x))) 

# Grilla COD1000 - Sexo
EMM_1000_s <- poblacion2022[, median(P03), by = .(COD1000,P02)] %>% 
  dcast(COD1000~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMMs_1", ifelse(.x == "2", "EMMs_2", .x))) 

# Grilla COD500 - Sexo
EMM_500_s <- poblacion2022[, median(P03), by = .(COD500,P02)] %>% 
  dcast(COD500 ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMMs_1", ifelse(.x == "2", "EMMs_2", .x))) 

# Grilla H3_N8 - Sexo
EMM_N8_s <- poblacion2022[, median(P03), by = .(H3_N8,P02)] %>% 
  dcast(H3_N8 ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMMs_1", ifelse(.x == "2", "EMMs_2", .x))) 

# Grilla H3_N9 - Sexo
EMM_N9_s <- poblacion2022[, median(P03), by = .(H3_N9,P02)] %>% 
  dcast(H3_N9 ~ P02, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EMMs_1", ifelse(.x == "2", "EMMs_2", .x))) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("EMM")

addWorksheet(wb, "EMM_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_c", x=EMM_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_az", x=EMM_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_pr", x=EMM_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_s", x=EMM_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "EMM_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_1000", x=EMM_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_500", x=EMM_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_N8", x=EMM_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_N9", x=EMM_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "EMM_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_c_s", x=EMM_c_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_az_s", x=EMM_az_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_pr_s", x=EMM_pr_s, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_s_s", x=EMM_s_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "EMM_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_1000_s", x=EMM_1000_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_500_s", x=EMM_500_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_N8_s", x=EMM_N8_s, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EMM_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EMM_N9_s", x=EMM_N9_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Edad mediana de la población.xlsx") 
