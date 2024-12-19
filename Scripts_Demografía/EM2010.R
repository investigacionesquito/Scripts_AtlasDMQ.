# Dimensión: Demografía 

# Nombre del Indicador: Edad media de la población 2010

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

# PO3 = Cuantos años cumplidos tiene

## Geográfico/Territorial

# Cantón 
EM10_c <- poblacion2010[, round(mean(P03),1), by = .(canton)] %>% 
  setnames("V1", "EM10") 

# Administracion zonal 
EM10_az <- poblacion2010[, round(mean(P03),1), by = .(adm_zonal)] %>% 
  setnames("V1", "EM10") 

# Parroquial
EM10_pr <- poblacion2010[, round(mean(P03),1), by = .(parroquia)] %>% 
  setnames("V1", "EM10") 

# Sector
EM10_s <- poblacion2010[, round(mean(P03),1), by = .(Sector_DMQ)] %>%
  setnames("V1", "EM10") 

# Grilla COD1000
EM10_1000 <- poblacion2010[, round(mean(P03),1), by = .(COD1000)] %>% 
  setnames("V1", "EM10") 

# Grilla COD500
EM10_500 <- poblacion2010[, round(mean(P03),1), by = .(COD500)] %>% 
  setnames("V1", "EM10") 

# Grilla H3_N8
EM10_N8 <- poblacion2010[, round(mean(P03),1), by = .(H3_N8)] %>% 
  setnames("V1", "EM10") 

# Grilla H3_N9

EM10_N9 <- poblacion2010[, round(mean(P03),1), by = .(H3_N9)] %>% 
  setnames("V1", "EM10") 

## Socio Demográfico/Económico

#P01 = Cual es el Sexo

# Cantón - Sexo
EM10_c_s <- poblacion2010[, round(mean(P03),1), by = .(canton,P01)] %>% 
  dcast(canton~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EM10s_1", ifelse(.x == "2", "EM10s_2", .x))) 

# Administración zonal - Sexo 
EM10_az_s <- poblacion2010[, round(mean(P03),1), by = .(adm_zonal,P01)] %>% 
  dcast(adm_zonal  ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EM10s_1", ifelse(.x == "2", "EM10s_2", .x))) 

# Parroquia - Sexo 
EM10_pr_s <- poblacion2010[, round(mean(P03),1), by = .(parroquia,P01)] %>% 
  dcast(parroquia  ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EM10s_1", ifelse(.x == "2", "EM10s_2", .x))) 

# Sector - Sexo
EM10_s_s <- poblacion2010[, round(mean(P03),1), by = .(Sector_DMQ, P01)] %>% 
  dcast(Sector_DMQ ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EM10s_1", ifelse(.x == "2", "EM10s_2", .x))) 

# Grilla COD1000 - Sexo 
EM10_1000_s <- poblacion2010[, round(mean(P03),1), by = .(COD1000 , P01)] %>% 
  dcast(COD1000  ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EM10s_1", ifelse(.x == "2", "EM10s_2", .x))) 

# Grilla COD500 - Sexo 
EM10_500_s <- poblacion2010[, round(mean(P03),1), by = .(COD500, P01)] %>% 
  dcast(COD500 ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EM10s_1", ifelse(.x == "2", "EM10s_2", .x))) 

# Grilla H3_N8 - Sexo 
EM10_N8_s <- poblacion2010[, round(mean(P03),1), by = .(H3_N8 , P01)] %>% 
  dcast(H3_N8  ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EM10s_1", ifelse(.x == "2", "EM10s_2", .x))) 

# Grilla H3_N9 - Sexo 
EM10_N9_s <- poblacion2010[, round(mean(P03),1), by = .(H3_N9, P01)] %>% 
  dcast(H3_N9 ~ P01, value.var = "V1") %>% 
  rename_with(~ ifelse(.x == "1", "EM10s_1", ifelse(.x == "2", "EM10s_2", .x))) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("EM")

addWorksheet(wb, "EM10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_c", x=EM10_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_az", x=EM10_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_pr", x=EM10_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_s", x=EM10_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "EM10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_1000", x=EM10_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_500", x=EM10_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_N8", x=EM10_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_N9", x=EM10_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


addWorksheet(wb, "EM10_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_c_s", x=EM10_c_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_az_s", x=EM10_az_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_pr_s", x=EM10_pr_s, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_s_s", x=EM10_s_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "EM10_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_1000_s", x=EM10_1000_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_500_s", x=EM10_500_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_N8_s", x=EM10_N8_s, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "EM10_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "EM10_N9_s", x=EM10_N9_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Edad media de la población 2010.xlsx") 
