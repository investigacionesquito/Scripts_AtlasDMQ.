# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador: Hacinamiento 

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3. Cargar librerías
# 4. Importar base de datos
# 5. Filtrar las bases 
# 6. Calcular indicadores
# 7. Guardar los resultados

# 1. Descargar bases de datos----------------------------------------------------

# 2. Establecer el directorio ---------------------------------------------------
setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3. Cargar librerías-----------------------------------------------------------

pacman::p_load(data.table,openxlsx, foreign, haven, dplyr)

# 4. Importar base de datos-----------------------------------------------------

hogar2022 <- read_sav("hogar2022_Atlas.sav") %>%  as.data.table() 

# 5. Calcular indicadores ------------------------------------------------------

# Se considera deficitario si
# r
# *Cuando en el hogar no existen dormitorios exclusivos para dormir, se asume que existe uno.

#H1303: Total de personas en el hogar
#H01: Número de dormitorios

hogar2022$INH <- as.numeric(hogar2022$INH) #en la base original se encontraba como tipo caracter 

# Relación personas por dormitorio

hogar2022[H01>0, PERDOR:=H1303/H01]
hogar2022[H01==0, PERDOR:=H1303]
hogar2022[is.na(H01), PERDOR:=NA] 

# Resultado dimensión
hogar2022[PERDOR<=3, comp5:=0] # No hacinamiento 
hogar2022[PERDOR>3, comp5:=1]  # Si hacinamiento 
hogar2022[is.na(PERDOR), comp5:=NA]

# Desagregacion es:

## Geográfico/Territorial

# Cantón 

HAC_c <- hogar2022[INH >= 1, .(
  total_hog = .N,  # Total de hogares que cumplen con INH >= 1
  h_haci = sum(comp5 == 1, na.rm = TRUE),  # Total de hogares con hacinamiento
  h_no_haci = sum(comp5 == 0, na.rm = TRUE),  # Total de hogares sin hacinamiento
  # Porcentajes de hogares
  porc_haci = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  # Porcentaje de hogares con hacinamiento
  porc_no_haci = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100  # Porcentaje de hogares sin hacinamiento
), by = canton]

# Administracion zonal

HAC_az <- hogar2022[INH >= 1, .(
  total_hog = .N,  # Total de hogares que cumplen con INH >= 1
  h_haci = sum(comp5 == 1, na.rm = TRUE),  # Total de hogares con hacinamiento
  h_no_haci = sum(comp5 == 0, na.rm = TRUE),  # Total de hogares sin hacinamiento
  # Porcentajes de hogares
  porc_haci = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  # Porcentaje de hogares con hacinamiento
  porc_no_haci = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100  # Porcentaje de hogares sin hacinamiento
), by = adm_zonal]

# Parroquial
HAC_pr <- hogar2022[INH >= 1, .(
  total_hog = .N,  # Total de hogares que cumplen con INH >= 1
  h_haci = sum(comp5 == 1, na.rm = TRUE),  # Total de hogares con hacinamiento
  h_no_haci = sum(comp5 == 0, na.rm = TRUE),  # Total de hogares sin hacinamiento
  # Porcentajes de hogares
  porc_haci = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  # Porcentaje de hogares con hacinamiento
  porc_no_haci = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100  # Porcentaje de hogares sin hacinamiento
), by = parroquia]  

# Sector
HAC_s <- hogar2022[INH >= 1, .(
  total_hog = .N,  # Total de hogares que cumplen con INH >= 1
  h_haci = sum(comp5 == 1, na.rm = TRUE),  # Total de hogares con hacinamiento
  h_no_haci = sum(comp5 == 0, na.rm = TRUE),  # Total de hogares sin hacinamiento
  # Porcentajes de hogares
  porc_haci = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  # Porcentaje de hogares con hacinamiento
  porc_no_haci = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100  # Porcentaje de hogares sin hacinamiento
), by = Sector_DMQ]

# Grilla COD1000
HAC_1000 <- hogar2022[INH >= 1, .(
  total_hog = .N,  # Total de hogares que cumplen con INH >= 1
  h_haci = sum(comp5 == 1, na.rm = TRUE),  # Total de hogares con hacinamiento
  h_no_haci = sum(comp5 == 0, na.rm = TRUE),  # Total de hogares sin hacinamiento
  # Porcentajes de hogares
  porc_haci = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  # Porcentaje de hogares con hacinamiento
  porc_no_haci = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100  # Porcentaje de hogares sin hacinamiento
), by = COD1000]

# Grilla COD500
HAC_500 <- hogar2022[INH >= 1, .(
  total_hog = .N,  # Total de hogares que cumplen con INH >= 1
  h_haci = sum(comp5 == 1, na.rm = TRUE),  # Total de hogares con hacinamiento
  h_no_haci = sum(comp5 == 0, na.rm = TRUE),  # Total de hogares sin hacinamiento
  # Porcentajes de hogares
  porc_haci = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  # Porcentaje de hogares con hacinamiento
  porc_no_haci = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100  # Porcentaje de hogares sin hacinamiento
), by = COD500] 

# Grilla H3_N8
HAC_N8 <- hogar2022[INH >= 1, .(
  total_hog = .N,  # Total de hogares que cumplen con INH >= 1
  h_haci = sum(comp5 == 1, na.rm = TRUE),  # Total de hogares con hacinamiento
  h_no_haci = sum(comp5 == 0, na.rm = TRUE),  # Total de hogares sin hacinamiento
  # Porcentajes de hogares
  porc_haci = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  # Porcentaje de hogares con hacinamiento
  porc_no_haci = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100  # Porcentaje de hogares sin hacinamiento
), by = H3_N8]

# Grilla H3_N9
HAC_N9 <- hogar2022[INH >= 1, .(
  total_hog = .N,  # Total de hogares que cumplen con INH >= 1
  h_haci = sum(comp5 == 1, na.rm = TRUE),  # Total de hogares con hacinamiento
  h_no_haci = sum(comp5 == 0, na.rm = TRUE),  # Total de hogares sin hacinamiento
  # Porcentajes de hogares
  porc_haci = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  # Porcentaje de hogares con hacinamiento
  porc_no_haci = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100  # Porcentaje de hogares sin hacinamiento
), by = H3_N9] 

# 6. Guardar los resultados ----------------------------------------------------

wb <- createWorkbook("HAC")

addWorksheet(wb, "HAC_c", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HAC_c", x= HAC_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_az", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HAC_az", x= HAC_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_pr", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HAC_pr", x= HAC_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_s", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HAC_s", x= HAC_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HAC_1000", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HAC_1000", x= HAC_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_500", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HAC_500", x= HAC_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_N9", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HAC_N9", x= HAC_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_N8", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HAC_N8", x= HAC_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Hacinamiento.xlsx", overwrite = TRUE)