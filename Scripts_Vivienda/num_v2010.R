# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  Cantidad de viviendas particulares 2010

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

vivienda2010 <- read_sav("vivienda2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores -------------------------------------------

# VTV = Tipo de vivienda 
# VCO = Condición de ocupación de vivienda particular (si es 1 = ocupada)

vivienda2010[VTV<=8 & VCO==1, CV := 1]

# Cantonal
num_v2010_c <-  vivienda2010[VTV<=8 & VCO==1, sum (CV, na.rm=T), 
                        by=.(canton)] %>% setnames("V1", "num_v2010") 

# Administracion zonal 
num_v2010_az <-  vivienda2010[VTV<=8 & VCO==1, sum(CV , na.rm=T), 
                         by=.(adm_zonal)] %>% setnames("V1", "num_v2010") 

# Parroquial
num_v2010_pr <-  vivienda2010[VTV<=8 & VCO==1, sum(CV, na.rm=T), 
                         by=.(parroquia)] %>% setnames("V1", "num_v2010") 

# Sector
num_v2010_s <-  vivienda2010[VTV<=8 & VCO==1, sum (CV, na.rm=T), 
                        by=.(Sector_DMQ)] %>% setnames("V1", "num_v2010") 

# Grilla COD1000
num_v2010_1000 <-  vivienda2010[VTV<=8 & VCO==1, sum (CV, na.rm=T), 
                           by=.(COD1000)] %>% setnames("V1", "num_v2010") 

# Grilla COD500
num_v2010_500 <-  vivienda2010[VTV<=8 & VCO==1, sum (CV, na.rm=T), 
                          by=.(COD500)] %>% setnames("V1", "num_v2010") 

# Grilla H3_N8
num_v2010_N8 <-  vivienda2010[VTV<=8 & VCO==1, sum (CV, na.rm=T), 
                         by=.(H3_N8)] %>% setnames("V1", "num_v2010") 

# Grilla H3_N9
num_v2010_N9 <-  vivienda2010[VTV<=8 & VCO==1, sum (CV, na.rm=T), 
                         by=.(H3_N9)] %>% setnames("V1", "num_v2010") 

# 6. Guardar los resultados ----------------------------------------------------

wb <-  createWorkbook("num_v2010")

addWorksheet(wb, "num_v2010_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v2010_c", x=num_v2010_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v2010_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v2010_az", x=num_v2010_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v2010_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v2010_pr", x=num_v2010_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v2010_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v2010_s", x=num_v2010_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "num_v2010_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v2010_1000", x=num_v2010_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v2010_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v2010_500", x=num_v2010_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v2010_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v2010_N8", x=num_v2010_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v2010_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v2010_N9", x=num_v2010_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Número de Viviendas particulares 2010.xlsx")

