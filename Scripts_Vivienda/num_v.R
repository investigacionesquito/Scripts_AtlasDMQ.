# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  Cantidad de viviendas particulares 

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

vivienda2022 <- read_sav("vivienda2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores -------------------------------------------

# VO1 = Tipo de vivienda 

vivienda2022[V01<=8 & V0201R==1, CV := 1]

# Cantonal
num_v_c <-  vivienda2022[V01<=8 & V0201R==1, sum (CV, na.rm=T), 
                        by=.(canton)] %>% setnames("V1", "num_v") 

# Administracion zonal 
num_v_az <-  vivienda2022[V01<=8 & V0201R==1, sum(CV , na.rm=T), 
                         by=.(adm_zonal)] %>% setnames("V1", "num_V") 

# Parroquial
num_v_pr <-  vivienda2022[V01<=8 & V0201R==1, sum(CV, na.rm=T), 
                         by=.(parroquia)] %>% setnames("V1", "num_v") 

# Sector
num_v_s <-  vivienda2022[V01<=8 & V0201R==1, sum (CV, na.rm=T), 
                        by=.(Sector_DMQ)] %>% setnames("V1", "num_v") 

# Grilla COD1000
num_v_1000 <-  vivienda2022[V01<=8 & V0201R==1, sum (CV, na.rm=T), 
                           by=.(COD1000)] %>% setnames("V1", "num_v") 

# Grilla COD500
num_v_500 <-  vivienda2022[V01<=8 & V0201R==1, sum (CV, na.rm=T), 
                          by=.(COD500)] %>% setnames("V1", "num_v") 

# Grilla H3_N8
num_v_N8 <-  vivienda2022[V01<=8 & V0201R==1, sum (CV, na.rm=T), 
                         by=.(H3_N8)] %>% setnames("V1", "num_v") 

# Grilla H3_N9
num_v_N9 <-  vivienda2022[V01<=8 & V0201R==1, sum (CV, na.rm=T), 
                         by=.(H3_N9)] %>% setnames("V1", "num_v") 

# 6. Guardar los resultados ----------------------------------------------------

wb <-  createWorkbook("num_v")

addWorksheet(wb, "num_v_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v_c", x=num_v_c, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v_az", x=num_v_az, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v_pr", x=num_v_pr, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v_s", x=num_v_s, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "num_v_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v_1000", x=num_v_1000, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v_500", x=num_v_500, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v_N8", x=num_v_N8, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "num_v_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "num_v_N9", x=num_v_N9, startRow = 1,rowNames = FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Número de Viviendas particulares.xlsx")

