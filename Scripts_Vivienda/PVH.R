# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  Porcentaje de viviendas particulares con más de un hogar


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

# VO1 = Tipo de vivienda, c(1:8) particulares 
# V17 = Número de hogares, Incluyendo su hogar, ¿Cuántos grupos de personas (hogares) mantienen gastos separados para la alimentación? 
# V0201=1 Condición de ocupación de vivienda particular (ocupada con personas presentes)

vivienda2022[V17 > 1, TVH := 1]
vivienda2022[V01 %in% c(1:8), VP := 1]

# Cantonal
PVH_c = vivienda2022[V01 <= 8 & V0201 == 1, 
                     round(sum(TVH, na.rm =T)/sum(VP, na.rm = T)*100,1), by = .(canton)] %>% 
  setnames("V1", "PVH")  

# Administracion zonal 
PVH_az = vivienda2022[V01 <= 8 & V0201 == 1, 
                      round(sum(TVH, na.rm =T)/sum(VP, na.rm = T)*100,1), by = .(adm_zonal)] %>% 
  setnames("V1", "PVH")

# Parroquial
PVH_pr = vivienda2022[V01 <= 8 & V0201 == 1, 
                      round(sum(TVH, na.rm =T)/sum(VP, na.rm = T)*100,1), by = .(parroquia)] %>% 
  setnames("V1", "PVH")

# Sector
PVH_s = vivienda2022[V01 <= 8 & V0201 == 1, 
                     round(sum(TVH, na.rm =T)/sum(VP, na.rm = T)*100,1), by = .(Sector_DMQ)] %>% 
  setnames("V1", "PVH") 

# Grilla COD1000
PVH_1000 = vivienda2022[V01 <= 8 & V0201 == 1,
                        round(sum(TVH, na.rm =T)/sum(VP, na.rm = T)*100,1), by = .(COD1000)] %>% 
  setnames("V1", "PVH")

# Grilla COD500
PVH_500 = vivienda2022[V01 <= 8 & V0201 == 1,
                       round(sum(TVH, na.rm =T)/sum(VP, na.rm = T)*100,1), by = .(COD500)] %>% 
  setnames("V1", "PVH") 

# Grilla H3_N8
PVH_N8 = vivienda2022[V01 <= 8 & V0201 == 1,
                      round(sum(TVH, na.rm =T)/sum(VP, na.rm = T)*100,1), by = .(H3_N8)] %>% 
  setnames("V1", "PVH")

# Grilla H3_N9
PVH_N9 = vivienda2022[V01 <= 8 & V0201 == 1,
                      round(sum(TVH, na.rm =T)/sum(VP, na.rm = T)*100,1), by = .(H3_N9)] %>% 
  setnames("V1", "PVH") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVH")

addWorksheet(wb, "PVH_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVH_c", x=PVH_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVH_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVH_az", x=PVH_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVH_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVH_pr", x=PVH_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVH_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVH_s", x=PVH_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVH_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVH_1000", x=PVH_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVH_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVH_500", x=PVH_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVH_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVH_N8", x=PVH_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVH_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVH_N9", x=PVH_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas particulares con más de un hogar.xlsx") 