# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  Porcentaje de Viviendas con por lo menos una caracteristica (Techo, Paredes, Piso) en mal estado

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
# V04: Estado del techo o cubierta
# V06: Estado de las paredes exteriores
# V08: Estado del piso

vivienda2022[V01 %in% c(1:8), VP := 1]
vivienda2022[, mal_estado := 0]
vivienda2022[(V04==3 | V06==3 | V08==3),mal_estado:=1]

# Cantonal
PVCE_c = vivienda2022[V01 <= 8 , round(sum(mal_estado, na.rm =T)/sum(VP, na.rm = T)*100,1), 
                      by = .(canton)] %>% setnames("V1", "PVCE")  

# Administracion zonal 
PVCE_az = vivienda2022[V01 <= 8 , round(sum(mal_estado, na.rm =T)/sum(VP, na.rm = T)*100,1), 
                       by = .(adm_zonal)] %>% setnames("V1", "PVCE")

# Parroquial
PVCE_pr = vivienda2022[V01 <= 8 , round(sum(mal_estado, na.rm =T)/sum(VP, na.rm = T)*100,1), 
                       by = .(parroquia)] %>% setnames("V1", "PVCE")

# Sector
PVCE_s = vivienda2022[V01 <= 8 , round(sum(mal_estado, na.rm =T)/sum(VP, na.rm = T)*100,1), 
                      by = .(Sector_DMQ)] %>% setnames("V1", "PVCE") 

# Grilla COD1000
PVCE_1000 = vivienda2022[V01 <= 8 , round(sum(mal_estado, na.rm =T)/sum(VP, na.rm = T)*100,1), 
                         by = .(COD1000)] %>% setnames("V1", "PVCE")

# Grilla COD500
PVCE_500 = vivienda2022[V01 <= 8 , round(sum(mal_estado, na.rm =T)/sum(VP, na.rm = T)*100,1), 
                        by = .(COD500)] %>% setnames("V1", "PVCE") 

# Grilla H3_N8
PVCE_N8 = vivienda2022[V01 <= 8 , round(sum(mal_estado, na.rm =T)/sum(VP, na.rm = T)*100,1), 
                       by = .(H3_N8)] %>% setnames("V1", "PVCE")

# Grilla H3_N9
PVCE_N9 = vivienda2022[V01 <= 8 , round(sum(mal_estado, na.rm =T)/sum(VP, na.rm = T)*100,1), 
                       by = .(H3_N9)] %>% setnames("V1", "PVCE") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVCE")

addWorksheet(wb, "PVCE_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVCE_c", x=PVCE_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVCE_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVCE_az", x=PVCE_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVCE_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVCE_pr", x=PVCE_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVCE_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVCE_s", x=PVCE_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVCE_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVCE_1000", x=PVCE_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVCE_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVCE_500", x=PVCE_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVCE_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVCE_N8", x=PVCE_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVCE_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVCE_N9", x=PVCE_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de Viviendas con por lo menos una caracteristica (Techo, Paredes, Piso) en mal estado.xlsx") 