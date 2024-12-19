# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador: Porcentaje de viviendas particulares con acceso a agua por red pública

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
# V0201R = Condición de ocupación de vivienda particular (si es 1 = ocupada)
# V10 = El agua que recibe la vivienda proviene o es suministrada por

vivienda2022[V01<=8 & V0201R==1, VAA_A := 0]
vivienda2022[V10 %in% c(1:2), VAA_A := 1]


# Cantonal
VAA_c <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VAA_A *100, na.rm=T),1), 
                     by=.(canton)] %>% setnames("V1", "VAA") 

# Administracion zonal 
VAA_az <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VAA_A *100, na.rm=T),1), 
                    by=.(adm_zonal)] %>% setnames("V1", "VAA") 

# Parroquial
VAA_pr <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VAA_A *100, na.rm=T),1), 
                    by=.(parroquia)] %>% setnames("V1", "VAA") 

# Sector
VAA_s <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VAA_A *100, na.rm=T),1), 
                    by=.(Sector_DMQ)] %>% setnames("V1", "VAA") 

# Grilla COD1000
VAA_1000 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VAA_A *100, na.rm=T),1), 
                    by=.(COD1000)] %>% setnames("V1", "VAA") 

# Grilla COD500
VAA_500 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VAA_A *100, na.rm=T),1), 
                    by=.(COD500)] %>% setnames("V1", "VAA") 

# Grilla H3_N8
VAA_N8 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VAA_A *100, na.rm=T),1), 
                    by=.(H3_N8)] %>% setnames("V1", "VAA") 

# Grilla H3_N9
VAA_N9 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VAA_A *100, na.rm=T),1), 
                    by=.(H3_N9)] %>% setnames("V1", "VAA") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("VAA")

addWorksheet(wb, "VAA_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAA_c", x=VAA_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAA_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAA_az", x=VAA_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAA_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAA_pr", x=VAA_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAA_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAA_s", x=VAA_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "VAA_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAA_1000", x=VAA_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAA_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAA_500", x=VAA_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAA_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAA_N8", x=VAA_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAA_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAA_N9", x=VAA_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas particulares con acceso a agua por red pública.xlsx") 
