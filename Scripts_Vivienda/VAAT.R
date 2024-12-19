# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  Viviendas particulares con procedencia de agua por tubería, dentro o fuera de la vivienda

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
VAAT_c <-  vivienda2022[V01<=8 & V0201R==1, sum (VAA_A, na.rm=T), 
                       by=.(canton)] %>% setnames("V1", "VAAT") 

# Administracion zonal 
VAAT_az <-  vivienda2022[V01<=8 & V0201R==1, sum(VAA_A , na.rm=T), 
                        by=.(adm_zonal)] %>% setnames("V1", "VAAT") 

# Parroquial
VAAT_pr <-  vivienda2022[V01<=8 & V0201R==1, sum(VAA_A *100, na.rm=T), 
                        by=.(parroquia)] %>% setnames("V1", "VAAT") 

# Sector
VAAT_s <-  vivienda2022[V01<=8 & V0201R==1, sum (VAA_A *100, na.rm=T), 
                       by=.(Sector_DMQ)] %>% setnames("V1", "VAAT") 

# Grilla COD1000
VAAT_1000 <-  vivienda2022[V01<=8 & V0201R==1, sum (VAA_A *100, na.rm=T), 
                          by=.(COD1000)] %>% setnames("V1", "VAAT") 

# Grilla COD500
VAAT_500 <-  vivienda2022[V01<=8 & V0201R==1, sum (VAA_A *100, na.rm=T), 
                         by=.(COD500)] %>% setnames("V1", "VAAT") 

# Grilla H3_N8
VAAT_N8 <-  vivienda2022[V01<=8 & V0201R==1, sum (VAA_A *100, na.rm=T), 
                        by=.(H3_N8)] %>% setnames("V1", "VAAT") 

# Grilla H3_N9
VAAT_N9 <-  vivienda2022[V01<=8 & V0201R==1, sum (VAA_A *100, na.rm=T), 
                        by=.(H3_N9)] %>% setnames("V1", "VAAT") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("VAAT")

addWorksheet(wb, "VAAT_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAAT_c", x=VAAT_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAAT_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAAT_az", x=VAAT_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAAT_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAAT_pr", x=VAAT_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAAT_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAAT_s", x=VAAT_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "VAAT_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAAT_1000", x=VAAT_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAAT_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAAT_500", x=VAAT_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAAT_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAAT_N8", x=VAAT_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VAAT_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VAAT_N9", x=VAAT_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Viviendas particulares con procedencia de agua por tubería, dentro o fuera de la vivienda.xlsx") 