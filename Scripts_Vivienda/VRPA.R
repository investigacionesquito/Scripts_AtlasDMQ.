# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador: 
# Porcentaje de viviendas con red pública de alcantarillado

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
# V11 = El servicio higiénico de la vivienda es 

vivienda2022[V01<=8 & V0201R==1 , VRPA:=0]
vivienda2022[V01<=8 & V0201R==1 & V11==1, VRPA := 1]

# Cantonal
VRPA_c <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRPA *100, na.rm=T),1), 
                       by=.(canton)] %>% setnames("V1", "VRPA") 

# Administracion zonal 
VRPA_az <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRPA *100, na.rm=T),1), 
                        by=.(adm_zonal)] %>% setnames("V1", "VRPA") 

# Parroquial
VRPA_pr <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRPA *100, na.rm=T),1), 
                        by=.(parroquia)] %>% setnames("V1", "VRPA") 

# Sector
VRPA_s <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRPA *100, na.rm=T),1), 
                       by=.(Sector_DMQ)] %>% setnames("V1", "VRPA") 

# Grilla COD1000
VRPA_1000 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRPA *100, na.rm=T),1), 
                          by=.(COD1000)] %>% setnames("V1", "VRPA") 

# Grilla COD500
VRPA_500 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRPA *100, na.rm=T),1), 
                         by=.(COD500)] %>% setnames("V1", "VRPA") 

# Grilla H3_N8
VRPA_N8 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRPA *100, na.rm=T),1), 
                        by=.(H3_N8)] %>% setnames("V1", "VRPA") 

# Grilla H3_N9
VRPA_N9 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRPA *100, na.rm=T),1), 
                        by=.(H3_N9)] %>% setnames("V1", "VRPA") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("VRPA")

addWorksheet(wb, "VRPA_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRPA_c", x=VRPA_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRPA_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRPA_az", x=VRPA_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRPA_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRPA_pr", x=VRPA_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRPA_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRPA_s", x=VRPA_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "VRPA_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRPA_1000", x=VRPA_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRPA_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRPA_500", x=VRPA_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRPA_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRPA_N8", x=VRPA_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRPA_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRPA_N9", x=VRPA_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas con red pública de alcantarillado.xlsx") 