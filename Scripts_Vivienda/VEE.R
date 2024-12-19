# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador: 
#Porcentaje de viviendas particulares con disponibilidad de energía eléctrica proveniente de la red pública

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
# V12 = Disponibilidad de energía eléctrica por red pública

vivienda2022[V01<=8 & V0201R==1 , VEE:=0]
vivienda2022[V01<=8 & V0201R==1 & V12==1, VEE := 1]

# Cantonal
VEE_c <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), 
                       by=.(canton)] %>% setnames("V1", "VEE") 

# Administracion zonal 
VEE_az <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), 
                        by=.(adm_zonal)] %>% setnames("V1", "VEE") 

# Parroquial
VEE_pr <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), 
                        by=.(parroquia)] %>% setnames("V1", "VEE") 

# Sector
VEE_s <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), 
                       by=.(Sector_DMQ)] %>% setnames("V1", "VEE") 

# Grilla COD1000
VEE_1000 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), 
                          by=.(COD1000)] %>% setnames("V1", "VEE") 

# Grilla COD500
VEE_500 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), 
                         by=.(COD500)] %>% setnames("V1", "VEE") 

# Grilla H3_N8
VEE_N8 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), 
                        by=.(H3_N8)] %>% setnames("V1", "VEE") 

# Grilla H3_N9
VEE_N9 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VEE *100, na.rm=T),1), 
                        by=.(H3_N9)] %>% setnames("V1", "VEE") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("VEE")

addWorksheet(wb, "VEE_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VEE_c", x=VEE_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VEE_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VEE_az", x=VEE_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VEE_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VEE_pr", x=VEE_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VEE_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VEE_s", x=VEE_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "VEE_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VEE_1000", x=VEE_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VEE_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VEE_500", x=VEE_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VEE_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VEE_N8", x=VEE_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VEE_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VEE_N9", x=VEE_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas particulares con disponibilidad de energía eléctrica.xlsx") 