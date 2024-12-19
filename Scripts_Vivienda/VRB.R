# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador: 
#Porcentaje de viviendas particulares que disponen del servicio de recolección de basura

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
# V14 = Eliminación de la basura

vivienda2022[V01<=8 & V0201R==1 , VRB_EB:=0]
vivienda2022[V01<=8 & V0201R==1 & V14 %in% c(1:2), VRB_EB := 1]

# Cantonal
VRB_c <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRB_EB *100, na.rm=T),1), 
                       by=.(canton)] %>% setnames("V1", "VRB") 

# Administracion zonal 
VRB_az <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRB_EB *100, na.rm=T),1), 
                        by=.(adm_zonal)] %>% setnames("V1", "VRB") 

# Parroquial
VRB_pr <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRB_EB *100, na.rm=T),1), 
                        by=.(parroquia)] %>% setnames("V1", "VRB") 

# Sector
VRB_s <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRB_EB *100, na.rm=T),1), 
                       by=.(Sector_DMQ)] %>% setnames("V1", "VRB") 

# Grilla COD1000
VRB_1000 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRB_EB *100, na.rm=T),1), 
                          by=.(COD1000)] %>% setnames("V1", "VRB") 

# Grilla COD500
VRB_500 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRB_EB *100, na.rm=T),1), 
                         by=.(COD500)] %>% setnames("V1", "VRB") 

# Grilla H3_N8
VRB_N8 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRB_EB *100, na.rm=T),1), 
                        by=.(H3_N8)] %>% setnames("V1", "VRB") 

# Grilla H3_N9
VRB_N9 <-  vivienda2022[V01<=8 & V0201R==1, round(mean (VRB_EB *100, na.rm=T),1), 
                        by=.(H3_N9)] %>% setnames("V1", "VRB") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("VRB")

addWorksheet(wb, "VRB_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRB_c", x=VRB_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRB_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRB_az", x=VRB_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRB_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRB_pr", x=VRB_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRB_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRB_s", x=VRB_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "VRB_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRB_1000", x=VRB_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRB_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRB_500", x=VRB_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRB_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRB_N8", x=VRB_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "VRB_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "VRB_N9", x=VRB_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas particulares que disponen del servicio de recolección de basura.xlsx") 
