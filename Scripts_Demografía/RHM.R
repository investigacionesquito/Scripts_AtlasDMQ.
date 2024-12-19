# Dimensión: Demografía 

# Nombre del Indicador: Relación hombres mujeres

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5 . Calcular indicadores
# 6 . Guardar los resultados

# 1. Descargar bases de datos ---------------------------------------

# 2. Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ----------------------------------------------

pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

poblacion2022[P02==1, Hombre:=1]
poblacion2022[P02==2, Mujer:=1]

# Desagregaciones

## Geográfico territorial

# Cantón
RHM_c <-  poblacion2022[,round((freq=sum(Hombre, na.rm = T)/sum(Mujer,na.rm = T)*100),1), by=.(canton)] %>% 
  setnames("V1", "RHM") 

# Administración zonal 
RHM_az <-  poblacion2022[,round((freq=sum(Hombre, na.rm = T)/sum(Mujer,na.rm = T)*100),1), by=.(adm_zonal)] %>% 
  setnames("V1", "RHM") 

# Parroquial
RHM_pr <-  poblacion2022[,round((freq=sum(Hombre, na.rm = T)/sum(Mujer,na.rm = T)*100),1), by=.(parroquia)] %>% 
  setnames("V1", "RHM") 

# Sector
RHM_s <-  poblacion2022[,round((freq=sum(Hombre, na.rm = T)/sum(Mujer,na.rm = T)*100),1), by=.(Sector_DMQ)] %>% 
  setnames("V1", "RHM") 

# Grilla COD1000
RHM_1000 <-  poblacion2022[,round((freq=sum(Hombre, na.rm = T)/sum(Mujer,na.rm = T)*100),1), by=.(COD1000)] %>% 
  setnames("V1", "RHM") 

# Grilla COD500
RHM_500 <-  poblacion2022[,round((freq=sum(Hombre, na.rm = T)/sum(Mujer,na.rm = T)*100),1), by=.(COD500)] %>% 
  setnames("V1", "RHM") 

# Grilla H3_N8
RHM_N8 <-  poblacion2022[,round((freq=sum(Hombre, na.rm = T)/sum(Mujer,na.rm = T)*100),1), by=.(H3_N8)] %>% 
  setnames("V1", "RHM") 

# Grilla H3_N9
RHM_N9 <-  poblacion2022[,round((freq=sum(Hombre, na.rm = T)/sum(Mujer,na.rm = T)*100),1), by=.(H3_N9)] %>% 
  setnames("V1", "RHM") 

# 6. Guardar los resultados

wb <- createWorkbook("RHM")

addWorksheet(wb, "RHM_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RHM_c", x= RHM_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RHM_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RHM_az", x= RHM_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RHM_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RHM_pr", x= RHM_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RHM_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RHM_s", x= RHM_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "RHM_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RHM_1000", x= RHM_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RHM_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RHM_500", x= RHM_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RHM_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RHM_N8", x= RHM_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "RHM_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "RHM_N9", x= RHM_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Relación hombres_mujeres.xlsx")
