# Dimensión: Demografía 

# Nombre del Indicador: Promedio de personas por hogar

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3. Cargar librerías
# 4. Importar base de datos
# 5. Filtrar las bases 
# 6. Calcular indicadores
# 7. Guardar los resultados

# 1. Descargar bases de datos----------------------------------------------------

# 2. Establecer el directorio ---------------------------------------------------
setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3. Cargar librerías-----------------------------------------------------------

pacman::p_load(data.table,openxlsx, foreign, haven, dplyr)

# 4. Importar base de datos-----------------------------------------------------

hogar2022 <- read_sav("hogar2022_Atlas.sav") %>%  as.data.table() 

# 5. Calcular indicadores ------------------------------------------------------

hogar2022$INH <- as.numeric(hogar2022$INH) #en la base original se encontraba como tipo caracter 

# Desagregacion es:

## Geográfico/Territorial

# Cantón 
HPM_c <- hogar2022[INH>=1,round(mean(H1303,na.rm =T),1), by=.(canton)] %>% 
  setnames ("V1", "HPM")  

# Administracion zonal

HPM_az <- hogar2022[INH>=1,round(mean(H1303,na.rm =T),1), by=.(adm_zonal)] %>% 
  setnames ("V1", "HPM") 

# Parroquial
HPM_pr <- hogar2022[INH>=1,round(mean(H1303,na.rm =T),1), by=.(parroquia)] %>% 
  setnames ("V1", "HPM")  

# Sector
HPM_s <- hogar2022[INH>=1,round(mean(H1303,na.rm =T),1), by=.(Sector_DMQ)] %>% 
  setnames ("V1", "HPM") 

# Grilla COD1000
HPM_1000 <- hogar2022[INH>=1,round(mean(H1303,na.rm =T),1), by=.(COD1000)] %>% 
  setnames ("V1", "HPM") 

# Grilla COD500
HPM_500 <- hogar2022[INH>=1,round(mean(H1303,na.rm =T),1), by=.(COD500)] %>% 
  setnames ("V1", "HPM") 

# Grilla H3_N8
HPM_N8 <- hogar2022[INH>=1,round(mean(H1303,na.rm =T),1), by=.(H3_N8)] %>% 
  setnames ("V1", "HPM") 

# Grilla H3_N9
HPM_N9 <- hogar2022[INH>=1,round(mean(H1303,na.rm =T),1), by=.(H3_N9)] %>% 
  setnames ("V1", "HPM") 

# 6. Guardar los resultados ----------------------------------------------------

wb <- createWorkbook("HPM")

addWorksheet(wb, "HPM_c", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HPM_c", x= HPM_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HPM_az", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HPM_az", x= HPM_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HPM_pr", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HPM_pr", x= HPM_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HPM_s", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HPM_s", x= HPM_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HPM_1000", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HPM_1000", x= HPM_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HPM_500", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HPM_500", x= HPM_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HPM_N9", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HPM_N9", x= HPM_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HPM_N8", gridLines = FALSE, tabColour = "#FFFFFF")
writeDataTable(wb, sheet = "HPM_N8", x= HPM_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Promedio de personas por hogar.xlsx", overwrite = TRUE)
