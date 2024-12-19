# Dimensión: Demografía 

# Nombre del Indicador: 
# Cantidad de oblación de acuerdo a la identificación según cultura y costumbres (Número) 2010

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
               , sjlabelled, stringr,labelled,dplyr, tidyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2010 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

#P16 = Autoidentificación según cultura y costumbres

# 1 Indigena
# 2 Afroecuatoriano
# 3 Negro
# 4 Mulato
# 5 Montubio
# 6 Mestizo
# 7 Blanco
# 8 Otro

# Recodifico la pregunta 16 para poder hacer la comparación 

# 1 Indígena
# 2 Afroecuatoriana/o, Afrodescendiente, Negra/o, Mulata/o
# 3 Montubia/o
# 4 Mestiza/o
# 5 Blanca/o
# 6 Otro

poblacion2010[P16==1, P16R:=1]
poblacion2010[P16 %in% (2:4), P16R:=2]
poblacion2010[P16==5, P16R:=3]
poblacion2010[P16==6, P16R:=4]
poblacion2010[P16==7, P16R:=5]
poblacion2010[P16==8, P16R:=6]

# Desagregaciones

## Geográfico territorial

# Cantón
PPCN10_c <- poblacion2010[, .(freq=.N), by = .(P16R, canton)] %>% 
  dcast(canton~ P16R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN10_", .x), .x))  

# Administración zonal 
PPCN10_az <- poblacion2010[, .(freq=.N), by = .(P16R, adm_zonal)] %>% 
  dcast(adm_zonal ~ P16R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN10_", .x), .x))  

# Parroquial
PPCN10_pr <- poblacion2010[, .(freq=.N), by = .(P16R, parroquia)] %>% 
  dcast(parroquia~ P16R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN10_", .x), .x))  

# Sector
PPCN10_s <- poblacion2010[, .(freq=.N), by = .(P16R, Sector_DMQ)]%>% 
  dcast( Sector_DMQ ~ P16R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN10_", .x), .x))  

# Grilla COD1000
PPCN10_1000 <- poblacion2010[, .(freq=.N), by = .(P16R, COD1000)]%>% 
  dcast(COD1000 ~ P16R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN10_", .x), .x))  

# Grilla COD500
PPCN10_500 <- poblacion2010[, .(freq=.N), by = .(P16R, COD500)]%>% 
  dcast(COD500 ~ P16R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN10_", .x), .x))  

# Grilla H3_N8
PPCN10_N8 <- poblacion2010[, .(freq=.N), by = .(P16R, H3_N8)]%>% 
  dcast(H3_N8 ~ P16R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN10_", .x), .x))  

# Grilla H3_N9
PPCN10_N9 <- poblacion2010[, .(freq=.N), by = .(P16R, H3_N9)] %>% 
  dcast(H3_N9 ~ P16R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPCN10_", .x), .x))  


# 6. Guardar los resultados

wb <-  createWorkbook("PPCN")

addWorksheet(wb, "PPCN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN10_c", x= PPCN10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN10_az", x= PPCN10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN10_pr", x= PPCN10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN10_s", x= PPCN10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN10_1000", x= PPCN10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN10_500", x= PPCN10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN10_N8", x= PPCN10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPCN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPCN10_N9", x= PPCN10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de población de acuerdo a la identificación según cultura y costumbres 2010.xlsx")