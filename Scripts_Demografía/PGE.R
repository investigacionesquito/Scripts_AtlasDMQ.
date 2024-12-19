# Dimensión: Demografía 

# Nombre del Indicador: 
# Porcentaje de la población por grupos quinquenales de edad

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

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

# Desagregaciones

## Geográfico territorial

# Cantón
PGE_c <- poblacion2022[, .(freq=.N), by = .(GEDAD, canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton)][, 
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(canton ~ GEDAD, value.var = c("prop")) %>% 
  rename_with(~ ifelse(. %in% as.character(1:22), paste0("PGE", .), .))  

# Administracion zonal
PGE_az <- poblacion2022[, .(freq=.N), by = .(GEDAD, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][, 
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ GEDAD, value.var = c("prop")) %>% 
  rename_with(~ ifelse(. %in% as.character(1:22), paste0("PGE", .), .))  

# Parroquial
PGE_pr <- poblacion2022[, .(freq=.N), by = .(GEDAD, parroquia)][,prop := 
         round((freq/sum(freq)*100),1),by=.(parroquia)][, 
        ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(parroquia ~ GEDAD, value.var = c("prop")) %>% 
  rename_with(~ ifelse(. %in% as.character(1:22), paste0("PGE", .), .)) 

# Sector
PGE_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, Sector_DMQ)][,prop := 
         round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,
         ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ ~ GEDAD, value.var = c("prop")) %>% 
  rename_with(~ ifelse(. %in% as.character(1:22), paste0("PGE", .), .))

# Grilla COD1000
PGE_1000 <- poblacion2022[, .(freq=.N), by = .(GEDAD, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][, 
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ GEDAD, value.var = c("prop")) %>% 
  rename_with(~ ifelse(. %in% as.character(1:22), paste0("PGE", .), .)) 

# Grilla COD500
PGE_500 <- poblacion2022[, .(freq=.N), by = .(GEDAD, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][, 
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(COD500 ~ GEDAD, value.var = c("prop")) %>% 
  rename_with(~ ifelse(. %in% as.character(1:22), paste0("PGE", .), .)) 

# Grilla H3_N8
PGE_N8 <- poblacion2022[, .(freq=.N), by = .(GEDAD, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8)][, 
 ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(H3_N8 ~ GEDAD, value.var = c("prop")) %>% 
  rename_with(~ ifelse(. %in% as.character(1:22), paste0("PGE", .), .)) 

# Grilla H3_N9
PGE_N9 <- poblacion2022[, .(freq=.N), by = .(GEDAD, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][, 
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ GEDAD, value.var = c("prop")) %>% 
  rename_with(~ ifelse(. %in% as.character(1:22), paste0("PGE", .), .)) 

# Socio Demográfico/Económico

# Cantón - sexo
PGE_c_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, P02,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,P02)][, 
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(canton + P02 ~ GEDAD, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = as.character(1:22), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:22, "_1") | .x %in% paste0(1:22, "_2"), paste0("PGE", .x), .x)) 

# Administracion zonal- sexo
PGE_az_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, P02,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P02)][, 
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P02 ~ GEDAD, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = as.character(1:22), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:22, "_1") | .x %in% paste0(1:22, "_2"), paste0("PGE", .x), .x))

# Parroquial - sexo
PGE_pr_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, P02,parroquia)][,prop := 
          round((freq/sum(freq)*100),1),by=.(parroquia,P02)][, 
          ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast(parroquia + P02 ~ GEDAD, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = as.character(1:22), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:22, "_1") | .x %in% paste0(1:22, "_2"), paste0("PGE", .x), .x))  

# Sector - sexo 
PGE_s_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, P02, Sector_DMQ)][,prop := 
         round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P02)][,
        ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast( Sector_DMQ + P02~ GEDAD, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:22), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:22, "_1") | .x %in% paste0(1:22, "_2"), paste0("PGE", .x), .x)) 

# Grilla COD1000 - sexo 
PGE_1000_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, P02, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P02)][,
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast( COD1000 + P02~ GEDAD, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:22), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:22, "_1") | .x %in% paste0(1:22, "_2"), paste0("PGE", .x), .x)) 

# Grilla COD500 - sexo 
PGE_500_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, P02, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P02)][,
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast( COD500 + P02~ GEDAD, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:22), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:22, "_1") | .x %in% paste0(1:22, "_2"), paste0("PGE", .x), .x)) 

# Grilla H3_N8 - sexo 
PGE_N8_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, P02, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P02)][,
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast( H3_N8 + P02~ GEDAD, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:22), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:22, "_1") | .x %in% paste0(1:22, "_2"), paste0("PGE", .x), .x)) 

# Grilla H3_N9 - sexo 
PGE_N9_s <- poblacion2022[, .(freq=.N), by = .(GEDAD, P02, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,P02)][,
  ':='(freq = NULL)][order(GEDAD,decreasing = FALSE )] %>% 
  dcast( H3_N9 + P02~ GEDAD, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:22), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:22, "_1") | .x %in% paste0(1:22, "_2"), paste0("PGE", .x), .x)) 

# 6. Guardar los resultados

wb <-  createWorkbook("PGE")

addWorksheet(wb, "PGE_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_c", x= PGE_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_az", x= PGE_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_pr", x= PGE_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_s", x= PGE_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PGE_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_1000", x= PGE_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_500", x= PGE_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_N8", x= PGE_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_N9", x= PGE_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PGE_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_c_s", x= PGE_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_az_s", x= PGE_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_pr_s", x= PGE_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_s_s", x= PGE_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PGE_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_1000_s", x= PGE_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_500_s", x= PGE_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_N8_s", x= PGE_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PGE_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PGE_N9_s", x= PGE_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, " Porcentaje de la población por grupos quinquenales de edad.xlsx")

