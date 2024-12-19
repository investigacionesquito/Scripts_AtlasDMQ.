# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Porcentaje de viviendas particulares según condición de ocupación 
#(ocupada con personas presentes, ocupada con personas ausentes, de temporada o vacacional, desocupada, en construcción).

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

# VO1 = Tipo de vivienda , c(1:8) particulares 
# V0201 = Condición de ocupación de vivienda particular

#V0201=1 ocupada con personas presentes 
#V0201=2 ocupada con personas ausentes 
#V0201=3 de temporada o vacacional 
#V0201=4 desocupada
#V0201=5 en construcción

# Cantonal
PVPO_c <-  vivienda2022[V01 %in% c(1:8),.(freq=.N),by=.(canton, V0201)] [, 
         prop := round((freq/sum(freq)*100),1), by =.(canton)][,':='(freq = NULL)] %>% 
  dcast(canton ~ V0201, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("PVPO_", .x), .x))  

# Administracion zonal 
PVPO_az <-  vivienda2022[V01 %in% c(1:8),.(freq=.N),by=.(adm_zonal, V0201)] [, 
          prop := round((freq/sum(freq)*100),1), by =.(adm_zonal)][,':='(freq = NULL)] %>% 
  dcast(adm_zonal ~ V0201, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("PVPO_", .x), .x)) 

# Parroquial
PVPO_pr <-  vivienda2022[V01 %in% c(1:8),.(freq=.N),by=.(parroquia, V0201)] [, 
          prop := round((freq/sum(freq)*100),1), by =.(parroquia)][,':='(freq = NULL)] %>% 
  dcast(parroquia ~ V0201, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("PVPO_", .x), .x))
# Sector
PVPO_s <-  vivienda2022[V01 %in% c(1:8),.(freq=.N),by=.(Sector_DMQ, V0201)] [, 
        prop := round((freq/sum(freq)*100),1), by =.(Sector_DMQ)][,':='(freq = NULL)] %>% 
  dcast(Sector_DMQ ~ V0201, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("PVPO_", .x), .x))

# Grilla COD1000
PVPO_1000 <-  vivienda2022[V01 %in% c(1:8),.(freq=.N),by=.(COD1000, V0201)] [, 
          prop := round((freq/sum(freq)*100),1), by =.(COD1000)][,':='(freq = NULL)] %>% 
  dcast(COD1000 ~ V0201, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("PVPO_", .x), .x)) 

# Grilla COD500
PVPO_500 <-  vivienda2022[V01 %in% c(1:8),.(freq=.N),by=.(COD500, V0201)] [,
        prop := round((freq/sum(freq)*100),1), by =.(COD500)][,':='(freq = NULL)] %>% 
  dcast(COD500 ~ V0201, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("PVPO_", .x), .x))

# Grilla H3_N8
PVPO_N8 <-  vivienda2022[V01 %in% c(1:8),.(freq=.N),by=.(H3_N8, V0201)] [, 
        prop := round((freq/sum(freq)*100),1), by =.(H3_N8)][,':='(freq = NULL)] %>% 
  dcast(H3_N8 ~ V0201, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("PVPO_", .x), .x))

# Grilla H3_N9
PVPO_N9 <-  vivienda2022[V01 %in% c(1:8),.(freq=.N),by=.(H3_N9, V0201)] [, 
      prop := round((freq/sum(freq)*100),1), by =.(H3_N9)][,':='(freq = NULL)] %>% 
  dcast(H3_N9 ~ V0201, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:5), paste0("PVPO_", .x), .x))

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVPO")

addWorksheet(wb, "PVPO_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPO_c", x=PVPO_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPO_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPO_az", x=PVPO_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPO_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPO_pr", x=PVPO_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPO_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPO_s", x=PVPO_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVPO_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPO_1000", x=PVPO_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPO_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPO_500", x=PVPO_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPO_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPO_N8", x=PVPO_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPO_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPO_N9", x=PVPO_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas particulares según condición de ocupación.xlsx") 