# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Porcentaje de viviendas según tipo de vivienda (particulares y colectivas)

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

vivienda2022[V01 %in% 1:8, VPT2 := 1] # Particular
vivienda2022[V01 %in% 9:18, VPT2 := 2] # Colectiva

# Cantonal
PVTV_c <- vivienda2022[V01 %in% c(1:18),.(freq=.N),by=.(canton,VPT2)][, 
        prop := round((freq/sum(freq)*100),1), by =.(canton)][,':='(freq = NULL)] %>% 
  dcast(canton ~ VPT2, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PVTV_", .x), .x))  

# Administracion zonal 
PVTV_az <- vivienda2022[V01 %in% c(1:18),.(freq=.N),by=.(adm_zonal,VPT2)][, 
        prop := round((freq/sum(freq)*100),1), by =.(adm_zonal)][,':='(freq = NULL)] %>% 
  dcast(adm_zonal ~ VPT2, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PVTV_", .x), .x)) 

# Parroquial
PVTV_pr <- vivienda2022[V01 %in% c(1:18),.(freq=.N),by=.(parroquia,VPT2)][, 
        prop := round((freq/sum(freq)*100),1), by =.(parroquia)][,':='(freq = NULL)] %>% 
  dcast(parroquia ~ VPT2, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PVTV_", .x), .x)) 

# Sector
PVTV_s <- vivienda2022[V01 %in% c(1:18),.(freq=.N),by=.(Sector_DMQ,VPT2)][, 
      prop := round((freq/sum(freq)*100),1), by =.(Sector_DMQ)][,':='(freq = NULL)] %>% 
  dcast(Sector_DMQ ~ VPT2, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PVTV_", .x), .x)) 

# Grilla COD1000
PVTV_1000 <- vivienda2022[V01 %in% c(1:18),.(freq=.N),by=.(COD1000,VPT2)][, 
          prop := round((freq/sum(freq)*100),1), by =.(COD1000)][,':='(freq = NULL)] %>% 
  dcast(COD1000 ~ VPT2, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PVTV_", .x), .x)) 

# Grilla COD500
PVTV_500 <- vivienda2022[V01 %in% c(1:18),.(freq=.N),by=.(COD500,VPT2)][, 
         prop := round((freq/sum(freq)*100),1), by =.(COD500)][,':='(freq = NULL)] %>% 
  dcast(COD500 ~ VPT2, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PVTV_", .x), .x)) 

# Grilla H3_N8
PVTV_N8 <- vivienda2022[V01 %in% c(1:18),.(freq=.N),by=.(H3_N8,VPT2)][, 
        prop := round((freq/sum(freq)*100),1), by =.(H3_N8)][,':='(freq = NULL)] %>% 
  dcast(H3_N8 ~ VPT2, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PVTV_", .x), .x)) 

# Grilla H3_N9
PVTV_N9 <- vivienda2022[V01 %in% c(1:18),.(freq=.N),by=.(H3_N9,VPT2)][, 
        prop := round((freq/sum(freq)*100),1), by =.(H3_N9)][,':='(freq = NULL)] %>% 
  dcast(H3_N9 ~ VPT2, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PVTV_", .x), .x)) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVTV")

addWorksheet(wb, "PVTV_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVTV_c", x=PVTV_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVTV_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVTV_az", x=PVTV_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVTV_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVTV_pr", x=PVTV_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVTV_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVTV_s", x=PVTV_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVTV_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVTV_1000", x=PVTV_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVTV_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVTV_500", x=PVTV_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVTV_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVTV_N8", x=PVTV_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVTV_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVTV_N9", x=PVTV_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas según tipo de vivienda (particulares y colectivas).xlsx") 