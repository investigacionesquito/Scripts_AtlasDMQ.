# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Porcentaje de viviendas particulares ocupadas con personas presentes según tipo de vivienda. 
#(casa/villa, departamento en casa o edificio, cuarto/s en casa de inquilinato, mediagua, rancho, covacha, choza, otra vivienda particular) 2010.

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

vivienda2010 <- read_sav("vivienda2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores -------------------------------------------

# VTV = Tipo de vivienda 
# VCO = Condición de ocupación de vivienda particular (si es 1 = ocupada)

vivienda2010[VTV==1, VPT := 1] #Casa/villa
vivienda2010[VTV==2, VPT := 2] #Departamento en casa o edificio
vivienda2010[VTV==3, VPT := 3] #Cuarto/s en casa de inquilinato
vivienda2010[VTV==4, VPT := 4] #Mediagua
vivienda2010[VTV==5, VPT := 5] #Rancho
vivienda2010[VTV==6, VPT := 6] #Covacha
vivienda2010[VTV==7, VPT := 7] #Choza
vivienda2010[VTV==8, VPT := 8] #Otra vivienda particular

# Cantonal
PVPT10_c <- vivienda2010[VTV %in% c(1:8)& VCO == 1,.(freq=.N),by=.(canton,VPT)][, 
          prop := round((freq/sum(freq)*100),1), by =.(canton)][,':='(freq = NULL)] %>% 
  dcast(canton ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT10_", .x), .x))  

# Administracion zonal 
PVPT10_az <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(adm_zonal,VPT)][, 
           prop := round((freq/sum(freq)*100),1), by =.(adm_zonal)][,':='(freq = NULL)] %>% 
  dcast(adm_zonal ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT10_", .x), .x)) 

# Parroquial
PVPT10_pr <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(parroquia,VPT)][, 
          prop := round((freq/sum(freq)*100),1), by =.(parroquia)][,':='(freq = NULL)] %>% 
  dcast(parroquia ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT10_", .x), .x)) 

# Sector
PVPT10_s <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(Sector_DMQ,VPT)][, 
           prop := round((freq/sum(freq)*100),1), by =.(Sector_DMQ)][,':='(freq = NULL)] %>% 
  dcast(Sector_DMQ ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT10_", .x), .x)) 

# Grilla COD1000
PVPT10_1000 <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(COD1000,VPT)][, 
            prop := round((freq/sum(freq)*100),1), by =.(COD1000)][,':='(freq = NULL)] %>% 
  dcast(COD1000 ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT10_", .x), .x)) 

# Grilla COD500
PVPT10_500 <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(COD500,VPT)][, 
            prop := round((freq/sum(freq)*100),1), by =.(COD500)][,':='(freq = NULL)] %>% 
  dcast(COD500 ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT10_", .x), .x)) 

# Grilla H3_N8
PVPT10_N8 <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(H3_N8,VPT)][, 
           prop := round((freq/sum(freq)*100),1), by =.(H3_N8)][,':='(freq = NULL)] %>% 
  dcast(H3_N8 ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT10_", .x), .x)) 

# Grilla H3_N9
PVPT10_N9 <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(H3_N9,VPT)][, 
          prop := round((freq/sum(freq)*100),1), by =.(H3_N9)][,':='(freq = NULL)] %>% 
  dcast(H3_N9 ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT10_", .x), .x)) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVPT10")

addWorksheet(wb, "PVPT10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT10_c", x=PVPT10_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT10_az", x=PVPT10_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT10_pr", x=PVPT10_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT10_s", x=PVPT10_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVPT10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT10_1000", x=PVPT10_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT10_500", x=PVPT10_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT10_N8", x=PVPT10_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT10_N9", x=PVPT10_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas particulares ocupadas con personas presentes según tipo de vivienda 2010.xlsx") 