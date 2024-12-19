# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Número de viviendas particulares ocupadas con personas presentes según tipo de vivienda. 
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
PVPTN10_c <- vivienda2010[VTV %in% c(1:8)& VCO == 1,.(freq=.N),by=.(canton,VPT)] %>% 
  dcast(canton ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN10_", .x), .x))  

# Administracion zonal 
PVPTN10_az <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(adm_zonal,VPT)] %>% 
  dcast(adm_zonal ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN10_", .x), .x)) 

# Parroquial
PVPTN10_pr <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(parroquia,VPT)]%>% 
  dcast(parroquia ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN10_", .x), .x)) 

# Sector
PVPTN10_s <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(Sector_DMQ,VPT)] %>% 
  dcast(Sector_DMQ ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN10_", .x), .x)) 

# Grilla COD1000
PVPTN10_1000 <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(COD1000,VPT)]%>% 
  dcast(COD1000 ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN10_", .x), .x)) 

# Grilla COD500
PVPTN10_500 <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(COD500,VPT)]%>% 
  dcast(COD500 ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN10_", .x), .x)) 

# Grilla H3_N8
PVPTN10_N8 <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(H3_N8,VPT)]%>% 
  dcast(H3_N8 ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN10_", .x), .x)) 

# Grilla H3_N9
PVPTN10_N9 <- vivienda2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(H3_N9,VPT)] %>% 
  dcast(H3_N9 ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN10_", .x), .x)) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVPTN10")

addWorksheet(wb, "PVPTN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN10_c", x=PVPTN10_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN10_az", x=PVPTN10_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN10_pr", x=PVPTN10_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN10_s", x=PVPTN10_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVPTN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN10_1000", x=PVPTN10_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN10_500", x=PVPTN10_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN10_N8", x=PVPTN10_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN10_N9", x=PVPTN10_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Número de viviendas particulares ocupadas con personas presentes según tipo de vivienda 2010.xlsx") 