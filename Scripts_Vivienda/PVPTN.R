# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Número de viviendas particulares ocupadas con personas presentes según tipo de vivienda. 
#(casa/villa, departamento en casa o edificio, cuarto/s en casa de inquilinato, mediagua, rancho, covacha, choza, otra vivienda particular).

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
# V0201=1 Condición de ocupación de vivienda particular (ocupada con personas presentes)

vivienda2022[V01==1, VPT := 1] #Casa/villa
vivienda2022[V01==2, VPT := 2] #Departamento en casa o edificio
vivienda2022[V01==3, VPT := 3] #Cuarto/s en casa de inquilinato
vivienda2022[V01==4, VPT := 4] #Mediagua
vivienda2022[V01==5, VPT := 5] #Rancho
vivienda2022[V01==6, VPT := 6] #Covacha
vivienda2022[V01==7, VPT := 7] #Choza
vivienda2022[V01==8, VPT := 8] #Otra vivienda particular

# Cantonal
PVPTN_c <- vivienda2022[V01 %in% c(1:8)& V0201 == 1,.(freq=.N),by=.(canton,VPT)] %>% 
  dcast(canton ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN_", .x), .x))  

# Administracion zonal 
PVPTN_az <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(adm_zonal,VPT)] %>% 
  dcast(adm_zonal ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN_", .x), .x)) 

# Parroquial
PVPTN_pr <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(parroquia,VPT)]%>% 
  dcast(parroquia ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN_", .x), .x)) 

# Sector
PVPTN_s <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(Sector_DMQ,VPT)] %>% 
  dcast(Sector_DMQ ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN_", .x), .x)) 

# Grilla COD1000
PVPTN_1000 <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(COD1000,VPT)]%>% 
  dcast(COD1000 ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN_", .x), .x)) 

# Grilla COD500
PVPTN_500 <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(COD500,VPT)]%>% 
  dcast(COD500 ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN_", .x), .x)) 

# Grilla H3_N8
PVPTN_N8 <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(H3_N8,VPT)]%>% 
  dcast(H3_N8 ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN_", .x), .x)) 

# Grilla H3_N9
PVPTN_N9 <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(H3_N9,VPT)] %>% 
  dcast(H3_N9 ~ VPT, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPTN_", .x), .x)) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVPTN")

addWorksheet(wb, "PVPTN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN_c", x=PVPTN_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN_az", x=PVPTN_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN_pr", x=PVPTN_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN_s", x=PVPTN_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVPTN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN_1000", x=PVPTN_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN_500", x=PVPTN_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN_N8", x=PVPTN_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPTN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPTN_N9", x=PVPTN_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Número de viviendas particulares ocupadas con personas presentes según tipo de vivienda.xlsx") 