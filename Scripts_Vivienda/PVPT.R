# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Porcentaje de viviendas particulares ocupadas con personas presentes según tipo de vivienda. 
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
PVPT_c <- vivienda2022[V01 %in% c(1:8)& V0201 == 1,.(freq=.N),by=.(canton,VPT)][, 
          prop := round((freq/sum(freq)*100),1), by =.(canton)][,':='(freq = NULL)] %>% 
  dcast(canton ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT_", .x), .x))  

# Administracion zonal 
PVPT_az <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(adm_zonal,VPT)][, 
           prop := round((freq/sum(freq)*100),1), by =.(adm_zonal)][,':='(freq = NULL)] %>% 
  dcast(adm_zonal ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT_", .x), .x)) 

# Parroquial
PVPT_pr <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(parroquia,VPT)][, 
          prop := round((freq/sum(freq)*100),1), by =.(parroquia)][,':='(freq = NULL)] %>% 
  dcast(parroquia ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT_", .x), .x)) 

# Sector
PVPT_s <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(Sector_DMQ,VPT)][, 
           prop := round((freq/sum(freq)*100),1), by =.(Sector_DMQ)][,':='(freq = NULL)] %>% 
  dcast(Sector_DMQ ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT_", .x), .x)) 

# Grilla COD1000
PVPT_1000 <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(COD1000,VPT)][, 
            prop := round((freq/sum(freq)*100),1), by =.(COD1000)][,':='(freq = NULL)] %>% 
  dcast(COD1000 ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT_", .x), .x)) 

# Grilla COD500
PVPT_500 <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(COD500,VPT)][, 
            prop := round((freq/sum(freq)*100),1), by =.(COD500)][,':='(freq = NULL)] %>% 
  dcast(COD500 ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT_", .x), .x)) 

# Grilla H3_N8
PVPT_N8 <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(H3_N8,VPT)][, 
           prop := round((freq/sum(freq)*100),1), by =.(H3_N8)][,':='(freq = NULL)] %>% 
  dcast(H3_N8 ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT_", .x), .x)) 

# Grilla H3_N9
PVPT_N9 <- vivienda2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(H3_N9,VPT)][, 
          prop := round((freq/sum(freq)*100),1), by =.(H3_N9)][,':='(freq = NULL)] %>% 
  dcast(H3_N9 ~ VPT, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVPT_", .x), .x)) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVPT")

addWorksheet(wb, "PVPT_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT_c", x=PVPT_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT_az", x=PVPT_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT_pr", x=PVPT_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT_s", x=PVPT_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVPT_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT_1000", x=PVPT_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT_500", x=PVPT_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT_N8", x=PVPT_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPT_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPT_N9", x=PVPT_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas particulares ocupadas con personas presentes según tipo de vivienda.xlsx") 