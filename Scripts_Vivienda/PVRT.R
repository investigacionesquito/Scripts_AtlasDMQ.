# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#porcentaje de viviendas según su régimen de tenencia

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

pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr,tidyr)

# 4. Cargar base de datos ----------------------------------------------------

hogar2022 <- read_sav("hogar2022_Atlas.sav") %>% as.data.table() 
vivienda2022 <- read_sav("vivienda2022_Atlas.sav") %>% as.data.table() 

# 5. Calcular indicadores --------------------------------------------------------

# Para el cálculo del indicador es necesario unir las bases de datos de vivienda y hogar

VH2022 <- merge(hogar2022,vivienda2022, by="ID_VIV")

# VO1 = Tipo de vivienda 
# V0201R = Condición de ocupación de vivienda particular (si es 1 = ocupada)
# H09 = Tenencia o propiedad de la vivienda:

# 1 Propia y totalmente pagada
# 2 Propia y la está pagando
# 3 Propia (regalada, donada, heredada o por posesión)
# 4 Arrendada/anticresis
# 5 Prestada o cedida (no paga)
# 6 Por servicios

# Cantonal
PVRT_c <- VH2022[V01 %in% c(1:8)& V0201 == 1,.(freq=.N),by=.(canton.x,H09)][, 
  prop := round((freq/sum(freq)*100),1), by =.(canton.x)][,':='(freq = NULL)] %>% 
  dcast(canton.x ~ H09, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT_", .x), .x))  

# Administracion zonal 

PVRT_az <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(adm_zonal.x,H09)][, 
  prop := round((freq/sum(freq)*100),1), by =.(adm_zonal.x)][,':='(freq = NULL)] %>% 
  dcast(adm_zonal.x ~ H09, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT_", .x), .x)) 

# Parroquial
PVRT_pr <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(parroquia.x,H09)][, 
  prop := round((freq/sum(freq)*100),1), by =.(parroquia.x)][,':='(freq = NULL)] %>% 
  dcast(parroquia.x ~ H09, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT_", .x), .x)) 

# Sector
PVRT_s <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(Sector_DMQ.x,H09)][, 
  prop := round((freq/sum(freq)*100),1), by =.(Sector_DMQ.x)][,':='(freq = NULL)] %>% 
  dcast(Sector_DMQ.x ~ H09, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT_", .x), .x)) 

# Grilla COD1000
PVRT_1000 <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(COD1000.x,H09)][, 
  prop := round((freq/sum(freq)*100),1), by =.(COD1000.x)][,':='(freq = NULL)] %>% 
  dcast(COD1000.x ~ H09, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT_", .x), .x)) 

# Grilla COD500
PVRT_500 <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(COD500.x,H09)][, 
  prop := round((freq/sum(freq)*100),1), by =.(COD500.x)][,':='(freq = NULL)] %>% 
  dcast(COD500.x ~ H09, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT_", .x), .x)) 

# Grilla H3_N8
PVRT_N8 <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(H3_N8.x,H09)][, 
  prop := round((freq/sum(freq)*100),1), by =.(H3_N8.x)][,':='(freq = NULL)] %>% 
  dcast(H3_N8.x ~ H09, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT_", .x), .x)) 

# Grilla H3_N9
PVRT_N9 <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(H3_N9.x,H09)][, 
  prop := round((freq/sum(freq)*100),1), by =.(H3_N9.x)][,':='(freq = NULL)] %>% 
  dcast(H3_N9.x ~ H09, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT_", .x), .x)) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVRT")

addWorksheet(wb, "PVRT_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT_c", x=PVRT_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT_az", x=PVRT_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT_pr", x=PVRT_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT_s", x=PVRT_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVRT_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT_1000", x=PVRT_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT_500", x=PVRT_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT_N8", x=PVRT_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT_N9", x=PVRT_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de viviendas según su régimen de tenencia.xlsx") 


