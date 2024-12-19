# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#porcentaje de viviendas según su régimen de tenencia 2010

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

hogar2010 <- read_sav("hogar2010_Atlas.sav") %>% as.data.table() 
vivienda2010 <- read_sav("vivienda2010_Atlas.sav") %>% as.data.table() 

# 5. Calcular indicadores --------------------------------------------------------

# Para el cálculo del indicador es necesario unir las bases de datos de vivienda y hogar

VH2010 <- merge(hogar2010,vivienda2010, by="id_viv")

# VTV = Tipo de vivienda 
# VCO = Condición de ocupación de vivienda particular (si es 1 = ocupada)

# H15 = Tenencia o propiedad de la vivienda

# 1 Propia y totalmente pagada
# 2 Propia y la está pagando
# 3 Propia (regalada, donada, heredada o por posesión)
# 4 Prestada o cedida (no pagada)
# 5 Por servicios
# 6 Arrendada
# 7 Anticresis

# Se debe recodificar la pregunta H15 para poder comparar 

# 1 Propia y totalmente pagada
# 2 Propia y la está pagando
# 3 Propia (regalada, donada, heredada o por posesión)
# 4 Arrendada/anticresis
# 5 Prestada o cedida (no paga)
# 6 Por servicios

VH2010[H15==1, H15R:=1]
VH2010[H15==2, H15R:=2]
VH2010[H15==3, H15R:=3]
VH2010[H15 %in% (6:7), H15R:=4]
VH2010[H15==4, H15R:=5]
VH2010[H15==5, H15R:=6]

# Cantonal
PVRT10_c <- VH2010[VTV %in% c(1:8)& VCO == 1,.(freq=.N),by=.(canton.x,H15R)][, 
  prop := round((freq/sum(freq)*100),1), by =.(canton.x)][,':='(freq = NULL)] %>% 
  dcast(canton.x ~ H15R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT10_", .x), .x))  

# Administracion zonal 

PVRT10_az <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(adm_zonal.x,H15R)][, 
  prop := round((freq/sum(freq)*100),1), by =.(adm_zonal.x)][,':='(freq = NULL)] %>% 
  dcast(adm_zonal.x ~ H15R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT10_", .x), .x)) 

# Parroquial
PVRT10_pr <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(parroquia.x,H15R)][, 
  prop := round((freq/sum(freq)*100),1), by =.(parroquia.x)][,':='(freq = NULL)] %>% 
  dcast(parroquia.x ~ H15R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT10_", .x), .x)) 

# Sector
PVRT10_s <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(Sector_DMQ.x,H15R)][, 
  prop := round((freq/sum(freq)*100),1), by =.(Sector_DMQ.x)][,':='(freq = NULL)] %>% 
  dcast(Sector_DMQ.x ~ H15R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT10_", .x), .x)) 

# Grilla COD1000
PVRT10_1000 <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(COD1000.x,H15R)][, 
  prop := round((freq/sum(freq)*100),1), by =.(COD1000.x)][,':='(freq = NULL)] %>% 
  dcast(COD1000.x ~ H15R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT10_", .x), .x)) 

# Grilla COD500
PVRT10_500 <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(COD500.x,H15R)][, 
  prop := round((freq/sum(freq)*100),1), by =.(COD500.x)][,':='(freq = NULL)] %>% 
  dcast(COD500.x ~ H15R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT10_", .x), .x)) 

# Grilla H3_N8
PVRT10_N8 <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(H3_N8.x,H15R)][, 
  prop := round((freq/sum(freq)*100),1), by =.(H3_N8.x)][,':='(freq = NULL)] %>% 
  dcast(H3_N8.x ~ H15R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT10_", .x), .x)) 

# Grilla H3_N9
PVRT10_N9 <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(H3_N9.x,H15R)][, 
  prop := round((freq/sum(freq)*100),1), by =.(H3_N9.x)][,':='(freq = NULL)] %>% 
  dcast(H3_N9.x ~ H15R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:8), paste0("PVRT10_", .x), .x)) 


# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVRT10")

addWorksheet(wb, "PVRT10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT10_c", x=PVRT10_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT10_az", x=PVRT10_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT10_pr", x=PVRT10_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT10_s", x=PVRT10_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVRT10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT10_1000", x=PVRT10_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT10_500", x=PVRT10_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT10_N8", x=PVRT10_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRT10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRT10_N9", x=PVRT10_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de viviendas según su régimen de tenencia 2010.xlsx") 


