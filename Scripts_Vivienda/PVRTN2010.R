# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Número de viviendas según su régimen de tenencia 2010

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
PVRTN10_c <- VH2010[VTV %in% c(1:8)& VCO == 1,.(freq=.N),by=.(canton.x,H15R)]%>% 
  dcast(canton.x ~ H15R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN10_", .x), .x))  

# Administracion zonal 

PVRTN10_az <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(adm_zonal.x,H15R)] %>% 
  dcast(adm_zonal.x ~ H15R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN10_", .x), .x)) 

# Parroquial
PVRTN10_pr <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(parroquia.x,H15R)]%>% 
  dcast(parroquia.x ~ H15R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN10_", .x), .x)) 

# Sector
PVRTN10_s <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(Sector_DMQ.x,H15R)] %>% 
  dcast(Sector_DMQ.x ~ H15R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN10_", .x), .x)) 

# Grilla COD1000
PVRTN10_1000 <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(COD1000.x,H15R)] %>% 
  dcast(COD1000.x ~ H15R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN10_", .x), .x)) 

# Grilla COD500
PVRTN10_500 <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(COD500.x,H15R)]%>% 
  dcast(COD500.x ~ H15R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN10_", .x), .x)) 

# Grilla H3_N8
PVRTN10_N8 <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(H3_N8.x,H15R)] %>% 
  dcast(H3_N8.x ~ H15R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN10_", .x), .x)) 

# Grilla H3_N9
PVRTN10_N9 <- VH2010[VTV %in% c(1:8) & VCO == 1,.(freq=.N),by=.(H3_N9.x,H15R)] %>% 
  dcast(H3_N9.x ~ H15R, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN10_", .x), .x)) 


# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVRTN10")

addWorksheet(wb, "PVRTN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN10_c", x=PVRTN10_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN10_az", x=PVRTN10_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN10_pr", x=PVRTN10_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN10_s", x=PVRTN10_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVRTN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN10_1000", x=PVRTN10_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN10_500", x=PVRTN10_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN10_N8", x=PVRTN10_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN10_N9", x=PVRTN10_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Número de viviendas según su régimen de tenencia 2010.xlsx") 


