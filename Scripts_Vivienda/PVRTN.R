# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Número de viviendas según su régimen de tenencia

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
# H09 = Tenencia o freqiedad de la vivienda:

# 1 freqia y totalmente pagada
# 2 freqia y la está pagando
# 3 freqia (regalada, donada, heredada o por posesión)
# 4 Arrendada/anticresis
# 5 Prestada o cedida (no paga)
# 6 Por servicios

# Cantonal
PVRTN_c <- VH2022[V01 %in% c(1:8)& V0201 == 1,.(freq=.N),by=.(canton.x,H09)]%>% 
  dcast(canton.x ~ H09, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN_", .x), .x))  

# Administracion zonal 

PVRTN_az <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(adm_zonal.x,H09)] %>% 
  dcast(adm_zonal.x ~ H09, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN_", .x), .x)) 

# Parroquial
PVRTN_pr <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(parroquia.x,H09)]%>% 
  dcast(parroquia.x ~ H09, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN_", .x), .x)) 

# Sector
PVRTN_s <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(Sector_DMQ.x,H09)] %>% 
  dcast(Sector_DMQ.x ~ H09, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN_", .x), .x)) 

# Grilla COD1000
PVRTN_1000 <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(COD1000.x,H09)] %>% 
  dcast(COD1000.x ~ H09, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN_", .x), .x)) 

# Grilla COD500
PVRTN_500 <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(COD500.x,H09)]%>% 
  dcast(COD500.x ~ H09, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN_", .x), .x)) 

# Grilla H3_N8
PVRTN_N8 <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(H3_N8.x,H09)] %>% 
  dcast(H3_N8.x ~ H09, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN_", .x), .x)) 

# Grilla H3_N9
PVRTN_N9 <- VH2022[V01 %in% c(1:8) & V0201 == 1,.(freq=.N),by=.(H3_N9.x,H09)] %>% 
  dcast(H3_N9.x ~ H09, value.var = c("freq")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVRTN_", .x), .x)) 


# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVRTN")

addWorksheet(wb, "PVRTN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN_c", x=PVRTN_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN_az", x=PVRTN_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN_pr", x=PVRTN_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN_s", x=PVRTN_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVRTN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN_1000", x=PVRTN_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN_500", x=PVRTN_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN_N8", x=PVRTN_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVRTN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVRTN_N9", x=PVRTN_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Número de viviendas según su régimen de tenencia.xlsx") 


