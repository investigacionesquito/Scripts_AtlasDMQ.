# Dimensión: Infraestructura - Acceso TICS

# Nombre del Indicador: Porcentaje de hogares que disponen del servicio de internet fijo

# Proceso

# 1. Descargar bas es de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5. Calcular indicadores
# 6. Guardar los resultados

# 1. Descargar bases de datos -------------------------------------------------

# 2. Establecer el directorio -------------------------------------------------

# 2. Establecer el directorio -------------------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías -------------------------------------------------------

pacman::p_load(data.table,openxlsx, foreign, haven, writexl, sjlabelled, stringr)

# 4. Cargar base de datos ----------------------------------------------------

hogar2022 <- read_sav("hogar2022_Atlas.sav") %>% as.data.table() 
vivienda2022 <- read_sav("vivienda2022_Atlas.sav") %>% as.data.table() 

# 5. Calcular indicadores --------------------------------------------------------

# Para el cálculo del indicador es necesario unir las bases de datos de vivienda y hogar

VH2022 <- merge(hogar2022,vivienda2022, by="ID_VIV")

# VO1 = Tipo de vivienda 
# V0201R = Condición de ocupación de vivienda particular (si es 1 = ocupada)
# H1004 = Dispone de servicio de internet fijo (Si es 1=SI)

VH2022[V01<=8 & V0201R==1, HSI := 0]
VH2022[H1004 == 1, HSI := 1]

# Desegregaciones: 

## Geográfico/Territorial

# Cantonal
HSI_c <-  VH2022[V01<=8 & V0201R==1, round(mean (HSI *100, na.rm=T),1), 
                 by=.(canton.x)] %>% setnames("V1", "HSI") 

# Administracion zonal 

HSI_az <-  VH2022[V01<=8 & V0201R==1, round(mean (HSI *100, na.rm=T),1), 
                  by=.(adm_zonal.x)] %>% setnames("V1", "HSI") 

# Parroquial
HSI_pr <-  VH2022[V01<=8 & V0201R==1, round(mean (HSI *100, na.rm=T),1), 
                  by=.(parroquia.x)] %>% setnames("V1", "HSI") 

# Sector
HSI_s <-  VH2022[V01<=8 & V0201R==1, round(mean (HSI *100, na.rm=T),1), 
                 by=.(Sector_DMQ.x)] %>% setnames("V1", "HSI") 

# Grilla COD1000
HSI_1000 <-  VH2022[V01<=8 & V0201R==1, round(mean (HSI *100, na.rm=T),1), 
                    by=.(COD1000.x)] %>% setnames("V1", "HSI") 

# Grilla COD500
HSI_500 <-  VH2022[V01<=8 & V0201R==1, round(mean (HSI *100, na.rm=T),1), 
                   by=.(COD500.x)] %>% setnames("V1", "HSI") 

# Grilla H3_N8
HSI_N8 <-  VH2022[V01<=8 & V0201R==1, round(mean (HSI *100, na.rm=T),1), 
                  by=.(H3_N8.x)] %>% setnames("V1", "HSI") 

# Grilla H3_N9
HSI_N9 <-  VH2022[V01<=8 & V0201R==1, round(mean (HSI *100, na.rm=T),1), 
                  by=.(H3_N9.x)] %>% setnames("V1", "HSI") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("HSI")

addWorksheet(wb, "HSI_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSI_c", x=HSI_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSI_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSI_az", x=HSI_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSI_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSI_pr", x=HSI_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSI_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSI_s", x=HSI_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HSI_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSI_1000", x=HSI_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSI_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSI_500", x=HSI_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSI_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSI_N8", x=HSI_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSI_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSI_N9", x=HSI_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de hogares que disponen del servicio de internet fijo.xlsx") 