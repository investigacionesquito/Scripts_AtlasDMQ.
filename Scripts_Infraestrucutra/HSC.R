# Dimensión: Infraestructura - Acceso TICS

# Nombre del Indicador: Porcentaje de hogares que disponen del servicio de teléfono celular

# Proceso

# 1. Descargar bas es de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5. Calcular indicadores
# 6. Guardar los resultados

# 1. Descargar bases de datos -------------------------------------------------

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
# H1002 = Dispone de servicio de teléfono celular (Si es 1=SI)

VH2022[V01<=8 & V0201R==1, HSC := 0]
VH2022[H1002 == 1, HSC := 1]

# Desegregaciones: 

## Geográfico/Territorial

# Cantonal
HSC_c <-  VH2022[V01<=8 & V0201R==1, round(mean (HSC *100, na.rm=T),1), 
                 by=.(canton.x)] %>% setnames("V1", "HSC") 

# Administracion zonal 

HSC_az <-  VH2022[V01<=8 & V0201R==1, round(mean (HSC *100, na.rm=T),1), 
                  by=.(adm_zonal.x)] %>% setnames("V1", "HSC") 

# Parroquial
HSC_pr <-  VH2022[V01<=8 & V0201R==1, round(mean (HSC *100, na.rm=T),1), 
                  by=.(parroquia.x)] %>% setnames("V1", "HSC") 

# Sector
HSC_s <-  VH2022[V01<=8 & V0201R==1, round(mean (HSC *100, na.rm=T),1), 
                 by=.(Sector_DMQ.x)] %>% setnames("V1", "HSC") 

# Grilla COD1000
HSC_1000 <-  VH2022[V01<=8 & V0201R==1, round(mean (HSC *100, na.rm=T),1), 
                    by=.(COD1000.x)] %>% setnames("V1", "HSC") 

# Grilla COD500
HSC_500 <-  VH2022[V01<=8 & V0201R==1, round(mean (HSC *100, na.rm=T),1), 
                   by=.(COD500.x)] %>% setnames("V1", "HSC") 

# Grilla H3_N8
HSC_N8 <-  VH2022[V01<=8 & V0201R==1, round(mean (HSC *100, na.rm=T),1), 
                  by=.(H3_N8.x)] %>% setnames("V1", "HSC") 

# Grilla H3_N9
HSC_N9 <-  VH2022[V01<=8 & V0201R==1, round(mean (HSC *100, na.rm=T),1), 
                  by=.(H3_N9.x)] %>% setnames("V1", "HSC") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("HSC")

addWorksheet(wb, "HSC_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSC_c", x=HSC_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSC_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSC_az", x=HSC_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSC_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSC_pr", x=HSC_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSC_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSC_s", x=HSC_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HSC_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSC_1000", x=HSC_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSC_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSC_500", x=HSC_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSC_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSC_N8", x=HSC_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSC_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSC_N9", x=HSC_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de hogares que disponen del servicio de teléfono celular.xlsx") 