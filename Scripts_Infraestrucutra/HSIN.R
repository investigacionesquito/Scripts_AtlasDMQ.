# Dimensión: Infraestructura - Acceso TICS

# Nombre del Indicador: Número de hogares que disponen del servicio de internet fijo

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
HSIN_c <-  VH2022[V01<=8 & V0201R==1, sum(HSI), 
                 by=.(canton.x)] %>% setnames("V1", "HSIN") 

# Administracion zonal 

HSIN_az <-  VH2022[V01<=8 & V0201R==1, sum(HSI), 
                  by=.(adm_zonal.x)] %>% setnames("V1", "HSIN") 

# Parroquial
HSIN_pr <-  VH2022[V01<=8 & V0201R==1, sum(HSI), 
                  by=.(parroquia.x)] %>% setnames("V1", "HSIN") 

# Sector
HSIN_s <-  VH2022[V01<=8 & V0201R==1, sum(HSI), 
                 by=.(Sector_DMQ.x)] %>% setnames("V1", "HSIN") 

# Grilla COD1000
HSIN_1000 <-  VH2022[V01<=8 & V0201R==1, sum(HSI), 
                    by=.(COD1000.x)] %>% setnames("V1", "HSIN") 

# Grilla COD500
HSIN_500 <-  VH2022[V01<=8 & V0201R==1, sum(HSI), 
                   by=.(COD500.x)] %>% setnames("V1", "HSIN") 

# Grilla H3_N8
HSIN_N8 <-  VH2022[V01<=8 & V0201R==1, sum(HSI), 
                  by=.(H3_N8.x)] %>% setnames("V1", "HSIN") 

# Grilla H3_N9
HSIN_N9 <-  VH2022[V01<=8 & V0201R==1, sum(HSI), 
                  by=.(H3_N9.x)] %>% setnames("V1", "HSIN") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("HSIN")

addWorksheet(wb, "HSIN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSIN_c", x=HSIN_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSIN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSIN_az", x=HSIN_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSIN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSIN_pr", x=HSIN_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSIN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSIN_s", x=HSIN_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HSIN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSIN_1000", x=HSIN_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSIN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSIN_500", x=HSIN_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSIN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSIN_N8", x=HSIN_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSIN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSIN_N9", x=HSIN_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Número de hogares que disponen del servicio de internet fijo.xlsx") 