# Dimensión: Infraestructura - Acceso TICS

# Nombre del Indicador: Número de hogares que disponen de computadora (de escritorio o laptop)

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
# H1005 = Dispone de computadora (Si es 1=SI)

VH2022[V01<=8 & V0201R==1, HCL := 0]
VH2022[H1005 == 1, HCL := 1]

# Desegregaciones: 

## Geográfico/Territorial

# Cantonal
HCLN_c <-  VH2022[V01<=8 & V0201R==1, sum(HCL), 
                 by=.(canton.x)] %>% setnames("V1", "HCLN") 

# Administracion zonal 

HCLN_az <-  VH2022[V01<=8 & V0201R==1, sum(HCL), 
                        by=.(adm_zonal.x)] %>% setnames("V1", "HCLN") 

# Parroquial
HCLN_pr <-  VH2022[V01<=8 & V0201R==1, sum(HCL), 
                        by=.(parroquia.x)] %>% setnames("V1", "HCLN") 

# Sector
HCLN_s <-  VH2022[V01<=8 & V0201R==1, sum(HCL), 
                       by=.(Sector_DMQ.x)] %>% setnames("V1", "HCLN") 

# Grilla COD1000
HCLN_1000 <-  VH2022[V01<=8 & V0201R==1, sum(HCL), 
                          by=.(COD1000.x)] %>% setnames("V1", "HCLN") 

# Grilla COD500
HCLN_500 <-  VH2022[V01<=8 & V0201R==1, sum(HCL), 
                         by=.(COD500.x)] %>% setnames("V1", "HCLN") 

# Grilla H3_N8
HCLN_N8 <-  VH2022[V01<=8 & V0201R==1, sum(HCL), 
                        by=.(H3_N8.x)] %>% setnames("V1", "HCLN") 

# Grilla H3_N9
HCLN_N9 <-  VH2022[V01<=8 & V0201R==1, sum(HCL), 
                        by=.(H3_N9.x)] %>% setnames("V1", "HCLN") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("HCLN")

addWorksheet(wb, "HCLN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN_c", x=HCLN_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN_az", x=HCLN_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN_pr", x=HCLN_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN_s", x=HCLN_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HCLN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN_1000", x=HCLN_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN_500", x=HCLN_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN_N8", x=HCLN_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN_N9", x=HCLN_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Número de hogares que disponen de computadora (de escritorio o laptop).xlsx") 
