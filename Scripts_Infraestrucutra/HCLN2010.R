# Dimensión: Infraestructura - Acceso TICS

# Nombre del Indicador: Número de hogares que disponen de computadora (de escritorio o laptop) 2010

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

hogar2010 <- read_sav("hogar2010_Atlas.sav") %>% as.data.table() 
vivienda2010 <- read_sav("vivienda2010_Atlas.sav") %>% as.data.table() 

# 5. Calcular indicadores --------------------------------------------------------

# Para el cálculo del indicador es necesario unir las bases de datos de vivienda y hogar

VH2010 <- merge(hogar2010,vivienda2010, by="id_viv")

# VTV = Tipo de vivienda 
# VCO = Condición de ocupación de vivienda particular (si es 1 = ocupada)
# H10 = Dispone de computadora (Si es 1=SI)

VH2010[VTV<=8 & VCO==1, HCL2010 := 0]
VH2010[H10 == 1, HCL2010 := 1]

# Desegregaciones: 

## Geográfico/Territorial

# Cantonal
HCLN10_c <-  VH2010[VTV<=8 & VCO==1, sum(HCL2010), 
                 by=.(canton.x)] %>% setnames("V1", "HCLN10") 

# Administracion zonal 

HCLN10_az <-  VH2010[VTV<=8 & VCO==1, sum(HCL2010), 
                        by=.(adm_zonal.x)] %>% setnames("V1", "HCLN10") 

# Parroquial
HCLN10_pr <-  VH2010[VTV<=8 & VCO==1, sum(HCL2010), 
                        by=.(parroquia.x)] %>% setnames("V1", "HCLN10") 

# Sector
HCLN10_s <-  VH2010[VTV<=8 & VCO==1, sum(HCL2010), 
                       by=.(Sector_DMQ.x)] %>% setnames("V1", "HCLN10") 

# Grilla COD1000
HCLN10_1000 <-  VH2010[VTV<=8 & VCO==1, sum(HCL2010), 
                          by=.(COD1000.x)] %>% setnames("V1", "HCLN10") 

# Grilla COD500
HCLN10_500 <-  VH2010[VTV<=8 & VCO==1, sum(HCL2010), 
                         by=.(COD500.x)] %>% setnames("V1", "HCLN10") 

# Grilla H3_N8
HCLN10_N8 <-  VH2010[VTV<=8 & VCO==1, sum(HCL2010), 
                        by=.(H3_N8.x)] %>% setnames("V1", "HCLN10") 

# Grilla H3_N9
HCLN10_N9 <-  VH2010[VTV<=8 & VCO==1, sum(HCL2010), 
                        by=.(H3_N9.x)] %>% setnames("V1", "HCLN10") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("HCLN10")

addWorksheet(wb, "HCLN10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN10_c", x=HCLN10_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN10_az", x=HCLN10_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN10_pr", x=HCLN10_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN10_s", x=HCLN10_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HCLN10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN10_1000", x=HCLN10_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN10_500", x=HCLN10_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN10_N8", x=HCLN10_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCLN10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCLN10_N9", x=HCLN10_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Número de hogares que disponen de computadora (de escritorio o laptop) 2010.xlsx") 
