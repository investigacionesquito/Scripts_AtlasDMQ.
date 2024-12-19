# Dimensión: Infraestructura - Acceso TICS

# Nombre del Indicador: Porcentaje de hogares que disponen de computadora (de escritorio o laptop) 2010

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

VH2010[VTV<=8 & VCO==1, HCL10 := 0]
VH2010[H10 == 1, HCL10 := 1]

# Desegregaciones: 

## Geográfico/Territorial

# Cantonal
HCL10_c <-  VH2010[VTV<=8 & VCO==1, round(mean (HCL10 *100, na.rm=T),1), 
                 by=.(canton.x)] %>% setnames("V1", "HCL10") 

# Administracion zonal 

HCL10_az <-  VH2010[VTV<=8 & VCO==1, round(mean (HCL10 *100, na.rm=T),1), 
                        by=.(adm_zonal.x)] %>% setnames("V1", "HCL10") 

# Parroquial
HCL10_pr <-  VH2010[VTV<=8 & VCO==1, round(mean (HCL10 *100, na.rm=T),1), 
                        by=.(parroquia.x)] %>% setnames("V1", "HCL10") 

# Sector
HCL10_s <-  VH2010[VTV<=8 & VCO==1, round(mean (HCL10 *100, na.rm=T),1), 
                       by=.(Sector_DMQ.x)] %>% setnames("V1", "HCL10") 

# Grilla COD1000
HCL10_1000 <-  VH2010[VTV<=8 & VCO==1, round(mean (HCL10 *100, na.rm=T),1), 
                          by=.(COD1000.x)] %>% setnames("V1", "HCL10") 

# Grilla COD500
HCL10_500 <-  VH2010[VTV<=8 & VCO==1, round(mean (HCL10 *100, na.rm=T),1), 
                         by=.(COD500.x)] %>% setnames("V1", "HCL10") 

# Grilla H3_N8
HCL10_N8 <-  VH2010[VTV<=8 & VCO==1, round(mean (HCL10 *100, na.rm=T),1), 
                        by=.(H3_N8.x)] %>% setnames("V1", "HCL10") 

# Grilla H3_N9
HCL10_N9 <-  VH2010[VTV<=8 & VCO==1, round(mean (HCL10 *100, na.rm=T),1), 
                        by=.(H3_N9.x)] %>% setnames("V1", "HCL10") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("HCL10")

addWorksheet(wb, "HCL10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL10_c", x=HCL10_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL10_az", x=HCL10_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL10_pr", x=HCL10_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL10_s", x=HCL10_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HCL10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL10_1000", x=HCL10_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL10_500", x=HCL10_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL10_N8", x=HCL10_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL10_N9", x=HCL10_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de hogares que disponen de computadora (de escritorio o laptop) 2010.xlsx") 
