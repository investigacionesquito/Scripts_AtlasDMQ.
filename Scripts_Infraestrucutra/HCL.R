# Dimensión: Infraestructura - Acceso TICS

# Nombre del Indicador: Porcentaje de hogares que disponen de computadora (de escritorio o laptop)

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
HCL_c <-  VH2022[V01<=8 & V0201R==1, round(mean (HCL *100, na.rm=T),1), 
                 by=.(canton.x)] %>% setnames("V1", "HCL") 

# Administracion zonal 

HCL_az <-  VH2022[V01<=8 & V0201R==1, round(mean (HCL *100, na.rm=T),1), 
                        by=.(adm_zonal.x)] %>% setnames("V1", "HCL") 

# Parroquial
HCL_pr <-  VH2022[V01<=8 & V0201R==1, round(mean (HCL *100, na.rm=T),1), 
                        by=.(parroquia.x)] %>% setnames("V1", "HCL") 

# Sector
HCL_s <-  VH2022[V01<=8 & V0201R==1, round(mean (HCL *100, na.rm=T),1), 
                       by=.(Sector_DMQ.x)] %>% setnames("V1", "HCL") 

# Grilla COD1000
HCL_1000 <-  VH2022[V01<=8 & V0201R==1, round(mean (HCL *100, na.rm=T),1), 
                          by=.(COD1000.x)] %>% setnames("V1", "HCL") 

# Grilla COD500
HCL_500 <-  VH2022[V01<=8 & V0201R==1, round(mean (HCL *100, na.rm=T),1), 
                         by=.(COD500.x)] %>% setnames("V1", "HCL") 

# Grilla H3_N8
HCL_N8 <-  VH2022[V01<=8 & V0201R==1, round(mean (HCL *100, na.rm=T),1), 
                        by=.(H3_N8.x)] %>% setnames("V1", "HCL") 

# Grilla H3_N9
HCL_N9 <-  VH2022[V01<=8 & V0201R==1, round(mean (HCL *100, na.rm=T),1), 
                        by=.(H3_N9.x)] %>% setnames("V1", "HCL") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("HCL")

addWorksheet(wb, "HCL_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL_c", x=HCL_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL_az", x=HCL_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL_pr", x=HCL_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL_s", x=HCL_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HCL_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL_1000", x=HCL_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL_500", x=HCL_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL_N8", x=HCL_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HCL_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HCL_N9", x=HCL_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de hogares que disponen de computadora (de escritorio o laptop).xlsx") 
