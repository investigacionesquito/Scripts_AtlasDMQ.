# Dimensión: Infraestructura - Acceso TICS

# Nombre del Indicador: Porcentaje de hogares que disponen del servicio de televisión pagada

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
# H1003 = Dispone de servicio de televisión pagada (Si es 1=SI)

VH2022[V01<=8 & V0201R==1, HTV := 0]
VH2022[H1003 == 1, HTV := 1]

# Desegregaciones: 

## Geográfico/Territorial

# Cantonal
HTV_c <-  VH2022[V01<=8 & V0201R==1, round(mean (HTV *100, na.rm=T),1), 
                 by=.(canton.x)] %>% setnames("V1", "HTV") 

# Administracion zonal 

HTV_az <-  VH2022[V01<=8 & V0201R==1, round(mean (HTV *100, na.rm=T),1), 
                  by=.(adm_zonal.x)] %>% setnames("V1", "HTV") 

# Parroquial
HTV_pr <-  VH2022[V01<=8 & V0201R==1, round(mean (HTV *100, na.rm=T),1), 
                  by=.(parroquia.x)] %>% setnames("V1", "HTV") 

# Sector
HTV_s <-  VH2022[V01<=8 & V0201R==1, round(mean (HTV *100, na.rm=T),1), 
                 by=.(Sector_DMQ.x)] %>% setnames("V1", "HTV") 

# Grilla COD1000
HTV_1000 <-  VH2022[V01<=8 & V0201R==1, round(mean (HTV *100, na.rm=T),1), 
                    by=.(COD1000.x)] %>% setnames("V1", "HTV") 

# Grilla COD500
HTV_500 <-  VH2022[V01<=8 & V0201R==1, round(mean (HTV *100, na.rm=T),1), 
                   by=.(COD500.x)] %>% setnames("V1", "HTV") 

# Grilla H3_N8
HTV_N8 <-  VH2022[V01<=8 & V0201R==1, round(mean (HTV *100, na.rm=T),1), 
                  by=.(H3_N8.x)] %>% setnames("V1", "HTV") 

# Grilla H3_N9
HTV_N9 <-  VH2022[V01<=8 & V0201R==1, round(mean (HTV *100, na.rm=T),1), 
                  by=.(H3_N9.x)] %>% setnames("V1", "HTV") 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("HTV")

addWorksheet(wb, "HTV_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTV_c", x=HTV_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTV_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTV_az", x=HTV_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTV_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTV_pr", x=HTV_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTV_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTV_s", x=HTV_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTV_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTV_1000", x=HTV_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTV_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTV_500", x=HTV_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTV_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTV_N8", x=HTV_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTV_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTV_N9", x=HTV_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de hogares que disponen del servicio de televisión pagada.xlsx") 