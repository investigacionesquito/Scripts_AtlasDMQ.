# Dimensión: Demografía 

# Nombre del Indicador: 
# Cantidad de la población.

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5 . Calcular indicadores
# 6 . Guardar los resultados

# 1. Descargar bases de datos ---------------------------------------

# 2. Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ----------------------------------------------

pacman::p_load(raster, rgdal, rgeos, ggrepel, stringr, sf, ggspatial, exactextractr, tidyverse,xlsx, 
               maptools,scales,dplyr,writexl,dplyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

sec2022 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Sectores_2022\\Sec2022_ParrDMQ_CODGrillas.shp") %>% 
  as.data.table() 

# 5. Calcular indicadores --------------------------------------------------

# PO2: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Sector DMQ
CPA_s <- poblacion2022[, .N, by = .(P02,Sector_DMQ)] %>%                          # Calculo de población por sexo 
  dcast(Sector_DMQ ~ P02, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "S1", ifelse(.x == "2", "S2", .x))) %>%        
  mutate(
    pob = S1 + S2                                                                 #Calculo la población total 
  ) %>% merge(sec2022,                                                            # Union con la base que contiene el porcentaje y UA
      by = "Sector_DMQ", 
      all.x = TRUE) %>% 
  mutate(
    CPA = pob * Porcentaje,
    CPAS1 = S1 * Porcentaje,
    CPAS2 = S2 * Porcentaje,
  ) 

CPA_s2 <- CPA_s[, c("Sector_DMQ","CPA", "CPAS1", "CPAS2")]

# Otras unidades de análisis

# Cantón
CPA_c <- CPA_s[, .(
  CPA = sum(CPA, na.rm = TRUE),
  CPAS1 = sum(CPAS1, na.rm = TRUE),
  CPAS2 = sum(CPAS2, na.rm = TRUE)
), by = .(canton)]

# Administración zonal 
CPA_az <- CPA_s[, .(
  CPA = sum(CPA, na.rm = TRUE),
  CPAS1 = sum(CPAS1, na.rm = TRUE),
  CPAS2 = sum(CPAS2, na.rm = TRUE)
), by = .(adm_zonal)]

# Parroquial
CPA_pr <- CPA_s[, .(
  CPA = sum(CPA, na.rm = TRUE),
  CPAS1 = sum(CPAS1, na.rm = TRUE),
  CPAS2 = sum(CPAS2, na.rm = TRUE)
), by = .(parroquia)]

# Grilla COD1000
CPA_1000 <- CPA_s[, .(
  CPA = sum(CPA, na.rm = TRUE),
  CPAS1 = sum(CPAS1, na.rm = TRUE),
  CPAS2 = sum(CPAS2, na.rm = TRUE)
), by = .(COD1000)]

# Grilla COD500
CPA_500 <- CPA_s[, .(
  CPA = sum(CPA, na.rm = TRUE),
  CPAS1 = sum(CPAS1, na.rm = TRUE),
  CPAS2 = sum(CPAS2, na.rm = TRUE)
), by = .(COD500)]

# Grilla H3_N8
CPA_N8 <- CPA_s[, .(
  CPA = sum(CPA, na.rm = TRUE),
  CPAS1 = sum(CPAS1, na.rm = TRUE),
  CPAS2 = sum(CPAS2, na.rm = TRUE)
), by = .(H3_N8)]

# Grilla H3_N9
CPA_N9 <- CPA_s[, .(
  CPA = sum(CPA, na.rm = TRUE),
  CPAS1 = sum(CPAS1, na.rm = TRUE),
  CPAS2 = sum(CPAS2, na.rm = TRUE)
), by = .(H3_N9)]

CPA_s <- as.data.frame(CPA_s)

# 6. Guardar los resultados

wb <-  createWorkbook("CPA")

addWorksheet(wb, "CPA_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA_c", x= CPA_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA_az", x= CPA_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA_pr", x= CPA_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA_s2", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA_s2", x= CPA_s2, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA_1000", x= CPA_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA_500", x= CPA_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA_N8", x= CPA_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA_N9", x= CPA_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de población y por sexo.xlsx")