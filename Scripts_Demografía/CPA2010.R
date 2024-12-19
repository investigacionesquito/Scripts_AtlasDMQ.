# Dimensión: Demografía 

# Nombre del Indicador: Cantidad de la población 2010

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

pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr,tidyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2010 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

# PO1: Sexo al nacer
## 1 Hombre
## 2 Mujer

# Manzana 
CPA2010_m <- poblacion2010[, .N, by = .(P01,id_man)] %>% 
  dcast(id_man ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
  CPA2010 = CPA2010S1 + CPA2010S2
)

# Otras unidades de análisis

# Cantón
CPA2010_c <- poblacion2010[, .N, by = .(P01,canton)] %>% 
  dcast(canton ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
    CPA2010 = CPA2010S1 + CPA2010S2
  )

# Administración zonal 
CPA2010_az <- poblacion2010[, .N, by = .(P01,adm_zonal)] %>% 
  dcast(adm_zonal ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
    CPA2010 = CPA2010S1 + CPA2010S2
  )
# Parroquial
CPA2010_pr <- poblacion2010[, .N, by = .(P01,parroquia)] %>% 
  dcast(parroquia ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
    CPA2010 = CPA2010S1 + CPA2010S2
  )

# Sector 
CPA2010_s <- poblacion2010[, .N, by = .(P01,Sector_DMQ)] %>% 
  dcast(Sector_DMQ ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
    CPA2010 = CPA2010S1 + CPA2010S2
  )

# Grilla COD1000
CPA2010_1000 <- poblacion2010[, .N, by = .(P01,COD1000)] %>% 
  dcast(COD1000 ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
    CPA2010 = CPA2010S1 + CPA2010S2
  )

# Grilla COD500
CPA2010_500 <- poblacion2010[, .N, by = .(P01,COD500)] %>% 
  dcast(COD500 ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
    CPA2010 = CPA2010S1 + CPA2010S2
  )

# Grilla H3_N8
CPA2010_N8 <- poblacion2010[, .N, by = .(P01,H3_N8)] %>% 
  dcast(H3_N8 ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
    CPA2010 = CPA2010S1 + CPA2010S2
  )

# Grilla H3_N9
CPA2010_N9 <- poblacion2010[, .N, by = .(P01,H3_N9)] %>% 
  dcast(H3_N9 ~ P01, value.var = "N") %>% 
  rename_with(~ ifelse(.x == "1", "CPA2010S1", ifelse(.x == "2", "CPA2010S2", .x))) %>% 
  mutate(
    CPA2010 = CPA2010S1 + CPA2010S2
  )

# 6. Guardar los resultados

wb <-  createWorkbook("CPA2010")

addWorksheet(wb, "CPA2010_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA2010_c", x= CPA2010_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA2010_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA2010_az", x= CPA2010_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA2010_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA2010_pr", x= CPA2010_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA2010_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA2010_s", x= CPA2010_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA2010_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA2010_1000", x= CPA2010_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA2010_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA2010_500", x= CPA2010_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA2010_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA2010_N8", x= CPA2010_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "CPA2010_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "CPA2010_N9", x= CPA2010_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Cantidad de población y por sexo 2010.xlsx")