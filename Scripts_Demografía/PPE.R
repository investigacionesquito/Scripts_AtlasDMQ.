# Dimensión: Demografía 

# Nombre del Indicador: 
# Porcentaje de la población de 12 años o más de acuerdo a su estado conyugal

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
               , sjlabelled, stringr,labelled,dplyr, tidyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

poblacion2022[P03>=12, E12:=1]

# Desagregaciones

## Geográfico territorial

# Cantón
PPE_c <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, canton)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(canton)][, freq := NULL][order(P31, decreasing = FALSE)] %>% 
  dcast(canton ~ P31, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPE", .x), .x))  

# Administración zonal 
PPE_az <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, adm_zonal)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(adm_zonal)][, freq := NULL][order(P31, decreasing = FALSE)] %>% 
  dcast(adm_zonal ~ P31, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPE", .x), .x))  

# Parroquial
PPE_pr <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, parroquia)][, prop := 
          round((freq / sum(freq) * 100), 1), by = .(parroquia)][, freq := NULL][order(P31, decreasing = FALSE)] %>% 
  dcast(parroquia ~ P31, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPE", .x), .x))  

# Sector
PPE_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, Sector_DMQ)][, prop := 
          round((freq / sum(freq) * 100), 1), by = .(Sector_DMQ)][, freq := NULL][order(P31, decreasing = FALSE)] %>% 
  dcast(Sector_DMQ ~ P31, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPE", .x), .x)) 

# Grilla COD1000
PPE_1000 <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, COD1000)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(COD1000)][, freq := NULL][order(P31, decreasing = FALSE)] %>% 
  dcast(COD1000 ~ P31, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPE", .x), .x))  

# Grilla COD500
PPE_500 <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, COD500)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(COD500)][, freq := NULL][order(P31, decreasing = FALSE)] %>% 
  dcast(COD500 ~ P31, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPE", .x), .x))  

# Grilla H3_N8
PPE_N8 <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, H3_N8)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(H3_N8)][, freq := NULL][order(P31, decreasing = FALSE)] %>% 
  dcast(H3_N8 ~ P31, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPE", .x), .x))  

# Grilla H3_N9
PPE_N9 <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, H3_N9)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(H3_N9)][, freq := NULL][order(P31, decreasing = FALSE)] %>% 
  dcast(H3_N9 ~ P31, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPE", .x), .x))  

# Socio Demográfico/Económico

# Cantón - sexo
PPE_c_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, P02, canton)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(canton,P02)][, ':='(freq = NULL)][order(P31, decreasing = FALSE)] %>% 
  dcast(canton + P02 ~ P31, value.var = c("prop"))%>%
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPEs", .x), .x))  

# Administración zonal 
PPE_az_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, P02, adm_zonal)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(adm_zonal,P02)][, ':='(freq = NULL)][order(P31, decreasing = FALSE)] %>% 
  dcast(adm_zonal + P02 ~ P31, value.var = c("prop"))%>%
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPEs", .x), .x))  

# Parroquial - sexo
PPE_pr_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, P02, parroquia)][, prop := 
          round((freq / sum(freq) * 100), 1), by = .(parroquia,P02)][, ':='(freq = NULL)][order(P31, decreasing = FALSE)] %>% 
  dcast(parroquia + P02 ~ P31, value.var = c("prop"))%>%
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPEs", .x), .x))  

# Sector - sexo 
PPE_s_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, P02, Sector_DMQ)][, prop := 
            round((freq / sum(freq) * 100), 1), by = .(Sector_DMQ,P02)][, 
            ':='(freq = NULL)][order(P31, decreasing = FALSE)] %>% 
  dcast(Sector_DMQ+ P02 ~ P31, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPEs", .x), .x)) 

# Grilla COD1000 - sexo
PPE_1000_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, P02, COD1000)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(COD1000,P02)][, 
  ':='(freq = NULL)][order(P31, decreasing = FALSE)] %>% 
  dcast(COD1000 + P02 ~ P31, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPEs", .x), .x)) 

# Grilla COD500 - sexo
PPE_500_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, P02, COD500)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(COD500,P02)][, 
  ':='(freq = NULL)][order(P31, decreasing = FALSE)] %>% 
  dcast(COD500 + P02 ~ P31, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPEs", .x), .x)) 

# Grilla H3_N8 - sexo
PPE_N8_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, P02, H3_N8)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(H3_N8,P02)][, 
  ':='(freq = NULL)][order(P31, decreasing = FALSE)] %>% 
  dcast(H3_N8 + P02 ~ P31, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPEs", .x), .x)) 

# Grilla H3_N9 - sexo
PPE_N9_s <- poblacion2022[E12 == 1, .(freq = .N), by = .(P31, P02, H3_N9)][, prop := 
  round((freq / sum(freq) * 100), 1), by = .(H3_N9,P02)][, 
  ':='(freq = NULL)][order(P31, decreasing = FALSE)] %>% 
  dcast(H3_N9 + P02 ~ P31, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPEs", .x), .x)) 


# 6. Guardar los resultados

wb <-  createWorkbook("PPE")

addWorksheet(wb, "PPE_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_c", x= PPE_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_az", x= PPE_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_pr", x= PPE_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_s", x= PPE_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PPE_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_1000", x= PPE_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_500", x= PPE_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_N8", x= PPE_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_N9", x= PPE_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PPE_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_c_s", x= PPE_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_az_s", x= PPE_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_pr_s", x= PPE_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_s_s", x= PPE_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PPE_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_1000_s", x= PPE_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_500_s", x= PPE_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_N8_s", x= PPE_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPE_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPE_N9_s", x= PPE_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
saveWorkbook(wb, "Porcentaje de la población de 12 años o más de acuerdo a su estado conyugal.xlsx")

