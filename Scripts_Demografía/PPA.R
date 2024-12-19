# Dimensión: Demografía 

# Nombre del Indicador: 
# Porcentaje de población de acuerdo al área en que reside

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

# Desagregaciones

## Geográfico territorial

# Cantón
PPA_c <- poblacion2022[, .(freq=.N), by = .(AUR, canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(canton ~ AUR, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PPA_", .x), .x))  

# Administración zonal 
PPA_az <- poblacion2022[, .(freq=.N), by = .(AUR, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ AUR, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PPA_", .x), .x)) 

#Parroquial

PPA_pr <- poblacion2022[, .(freq=.N), by = .(AUR, parroquia)][,prop := 
          round((freq/sum(freq)*100),1),by=.(parroquia)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(parroquia ~ AUR, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PPA_", .x), .x))  

# Sector
PPA_s <- poblacion2022[, .(freq=.N), by = .(AUR, Sector_DMQ)][,prop := 
         round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ ~ AUR, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PPA_", .x), .x)) 

# Grilla COD1000
PPA_1000 <- poblacion2022[, .(freq=.N), by = .(AUR, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ AUR, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PPA_", .x), .x)) 

# Grilla COD500
PPA_500 <- poblacion2022[, .(freq=.N), by = .(AUR, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(COD500 ~ AUR, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PPA_", .x), .x)) 

# Grilla H3_N8
PPA_N8 <- poblacion2022[, .(freq=.N), by = .(AUR, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(H3_N8~ AUR, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PPA_", .x), .x)) 

# Grilla H3_N9
PPA_N9 <- poblacion2022[, .(freq=.N), by = .(AUR, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ AUR, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:2), paste0("PPA_", .x), .x)) 

# Socio Demográfico/Económico

# Cantón - sexo 
PPA_c_s <- poblacion2022[, .(freq=.N), by = .(AUR, P02,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,P02)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(canton + P02 ~ AUR, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:2, "_1") | .x %in% paste0(1:2, "_2"), paste0("PPAs", .x), .x))  

# Administración zonal - sexo 
PPA_az_s <- poblacion2022[, .(freq=.N), by = .(AUR, P02,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P02)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P02 ~ AUR, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:2, "_1") | .x %in% paste0(1:2, "_2"), paste0("PPAs", .x), .x))  

# Parroquial - sexo
PPA_pr_s <- poblacion2022[, .(freq=.N), by = .(AUR, P02,parroquia)][,prop := 
            round((freq/sum(freq)*100),1),by=.(parroquia,P02)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(parroquia + P02 ~ AUR, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:2, "_1") | .x %in% paste0(1:2, "_2"), paste0("PPAs", .x), .x))  

# Sector - sexo 
PPA_s_s <- poblacion2022[, .(freq=.N), by = .(AUR, P02,Sector_DMQ)][,prop := 
            round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P02)][, ':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P02 ~ AUR, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`),names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:2, "_1") | .x %in% paste0(1:2, "_2"), paste0("PPAs", .x), .x)) 

# Grilla COD1000 - sexo 
PPA_1000_s <- poblacion2022[, .(freq=.N), by = .(AUR, P02,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P02)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(COD1000 + P02 ~ AUR, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:2, "_1") | .x %in% paste0(1:2, "_2"), paste0("PPAs", .x), .x))  

# Grilla COD500 - sexo 
PPA_500_s <- poblacion2022[, .(freq=.N), by = .(AUR, P02,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P02)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(COD500 + P02 ~ AUR, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:2, "_1") | .x %in% paste0(1:2, "_2"), paste0("PPAs", .x), .x))  

# Grilla H3_N8 - sexo 
PPA_N8_s <- poblacion2022[, .(freq=.N), by = .(AUR, P02,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P02)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(H3_N8 + P02 ~ AUR, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:2, "_1") | .x %in% paste0(1:2, "_2"), paste0("PPAs", .x), .x))  

# Grilla H3_N9 - sexo 
PPA_N9_s <- poblacion2022[, .(freq=.N), by = .(AUR, P02,H3_N9 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9 ,P02)][,':='(freq = NULL)][order(AUR,decreasing = FALSE )] %>% 
  dcast(H3_N9  + P02 ~ AUR, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:2, "_1") | .x %in% paste0(1:2, "_2"), paste0("PPAs", .x), .x))  

# 6. Guardar los resultados

wb <-  createWorkbook("PPA")

addWorksheet(wb, "PPA_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_c", x= PPA_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_az", x= PPA_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_pr", x= PPA_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_s", x= PPA_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PPA_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_1000", x= PPA_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_500", x= PPA_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_N8", x= PPA_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_N9", x= PPA_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PPA_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_c_s", x= PPA_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_az_s", x= PPA_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_pr_s", x= PPA_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_s_s", x= PPA_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PPA_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_1000_s", x= PPA_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_500_s", x= PPA_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_N8_s", x= PPA_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPA_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPA_N9_s", x= PPA_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de población de acuerdo al área en que reside.xlsx")

