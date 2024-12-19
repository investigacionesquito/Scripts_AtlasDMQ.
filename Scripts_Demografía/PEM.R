# Dimensión: Demografía 

# Nombre del Indicador: 
# Porcentaje de de emigrantes que residen en el exterior, de acuerdo al país de residencia actual 

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

emigracion2022 <- read_sav("emigracion2022_Atlas.sav") %>% as.data.table() 

# 5. Calcular indicadores --------------------------------------------------

# Desagregaciones

## Geográfico territorial

# Cantón

PEM_c <- emigracion2022[, .(freq=.N), by=. (canton, E04)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(canton)][,':='(freq = NULL)] %>% 
  dcast (canton ~ E04, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PE", .x), .x)) 

# Aministracion zonal 
PEM_az <- emigracion2022[, .(freq=.N), by=. (adm_zonal, E04)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(adm_zonal)][,':='(freq = NULL)] %>% 
  dcast (adm_zonal ~ E04, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PE", .x), .x))

# Parroquial
PEM_pr <- emigracion2022[, .(freq=.N), by=. (parroquia, E04)] [,prop := 
          round((freq/sum(freq)*100),1), by=.(parroquia)][,':='(freq = NULL)] %>% 
  dcast (parroquia ~ E04, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PE", .x), .x)) 

# Sector
PEM_s <- emigracion2022[, .(freq=.N), by=. (Sector_DMQ, E04)] [,prop := 
          round((freq/sum(freq)*100),1), by=.(Sector_DMQ)][,':='(freq = NULL)] %>% 
  dcast (Sector_DMQ ~ E04, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PE", .x), .x))

# Grilla COD1000
PEM_1000 <- emigracion2022[, .(freq=.N), by=. (COD1000, E04)] [,prop := 
         round((freq/sum(freq)*100),1), by=.(COD1000)][,':='(freq = NULL)] %>% 
  dcast (COD1000 ~ E04, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PE", .x), .x))

# Grilla COD500
PEM_500 <- emigracion2022[, .(freq=.N), by=. (COD500, E04)] [,prop := 
         round((freq/sum(freq)*100),1), by=.(COD500)][,':='(freq = NULL)] %>% 
  dcast (COD500 ~ E04, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PE", .x), .x))

# Grilla H3_N8
PEM_N8 <- emigracion2022[, .(freq=.N), by=. (H3_N8, E04)] [,prop := 
        round((freq/sum(freq)*100),1), by=.(H3_N8)][,':='(freq = NULL)] %>% 
  dcast (H3_N8 ~ E04, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PE", .x), .x))

# Grilla H3_N9
PEM_N9 <- emigracion2022[, .(freq=.N), by=. (H3_N9, E04)] [,prop := 
         round((freq/sum(freq)*100),1), by=.(H3_N9)][,':='(freq = NULL)] %>% 
  dcast (H3_N9 ~ E04, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PE", .x), .x))

# 6. Guardar los resultados --------------------------------------------------------

wb <- createWorkbook("PEM")

addWorksheet(wb, "PEM_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PEM_c", x= PEM_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PEM_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PEM_az", x= PEM_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PEM_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PEM_pr", x= PEM_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PEM_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PEM_s", x= PEM_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PEM_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PEM_1000", x= PEM_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PEM_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PEM_500", x= PEM_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PEM_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PEM_N8", x= PEM_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PEM_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PEM_N9", x= PEM_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de de emigrantes que residen en el exterior, de acuerdo al país de residencia actua.xlsx", overwrite = TRUE)

