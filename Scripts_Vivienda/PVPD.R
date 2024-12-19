# Dimensión: Vivienda y servicios básicos

# Nombre del Indicador:  

#Porcentaje de viviendas particulares según número de dormitorios

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3. Cargar librerías
# 4. Importar base de datos
# 5. Calcular indicadores
# 6. Guardar los resultados

# 1. Descargar bases de datos

# 2. Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ----------------------------------------------

pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr,tidyr)

# 4. Importar bases de datos-----------------------------------------

hogar2022 <- read_sav("hogar2022_Atlas.sav") %>% as.data.table() 
vivienda2022 <- read_sav("vivienda2022_Atlas.sav") %>% as.data.table() 

# 5. Calcular indicadores --------------------------------------------------------

# Para el cálculo del indicador es necesario unir las bases de datos de vivienda y hogar

VH2022 <- merge(hogar2022,vivienda2022, by="ID_VIV")

# VO1 = Tipo de vivienda 
# H01R = Número de dormitorio (recodificado)
# V0201=1 Condición de ocupación de vivienda particular (ocupada con personas presentes)

VH2022[H01R==1, NC := 1] # 1 dormitorio
VH2022[H01R==2, NC := 2] # 2 dormitorio
VH2022[H01R==3, NC := 3] # 3 dormitorio
VH2022[H01R==4, NC := 4] # 4 dormitorio
VH2022[H01R==5, NC := 5] # 5 dormitorio
VH2022[H01R==6, NC := 6] # 6 o más dormitorio

# Cantonal
PVPD_c <- VH2022[V01 %in% c(1:8) & V0201 == 1 & !is.na(NC),.(freq=.N),by=.(canton.x,NC)][, 
       prop := round((freq/sum(freq)*100),1), by =.(canton.x)][,':='(freq = NULL)] %>% 
  dcast(canton.x ~ NC, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVPD_", .x), .x))  

# Administracion zonal 
PVPD_az <- VH2022[V01 %in% c(1:8) & V0201 == 1 & !is.na(NC) & V0201 == 1,.(freq=.N),by=.(adm_zonal.x,NC)][, 
        prop := round((freq/sum(freq)*100),1), by =.(adm_zonal.x)][,':='(freq = NULL)] %>% 
  dcast(adm_zonal.x ~ NC, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVPD_", .x), .x)) 

# Parroquial
PVPD_pr <- VH2022[V01 %in% c(1:8) & V0201 == 1 & !is.na(NC) & V0201 == 1,.(freq=.N),by=.(parroquia.x,NC)][, 
        prop := round((freq/sum(freq)*100),1), by =.(parroquia.x)][,':='(freq = NULL)] %>% 
  dcast(parroquia.x ~ NC, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVPD_", .x), .x)) 

# Sector
PVPD_s <- VH2022[V01 %in% c(1:8) & V0201 == 1 & !is.na(NC) ,.(freq=.N),by=.(Sector_DMQ.x,NC)][, 
       prop := round((freq/sum(freq)*100),1), by =.(Sector_DMQ.x)][,':='(freq = NULL)] %>% 
  dcast(Sector_DMQ.x ~ NC, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVPD_", .x), .x)) 

# Grilla COD1000
PVPD_1000 <- VH2022[V01 %in% c(1:8) & V0201 == 1 & !is.na(NC),.(freq=.N),by=.(COD1000.x,NC)][, 
          prop := round((freq/sum(freq)*100),1), by =.(COD1000.x)][,':='(freq = NULL)] %>% 
  dcast(COD1000.x ~ NC, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVPD_", .x), .x)) 

# Grilla COD500
PVPD_500 <- VH2022[V01 %in% c(1:8) & V0201 == 1 & !is.na(NC),.(freq=.N),by=.(COD500.x,NC)][, 
         prop := round((freq/sum(freq)*100),1), by =.(COD500.x)][,':='(freq = NULL)] %>% 
  dcast(COD500.x ~ NC, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVPD_", .x), .x)) 

# Grilla H3_N8
PVPD_N8 <- VH2022[V01 %in% c(1:8) & V0201 == 1 & !is.na(NC),.(freq=.N),by=.(H3_N8.x,NC)][, 
        prop := round((freq/sum(freq)*100),1), by =.(H3_N8.x)][,':='(freq = NULL)] %>% 
  dcast(H3_N8.x ~ NC, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVPD_", .x), .x)) 

# Grilla H3_N9
PVPD_N9 <- VH2022[V01 %in% c(1:8) & V0201 == 1& !is.na(NC),.(freq=.N),by=.(H3_N9.x,NC)][, 
        prop := round((freq/sum(freq)*100),1), by =.(H3_N9.x)][,':='(freq = NULL)] %>% 
  dcast(H3_N9.x ~ NC, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PVPD_", .x), .x)) 

# 6. Guardar los resultados------------------------------------------

wb <- createWorkbook("PVPD_")

addWorksheet(wb, "PVPD_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPD_c", x=PVPD_c, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPD_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPD_az", x=PVPD_az, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPD_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPD_pr", x=PVPD_pr, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPD_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPD_s", x=PVPD_s, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")

addWorksheet(wb, "PVPD_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPD_1000", x=PVPD_1000, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPD_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPD_500", x=PVPD_500, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPD_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPD_N8", x=PVPD_N8, startRow = 1,rowNames= FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PVPD_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PVPD_N9", x=PVPD_N9, startRow = 1,rowNames =FALSE,
               startCol = 1, tableStyle = "TableStyleMedium9")


saveWorkbook(wb, "Porcentaje de viviendas particulares según número de dormitorios.xlsx") 