# Dimensión: Demografía 

# Nombre del Indicador: 
# Porcentaje de la población de acuerdo a la identificación según cultura y costumbres 2010

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

poblacion2010 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

#P16 = Autoidentificación según cultura y costumbres

# 1 Indigena
# 2 Afroecuatoriano
# 3 Negro
# 4 Mulato
# 5 Montubio
# 6 Mestizo
# 7 Blanco
# 8 Otro

# Recodifico la pregunta 16 para poder hacer la comparación 

# 1 Indígena
# 2 Afroecuatoriana/o, Afrodescendiente, Negra/o, Mulata/o
# 3 Montubia/o
# 4 Mestiza/o
# 5 Blanca/o
# 6 Otro

poblacion2010[P16==1, P16R:=1]
poblacion2010[P16 %in% (2:4), P16R:=2]
poblacion2010[P16==5, P16R:=3]
poblacion2010[P16==6, P16R:=4]
poblacion2010[P16==7, P16R:=5]
poblacion2010[P16==8, P16R:=6]


# Desagregaciones

## Geográfico territorial

# Cantón
PPC10_c <- poblacion2010[, .(freq=.N), by = .(P16R, canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(canton~ P16R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC10_", .x), .x))  

# Administración zonal 
PPC10_az <- poblacion2010[, .(freq=.N), by = .(P16R, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ P16R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC10_", .x), .x))  

# Parroquial
PPC10_pr <- poblacion2010[, .(freq=.N), by = .(P16R, parroquia)][,prop := 
          round((freq/sum(freq)*100),1),by=.(parroquia)][, 
          ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(parroquia~ P16R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC10_", .x), .x))  

# Sector
PPC10_s <- poblacion2010[, .(freq=.N), by = .(P16R, Sector_DMQ)][,prop := 
         round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,
         ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast( Sector_DMQ ~ P16R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC10_", .x), .x))  

# Grilla COD1000
PPC10_1000 <- poblacion2010[, .(freq=.N), by = .(P16R, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ P16R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC10_", .x), .x))  

# Grilla COD500
PPC10_500 <- poblacion2010[, .(freq=.N), by = .(P16R, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(COD500 ~ P16R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC10_", .x), .x))  

# Grilla H3_N8
PPC10_N8 <- poblacion2010[, .(freq=.N), by = .(P16R, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(H3_N8 ~ P16R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC10_", .x), .x))  

# Grilla H3_N9
PPC10_N9 <- poblacion2010[, .(freq=.N), by = .(P16R, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ P16R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC10_", .x), .x))  

## Socio Demográfico/Económico

# P01 = Sexo al nacer 

# Cantón - sexo
PPC10_c_s <- poblacion2010[, .(freq=.N), by = .(P16R, P01,canton)][,prop := 
   round((freq/sum(freq)*100),1),by=.(canton,P01)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(canton + P01 ~ P16R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPC10s", .x), .x))  

# Administración zonal - sexo
PPC10_az_s <- poblacion2010[, .(freq=.N), by = .(P16R, P01,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P01)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P01 ~ P16R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPC10s", .x), .x))  

# Parroquial - sexo
PPC10_pr_s <- poblacion2010[, .(freq=.N), by = .(P16R, P01,parroquia)][,prop := 
            round((freq/sum(freq)*100),1),by=.(parroquia,P01)][, 
            ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(parroquia + P01 ~ P16R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPC10s", .x), .x))  

# Sector - sexo 
PPC10_s_s <- poblacion2010[, .(freq=.N), by = .(P16R, P01,Sector_DMQ)][,prop := 
           round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P01)][,
           ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P01~ P16R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPC10s", .x), .x)) 

# Grilla COD1000 - sexo
PPC10_1000_s <- poblacion2010[, .(freq=.N), by = .(P16R, P01,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P01)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(COD1000 + P01 ~ P16R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPC10s", .x), .x))  

# Grilla COD500 - sexo
PPC10_500_s <- poblacion2010[, .(freq=.N), by = .(P16R, P01,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P01)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(COD500 + P01 ~ P16R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPC10s", .x), .x))  

# Grilla H3_N8 - sexo
PPC10_N8_s <- poblacion2010[, .(freq=.N), by = .(P16R, P01,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P01)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(H3_N8 + P01 ~ P16R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPC10s", .x), .x))  

# Grilla H3_N9 - sexo
PPC10_N9_s <- poblacion2010[, .(freq=.N), by = .(P16R, P01,H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,P01)][, 
  ':='(freq = NULL)][order(P16R,decreasing = FALSE )] %>% 
  dcast(H3_N9 + P01 ~ P16R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPC10s", .x), .x))  

# 6. Guardar los resultados

wb <-  createWorkbook("PPC")

addWorksheet(wb, "PPC10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_c", x= PPC10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_az", x= PPC10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_pr", x= PPC10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_s", x= PPC10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_1000", x= PPC10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_500", x= PPC10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_N8", x= PPC10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_N9", x= PPC10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PPC10_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_c_s", x= PPC10_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_az_s", x= PPC10_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_pr_s", x= PPC10_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_s_s", x= PPC10_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_1000_s", x= PPC10_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_500_s", x= PPC10_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_N8_s", x= PPC10_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC10_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC10_N9_s", x= PPC10_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de la población de acuerdo a la identificación según cultura y costumbres 2010.xlsx")