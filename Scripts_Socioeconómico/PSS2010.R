# Dimensión: Socioeconomía

# Nombre del Indicador: 
# Distribución porcentual de la población, según aporte de seguridad social 2010

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5 . Calcular indicadores
# 9 . Guardar los resultados

# 1. Descargar bases de datos ---------------------------------------

# 2. Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ----------------------------------------------

pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr, tidyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2010 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

# P35 = Aporte o afiliación a la Seguridad Social
# 1 Seguro ISSFA
# 2 Seguro ISSPOL
# 3 IESS Seguro general
# 4 IESS Seguro voluntario
# 5 IESS Seguro campesino
# 6 Es jubilado del IESS/ISSFA/ISSPOL
# 7 No aporta
# 9 Se ignora


# Recodifico para poder comparar con 2022 

# 1 IESS Seguro General
# 2 IESS Seguro Voluntario
# 3 IESS Seguro Campesino
# 4 Seguro ISSFA
# 5 Seguro ISSPOL
# 6 No aporta, es jubilada/o IESS/ ISSFA/ ISSPOL.
# 7 No aporta
# 9 Se ignora

poblacion2010[P35==1, P35R:=4]
poblacion2010[P35==2, P35R:=5]
poblacion2010[P35==3, P35R:=1]
poblacion2010[P35==4, P35R:=2]
poblacion2010[P35==5, P35R:=3]
poblacion2010[P35==6, P35R:=6]
poblacion2010[P35==7, P35R:=7]
poblacion2010[P35==9, P35R:=9]

# Desagregaciones

## Geográfico territorial

# Cantón
PSS10_c <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, canton)][,prop := 
   round((freq/sum(freq)*100),1),by=.(canton)][, 
   ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(canton~ P35R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS10_", .x), .x))  

# Administración zonal
PSS10_az <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ P35R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS10_", .x), .x))  

# Parroquial
PSS10_pr <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(parroquia~ P35R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS10_", .x), .x))  

# Sector
PSS10_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast( Sector_DMQ ~ P35R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS10_", .x), .x))  

# Grilla COD1000
PSS10_1000 <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ P35R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS10_", .x), .x))  

# Grilla COD500
PSS10_500 <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(COD500 ~ P35R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS10_", .x), .x))  

# Grilla H3_N8
PSS10_N8 <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(H3_N8 ~ P35R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS10_", .x), .x))  

# Grilla H3_N9
PSS10_N9 <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ P35R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS10_", .x), .x))  

## Socio Demográfico/Económico

# Cantón - sexo
PSS10_c_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, P01,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,P01)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(canton + P01 ~ P35R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = `1`:`9`, 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSS10s", .x), .x))

# Administración zonal - sexo
PSS10_az_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, P01,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P01)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P01 ~ P35R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"),
                       paste0("PSS10s", .x), .x))  

# Parroquial - sexo
PSS10_pr_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, P01,parroquia)][,prop := 
   round((freq/sum(freq)*100),1),by=.(parroquia,P01)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(parroquia + P01 ~ P35R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSS10s", .x), .x))  

# Sector - sexo 
PSS10_s_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, P01,Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P01)][,
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P01~ P35R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSS10s", .x), .x)) 

# Grilla COD1000 - sexo
PSS10_1000_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, P01,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P01)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(COD1000 + P01 ~ P35R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSS10s", .x), .x))  

# Grilla COD500 - sexo
PSS10_500_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, P01,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P01)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(COD500 + P01 ~ P35R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = `1`:`9`, 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSS10s", .x), .x))  

# Grilla H3_N8 - sexo
PSS10_N8_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, P01,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P01)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(H3_N8 + P01 ~ P35R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = `1`:`9`, 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSS10s", .x), .x))  

# Grilla H3_N9 - sexo
PSS10_N9_s <- poblacion2010[!is.na(P35R), .(freq=.N), by = .(P35R, P01,H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,P01)][, 
  ':='(freq = NULL)][order(P35R,decreasing = FALSE )] %>% 
  dcast(H3_N9 + P01 ~ P35R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P01, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSS10s", .x), .x))  

# 9. Guardar los resultados

wb <-  createWorkbook("PSS10")

addWorksheet(wb, "PSS10_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_c", x= PSS10_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_az", x= PSS10_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_pr", x= PSS10_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_s", x= PSS10_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_1000", x= PSS10_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_500", x= PSS10_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_N8", x= PSS10_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_N9", x= PSS10_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PSS10_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_c_s", x= PSS10_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_az_s", x= PSS10_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_pr_s", x= PSS10_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_s_s", x= PSS10_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_1000_s", x= PSS10_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_500_s", x= PSS10_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_N8_s", x= PSS10_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS10_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS10_N9_s", x= PSS10_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Distribución porcentual de la población, según aporte de seguridad social 2010.xlsx")