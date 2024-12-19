# Dimensión: Socioeconomía

# Nombre del Indicador: 
# Población de 14 años y más por condición de actividad

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

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

# CONDACT1 = Condicion de actividad (desagregada)
# 1 Menor de 5 años
# 2 Ocupado
# 3 Desocupado
# 4 Fuera de la fuerza de trabajo

# Desagregaciones

## Geográfico territorial

# Cantón
PCA_c <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(canton~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PCA_", .x), .x))  

# Administración zonal 
PCA_az <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PCA_", .x), .x))  

# Parroquial
PCA_pr <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(parroquia~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PCA_", .x), .x))  

# Sector
PCA_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast( Sector_DMQ ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PCA_", .x), .x))  

# Grilla COD1000
PCA_1000 <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PCA_", .x), .x))  

# Grilla COD500
PCA_500 <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD500 ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PCA_", .x), .x))  

# Grilla H3_N8
PCA_N8 <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, H3_N8)][,prop := 
   round((freq/sum(freq)*100),1),by=.(H3_N8)][, 
   ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N8 ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PCA_", .x), .x))  

# Grilla H3_N9
PCA_N9 <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ CONDACT1, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(2:4), paste0("PCA_", .x), .x))  

## Socio Demográfico/Económico

# Cantón - sexo
PCA_c_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P02,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(canton + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(2:4), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PCAs", .x), .x))

# Administración zonal - sexo
PCA_az_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P02,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")
                       , paste0("PCAs", .x), .x))  

# Parroquial - sexo
PCA_pr_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P02,parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(parroquia + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PCAs", .x), .x))  

# Sector - sexo 
PCA_s_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P02,Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P02)][,
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P02~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PCAs", .x), .x)) 

# Grilla COD1000 - sexo
PCA_1000_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P02,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD1000 + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PCAs", .x), .x))  

# Grilla COD500 - sexo
PCA_500_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P02,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD500 + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PCAs", .x), .x))  

# Grilla H3_N8 - sexo
PCA_N8_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P02,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N8 + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PCAs", .x), .x))  

# Grilla H3_N9 - sexo
PCA_N9_s <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P02,H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,P02)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N9 + P02 ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from =  as.character(2:4),
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2"), paste0("PCAs", .x), .x))  

## Autodeterminación

#P11R; Cómo se identifica según su cultura y costumbres
## 1 Indígena
## 2 Afroecuatoriana/o, Afrodescendiente, Negra/o, Mulata/o
## 3 Montubia/o
## 4 Mestiza/o
## 5 Blanca/o
## 6 Otro

# Cantón - Autodeterminación
PCA_c_a <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P11R,canton)][,prop := 
           round((freq/sum(freq)*100),1),by=.(canton,P11R)][, 
           ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(canton + P11R ~ CONDACT1, value.var = c("prop")) %>%
  pivot_wider(names_from = P11R, values_from = as.character(2:4), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")| .x %in% paste0(2:4, "_3")
                       | .x %in% paste0(2:4, "_4")| .x %in% paste0(2:4, "_5")| .x %in% paste0(2:4, "_6"), 
                       paste0("PCAa", .x), .x))  

# Administración zonal - Autodeterminación
PCA_az_a <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P11R,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P11R)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P11R ~ CONDACT1, value.var = c("prop")) %>%
  pivot_wider(names_from = P11R, values_from = as.character(2:4), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")
                       | .x %in% paste0(2:4, "_3")| .x %in% paste0(2:4, "_4")
                       | .x %in% paste0(2:4, "_5")| .x %in% paste0(2:4, "_6"), 
                       paste0("PCAa", .x), .x)) 

# Parroquial - Autodeterminación
PCA_pr_a <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P11R,parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia,P11R)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(parroquia + P11R ~ CONDACT1, value.var = c("prop")) %>%
  pivot_wider(names_from = P11R, values_from = as.character(2:4), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")| .x %in% paste0(2:4, "_3")
                       | .x %in% paste0(2:4, "_4")| .x %in% paste0(2:4, "_5")| .x %in% paste0(2:4, "_6"), 
                       paste0("PCAa", .x), .x))  

# Sector - Autodeterminación
PCA_s_a <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P11R,Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P11R)][,
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P11R ~ CONDACT1, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = as.character(2:4),  names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")| .x %in% paste0(2:4, "_3")
                       | .x %in% paste0(2:4, "_4")| .x %in% paste0(2:4, "_5")| .x %in% paste0(2:4, "_6"), 
                       paste0("PCAa", .x), .x)) 

# Grilla COD1000 - Autodeterminación
PCA_1000_a <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P11R,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P11R)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD1000 + P11R ~ CONDACT1, value.var = c("prop")) %>%
  pivot_wider(names_from = P11R, values_from = as.character(2:4), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")| .x %in% paste0(2:4, "_3")
                       | .x %in% paste0(2:4, "_4")| .x %in% paste0(2:4, "_5")| .x %in% paste0(2:4, "_6"), 
                       paste0("PCAa", .x), .x)) 

# Grilla COD500 - Autodeterminación
PCA_500_a <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P11R,COD500 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500 ,P11R)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(COD500  + P11R ~ CONDACT1, value.var = c("prop")) %>%
  pivot_wider(names_from = P11R, values_from = as.character(2:4), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")| .x %in% paste0(2:4, "_3")
                       | .x %in% paste0(2:4, "_4")| .x %in% paste0(2:4, "_5")| .x %in% paste0(2:4, "_6"),  
                       paste0("PCAa", .x), .x)) 

# Grilla H3_N8 - Autodeterminación
PCA_N8_a <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P11R,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P11R)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N8+ P11R ~ CONDACT1, value.var = c("prop")) %>%
  pivot_wider(names_from = P11R, values_from = as.character(2:4), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")| .x %in% paste0(2:4, "_3")
                       | .x %in% paste0(2:4, "_4")| .x %in% paste0(2:4, "_5")| .x %in% paste0(2:4, "_6"),  
                       paste0("PCAa", .x), .x)) 

# Grilla H3_N9 - Autodeterminación
PCA_N9_a <- poblacion2022[P03 >= 14, .(freq=.N), by = .(CONDACT1, P11R,H3_N9 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9 ,P11R)][, 
  ':='(freq = NULL)][order(CONDACT1,decreasing = FALSE )] %>% 
  dcast(H3_N9  + P11R ~ CONDACT1, value.var = c("prop")) %>%
  pivot_wider(names_from = P11R, values_from = as.character(2:4), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(2:4, "_1") | .x %in% paste0(2:4, "_2")| .x %in% paste0(2:4, "_3")
                       | .x %in% paste0(2:4, "_4")| .x %in% paste0(2:4, "_5")| .x %in% paste0(2:4, "_6"), 
                       paste0("PCAa", .x), .x)) 

# 9. Guardar los resultados

wb <-  createWorkbook("PCA")

addWorksheet(wb, "PCA_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_c", x= PCA_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_az", x= PCA_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_pr", x= PCA_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_s", x= PCA_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_1000", x= PCA_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_500", x= PCA_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_N8", x= PCA_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_N9", x= PCA_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PCA_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_c_s", x= PCA_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_az_s", x= PCA_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_pr_s", x= PCA_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_s_s", x= PCA_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_1000_s", x= PCA_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_500_s", x= PCA_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_N8_s", x= PCA_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_N9_s", x= PCA_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")



addWorksheet(wb, "PCA_c_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_c_a", x= PCA_c_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_az_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_az_a", x= PCA_az_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_pr_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_pr_a", x= PCA_pr_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_s_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_s_a", x= PCA_s_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_1000_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_1000_a", x= PCA_1000_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_500_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_500_a", x= PCA_500_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_N8_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_N8_a", x= PCA_N8_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PCA_N9_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PCA_N9_a", x= PCA_N9_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Población de 14 años y más por condición de actividad.xlsx")