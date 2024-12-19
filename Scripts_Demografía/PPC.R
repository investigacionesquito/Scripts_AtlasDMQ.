# Dimensión: Demografía 

# Nombre del Indicador: 
# Porcentaje de la población de acuerdo a la identificación según cultura y costumbres

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
PPC_c <- poblacion2022[, .(freq=.N), by = .(P11R, canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(canton~ P11R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC_", .x), .x))  

# Administración zonal 
PPC_az <- poblacion2022[, .(freq=.N), by = .(P11R, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ P11R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC_", .x), .x))  

# Parroquial
PPC_pr <- poblacion2022[, .(freq=.N), by = .(P11R, parroquia)][,prop := 
          round((freq/sum(freq)*100),1),by=.(parroquia)][, 
          ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(parroquia~ P11R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC_", .x), .x))  

# Sector
PPC_s <- poblacion2022[, .(freq=.N), by = .(P11R, Sector_DMQ)][,prop := 
         round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,
         ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast( Sector_DMQ ~ P11R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC_", .x), .x))  

# Grilla COD1000
PPC_1000 <- poblacion2022[, .(freq=.N), by = .(P11R, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ P11R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC_", .x), .x))  

# Grilla COD500
PPC_500 <- poblacion2022[, .(freq=.N), by = .(P11R, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(COD500 ~ P11R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC_", .x), .x))  

# Grilla H3_N8
PPC_N8 <- poblacion2022[, .(freq=.N), by = .(P11R, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(H3_N8 ~ P11R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC_", .x), .x))  

# Grilla H3_N9
PPC_N9 <- poblacion2022[, .(freq=.N), by = .(P11R, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ P11R, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PPC_", .x), .x))  

## Socio Demográfico/Económico

# Cantón - sexo
PPC_c_s <- poblacion2022[, .(freq=.N), by = .(P11R, P02,canton)][,prop := 
   round((freq/sum(freq)*100),1),by=.(canton,P02)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(canton + P02 ~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPCs", .x), .x))  

# Administración zonal - sexo
PPC_az_s <- poblacion2022[, .(freq=.N), by = .(P11R, P02,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P02)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P02 ~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPCs", .x), .x))  

# Parroquial - sexo
PPC_pr_s <- poblacion2022[, .(freq=.N), by = .(P11R, P02,parroquia)][,prop := 
            round((freq/sum(freq)*100),1),by=.(parroquia,P02)][, 
            ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(parroquia + P02 ~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPCs", .x), .x))  

# Sector - sexo 
PPC_s_s <- poblacion2022[, .(freq=.N), by = .(P11R, P02,Sector_DMQ)][,prop := 
           round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P02)][,
           ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P02~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPCs", .x), .x)) 

# Grilla COD1000 - sexo
PPC_1000_s <- poblacion2022[, .(freq=.N), by = .(P11R, P02,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P02)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(COD1000 + P02 ~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPCs", .x), .x))  

# Grilla COD500 - sexo
PPC_500_s <- poblacion2022[, .(freq=.N), by = .(P11R, P02,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P02)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(COD500 + P02 ~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPCs", .x), .x))  

# Grilla H3_N8 - sexo
PPC_N8_s <- poblacion2022[, .(freq=.N), by = .(P11R, P02,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P02)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(H3_N8 + P02 ~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPCs", .x), .x))  

# Grilla H3_N9 - sexo
PPC_N9_s <- poblacion2022[, .(freq=.N), by = .(P11R, P02,H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,P02)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(H3_N9 + P02 ~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = as.character(1:6), 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2"), paste0("PPCs", .x), .x))  

## Grupo de edad

# Cantón - Grupo de edad
PPC_c_e <- poblacion2022[, .(freq=.N), by = .(P11R, ETAEDAD,canton)][,prop := 
            round((freq/sum(freq)*100),1),by=.(canton,ETAEDAD)][, 
            ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(canton + ETAEDAD ~ P11R, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2")| .x %in% paste0(1:6, "_3")
                       | .x %in% paste0(1:6, "_4")| .x %in% paste0(1:6, "_5"), 
                       paste0("PPCe", .x), .x))  

# Administración zonal - Grupo de edad
PPC_az_e <- poblacion2022[, .(freq=.N), by = .(P11R, ETAEDAD,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,ETAEDAD)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(adm_zonal + ETAEDAD ~ P11R, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2")| .x %in% paste0(1:6, "_3")
                       | .x %in% paste0(1:6, "_4")| .x %in% paste0(1:6, "_5"), 
                       paste0("PPCe", .x), .x)) 

# Parroquial - Grupo de edad
PPC_pr_e <- poblacion2022[, .(freq=.N), by = .(P11R, ETAEDAD,parroquia)][,prop := 
            round((freq/sum(freq)*100),1),by=.(parroquia,ETAEDAD)][, 
                   ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(parroquia + ETAEDAD ~ P11R, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2")| .x %in% paste0(1:6, "_3")
                       | .x %in% paste0(1:6, "_4")| .x %in% paste0(1:6, "_5"), 
                       paste0("PPCe", .x), .x))  

# Sector - Grupo de edad
PPC_s_e <- poblacion2022[, .(freq=.N), by = .(P11R, ETAEDAD,Sector_DMQ)][,prop := 
            round((freq/sum(freq)*100),1),by=.(Sector_DMQ,ETAEDAD)][,
              ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + ETAEDAD ~ P11R, value.var = c("prop")) %>% 
  pivot_wider(names_from = ETAEDAD, values_from = as.character(1:6),  names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2")| .x %in% paste0(1:6, "_3")
                       | .x %in% paste0(1:6, "_4")| .x %in% paste0(1:6, "_5"), 
                       paste0("PPCe", .x), .x)) 

# Grilla COD1000 - Grupo de edad
PPC_1000_e <- poblacion2022[, .(freq=.N), by = .(P11R, ETAEDAD,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,ETAEDAD)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(COD1000 + ETAEDAD ~ P11R, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2")| .x %in% paste0(1:6, "_3")
                       | .x %in% paste0(1:6, "_4")| .x %in% paste0(1:6, "_5"), 
                       paste0("PPCe", .x), .x)) 

# Grilla COD500 - Grupo de edad
PPC_500_e <- poblacion2022[, .(freq=.N), by = .(P11R, ETAEDAD,COD500 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500 ,ETAEDAD)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(COD500  + ETAEDAD ~ P11R, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2")| .x %in% paste0(1:6, "_3")
                       | .x %in% paste0(1:6, "_4")| .x %in% paste0(1:6, "_5"), 
                       paste0("PPCe", .x), .x)) 

# Grilla H3_N8 - Grupo de edad
PPC_N8_e <- poblacion2022[, .(freq=.N), by = .(P11R, ETAEDAD,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,ETAEDAD)][, 
  ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(H3_N8+ ETAEDAD ~ P11R, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2")| .x %in% paste0(1:6, "_3")
                       | .x %in% paste0(1:6, "_4")| .x %in% paste0(1:6, "_5"), 
                       paste0("PPCe", .x), .x)) 

# Grilla H3_N9 - Grupo de edad
PPC_N9_e <- poblacion2022[, .(freq=.N), by = .(P11R, ETAEDAD,H3_N9 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9 ,ETAEDAD)][, 
 ':='(freq = NULL)][order(P11R,decreasing = FALSE )] %>% 
  dcast(H3_N9  + ETAEDAD ~ P11R, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = as.character(1:6), names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:6, "_1") | .x %in% paste0(1:6, "_2")| .x %in% paste0(1:6, "_3")
                       | .x %in% paste0(1:6, "_4")| .x %in% paste0(1:6, "_5"), 
                       paste0("PPCe", .x), .x)) 

# 6. Guardar los resultados

wb <-  createWorkbook("PPC")

addWorksheet(wb, "PPC_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_c", x= PPC_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_az", x= PPC_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_pr", x= PPC_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_s", x= PPC_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_1000", x= PPC_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_500", x= PPC_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_N8", x= PPC_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_N9", x= PPC_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PPC_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_c_s", x= PPC_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_az_s", x= PPC_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_pr_s", x= PPC_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_s_s", x= PPC_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_1000_s", x= PPC_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_500_s", x= PPC_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_N8_s", x= PPC_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_N9_s", x= PPC_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")



addWorksheet(wb, "PPC_c_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_c_e", x= PPC_c_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_az_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_az_e", x= PPC_az_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_pr_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_pr_e", x= PPC_pr_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_s_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_s_e", x= PPC_s_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_1000_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_1000_e", x= PPC_1000_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_500_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_500_e", x= PPC_500_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_N8_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_N8_e", x= PPC_N8_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPC_N9_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPC_N9_e", x= PPC_N9_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de la población de acuerdo a la identificación según cultura y costumbres.xlsx")