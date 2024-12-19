# Dimensión: Demografía 

# Nombre del Indicador: 
# Porcentaje de la población que se autoidentifica como indígena según nacionalidad o pueblo al que pertenece

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

PPN<- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12)][,prop := 
  round((freq/sum(freq)*100),1)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )]  

# Desagregaciones

## Geográfico territorial

# Cantón
PPN_c <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(canton~ P12, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 1:99), paste0("PPN_", .x), .x)) 

# Administración zonal 
PPN_az <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ P12, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 1:99), paste0("PPN_", .x), .x)) 

# Parroquial
PPN_pr <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, parroquia)][,prop := 
          round((freq/sum(freq)*100),1),by=.(parroquia)][, 
          ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(parroquia ~ P12, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 1:99), paste0("PPN_", .x), .x)) 

# Sector
PPN_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, Sector_DMQ)][,prop := 
         round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,
         ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ~ P12, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 1:99), paste0("PPN_", .x), .x)) 

# Grilla COD1000
PPN_1000 <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ P12, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 1:99), paste0("PPN_", .x), .x)) 

# Grilla COD500
PPN_500 <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(COD500 ~ P12, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 1:99), paste0("PPN_", .x), .x)) 

# Grilla H3_N8
PPN_N8 <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(H3_N8 ~ P12, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 1:99), paste0("PPN_", .x), .x)) 

# Grilla H3_N9
PPN_N9 <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ P12, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 1:99), paste0("PPN_", .x), .x)) 

## Socio Demográfico/Económico

# Cantón - sexo
PPN_c_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, P02,canton)][,prop := 
          round((freq/sum(freq)*100),1),by=.(canton,P02)][, 
           ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(canton + P02 ~ P12, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = PPN$P12,names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") |.x %in% paste0(sprintf("%02d", 1:99), "_2"), 
                       paste0("PPNs", .x), .x)) 

# Administración zonal - sexo 
PPN_az_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, P02,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P02)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P02 ~ P12, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = PPN$P12,names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") |.x %in% paste0(sprintf("%02d", 1:99), "_2"), 
                       paste0("PPNs", .x), .x)) 

# Parroquial - sexo
PPN_pr_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, P02,parroquia)][,prop := 
            round((freq/sum(freq)*100),1),by=.(parroquia,P02)][, 
            ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(parroquia+ P02 ~ P12, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = PPN$P12,names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") |.x %in% paste0(sprintf("%02d", 1:99), "_2"), 
                       paste0("PPNs", .x), .x)) 

# Sector - sexo 
PPN_s_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, P02,Sector_DMQ)][,prop := 
           round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P02)][,
           ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P02~ P12, value.var = c("prop"))%>%
  pivot_wider(names_from = P02, values_from = PPN$P12,names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2"), 
                       paste0("PPNs", .x), .x)) 

# Grilla COD1000 - sexo
PPN_1000_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, P02,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P02)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(COD1000 + P02 ~ P12, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = PPN$P12,names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") |.x %in% paste0(sprintf("%02d", 1:99), "_2"), 
                       paste0("PPNs", .x), .x)) 

# Grilla COD500 - sexo
PPN_500_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, P02,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P02)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(COD500 + P02 ~ P12, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = PPN$P12,names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") |.x %in% paste0(sprintf("%02d", 1:99), "_2"), 
                       paste0("PPNs", .x), .x)) 

# Grilla H3_N8 - sexo
PPN_N8_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, P02,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P02)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(H3_N8 + P02 ~ P12, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = PPN$P12,names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") |.x %in% paste0(sprintf("%02d", 1:99), "_2"), 
                       paste0("PPNs", .x), .x)) 

# Grilla H3_N9 - sexo
PPN_N9_s <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, P02,H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,P02)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(H3_N9 + P02 ~ P12, value.var = c("prop")) %>%
  pivot_wider(names_from = P02, values_from = PPN$P12,names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") |.x %in% paste0(sprintf("%02d", 1:99), "_2"), 
                       paste0("PPNs", .x), .x)) 

## Grupo de edad

# Cantón - Grupo de edad
PPN_c_e <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, ETAEDAD,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,ETAEDAD)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(canton + ETAEDAD ~ P12, value.var = c("prop"))%>%
  pivot_wider(names_from = ETAEDAD, values_from = PPN$P12, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_3")  | .x %in% paste0(sprintf("%02d", 1:99), "_4")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_5"), paste0("PPNe", .x), .x)) 

# Administración zonal - Grupo de edad 
PPN_az_e <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, ETAEDAD,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,ETAEDAD)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(adm_zonal + ETAEDAD ~ P12, value.var = c("prop"))%>%
  pivot_wider(names_from = ETAEDAD, values_from = PPN$P12, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_3")  | .x %in% paste0(sprintf("%02d", 1:99), "_4")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_5"), paste0("PPNe", .x), .x)) 

# Parroquial - Grupo de edad
PPN_pr_e <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, ETAEDAD,parroquia)][,prop := 
            round((freq/sum(freq)*100),1),by=.(parroquia,ETAEDAD)][, 
            ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(parroquia+ ETAEDAD ~ P12, value.var = c("prop"))%>%
  pivot_wider(names_from = ETAEDAD, values_from = PPN$P12, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2")| 
                      .x %in% paste0(sprintf("%02d", 1:99), "_3")  | .x %in% paste0(sprintf("%02d", 1:99), "_4")| 
                        .x %in% paste0(sprintf("%02d", 1:99), "_5"), paste0("PPNe", .x), .x)) 

# Sector - Grupo de edad
PPN_s_e <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, ETAEDAD,Sector_DMQ)][,prop := 
            round((freq/sum(freq)*100),1),by=.(Sector_DMQ,ETAEDAD)][, 
            ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + ETAEDAD ~ P12, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = PPN$P12, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_3")| .x %in% paste0(sprintf("%02d", 1:99), "_4")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_5"), paste0("PPNe", .x), .x)) 

# Grilla COD1000 - Grupo de edad
PPN_1000_e <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, ETAEDAD,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,ETAEDAD)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(COD1000 + ETAEDAD ~ P12, value.var = c("prop"))%>%
  pivot_wider(names_from = ETAEDAD, values_from = PPN$P12, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_3")  | .x %in% paste0(sprintf("%02d", 1:99), "_4")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_5"), paste0("PPNe", .x), .x)) 

# Grilla COD500 - Grupo de edad
PPN_500_e <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, ETAEDAD,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,ETAEDAD)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(COD500 + ETAEDAD ~ P12, value.var = c("prop"))%>%
  pivot_wider(names_from = ETAEDAD, values_from = PPN$P12, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_3")  | .x %in% paste0(sprintf("%02d", 1:99), "_4")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_5"), paste0("PPNe", .x), .x)) 

# Grilla H3_N8 - Grupo de edad
PPN_N8_e <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, ETAEDAD,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,ETAEDAD)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(H3_N8 + ETAEDAD ~ P12, value.var = c("prop"))%>%
  pivot_wider(names_from = ETAEDAD, values_from = PPN$P12, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_3")  | .x %in% paste0(sprintf("%02d", 1:99), "_4")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_5"), paste0("PPNe", .x), .x)) 

# Grilla H3_N9 - Grupo de edad
PPN_N9_e <- poblacion2022[!is.na(P12) & P12 != "", .(freq=.N), by = .(P12, ETAEDAD,H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,ETAEDAD)][, 
  ':='(freq = NULL)][order(P12,decreasing = FALSE )] %>% 
  dcast(H3_N9 + ETAEDAD ~ P12, value.var = c("prop"))%>%
  pivot_wider(names_from = ETAEDAD, values_from = PPN$P12, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 1:99), "_1") | .x %in% paste0(sprintf("%02d", 1:99), "_2")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_3")  | .x %in% paste0(sprintf("%02d", 1:99), "_4")| 
                         .x %in% paste0(sprintf("%02d", 1:99), "_5"), paste0("PPNe", .x), .x)) 

# 6. Guardar los resultados

wb <-  createWorkbook("PPN")

addWorksheet(wb, "PPN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_c", x= PPN_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_az", x= PPN_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_pr", x= PPN_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_s", x= PPN_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_1000", x= PPN_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_500", x= PPN_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_N8", x= PPN_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_N9", x= PPN_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")



addWorksheet(wb, "PPN_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_c_s", x= PPN_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_az_s", x= PPN_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_pr_s", x= PPN_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_s_s", x= PPN_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_1000_s", x= PPN_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_500_s", x= PPN_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_N8_s", x= PPN_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_N9_s", x= PPN_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PPN_c_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_c_e", x= PPN_c_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_az_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_az_e", x= PPN_az_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_pr_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_pr_e", x= PPN_pr_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_s_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_s_e", x= PPN_s_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_1000_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_1000_e", x= PPN_1000_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_500_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_500_e", x= PPN_500_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_N8_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_N8_e", x= PPN_N8_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PPN_N9_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PPN_N9_e", x= PPN_N9_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de la población que se autoidentifica como indígena según nacionalidad o pueblo al que pertenece.xlsx")