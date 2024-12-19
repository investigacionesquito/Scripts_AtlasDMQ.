# Dimensión: Socioeconomía

# Nombre del Indicador: 
# Distribución porcentual de la población, según aporte de seguridad social

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

# P30 = Aporta actualmente
# 1 IESS Seguro General
# 2 IESS Seguro Voluntario
# 3 IESS Seguro Campesino
# 4 Seguro ISSFA
# 5 Seguro ISSPOL
# 6 No aporta, es jubilada/o IESS/ ISSFA/ ISSPOL.
# 7 No aporta
# 9 Se ignora

# Desagregaciones

## Geográfico territorial

# Cantón
PSS_c <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, canton)][,prop := 
   round((freq/sum(freq)*100),1),by=.(canton)][, 
   ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(canton~ P30, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS_", .x), .x))  

# Administración zonal
PSS_az <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(adm_zonal ~ P30, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS_", .x), .x))  

# Parroquial
PSS_pr <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(parroquia~ P30, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS_", .x), .x))  

# Sector
PSS_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ)][,
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast( Sector_DMQ ~ P30, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS_", .x), .x))  

# Grilla COD1000
PSS_1000 <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(COD1000 ~ P30, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS_", .x), .x))  

# Grilla COD500
PSS_500 <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(COD500 ~ P30, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS_", .x), .x))  

# Grilla H3_N8
PSS_N8 <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(H3_N8 ~ P30, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS_", .x), .x))  

# Grilla H3_N9
PSS_N9 <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(H3_N9 ~ P30, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:9), paste0("PSS_", .x), .x))  

## Socio Demográfico/Económico

# Cantón - sexo
PSS_c_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, P02,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,P02)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(canton + P02 ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = `1`:`9`, 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSSs", .x), .x))

# Administración zonal - sexo
PSS_az_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, P02,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,P02)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(adm_zonal + P02 ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"),
                       paste0("PSSs", .x), .x))  

# Parroquial - sexo
PSS_pr_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, P02,parroquia)][,prop := 
   round((freq/sum(freq)*100),1),by=.(parroquia,P02)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(parroquia + P02 ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSSs", .x), .x))  

# Sector - sexo 
PSS_s_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, P02,Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ,P02)][,
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + P02~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSSs", .x), .x)) 

# Grilla COD1000 - sexo
PSS_1000_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, P02,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,P02)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(COD1000 + P02 ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSSs", .x), .x))  

# Grilla COD500 - sexo
PSS_500_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, P02,COD500)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500,P02)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(COD500 + P02 ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = `1`:`9`, 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSSs", .x), .x))  

# Grilla H3_N8 - sexo
PSS_N8_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, P02,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,P02)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(H3_N8 + P02 ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = `1`:`9`, 
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSSs", .x), .x))  

# Grilla H3_N9 - sexo
PSS_N9_s <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, P02,H3_N9)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9,P02)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(H3_N9 + P02 ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = `1`:`9`,
              names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2"), paste0("PSSs", .x), .x))  

## Grupo de edad por estapas de vida 

# Cantón - Grupo de edad
PSS_c_e <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, ETAEDAD,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,ETAEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(canton + ETAEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2")| .x %in% paste0(1:9, "_3")
                       | .x %in% paste0(1:9, "_4")| .x %in% paste0(1:9, "_5"), 
                       paste0("PSSe", .x), .x))  

# Administración zonal - Grupo de edad
PSS_az_e <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, ETAEDAD,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,ETAEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(adm_zonal + ETAEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2")
                       | .x %in% paste0(1:9, "_3") | .x %in% paste0(1:9, "_4")
                       | .x %in% paste0(1:9, "_5"),  paste0("PSSe", .x), .x)) 

# Parroquial - Grupo de edad
PSS_pr_e <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, ETAEDAD,parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia,ETAEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(parroquia + ETAEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2")| .x %in% paste0(1:9, "_3")
                       | .x %in% paste0(1:9, "_4")| .x %in% paste0(1:9, "_5"), 
                       paste0("PSSe", .x), .x))  

# Sector - Grupo de edad
PSS_s_e <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, ETAEDAD,Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ,ETAEDAD)][,
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + ETAEDAD ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = ETAEDAD, values_from = `1`:`9`,  names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2")| .x %in% paste0(1:9, "_3")
                       | .x %in% paste0(1:9, "_4")| .x %in% paste0(1:9, "_5"), 
                       paste0("PSSe", .x), .x)) 

# Grilla COD1000 - Grupo de edad
PSS_1000_e <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, ETAEDAD,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,ETAEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(COD1000 + ETAEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2")| .x %in% paste0(1:9, "_3")
                       | .x %in% paste0(1:9, "_4")| .x %in% paste0(1:9, "_5"), 
                       paste0("PSSe", .x), .x)) 

# Grilla COD500 - Grupo de edad
PSS_500_e <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, ETAEDAD,COD500 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500 ,ETAEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(COD500  + ETAEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2")| .x %in% paste0(1:9, "_3")
                       | .x %in% paste0(1:9, "_4")| .x %in% paste0(1:9, "_5"), 
                       paste0("PSSe", .x), .x)) 

# Grilla H3_N8 - Grupo de edad
PSS_N8_e <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, ETAEDAD,H3_N8)][,prop := 
   round((freq/sum(freq)*100),1),by=.(H3_N8,ETAEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(H3_N8+ ETAEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2")| .x %in% paste0(1:9, "_3")
                       | .x %in% paste0(1:9, "_4")| .x %in% paste0(1:9, "_5"), 
                       paste0("PSSe", .x), .x)) 

# Grilla H3_N9 - Grupo de edad
PSS_N9_e <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, ETAEDAD,H3_N9 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9 ,ETAEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(H3_N9  + ETAEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = ETAEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(1:9, "_1") | .x %in% paste0(1:9, "_2")| .x %in% paste0(1:9, "_3")
                       | .x %in% paste0(1:9, "_4")| .x %in% paste0(1:9, "_5"), 
                       paste0("PSSe", .x), .x)) 


# Grupos de edad quinquenales

#GEDAD: Grupos de edad quinquenales

# Cantón - Grupos de edad quinquenales
PSS_c_eq <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, GEDAD,canton)][,prop := 
  round((freq/sum(freq)*100),1),by=.(canton,GEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(canton + GEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = GEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  { setnames(., old = names(.)[-1], new = paste0("PSSeq", names(.)[-1])); . }


# Administración zonal - Grupos de edad quinquenales
PSS_az_eq <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, GEDAD,adm_zonal)][,prop := 
  round((freq/sum(freq)*100),1),by=.(adm_zonal,GEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(adm_zonal + GEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = GEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  { setnames(., old = names(.)[-1], new = paste0("PSSeq", names(.)[-1])); . }

# Parroquial - Grupos de edad quinquenales
PSS_pr_eq <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, GEDAD,parroquia)][,prop := 
  round((freq/sum(freq)*100),1),by=.(parroquia,GEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(parroquia + GEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = GEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  { setnames(., old = names(.)[-1], new = paste0("PSSeq", names(.)[-1])); . }  

# Sector - Grupos de edad quinquenales
PSS_s_eq <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30,GEDAD,Sector_DMQ)][,prop := 
  round((freq/sum(freq)*100),1),by=.(Sector_DMQ,GEDAD)][,
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(Sector_DMQ + GEDAD ~ P30, value.var = c("prop")) %>% 
  pivot_wider(names_from = GEDAD, values_from = `1`:`9`,  names_sep = "_") %>%
  { setnames(., old = names(.)[-1], new = paste0("PSSeq", names(.)[-1])); . }

# Grilla COD1000 - Grupos de edad quinquenales
PSS_1000_eq <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, GEDAD,COD1000)][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD1000,GEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(COD1000 + GEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = GEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  { setnames(., old = names(.)[-1], new = paste0("PSSeq", names(.)[-1])); . } 

# Grilla COD500 - Grupos de edad quinquenales
PSS_500_eq <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30,GEDAD,COD500 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(COD500 ,GEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(COD500  + GEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = GEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  { setnames(., old = names(.)[-1], new = paste0("PSSeq", names(.)[-1])); . }

# Grilla H3_N8 - Grupos de edad quinquenales
PSS_N8_eq <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, GEDAD,H3_N8)][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N8,GEDAD)][, 
 ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(H3_N8+ GEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = GEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  { setnames(., old = names(.)[-1], new = paste0("PSSeq", names(.)[-1])); . }

# Grilla H3_N9 - Grupos de edad quinquenales
PSS_N9_eq <- poblacion2022[!is.na(P30), .(freq=.N), by = .(P30, GEDAD,H3_N9 )][,prop := 
  round((freq/sum(freq)*100),1),by=.(H3_N9 ,GEDAD)][, 
  ':='(freq = NULL)][order(P30,decreasing = FALSE )] %>% 
  dcast(H3_N9  + GEDAD ~ P30, value.var = c("prop")) %>%
  pivot_wider(names_from = GEDAD, values_from = `1`:`9`, names_sep = "_") %>%
  { setnames(., old = names(.)[-1], new = paste0("PSSeq", names(.)[-1])); . }

# 9. Guardar los resultados

wb <-  createWorkbook("PSS")

addWorksheet(wb, "PSS_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_c", x= PSS_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_az", x= PSS_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_pr", x= PSS_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_s", x= PSS_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_1000", x= PSS_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_500", x= PSS_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_N8", x= PSS_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_N9", x= PSS_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PSS_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_c_s", x= PSS_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_az_s", x= PSS_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_pr_s", x= PSS_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_s_s", x= PSS_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_1000_s", x= PSS_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_500_s", x= PSS_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_N8_s", x= PSS_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_N9_s", x= PSS_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")



addWorksheet(wb, "PSS_c_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_c_e", x= PSS_c_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_az_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_az_e", x= PSS_az_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_pr_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_pr_e", x= PSS_pr_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_s_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_s_e", x= PSS_s_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_1000_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_1000_e", x= PSS_1000_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_500_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_500_e", x= PSS_500_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_N8_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_N8_e", x= PSS_N8_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_N9_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_N9_e", x= PSS_N9_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PSS_c_eq", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_c_eq", x= PSS_c_eq, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_az_eq", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_az_eq", x= PSS_az_eq, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_pr_eq", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_pr_eq", x= PSS_pr_eq, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_s_eq", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_s_eq", x= PSS_s_eq, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_1000_eq", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_1000_eq", x= PSS_1000_eq, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_500_eq", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_500_eq", x= PSS_500_eq, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_N8_eq", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_N8_eq", x= PSS_N8_eq, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PSS_N9_eq", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PSS_N9_eq", x= PSS_N9_eq, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Distribución porcentual de la población, según aporte de seguridad social.xlsx")