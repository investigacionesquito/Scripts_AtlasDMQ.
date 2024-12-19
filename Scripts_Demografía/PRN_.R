# Dimensión: Demografía 

# Nombre del Indicador: 
#Porcentaje de la población residente según lugar de nacimiento (en quito, otra provincia o en el exterior)

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
PRN_c <- poblacion2022[, .(freq=.N), by=. (canton, P08)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(canton)][,':='(freq = NULL)] %>% 
  dcast (canton ~ P08, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:3), paste0("PRN", .x), .x))  

# Administración zonal 
PRN_az <- poblacion2022[, .(freq=.N), by=. (adm_zonal, P08)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(adm_zonal)][,':='(freq = NULL)] %>% 
  dcast (adm_zonal ~ P08, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:3), paste0("PRN", .x), .x))

# Parroquial
PRN_pr <- poblacion2022[, .(freq=.N), by=. (parroquia, P08)] [,prop := 
round((freq/sum(freq)*100),1), by=.(parroquia)][,':='(freq = NULL)] %>% 
  dcast (parroquia ~ P08, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:3), paste0("PRN", .x), .x)) 

# Sector
PRN_s <- poblacion2022[, .(freq=.N), by=. (Sector_DMQ,P08)] [,prop := 
round((freq/sum(freq)*100),1), by=.(Sector_DMQ)][,':='(freq = NULL)] %>% 
  dcast (Sector_DMQ ~ P08, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:6), paste0("PRN", .x), .x)) 

# Grilla COD1000
PRN_1000 <- poblacion2022[, .(freq=.N), by=. (COD1000, P08)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(COD1000)][,':='(freq = NULL)] %>% 
  dcast (COD1000~ P08, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:3), paste0("PRN", .x), .x))

# Grilla COD500
PRN_500 <- poblacion2022[, .(freq=.N), by=. (COD500, P08)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(COD500)][,':='(freq = NULL)] %>% 
  dcast (COD500 ~ P08, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:3), paste0("PRN", .x), .x))

# Grilla H3_N8
PRN_N8 <- poblacion2022[, .(freq=.N), by=. (H3_N8, P08)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(H3_N8)][,':='(freq = NULL)] %>% 
  dcast (H3_N8 ~ P08, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:3), paste0("PRN", .x), .x))

# Grilla H3_N9
PRN_N9 <- poblacion2022[, .(freq=.N), by=. (H3_N9, P08)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(H3_N9)][,':='(freq = NULL)] %>% 
  dcast (H3_N9 ~ P08, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% as.character(1:3), paste0("PRN", .x), .x))

## Socio Demográfico/Económico

# Cantón - Sexo 
PRN_c_s = poblacion2022[, .(freq=.N), by = .(canton,P02, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(canton,P02)][, ':=' (freq = NULL)] %>% 
  dcast(canton+ P02 ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | .x %in% paste0(1:3, "_2"), paste0("PRNs", .x), .x))  

# Administración zonal - sexo
PRN_az_s = poblacion2022[, .(freq=.N), by = .(adm_zonal,P02, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(adm_zonal,P02)][, ':=' (freq = NULL)] %>% 
  dcast(adm_zonal + P02 ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | .x %in% paste0(1:3, "_2"), paste0("PRNs", .x), .x))  

# Parroquia - Sexo 
PRN_pr_s = poblacion2022[, .(freq=.N), by = .(parroquia,P02, P08)][,prop :=
round((freq /sum(freq)*100),1), by=.(parroquia,P02)][, ':=' (freq = NULL)] %>% 
  dcast(parroquia + P02 ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | .x %in% paste0(1:3, "_2"), paste0("PRNs", .x), .x))  

# Sector - Sexo
PRN_s_s = poblacion2022[, .(freq=.N), by = .(Sector_DMQ,P02, P08)][,prop :=
round((freq /sum(freq)*100),1), by=.(Sector_DMQ,P02)][, ':=' (freq = NULL)] %>% 
  dcast(Sector_DMQ + P02 ~ P08, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | .x %in% paste0(1:3, "_2"), paste0("PRNs", .x), .x)) 

# Grilla COD1000 - Sexo 
PRN_1000_s = poblacion2022[, .(freq=.N), by = .(COD1000,P02, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD1000,P02)][, ':=' (freq = NULL)] %>% 
  dcast(COD1000 + P02 ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | .x %in% paste0(1:3, "_2"), paste0("PRNs", .x), .x))  

# Grilla COD500 - Sexo 
PRN_500_s = poblacion2022[, .(freq=.N), by = .(COD500,P02, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD500,P02)][, ':=' (freq = NULL)] %>% 
  dcast(COD500 + P02 ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | .x %in% paste0(1:3, "_2"), paste0("PRNs", .x), .x))  

# Grilla H3_N8 - Sexo 
PRN_N8_s = poblacion2022[, .(freq=.N), by = .(H3_N8,P02, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N8,P02)][, ':=' (freq = NULL)] %>% 
  dcast(H3_N8 + P02 ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | .x %in% paste0(1:3, "_2"), paste0("PRNs", .x), .x))

# Grilla H3_N9 - Sexo 
PRN_N9_s = poblacion2022[, .(freq=.N), by = .(H3_N9,P02, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N9,P02)][, ':=' (freq = NULL)] %>% 
  dcast(H3_N9 + P02 ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P02, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | .x %in% paste0(1:3, "_2"), paste0("PRNs", .x), .x))  

## Autodeterminación cultural 

# Cantón - Autodeterminación
PRN_c_a = poblacion2022[, .(freq=.N), by = .(canton ,P11R, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(canton ,P11R)][, ':=' (freq = NULL)] %>% 
  dcast(canton + P11R ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P11R, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | 
                         .x %in% paste0(1:3, "_2")| 
                         .x %in% paste0(1:3, "_3")| 
                         .x %in% paste0(1:3, "_4")| 
                         .x %in% paste0(1:3, "_5")| 
                         .x %in% paste0(1:3, "_6"), paste0("PRNa", .x), .x))

# Administración zonal - Autodeterminación
PRN_az_a = poblacion2022[, .(freq=.N), by = .(adm_zonal ,P11R, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(adm_zonal ,P11R)][, ':=' (freq = NULL)] %>% 
  dcast(adm_zonal + P11R ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P11R, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | 
                         .x %in% paste0(1:3, "_2")| 
                         .x %in% paste0(1:3, "_3")| 
                         .x %in% paste0(1:3, "_4")| 
                         .x %in% paste0(1:3, "_5")| 
                         .x %in% paste0(1:3, "_6"), paste0("PRNa", .x), .x))

# Parroquia - Autodeterminación
PRN_pr_a = poblacion2022[, .(freq=.N), by = .(parroquia,P11R, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(parroquia,P11R)][, ':=' (freq = NULL)] %>% 
  dcast(parroquia + P11R ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P11R, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | 
                       .x %in% paste0(1:3, "_2")| 
                       .x %in% paste0(1:3, "_3")| 
                       .x %in% paste0(1:3, "_4")| 
                       .x %in% paste0(1:3, "_5")| 
                       .x %in% paste0(1:3, "_6"), paste0("PRNa", .x), .x))  

# Sector - Autodeterminación
PRN_s_a = poblacion2022[, .(freq=.N), by = .(Sector_DMQ,P11R, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(Sector_DMQ,P11R)][, ':=' (freq = NULL)] %>% 
  dcast(Sector_DMQ + P11R ~ P08, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | 
                         .x %in% paste0(1:3, "_2")| 
                         .x %in% paste0(1:3, "_3")| 
                         .x %in% paste0(1:3, "_4")| 
                         .x %in% paste0(1:3, "_5")| 
                         .x %in% paste0(1:3, "_6"), paste0("PRNa", .x), .x)) 

# Grilla COD1000 - Autodeterminación
PRN_1000_a = poblacion2022[, .(freq=.N), by = .(COD1000 ,P11R, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD1000,P11R)][, ':=' (freq = NULL)] %>% 
  dcast(COD1000 + P11R ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P11R, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | 
                         .x %in% paste0(1:3, "_2")| 
                         .x %in% paste0(1:3, "_3")| 
                         .x %in% paste0(1:3, "_4")| 
                         .x %in% paste0(1:3, "_5")| 
                         .x %in% paste0(1:3, "_6"), paste0("PRNa", .x), .x))

# Grilla COD500 - Autodeterminación
PRN_500_a = poblacion2022[, .(freq=.N), by = .(COD500 ,P11R, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD500 ,P11R)][, ':=' (freq = NULL)] %>% 
  dcast(COD500 + P11R ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P11R, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | 
                         .x %in% paste0(1:3, "_2")| 
                         .x %in% paste0(1:3, "_3")| 
                         .x %in% paste0(1:3, "_4")| 
                         .x %in% paste0(1:3, "_5")| 
                         .x %in% paste0(1:3, "_6"), paste0("PRNa", .x), .x))

# Grilla H3_N8 - Autodeterminación
PRN_N8_a = poblacion2022[, .(freq=.N), by = .(H3_N8 ,P11R, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N8 ,P11R)][, ':=' (freq = NULL)] %>% 
  dcast(H3_N8 + P11R ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P11R, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | 
                         .x %in% paste0(1:3, "_2")| 
                         .x %in% paste0(1:3, "_3")| 
                         .x %in% paste0(1:3, "_4")| 
                         .x %in% paste0(1:3, "_5")| 
                         .x %in% paste0(1:3, "_6"), paste0("PRNa", .x), .x))

# Grilla H3_N9 - Autodeterminación
PRN_N9_a = poblacion2022[, .(freq=.N), by = .(H3_N9  ,P11R, P08)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N9  ,P11R)][, ':=' (freq = NULL)] %>% 
  dcast(H3_N9 + P11R ~ P08, value.var = c("prop"))%>% 
  pivot_wider(names_from = P11R, values_from = c(`1`, `2`, `3`)) %>% 
  rename_with(~ ifelse(.x %in% paste0(1:3, "_1") | 
                         .x %in% paste0(1:3, "_2")| 
                         .x %in% paste0(1:3, "_3")| 
                         .x %in% paste0(1:3, "_4")| 
                         .x %in% paste0(1:3, "_5")| 
                         .x %in% paste0(1:3, "_6"), paste0("PRNa", .x), .x))

# 6. Guardar los resultados

wb <-  createWorkbook("PRN")

addWorksheet(wb, "PRN_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_c", x= PRN_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_az", x= PRN_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_pr", x= PRN_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_s", x= PRN_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_1000", x= PRN_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_500", x= PRN_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_N8", x= PRN_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_N9", x= PRN_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PRN_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_c_s", x= PRN_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_az_s", x= PRN_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_pr_s", x= PRN_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_s_s", x= PRN_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_1000_s", x= PRN_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_500_s", x= PRN_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_N8_s", x= PRN_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_N9_s", x= PRN_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PRN_c_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_c_a", x= PRN_c_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_az_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_az_a", x= PRN_az_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_pr_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_pr_a", x= PRN_pr_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_s_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_s_a", x= PRN_s_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_1000_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_1000_a", x= PRN_1000_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_500_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_500_a", x= PRN_500_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_N8_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_N8_a", x= PRN_N8_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PRN_N9_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PRN_N9_a", x= PRN_N9_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, " Porcentaje de la población residente según lugar de nacimiento .xlsx")
                                                                                                            
