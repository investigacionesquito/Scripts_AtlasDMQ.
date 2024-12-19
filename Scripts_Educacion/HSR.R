# Dimensión: Educación

# Nombre del Indicador: Porcentaje de hogares que realizan al menos una práctica de separación de residuos

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3. Cargar librerías
# 4. ImporHSRr base de datos
# 5. Calcular indicadores
# 6. Guardar los resultados

# 1. Descargar bases de datos ---------------------------------------------------

# 2. Establecer el directorio ---------------------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ---------------------------------------------------------

pacman::p_load(daHSR.HSRble,openxlsx, foreign, haven, writexl, sjlabelled, stringr, dplyr)


# 4. Cargar base de datos ----------------------------------------------------

hogar2022 <- read_sav("hogar2022_Atlas.sav") %>% as.data.table()
poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()
vivienda2022 <- read_sav("vivienda2022_Atlas.sav") %>% as.data.table()

### Para el cálculo del indicador es necesario unir las bases de datos de población y hogar

poblacion2022[, I01_c:=str_pad(I01, 2, pad = "0")]
poblacion2022[, I02_c:=str_pad(I02, 2, pad = "0")]
poblacion2022[, I03_c:=str_pad(I03, 2, pad = "0")]
poblacion2022[, I04_c:=str_pad(I04, 3, pad = "0")]
poblacion2022[, I05_c:=str_pad(I05, 3, pad = "0")]
poblacion2022[, I10_c:=str_pad(I10, 3, pad = "0")]
poblacion2022[, INHP_c:=str_pad(INH, 2, pad = "0")]

poblacion2022[,ID_HOG:= do.call(paste, c(replace(.SD, is.na(.SD),
                                                 ""), sep = "")),.SDcols = c("I01_c", "I02_c", "I03_c", "I04_c",
                                                                             "I05_c","I10_c", "INHP_c")]  

HP2022 <- merge(hogar2022,poblacion2022, by="ID_HOG")
HP2022 <- HP2022 %>% rename(ID_VIV = ID_VIV.x)
VHP2022 <- merge(HP2022,vivienda2022, by="ID_VIV")

# 5. Calcular indicadores -------------------------------------------------------

# VO1 = Tipo de vivienda 
# V0201R = Condición de ocupación de vivienda particular (si es 1 = ocupada)
# H0701 = Acostumbra separar la basura en orgánica e inorgánica (1= SI)
# H0702 = Acostumbra separar desperdicios para dar a los animales o planHSRs (1= SI)
# H0703 = Acostumbra separar papel, cartón, plástico o vidrio para vender, regalar o reutilizar (1= SI)


VHP2022[V01<=8 & V0201R==1, SR := 0]
VHP2022[(H0701==1 | H0702==1 | H0703==1),SR:=1]

# Desegregaciones: 

## Geográfico/Territorial

# Cantonal
HSR_c <-  VHP2022[V01<=8 & V0201R==1, round(mean (SR *100, na.rm=T),1), 
                 by=.(canton)] %>% setnames("V1", "HSR") 

# Administracion zonal 

HSR_az <-  VHP2022[V01<=8 & V0201R==1, round(mean (SR *100, na.rm=T),1), 
                  by=.(adm_zonal.x)] %>% setnames("V1", "HSR") 

# Parroquial
HSR_pr <-  VHP2022[V01<=8 & V0201R==1, round(mean (SR *100, na.rm=T),1), 
                  by=.(parroquia.x)] %>% setnames("V1", "HSR") 

# Sector
HSR_s <-  VHP2022[V01<=8 & V0201R==1, round(mean (SR *100, na.rm=T),1), 
                 by=.(Sector_DMQ.x)] %>% setnames("V1", "HSR") 

# Grilla COD1000
HSR_1000 <-  VHP2022[V01<=8 & V0201R==1, round(mean (SR *100, na.rm=T),1), 
                    by=.(COD1000.x)] %>% setnames("V1", "HSR") 

# Grilla COD500
HSR_500 <-  VHP2022[V01<=8 & V0201R==1, round(mean (SR *100, na.rm=T),1), 
                   by=.(COD500.x)] %>% setnames("V1", "HSR") 

# Grilla H3_N8
HSR_N8 <-  VHP2022[V01<=8 & V0201R==1, round(mean (SR *100, na.rm=T),1), 
                  by=.(H3_N8.x)] %>% setnames("V1", "HSR") 

# Grilla H3_N9
HSR_N9 <-  VHP2022[V01<=8 & V0201R==1, round(mean (SR *100, na.rm=T),1), 
                  by=.(H3_N9.x)] %>% setnames("V1", "HSR") 

## Autodeterminación del jefe del hogar 

# P01= Parentesco o relación con el represenHSRnte del hogar (Si es 1 es represenHSRnte del hogar) 

#P11R; Cómo se identifica según su cultura y costumbres
## 1 Indígena
## 2 Afroecuatoriana/o, Afrodescendiente, Negra/o, MulaHSR/o
## 3 Montubia/o
## 4 Mestiza/o
## 5 Blanca/o
## 6 Otro

# Cantón - Autodeterminacióndel jefe del hogar
HSR_c_a <- VHP2022[V01<=8 & V0201R==1 & P01==1, .N, by = .(SR, P11R,canton)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(canton,P11R)][SR== 1, ][
    , `:=`(N = NULL, SR = NULL)][order(canton)] %>% 
  dcast(canton~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("HSR_A", 1:6))

# Administración zonal - Autodeterminación del jefe del hogar
HSR_az_a <- VHP2022[V01<=8 & V0201R==1 & P01==1, .N, by = .(SR, P11R,adm_zonal)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(adm_zonal,P11R)][SR == 1, ][
    , `:=`(N = NULL, SR = NULL)][order(adm_zonal)] %>% 
  dcast(adm_zonal~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("HSR_A", 1:6))

# Parroquia - Autodeterminación del jefe del hogar
HSR_pr_a <- VHP2022[V01<=8 & V0201R==1 & P01==1, .N, by = .(SR, P11R,parroquia)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(parroquia,P11R)][SR == 1, ][
    , `:=`(N = NULL, SR = NULL)][order(parroquia)] %>% 
  dcast(parroquia~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("HSR_A", 1:6)) 

# Sector - Autodeterminación del jefe del hogar
HSR_s_a <- VHP2022[V01<=8 & V0201R==1 & P01==1, .N, by = .(SR, P11R,Sector_DMQ)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(Sector_DMQ,P11R)][SR == 1, ][
    , `:=`(N = NULL, SR = NULL)][order(Sector_DMQ)] %>% 
  dcast(Sector_DMQ~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("HSR_A", 1:6))

# Grilla COD1000 - Autodeterminación del jefe del hogar
HSR_1000_a <- VHP2022[V01<=8 & V0201R==1 & P01==1, .N, by = .(SR, P11R,COD1000)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD1000,P11R)][SR == 1, ][
    , `:=`(N = NULL, SR = NULL)][order(COD1000)] %>% 
  dcast(COD1000~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("HSR_A", 1:6))

# Grilla COD500 - Autodeterminación del jefe del hogar
HSR_500_a <- VHP2022[V01<=8 & V0201R==1 & P01==1, .N, by = .(SR, P11R,COD500)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(COD500,P11R)][SR == 1, ][
    , `:=`(N = NULL, SR = NULL)][order(COD500)] %>% 
  dcast(COD500~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("HSR_A", 1:6))

# Grilla H3_N8 - Autodeterminación del jefe del hogar
HSR_N8_a <- VHP2022[V01<=8 & V0201R==1 & P01==1, .N, by = .(SR, P11R,H3_N8)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N8,P11R)][SR == 1, ][
    , `:=`(N = NULL, SR = NULL)][order(H3_N8)] %>% 
  dcast(H3_N8~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("HSR_A", 1:6))

# Grilla H3_N9 - Autodeterminación del jefe del hogar
HSR_N9_a <- VHP2022[V01<=8 & V0201R==1 & P01==1, .N, by = .(SR, P11R,H3_N9)][
  , freq := round(100 * (N / sum(N, na.rm = TRUE)), 1), by=.(H3_N9,P11R)][SR == 1, ][
    , `:=`(N = NULL, SR = NULL)][order(H3_N9)] %>% 
  dcast(H3_N9~ P11R, value.var = "freq") %>% 
  setnames(old = as.character(1:6), new = paste0("HSR_A", 1:6))


# 6. Guardar los resulHSRdos --------------------------------------------------------

wb <-  createWorkbook("HSR")

addWorksheet(wb, "HSR_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_c", x= HSR_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_az", x= HSR_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_pr", x= HSR_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_s", x= HSR_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_1000", x= HSR_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_500", x= HSR_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_N8", x= HSR_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_N9", x= HSR_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HSR_c_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_c_a", x= HSR_c_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_az_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_az_a", x= HSR_az_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_pr_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_pr_a", x= HSR_pr_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_s_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_s_a", x= HSR_s_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_1000_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_1000_a", x= HSR_1000_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_500_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_500_a", x= HSR_500_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_N8_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_N8_a", x= HSR_N8_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HSR_N9_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HSR_N9_a", x= HSR_N9_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de hogares que realizan al menos una práctica de separación de residuos.xlsx", overwrite = TRUE)