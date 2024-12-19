# Dimensión: Demografía 

# Nombre del Indicador: Porcentaje de hogares de acuerdo al sexo del representante del hogar

# Proceso

# 1. Descargar bases de datos
# 2. Establecer el directorio
# 3. Cargar librerías
# 4. Importar base de datos
# 5. Calcular indicadores
# 6. Guardar los resultados

# 1. Descargar bases de datos ---------------------------------------------------

# 2. Establecer el directorio ---------------------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ---------------------------------------------------------

pacman::p_load(data.table,openxlsx, foreign, haven, writexl, sjlabelled, stringr)


# 4. Cargar base de datos ----------------------------------------------------

hogar2022 <- read_sav("hogar2022_Atlas.sav")
poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores -------------------------------------------------------

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

## Geográfico/Territorial

# Cantón 
HTR_c <- HP2022[INH.x >=1 & P01==1, .(freq=.N), by=. (canton.x, P02)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(canton.x)][,':='(freq = NULL)] %>% 
  dcast (canton.x ~ P02, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x == "1", "HTR_1", ifelse(.x == "2", "HTR_2", .x))) 

# Administracion zonal
HTR_az <- HP2022[INH.x >=1 & P01==1, .(freq=.N), by=. (adm_zonal.x, P02)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(adm_zonal.x)][,':='(freq = NULL)] %>% 
  dcast (adm_zonal.x ~ P02, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x == "1", "HTR_1", ifelse(.x == "2", "HTR_2", .x))) 

# Parroquia
HTR_pr <- HP2022[INH.x >=1 & P01==1, .(freq=.N), by=. (parroquia.x, P02)] [,prop := 
round((freq/sum(freq)*100),1), by=.(parroquia.x)][,':='(freq = NULL)] %>% 
  dcast (parroquia.x ~ P02, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x == "1", "HTR_1", ifelse(.x == "2", "HTR_2", .x)))  

# Sector
HTR_s <- HP2022[INH.x >=1 & P01==1, .(freq=.N), by=. (Sector_DMQ.x, P02)] [,prop := 
round((freq/sum(freq)*100),1), by=.(Sector_DMQ.x)][,':='(freq = NULL)] %>% 
  dcast (Sector_DMQ.x ~ P02, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x == "1", "HTR_1", ifelse(.x == "2", "HTR_2", .x))) 

# Grilla COD1000
HTR_1000 <- HP2022[INH.x >=1 & P01==1, .(freq=.N), by=. (COD1000.x, P02)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(COD1000.x)][,':='(freq = NULL)] %>% 
  dcast (COD1000.x ~ P02, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x == "1", "HTR_1", ifelse(.x == "2", "HTR_2", .x))) 


# Grilla COD500
HTR_500 <- HP2022[INH.x >=1 & P01==1, .(freq=.N), by=. (COD500.x, P02)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(COD500.x)][,':='(freq = NULL)] %>% 
  dcast (COD500.x ~ P02, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x == "1", "HTR_1", ifelse(.x == "2", "HTR_2", .x))) 


# Grilla H3_N8
HTR_N8 <- HP2022[INH.x >=1 & P01==1, .(freq=.N), by=. (H3_N8.x, P02)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(H3_N8.x)][,':='(freq = NULL)] %>% 
  dcast (H3_N8.x ~ P02, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x == "1", "HTR_1", ifelse(.x == "2", "HTR_2", .x))) 


# Grilla H3_N9
HTR_N9 <- HP2022[INH.x >=1 & P01==1, .(freq=.N), by=. (H3_N9.x, P02)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(H3_N9.x)][,':='(freq = NULL)] %>% 
  dcast (H3_N9.x ~ P02, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x == "1", "HTR_1", ifelse(.x == "2", "HTR_2", .x))) 


# 6. Guardar los resultados --------------------------------------------------------

wb <- createWorkbook("HTR")

addWorksheet(wb, "HTR_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTR_c", x= HTR_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTR_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTR_az", x= HTR_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTR_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTR_pr", x= HTR_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTR_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTR_s", x= HTR_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "HTR_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTR_1000", x= HTR_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTR_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTR_500", x= HTR_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTR_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTR_N8", x= HTR_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HTR_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HTR_N9", x= HTR_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de hogares de acuerdo al sexo del representante de hogar.xlsx", overwrite = TRUE)