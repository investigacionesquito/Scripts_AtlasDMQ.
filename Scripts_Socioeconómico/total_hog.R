# Dimensión: Socioeconomía

# Nombre del Indicador:  Total de hogares  

# Proceso

# 1. Descargar bas es de datos
# 2. Establecer el directorio
# 3 . Cargar librerías
# 4. Importar base de datos
# 5. Calcular indicadores
# 6. Guardar los resultados

# 1. Descargar bases de datos ---------------------------------------

# 2. Establecer el directorio----------------------------------------
setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# 3 . Cargar librerías ----------------------------------------------
pacman::p_load(readr,data.table,openxlsx, foreign, haven, writexl
               , sjlabelled, stringr,labelled,dplyr, tidyr,expss,janitor)

# 4. Importar bases de datos-----------------------------------------
hogar2022 <- read_sav("hogar2022_Atlas.sav") %>% as.data.table()
poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()
VHP2022 <- read_sav("VHP2022_Atlas.sav") %>% as.data.table()

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
VHP2022 <- merge(HP2022,VHP2022, by="ID_VIV")

# Con el objetivo de tener en la base unificada unicamente personas 
# en viviendas particulares ocupadas con personas presentes,se aplica los siguientes filtros:
# VO1 = Tipo de vivienda 
# V0201R = Condición de ocupación de vivienda particular (si es 1 = ocupada)

VHP2022<-VHP2022[(V01<=8 & V0201R==1), ]

# 5. Calcular indicadores -------------------------------------------

# Cantonal
total_hog_c <-  VHP2022[P01==1, .N, 
                         by=.(canton.x)] %>% setnames("N", "total_hog") 

# Administracion zonal 
total_hog_az <-  VHP2022[P01==1, .N, 
                          by=.(adm_zonal)] %>% setnames("N", "total_hog") 

# Parroquial
total_hog_pr <-  VHP2022[P01==1, .N, 
                          by=.(parroquia)] %>% setnames("N", "total_hog") 

# Sector
total_hog_s <-  VHP2022[P01==1, .N, 
                         by=.(Sector_DMQ)] %>% setnames("N", "total_hog") 

# Grilla COD1000
total_hog_1000 <-  VHP2022[P01==1, .N, 
                            by=.(COD1000)] %>% setnames("N", "total_hog") 

# Grilla COD500
total_hog_500 <-  VHP2022[P01==1, .N, 
                           by=.(COD500)] %>% setnames("N", "total_hog") 

# Grilla H3_N8
total_hog_N8 <-  VHP2022[P01==1, .N, 
                          by=.(H3_N8)] %>% setnames("N", "total_hog") 

# Grilla H3_N9
total_hog_N9 <-  VHP2022[P01==1, .N, 
                          by=.(H3_N9)] %>% setnames("N", "total_hog") 