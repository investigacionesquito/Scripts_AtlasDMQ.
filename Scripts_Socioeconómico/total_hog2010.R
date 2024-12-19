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
hogar2010 <- read_sav("hogar2010_Atlas.sav") %>% as.data.table()
poblacion2010 <- read_sav("poblacion2010_Atlas.sav") %>% as.data.table()
vivienda2010 <- read_sav("vivienda2010_Atlas.sav") %>% as.data.table()

### Para el cálculo del indicador es necesario unir las bases de datos de población y hogar

HP2010 <- merge(hogar2010,poblacion2010, by="id_hog")
HP2010 <- HP2010 %>% rename(id_viv = id_viv.x)
VHP2010 <- merge(HP2010,vivienda2010, by="id_viv")

# 5. Calcular indicadores --------------------------------------------------

# Con el objetivo de tener en la base unificada unicamente personas 
# en viviendas particulares ocupadas con personas presentes,se aplica los siguientes filtros:

# VTV = Tipo de vivienda 
# VCO = Condición de ocupación de vivienda particular (si es 1 = ocupada)

VHP2010<-VHP2010[(VTV<=8 & VCO==1), ]

# 5. Calcular indicadores -------------------------------------------

# Cantonal
t_hog2010_c <-  VHP2010[P02==1, .N, 
                         by=.(canton.x)] %>% setnames("N", "t_hog2010") 

# Administracion zonal 
t_hog2010_az <-  VHP2010[P02==1, .N, 
                          by=.(adm_zonal)] %>% setnames("N", "t_hog2010") 

# Parroquial
t_hog2010_pr <-  VHP2010[P02==1, .N, 
                          by=.(parroquia)] %>% setnames("N", "t_hog2010") 

# Sector
t_hog2010_s <-  VHP2010[P02==1, .N, 
                         by=.(Sector_DMQ)] %>% setnames("N", "t_hog2010") 

# Grilla COD1000
t_hog2010_1000 <-  VHP2010[P02==1, .N, 
                            by=.(COD1000)] %>% setnames("N", "t_hog2010") 

# Grilla COD500
t_hog2010_500 <-  VHP2010[P02==1, .N, 
                           by=.(COD500)] %>% setnames("N", "t_hog2010") 

# Grilla H3_N8
t_hog2010_N8 <-  VHP2010[P02==1, .N, 
                          by=.(H3_N8)] %>% setnames("N", "t_hog2010") 

# Grilla H3_N9
t_hog2010_N9 <-  VHP2010[P02==1, .N, 
                          by=.(H3_N9)] %>% setnames("N", "t_hog2010") 