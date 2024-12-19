### Filtrar bases de datos nacionales 

# Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# Cargar librerías ----------------------------------------------

pacman::p_load(data.table,haven,dplyr,tidyr,sf)

# Importar bases de datos-----------------------------------------

poblacion2022_nac <- read_sav("CPV_Población_2022_Nacional.sav") %>% 
  as.data.table()

hogar2022_nac <- read_sav("CPV_Hogar_2022_Nacional.sav") %>% 
  as.data.table()

emigracion2022_nac<- read_sav("CPV_Emigración_2022_Nacional.sav") %>% 
  as.data.table()

vivienda2022_nac<- read_sav("CPV_Vivienda_2022_Nacional.sav") %>% 
  as.data.table()

# Filtro datos por los cantones DMQ, Mejia y Ruminahui y agrego la columna sec_anm ----------------------

poblacion2022_filter <- poblacion2022_nac %>% 
  filter(I01 == "17" , I02 %in% c("01", "03", "05")) %>% 
  unite("sec_anm", I01, I02, I03,I04 ,I05, sep = "", remove = FALSE)

hogar2022_filter <- hogar2022_nac %>% 
  filter(I01 == "17" , I02 %in% c("01", "03", "05")) %>% 
  unite("sec_anm", I01, I02, I03,I04 ,I05, sep = "", remove = FALSE)

emigracion2022_filter <- emigracion2022_nac %>% 
  filter(I01 == "17" , I02 %in% c("01", "03", "05")) %>% 
  unite("sec_anm", I01, I02, I03,I04 ,I05, sep = "", remove = FALSE)

vivienda2022_filter <- vivienda2022_nac %>% 
  filter(I01 == "17" , I02 %in% c("01", "03", "05")) %>% 
  unite("sec_anm", I01, I02, I03,I04 ,I05, sep = "", remove = FALSE)

# Cargar sf de los sectores 2022 
sec2022 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Sectores_2022\\Sec2022_ParrDMQ_CODGrillas.shp") %>% 
  as.data.table() 

# Union de la BD CENSO y GDB

poblacion2022_join <- poblacion2022_filter %>% 
  left_join(sec2022, by = "sec_anm") %>%  as.data.table()
hogar2022_join <- hogar2022_filter %>% 
  left_join(sec2022, by = "sec_anm") %>%  as.data.table()
emigracion2022_join <- emigracion2022_filter %>% 
  left_join(sec2022, by = "sec_anm") %>%  as.data.table()
vivienda2022_join <- vivienda2022_filter %>% 
  left_join(sec2022, by = "sec_anm") %>%  as.data.table()

# Guardar union

poblacion2022_join <- poblacion2022_join %>% 
  select(-CANTON,-geometry)
hogar2022_join <- hogar2022_join %>% 
  select(-CANTON,-geometry)
emigracion2022_join <- emigracion2022_join %>% 
  select(-CANTON,-geometry)
vivienda2022_join <- vivienda2022_join %>% 
  select(-CANTON,-geometry)

write_sav(poblacion2022_join, "poblacion2022_Atlas.sav")
write_sav(hogar2022_join, "hogar2022_Atlas.sav")
write_sav(emigracion2022_join, "emigracion2022_Atlas.sav")
write_sav(vivienda2022_join, "vivienda2022_Atlas.sav")



