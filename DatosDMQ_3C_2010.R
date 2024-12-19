### Filtrar bases de datos nacionales 

# Establecer el directorio----------------------------------------

setwd("C:/Users/denis/Documents/Atlas_Socioeconomico/Scripts_AtlasDMQ")

# Cargar librerías ----------------------------------------------

pacman::p_load(data.table,haven,dplyr,tidyr,sf)

# Importar bases de datos-----------------------------------------

poblacion2010_pich <- read_sav("Pichincha_Pob_Manz.sav") %>%
  as.data.table()

hogar2010_pich <- read_sav("Pichincha_Hog_Manz.sav") %>% 
  as.data.table()

emigracion2010_pich <- read_sav("Pichincha_Emig_Manz.sav") %>%
  as.data.table()

vivienda2010_pich <- read_sav("Pichincha_Viv_Manz.sav") %>%
  as.data.table()

# Filtro datos por los cantones DMQ, Mejia y Ruminahui y agrego la columna sec_anm ----------------------

poblacion2010_filter <- poblacion2010_pich %>% 
  filter(I01 == "17" , I02 %in% c("01", "03", "05")) 

hogar2010_filter <- hogar2010_pich %>% 
  filter(I01 == "17" , I02 %in% c("01", "03", "05")) 

emigracion2010_filter <- emigracion2010_pich %>% 
  filter(I01 == "17" , I02 %in% c("01", "03", "05")) 

vivienda2010_filter <- vivienda2010_pich %>% 
  filter(I01 == "17" , I02 %in% c("01", "03", "05")) 

# Cargar sf de los sectores 2022 
manz2010 <- st_read("C:\\Users\\denis\\Documents\\Atlas_Socioeconomico\\Scripts_AtlasDMQ\\Manzanas_2010\\manloc_2010_datos.shp") %>% 
  as.data.table() %>%
  rename(id_man = id)

# Union de la BD CENSO y GDB

poblacion2010_join <- poblacion2010_filter %>% 
  left_join(manz2010, by = "id_man") %>%  as.data.table()
hogar2010_join <- hogar2010_filter %>% 
  left_join(manz2010, by = "id_man") %>%  as.data.table()
emigracion2010_join <- emigracion2010_filter %>% 
  left_join(manz2010, by = "id_man") %>%  as.data.table()
vivienda2010_join <- vivienda2010_filter %>% 
  left_join(manz2010, by = "id_man") %>%  as.data.table()

# Guardar union

poblacion2010_join <- poblacion2010_join %>% 
  select(-geometry)
hogar2010_join <- hogar2010_join %>% 
  select(-geometry)
emigracion2010_join <- emigracion2010_join %>% 
  select(-geometry)
vivienda2010_join <- vivienda2010_join %>% 
  select(-geometry)

write_sav(poblacion2010_join, "poblacion2010_Atlas.sav")
write_sav(hogar2010_join, "hogar2010_Atlas.sav")
write_sav(emigracion2010_join, "emigracion2010_Atlas.sav")
write_sav(vivienda2010_join, "vivienda2010_Atlas.sav")



