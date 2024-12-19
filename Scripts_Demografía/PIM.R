# Dimensión: Demografía 

# Nombre del Indicador: 
#Porcentaje de inmigrantes que residen en Quito, de acuerdo al país de nacimiento

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
               ,sjlabelled, stringr,labelled,dplyr, tidyr)

# 4. Importar bases de datos-----------------------------------------

poblacion2022 <- read_sav("poblacion2022_Atlas.sav") %>% as.data.table()

# 5. Calcular indicadores --------------------------------------------------

PIM <- poblacion2022[P08 == 3, .(freq=.N), by=. (P08Q)] [,prop := 
  round((freq/sum(freq)*100),1)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  

# Desagregaciones

## Geográfico territorial

# Cantón
PIM_c <- poblacion2022[P08 == 3, .(freq=.N), by=. (canton, P08Q)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(canton)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast (canton ~ P08Q, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PI", .x), .x)) 

# Administracion zonal
PIM_az <- poblacion2022[P08 == 3, .(freq=.N), by=. (adm_zonal, P08Q)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(adm_zonal)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast (adm_zonal ~ P08Q, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PI", .x), .x)) 

# Parroquial
PIM_pr <- poblacion2022[P08 == 3, .(freq=.N), by=. (parroquia, P08Q)] [,prop := 
round((freq/sum(freq)*100),1), by=.(parroquia)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast (parroquia ~ P08Q, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PI", .x), .x))  

# Sector
PIM_s <- poblacion2022[P08 == 3, .(freq=.N), by=. (Sector_DMQ,P08Q)] [,prop := 
round((freq/sum(freq)*100),1), by=.(Sector_DMQ)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast (Sector_DMQ ~ P08Q, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PI", .x), .x)) 

# Grilla COD1000
PIM_1000 <- poblacion2022[P08 == 3, .(freq=.N), by=. (COD1000, P08Q)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(COD1000)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast (COD1000 ~ P08Q, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PI", .x), .x)) 

# Grilla COD500
PIM_500 <- poblacion2022[P08 == 3, .(freq=.N), by=. (COD500, P08Q)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(COD500)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast (COD500 ~ P08Q, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PI", .x), .x)) 

# Grilla H3_N8
PIM_N8 <- poblacion2022[P08 == 3, .(freq=.N), by=. (H3_N8, P08Q)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(H3_N8)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast (H3_N8 ~ P08Q, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PI", .x), .x)) 

# Grilla H3_N9
PIM_N9 <- poblacion2022[P08 == 3, .(freq=.N), by=. (H3_N9, P08Q)] [,prop := 
  round((freq/sum(freq)*100),1), by=.(H3_N9)][,':='(freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast (H3_N9 ~ P08Q, value.var = c("prop")) %>% 
  rename_with(~ ifelse(.x %in% sprintf("%02d", 880000:999999), paste0("PI", .x), .x)) 

## Socio Demográfico/Económico

# Cantón - Sexo 
PIM_c_s <- poblacion2022[P08 == 3, .(freq=.N), by = .(canton, P02, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(canton ,P02)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast(canton + P02 ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") |
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2"), paste0("Is", .x), .x))  

# Administracion zonal - Sexo 
PIM_az_s <- poblacion2022[P08 == 3, .(freq=.N), by = .(adm_zonal, P02, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(adm_zonal ,P02)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast(adm_zonal + P02 ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") |
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2"), paste0("Is", .x), .x)) 

# Parroquia - Sexo 
PIM_pr_s <- poblacion2022[P08 == 3, .(freq=.N), by = .(parroquia,P02, P08Q)][,prop :=
           round((freq /sum(freq)*100),1), by=.(parroquia,P02)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
           dcast(parroquia + P02 ~ P08Q, value.var = c("prop")) %>% 
           pivot_wider(names_from = P02, values_from = PIM$P08Q, names_sep = "_") %>%
          rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") |
                                 .x %in% paste0(sprintf("%02d", 880000:999999), "_2"), paste0("Is", .x), .x))  

# Sector - Sexo
PIM_s_s <- poblacion2022[P08 == 3, .(freq=.N), by = .(Sector_DMQ,P02, P08Q)][,prop :=
          round((freq /sum(freq)*100),1), by=.(Sector_DMQ,P02)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
          dcast(Sector_DMQ + P02 ~ P08Q, value.var = c("prop")) %>% 
          pivot_wider(names_from = P02, values_from = PIM$P08Q, names_sep = "_") %>%
          rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") |
                                 .x %in% paste0(sprintf("%02d", 880000:999999), "_2"), paste0("Is", .x), .x)) 

# Grilla COD1000 - Sexo 
PIM_1000_s <- poblacion2022[P08 == 3, .(freq=.N), by = .(COD1000,P02, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD1000,P02)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(COD1000 + P02 ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") |
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2"), paste0("Is", .x), .x)) 

# Grilla COD500 - Sexo 
PIM_500_s <- poblacion2022[P08 == 3, .(freq=.N), by = .(COD500,P02, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD500,P02)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(COD500 + P02 ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") |
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2"), paste0("Is", .x), .x)) 

# Grilla H3_N8 - Sexo 
PIM_N8_s <- poblacion2022[P08 == 3, .(freq=.N), by = .(H3_N8,P02, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N8,P02)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(H3_N8 + P02 ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") |
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2"), paste0("Is", .x), .x)) 

# Grilla H3_N9 - Sexo 
PIM_N9_s <- poblacion2022[P08 == 3, .(freq=.N), by = .(H3_N9,P02, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N9,P02)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(H3_N9 + P02 ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P02, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") |
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2"), paste0("Is", .x), .x)) 

## Autodeterminación cultural 

# Cantón - Autodeterminación 
PIM_c_a <- poblacion2022[P08 == 3, .(freq=.N), by = .(canton,P11R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(canton,P11R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast(canton + P11R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6"), paste0("Ia", .x), .x))  

# Administracion zonal - Autodeterminación 
PIM_az_a <- poblacion2022[P08 == 3, .(freq=.N), by = .(adm_zonal,P11R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(adm_zonal,P11R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast(adm_zonal + P11R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6"), paste0("Ia", .x), .x))  

# Parroquia - Autodeterminación
PIM_pr_a <- poblacion2022[P08 == 3, .(freq=.N), by = .(parroquia,P11R, P08Q)][,prop :=
            round((freq /sum(freq)*100),1), by=.(parroquia,P11R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast(parroquia + P11R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6"), paste0("Ia", .x), .x))  

# Sector - Autodeterminación
PIM_s_a <- poblacion2022[P08 == 3, .(freq=.N), by = .(Sector_DMQ,P11R, P08Q)][,prop :=
           round((freq /sum(freq)*100),1), by=.(Sector_DMQ,P11R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(Sector_DMQ + P11R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6"), paste0("Ia", .x), .x)) 
# Grilla COD1000 - Autodeterminación
PIM_1000_a <- poblacion2022[P08 == 3, .(freq=.N), by = .(COD1000,P11R, P08Q)][,prop :=
           round((freq /sum(freq)*100),1), by=.(COD1000,P11R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(COD1000 + P11R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6"), paste0("Ia", .x), .x)) 
# Grilla COD500 - Autodeterminación
PIM_500_a <- poblacion2022[P08 == 3, .(freq=.N), by = .(COD500,P11R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD500,P11R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(COD500 + P11R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6"), paste0("Ia", .x), .x)) 
# Grilla H3_N8 - Autodeterminación
PIM_N8_a <- poblacion2022[P08 == 3, .(freq=.N), by = .(H3_N8,P11R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N8,P11R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(H3_N8 + P11R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6"), paste0("Ia", .x), .x)) 
# Grilla H3_N9 - Autodeterminación
PIM_N9_a <- poblacion2022[P08 == 3, .(freq=.N), by = .(H3_N9,P11R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N9,P11R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(H3_N9 + P11R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P11R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6"), paste0("Ia", .x), .x)) 

## Nivel educativo 

# Cantón - Nivel educativo 
PIM_c_e <- poblacion2022[P08 == 3, .(freq=.N), by = .(canton,P17R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(canton,P17R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast(canton + P17R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P17R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_7")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_8")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_9")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_10")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_11"), paste0("Ie", .x), .x))  

# Administracion zonal  Nivel educativo 
PIM_az_e <- poblacion2022[P08 == 3, .(freq=.N), by = .(adm_zonal,P17R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(adm_zonal,P17R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast(adm_zonal + P17R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P17R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_7")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_8")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_9")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_10")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_11"), paste0("Ie", .x), .x))  

# Parroquia - Nivel educativo 
PIM_pr_e <- poblacion2022[P08 == 3, .(freq=.N), by = .(parroquia,P17R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(parroquia,P17R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)]  %>% 
  dcast(parroquia + P17R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P17R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_7")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_8")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_9")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_10")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_11"), paste0("Ie", .x), .x))  
# Sector - Nivel educativo 
PIM_s_e <- poblacion2022[P08 == 3, .(freq=.N), by = .(Sector_DMQ,P17R, P08Q)][,prop :=
           round((freq /sum(freq)*100),1), by=.(Sector_DMQ,P17R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(Sector_DMQ + P17R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P17R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_7")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_8")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_9")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_10")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_11"), paste0("Ie", .x), .x)) 

# Grilla COD1000 - Nivel educativo 
PIM_1000_e <- poblacion2022[P08 == 3, .(freq=.N), by = .(COD1000,P17R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD1000,P17R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(COD1000 + P17R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P17R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_7")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_8")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_9")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_10")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_11"), paste0("Ie", .x), .x)) 

# Grilla COD500 - Nivel educativo 
PIM_500_e <- poblacion2022[P08 == 3, .(freq=.N), by = .(COD500,P17R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(COD500,P17R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(COD500 + P17R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P17R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_7")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_8")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_9")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_10")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_11"), paste0("Ie", .x), .x)) 
# Grilla H3_N8 - Nivel educativo 
PIM_N8_e <- poblacion2022[P08 == 3, .(freq=.N), by = .(H3_N8,P17R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N8,P17R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(H3_N8 + P17R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P17R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_7")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_8")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_9")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_10")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_11"), paste0("Ie", .x), .x)) 
# Grilla H3_N9 - Nivel educativo 
PIM_N9_e <- poblacion2022[P08 == 3, .(freq=.N), by = .(H3_N9,P17R, P08Q)][,prop :=
  round((freq /sum(freq)*100),1), by=.(H3_N9,P17R)][, ':=' (freq = NULL)][order(P08Q,decreasing = FALSE)] %>% 
  dcast(H3_N9 + P17R ~ P08Q, value.var = c("prop")) %>% 
  pivot_wider(names_from = P17R, values_from = PIM$P08Q, names_sep = "_") %>%
  rename_with(~ ifelse(.x %in% paste0(sprintf("%02d", 880000:999999), "_1") | 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_2")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_3")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_4")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_5")| 
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_6")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_7")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_8")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_9")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_10")|
                         .x %in% paste0(sprintf("%02d", 880000:999999), "_11"), paste0("Ie", .x), .x)) 

# 6. Guardar los resultados ----

wb <-  createWorkbook("PIM")

addWorksheet(wb, "PIM_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_c", x= PIM_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_az", x= PIM_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_pr", x= PIM_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_s", x= PIM_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_1000", x= PIM_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_500", x= PIM_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_N8", x= PIM_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_N9", x= PIM_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PIM_c_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_c_s", x= PIM_c_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_az_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_az_s", x= PIM_az_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_pr_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_pr_s", x= PIM_pr_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_s_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_s_s", x= PIM_s_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_1000_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_1000_s", x= PIM_1000_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_500_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_500_s", x= PIM_500_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_N8_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_N8_s", x= PIM_N8_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_N9_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_N9_s", x= PIM_N9_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")



addWorksheet(wb, "PIM_c_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_c_a", x= PIM_c_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_az_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_az_a", x= PIM_az_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_pr_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_pr_a", x= PIM_pr_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_s_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_s_a", x= PIM_s_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_1000_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_1000_a", x= PIM_1000_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_500_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_500_a", x= PIM_500_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_N8_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_N8_a", x= PIM_N8_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_N9_a", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_N9_a", x= PIM_N9_a, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "PIM_c_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_c_e", x= PIM_c_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_az_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_az_e", x= PIM_az_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_pr_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_pr_e", x= PIM_pr_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_s_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_s_e", x= PIM_s_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_1000_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_1000_e", x= PIM_1000_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_500_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_500_e", x= PIM_500_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_N8_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_N8_e", x= PIM_N8_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "PIM_N9_e", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "PIM_N9_e", x= PIM_N9_e, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Porcentaje de inmigrantes que residen en Quito, de acuerdo al país de nacimiento.xlsx")
