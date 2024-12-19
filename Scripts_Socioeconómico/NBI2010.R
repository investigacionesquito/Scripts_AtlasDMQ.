# Dimensión: Socioeconomía

# Nombre del Indicador--------------------------------------------------------
# Pobreza por necesidades básicas insatisfechas 2010

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

# Generación de funciones
modSum<-function(x){
  if(all(is.na(x))){return(NA)}
  sum(x, na.rm = TRUE)}

# Con el objetivo de tener en la base unificada unicamente personas 
# en viviendas particulares ocupadas con personas presentes,se aplica los siguientes filtros:

# VTV = Tipo de vivienda 
# VCO = Condición de ocupación de vivienda particular (si es 1 = ocupada)

VHP2010<-VHP2010[(VTV<=8 & VCO==1), ]

#==============================================================================#
#                                   Pobreza por NBI
#==============================================================================#
#==============================================================================#
# El cálculo de necesidades básicas incluye cinco dimensiones:

# 1. Dependencia económica del hogar
# 2. Niños en edad escolar
# 3. Características físicas de la vivienda
# 4. Disponibilidad de servicios básicos de la vivienda
# 5. Estado de hacinamiento del hogar

#====================================================================#
# 1.- DEPENDENCIA ECONÓMICA DEL HOGAR (comp1)
#====================================================================#
# Se considera deficitario si:
# El representante del hogar tiene 3 o menos años de escolaridad (Escolaridad del representante del hogar); y
# Existen más de 3 personas por cada persona ocupada en el hogar (Dependencia económica).

# Escolaridad del Representante del  hogar
#P23 = Nivel de instrucción más alto al que asiste o asistió 
#P24 = Grado, curso o año más alto que aprobó

VHP2010[,ANIOS:=NULL]
VHP2010[, ANIOS := fcase(
  P23 == 1, 0,  # Ninguno
  P23 == 2, 0 + P24,  # Centro de alfabetización
  P23 == 2 & P24 %in% c(7:10), 99,  # Error o inconsistencias: 99
  P23 == 3, 1,  # Pre-escolar
  P23 == 4 & P24 != 99, 0 + P24,  # Primaria: años según P24 
  P23 == 5 & P24 != 99, 6 + P24,  # Secundario: 6 años de primaria + P24
  P23 == 6 & P24 != 99, 0 + P24,  # Educacion Básica años según P24 
  P23 == 7 & P24 != 99, 10 + P24,  # Educacion Media/Bachillerato: 10 básicos + P24
  P23 == 8 & P24 != 99, 13 + P24,  # Postbachillerato: 13 básicos + P24 
  P23 == 9 & P24 != 99, 13 + P24,  # Superior: 13 básicos + P24  
  P23 == 10 & P24 != 99, 18 + P24,  # Postgrado: 18 básicos y superior + P24
  P23 == 99 | P24 == 99, 99  # Valores faltantes o inconsistentes
)] 

# Dependencia económica

# Identificación de perceptores de ingresos

# P03 = Años cumplidos 
# P27 = Qué hizo la semana pasada

# 1 Trabajó al menos una hora
# 2 No trabajó pero si tiene trabajo
# 3 Al menos una hora fabricó algún producto o brindó algún servicio
# 4 Al menos una hora ayudó en algún negocio o trabajo de un familiar
# 5 Al menos una hora realizó labores agrícolas o cuidó animales
# 6 Es Cesante; Buscó trabajo habiendo trabajado antes y está disponible para trabajar
# 7 No trabajó

VHP2010[, y.temp:=0]
VHP2010[(P03>=10 & P27<7), y.temp:=1]

# Conteo de perceptores de ingresos por hogar

VHP2010[, den.temp:= modSum(y.temp), by = id_hog]

# Relación entre el número de miembros del hogar y el número de perceptores de ingresos
# Si en el hogar existen 0 perceptores de ingresos, se considera deficitario

# TP1 = Total de personas en el hogar
# P02 = Parentesco o relación con el representante del hogar

VHP2010[den.temp>0, z.temp:= TP1/den.temp]

# Resultado dimensión
VHP2010[,w.temp:=NULL]
VHP2010[(ANIOS<=3 & P02==1) & z.temp>3, w.temp:=1]
VHP2010[(ANIOS<=3 & P02==1) & z.temp<=3, w.temp:=0]
VHP2010[(ANIOS>3 & ANIOS<99 & P02==1) & z.temp>3, w.temp:=0]
VHP2010[(ANIOS>3 & ANIOS<99 & P02==1) & z.temp<=3, w.temp:=0]
VHP2010[den.temp==0 & (ANIOS<=3 & P02==1), w.temp:=1]
VHP2010[den.temp==0 & (ANIOS>3 & ANIOS<99 & P02==1), w.temp:=0]
VHP2010[(ANIOS==99 & P02==1), w.temp:=9]

VHP2010[,comp1:=NULL]
VHP2010[,comp1:= modSum(w.temp), by = id_hog] 

# Descomposición por indicadores ==============================#

# 1. Años de escolariodad del representante del hogar 
# El representante del hogar tiene 3 o menos años de escolaridad (Escolaridad del representante del hogar); y

VHP2010[,AER:=NULL]
VHP2010[(ANIOS<=3 & P02==1) ,AER:=1]
VHP2010[(ANIOS>3 & P02==1) ,AER:=0]

# 2. Número de personas por cada persona ocupada en el hogar 
# Existen más de 3 personas por cada persona ocupada en el hogar (Dependencia económica).

VHP2010[,POH:=NULL]
VHP2010[z.temp>3, POH:=1]
VHP2010[z.temp<=3, POH:=0]

#====================================================================#
# 2.- NIÑOS EN EDAD ESCOLAR (comp2)
#====================================================================#
# Se considera deficitario si:
# Existen en el hogar niños de 6 a 12 años que no asisten a clases

# Identificación de niños de 6 a 12 años que no asisten a clases

#P21 = Asiste a un establecimiento de enseñanza regular
# 1 Sí
# 2 No

VHP2010[, v.temp:=0]
VHP2010[P03 %in% c(6:12) & P21==1, v.temp:=0]
VHP2010[P03 %in% c(6:12) & P21==2, v.temp:=1]
VHP2010[P03 %in% c(6:12) & is.na(P21), v.temp:=NA]

# Conteo de niños de 6 a 12 años que no asisten a clases por hogar

VHP2010[, ASIST:=NULL]
VHP2010[, ASIST:= modSum(v.temp), by = id_hog]

# Resultado dimensión

VHP2010[, comp2:=0]
VHP2010[ASIST>0, comp2:= 1]
VHP2010[ASIST==0, comp2:= 0]
VHP2010[is.na(ASIST), comp2:= NA]

#====================================================================#
# 3.- CARACTERÍSTICAS FÍSICAS DE LA VIVIENDA (comp3)
#====================================================================#
# Se considera deficitario si:
# El material del piso de la vivienda es tierra u otros materiales; o
# El material de las paredes exteriores es caña no revestida, u otros materiales.

#V05 = Material predominante del piso

# 6 Tierra
# 7 Otros materiales

#V03 = Material predominante de las paredes exteriores

# 6 Caña no revestida
# 7 Otros materiales

# Resultado dimensión

VHP2010[, comp3:=0]
VHP2010[V05 %in% c(6,7) | V03 %in% c(6,7), comp3:=1]
VHP2010[is.na(V05) & is.na(V03), comp3:=NA]

# Descomposición por indicadores ==============================#

# El material del piso de la vivienda es tierra u otros materiales

vivienda2010[(VTV<=8 & VCO==1), MPV:=0]
vivienda2010[V05 %in% c(6,7), MPV:=1]
vivienda2010[is.na(V05), MPV:=NA]

# El material de las paredes exteriores es caña no revestida, u otros materiales.

vivienda2010[(VTV<=8 & VCO==1), MPEV:=0]
vivienda2010[V03 %in% c(6,7), MPEV:=1]
vivienda2010[is.na(V03), MPEV:=NA]

#====================================================================#
# 4.- DISPONIBILIDAD DE SERVICIOS BÁSICOS DE LA VIVIENDA (comp4)
#====================================================================#

#V08 = El agua que recibe la vivienda es 

# 2 Por tubería fuera de la vivienda pero dentro del edificio, lote o terreno
# 3 Por tubería fuera del edificio, lote o terreno
# 4 No recibe agua por tubería sino por otros medios

#V07 = El agua que recibe la vivienda proviene o es suministrada por 7

# 2 De pozo
# 3 De río, vertiente, acequia o canal
# 4 De carro repartidor
# 5 Otro (Agua lluvia/albarrada)

#V09 = El servicio higiénico de la vivienda es 

# 3 Conectado a pozo ciego
# 4 Con descarga directa al mar, río, lago o quebrada
# 5 Letrina
# 6 No tiene

# Resultado dimensión

VHP2010[(VTV<=8 & VCO==1), comp4:=0]
VHP2010[V07 %in% c(2:5) | V09 %in% c(3:6) | V08 %in% c(2:4), comp4:=1]
VHP2010[is.na(V07) & is.na(V09) & is.na(V08), comp4:=NA] 

# Descomposición por indicadores ==============================#

# La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado 
#conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina

vivienda2010[(VTV<=8 & VCO==1), SH:=0]
vivienda2010[ V09 %in% c(3:6), SH:=1]
vivienda2010[ is.na(V09), SH:=NA] 

# Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# (río, vertiente, acequia, canal, grieta o agua lluvia); o
# La vivienda no obtiene agua por tubería dentro de la vivienda.

vivienda2010[(VTV<=8 & VCO==1), FSA:=0]
vivienda2010[V07 %in% c(2:5) | V08 %in% c(2:4), FSA:=1]
vivienda2010[is.na(V07) & is.na(V08), FSA:=NA] 

#====================================================================#
# 5.- ESTADO DE HACINAMIENTO DEL HOGAR (comp5)
#====================================================================#
# Se considera deficitario si 
# Existen en el hogar en promedio más de tres personas por cuarto utilizado para dormir.
# *Cuando en el hogar no existen dormitorios exclusivos para dormir, se asume que existe uno.

# H01 = Número de dormitorios
# TP1 = Total de personas en el hogar

# Relación personas por dormitorio

VHP2010[H01>0, PERDOR:=TP1/H01]
VHP2010[H01==0, PERDOR:=TP1]
VHP2010[is.na(H01), PERDOR:=NA]

# Resultado dimensión

VHP2010[PERDOR<=3, comp5:=0]
VHP2010[PERDOR>3, comp5:=1]
VHP2010[is.na(PERDOR), comp5:=NA]

#====================================================================#
# CÁLCULO DEL INDICADOR DE POBREZA POR NECESIDADES BÁSICAS INSATISFECHAS - NBI
#====================================================================#

# Un hogar se considera pobre por NBI cuando tiene al menos una NBI y en extremo
# pobre por NBI cuando tiene dos o más NBI.

# Conteo del número de carencias por hogar

VHP2010[comp1<9, knbi:= rowSums(cbind(comp1, comp2, comp3, comp4, comp5), na.rm =T)]
VHP2010[comp1>=9, knbi:= rowSums(cbind(comp2, comp3, comp4, comp5), na.rm = T)]

# Resultado pobreza por NBI

VHP2010[knbi>=1, nbi:=1]
VHP2010[knbi==0 & comp1<9, nbi:=0]

# Resultado pobreza extrema por NBI

VHP2010[knbi>=2, xnbi:=1]
VHP2010[knbi< 2 & comp1<9, xnbi:=0]

# Descomposición NBI por componentes

VHP2010[, compk:=NULL]
VHP2010[knbi==1 & comp1==1, compk:=1] # DEPENDENCIA ECONÓMICA DEL HOGAR 
VHP2010[knbi==1 & comp2==1, compk:=2] # NIÑOS EN EDAD ESCOLAR
VHP2010[knbi==1 & comp3==1, compk:=3] # CARACTERÍSTICAS FÍSICAS DE LA VIVIENDA
VHP2010[knbi==1 & comp4==1, compk:=4] # DISPONIBILIDAD DE SERVICIOS BÁSICOS DE LA VIVIENDA
VHP2010[knbi==1 & comp5==1, compk:=5] # ESTADO DE HACINAMIENTO DEL HOGAR
VHP2010[knbi > 1, compk:=6]           # 
VHP2010[knbi==0 & comp1<9, compk:=0]

# val_lab(VHP2010$compk) = num_lab("
# 0 No Pobre por NBI
# 1 Pobre en comp 1
# 2 Pobre en comp 2
# 3 Pobre en comp 3
# 4 Pobre en comp 4
# 5 Pobre en comp 5
# 6 overlap
# ")

# Resultados

# Cantonal ----------------------------

# Pobreza por NBI Personas
NBIP_c <- VHP2010[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = canton]

# Pobreza por NBI Hogares
NBIH_c <- VHP2010[P02==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = canton]

# Pobreza extrema por NBI Personas
XNBIP_c <- VHP2010[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = canton]

# Pobreza extrema por NBI Hogares
XNBIH_c <- VHP2010[!is.na(xnbi) & P02==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = canton]

# Proxys
# 1. Años de escolariodad del representante del hogar 
AER_c <- VHP2010[P02==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_c <- VHP2010[P02==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 3. Niños en edad escolar por hogar 
NEE_c <- VHP2010[P02 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_c <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_c <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_c <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_c <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 8. Hacinamiento 
HAC_c <- VHP2010[P02==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# Administración zonal  ----------------------------

# Pobreza por NBI Personas
NBIP_az <- VHP2010[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),                                            #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),                                           #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,                              # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100                                 # porcentaje de personas no pobres por NBI
), by = adm_zonal]

# Hogares
NBIH_az <- VHP2010[P02==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),                                             # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),                                          # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,                                       # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100                                   # porcentaje de hogares no pobres por NBI
), by = adm_zonal]

# Pobreza extrema por NBI Personas
XNBIP_az <- VHP2010[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),                                          #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),                                        #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,                           # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100                             # porcentaje de personas no pobres extremo por NBI
), by = adm_zonal]

# Hogares
XNBIH_az <- VHP2010[!is.na(xnbi) & P02==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),                                              #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),                                          #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,                           # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100                          # porcentaje de hogares no pobres extremo por NBI
), by = adm_zonal]

# Proxys

# 1. Años de escolariodad del representante del hogar 

AER_az <- VHP2010[P02==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 2. Número de personas por cada persona ocupada en el hogar 

POH_az <- VHP2010[P02==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 3. Niños en edad escolar por hogar 

NEE_az <- VHP2010[P02 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 4. El material del piso de la vivienda es tierra u otros materiales 

MPV_az <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.

MPEV_az <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado 
# conectado a pozo ciego, inodoro o escusado con descarga directa al mar, 
# río, lago o quebrada o tiene letrina

SH_az <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero 
# repartidor u otras fuentes o La vivienda no obtiene agua por tubería dentro de la vivienda.

FSA_az <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 8. Hacinamiento 

HAC_az <- VHP2010[P02==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# Parroquial  ----------------------------

# Pobreza por NBI Personas
NBIP_pr <- VHP2010[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = parroquia]

# Hogares
NBIH_pr <- VHP2010[P02==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = parroquia]

# Pobreza extrema por NBI Personas
XNBIP_pr <- VHP2010[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = parroquia]

# Hogares
XNBIH_pr <- VHP2010[!is.na(xnbi) & P02==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = parroquia]

# 1. Años de escolariodad del representante del hogar 
AER_pr <- VHP2010[P02==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_pr <- VHP2010[P02==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 3. Niños en edad escolar por hogar 
NEE_pr <- VHP2010[P02 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_pr <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_pr <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_pr <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_pr <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 8. Hacinamiento 
HAC_pr <- VHP2010[P02==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# Sector  ----------------------------

# Pobreza por NBI Personas
NBIP_s <- VHP2010[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = Sector_DMQ]

# Hogares
NBIH_s <- VHP2010[P02==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = Sector_DMQ]

# Pobreza extrema por NBI Personas
XNBIP_s <- VHP2010[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = Sector_DMQ]

# Hogares
XNBIH_s <- VHP2010[!is.na(xnbi) & P02==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = Sector_DMQ]

# 1. Años de escolariodad del representante del hogar 
AER_s <- VHP2010[P02==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_s <- VHP2010[P02==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 3. Niños en edad escolar por hogar 
NEE_s <- VHP2010[P02 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_s <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_s <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_s <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_s <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 8. Hacinamiento 
HAC_s <- VHP2010[P02==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# Grilla COD1000  ----------------------------

# Pobreza por NBI Personas
NBIP_1000 <- VHP2010[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = COD1000]

# Hogares
NBIH_1000 <- VHP2010[P02==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = COD1000]

# Pobreza extrema por NBI Personas
XNBIP_1000 <- VHP2010[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = COD1000]

# Hogares
XNBIH_1000 <- VHP2010[!is.na(xnbi) & P02==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = COD1000]

# 1. Años de escolariodad del representante del hogar 
AER_1000 <- VHP2010[P02==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_1000 <- VHP2010[P02==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 3. Niños en edad escolar por hogar 
NEE_1000 <- VHP2010[P02 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_1000 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_1000 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_1000 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_1000 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 8. Hacinamiento 
HAC_1000 <- VHP2010[P02==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# Grilla COD500  ----------------------------

# Pobreza por NBI Personas
NBIP_500 <- VHP2010[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = COD500]

# Hogares
NBIH_500 <- VHP2010[P02==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = COD500]

# Pobreza extrema por NBI Personas
XNBIP_500 <- VHP2010[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = COD500]

# Hogares
XNBIH_500 <- VHP2010[!is.na(xnbi) & P02==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = COD500]

# 1. Años de escolariodad del representante del hogar 
AER_500 <- VHP2010[P02==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_500 <- VHP2010[P02==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 3. Niños en edad escolar por hogar 
NEE_500 <- VHP2010[P02 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_500 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_500 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_500 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_500 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 8. Hacinamiento 
HAC_500 <- VHP2010[P02==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# Grilla H3_N8  ----------------------------

# Pobreza por NBI Personas
NBIP_N8 <- VHP2010[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = H3_N8]

# Hogares
NBIH_N8 <- VHP2010[P02==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = H3_N8]

# Pobreza extrema por NBI Personas
XNBIP_N8 <- VHP2010[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = H3_N8]

# Hogares
XNBIH_N8 <- VHP2010[!is.na(xnbi) & P02==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = H3_N8]

# 1. Años de escolariodad del representante del hogar 
AER_N8 <- VHP2010[P02==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_N8 <- VHP2010[P02==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 3. Niños en edad escolar por hogar 
NEE_N8 <- VHP2010[P02 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_N8 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_N8 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_N8 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_N8 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 8. Hacinamiento 
HAC_N8 <- VHP2010[P02==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# Grilla H3_N9  ----------------------------

# Pobreza por NBI Personas
NBIP_N9 <- VHP2010[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = H3_N9]

# Hogares
NBIH_N9 <- VHP2010[P02==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = H3_N9]

# Pobreza extrema por NBI Personas
XNBIP_N9 <- VHP2010[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = H3_N9]

# Hogares
XNBIH_N9 <- VHP2010[!is.na(xnbi) & P02==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = H3_N9]

# 1. Años de escolariodad del representante del hogar 
AER_N9 <- VHP2010[P02==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_N9 <- VHP2010[P02==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 3. Niños en edad escolar por hogar 
NEE_N9 <- VHP2010[P02 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_N9 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_N9 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_N9 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_N9 <- vivienda2010[(VTV<=8 & VCO==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 8. Hacinamiento 
HAC_N9 <- VHP2010[P02==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 9. Guardar los resultados -------------

wb <- createWorkbook("NBI")

addWorksheet(wb, "NBIP_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIP_c", x= NBIP_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NBIH_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIH_c", x= NBIH_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIP_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIP_c", x= XNBIP_c , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIH_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIH_c", x= XNBIH_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "AER_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "AER_c", x= AER_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "POH_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "POH_c", x= POH_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEE_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEE_c", x= NEE_c , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPV_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPV_c", x= MPV_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPEV_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPEV_c", x= MPEV_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "SH_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "SH_c", x= SH_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "FSA_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "FSA_c", x= FSA_c , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_c", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HAC_c", x= HAC_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "NBIP_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIP_az", x= NBIP_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NBIH_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIH_az", x= NBIH_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIP_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIP_az", x= XNBIP_az , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIH_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIH_az", x= XNBIH_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "AER_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "AER_az", x= AER_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "POH_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "POH_az", x= POH_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEE_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEE_az", x= NEE_az , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPV_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPV_az", x= MPV_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPEV_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPEV_az", x= MPEV_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "SH_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "SH_az", x= SH_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "FSA_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "FSA_az", x= FSA_az , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_az", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HAC_az", x= HAC_az, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")


addWorksheet(wb, "NBIP_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIP_pr", x= NBIP_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NBIH_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIH_pr", x= NBIH_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIP_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIP_pr", x= XNBIP_pr , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIH_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIH_pr", x= XNBIH_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "AER_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "AER_pr", x= AER_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "POH_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "POH_pr", x= POH_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEE_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEE_pr", x= NEE_pr , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPV_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPV_pr", x= MPV_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPEV_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPEV_pr", x= MPEV_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "SH_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "SH_pr", x= SH_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "FSA_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "FSA_pr", x= FSA_pr , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_pr", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HAC_pr", x= HAC_pr, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "NBIP_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIP_s", x= NBIP_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NBIH_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIH_s", x= NBIH_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIP_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIP_s", x= XNBIP_s , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIH_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIH_s", x= XNBIH_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "AER_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "AER_s", x= AER_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "POH_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "POH_s", x= POH_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEE_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEE_s", x= NEE_s , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPV_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPV_s", x= MPV_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPEV_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPEV_s", x= MPEV_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "SH_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "SH_s", x= SH_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "FSA_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "FSA_s", x= FSA_s , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_s", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HAC_s", x= HAC_s, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "NBIP_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIP_1000", x= NBIP_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NBIH_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIH_1000", x= NBIH_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIP_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIP_1000", x= XNBIP_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIH_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIH_1000", x= XNBIH_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "AER_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "AER_1000", x= AER_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "POH_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "POH_1000", x= POH_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEE_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEE_1000", x= NEE_1000 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPV_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPV_1000", x= MPV_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPEV_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPEV_1000", x= MPEV_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "SH_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "SH_1000", x= SH_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "FSA_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "FSA_1000", x= FSA_1000 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_1000", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HAC_1000", x= HAC_1000, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "NBIP_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIP_500", x= NBIP_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NBIH_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIH_500", x= NBIH_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIP_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIP_500", x= XNBIP_500 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIH_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIH_500", x= XNBIH_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "AER_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "AER_500", x= AER_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "POH_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "POH_500", x= POH_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEE_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEE_500", x= NEE_500 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPV_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPV_500", x= MPV_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPEV_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPEV_500", x= MPEV_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "SH_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "SH_500", x= SH_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "FSA_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "FSA_500", x= FSA_500 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_500", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HAC_500", x= HAC_500, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "NBIP_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIP_N8", x= NBIP_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NBIH_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIH_N8", x= NBIH_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIP_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIP_N8", x= XNBIP_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIH_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIH_N8", x= XNBIH_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "AER_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "AER_N8", x= AER_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "POH_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "POH_N8", x= POH_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEE_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEE_N8", x= NEE_N8 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPV_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPV_N8", x= MPV_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPEV_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPEV_N8", x= MPEV_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "SH_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "SH_N8", x= SH_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "FSA_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "FSA_N8", x= FSA_N8 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_N8", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HAC_N8", x= HAC_N8, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "NBIP_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIP_N9", x= NBIP_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NBIH_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NBIH_N9", x= NBIH_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIP_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIP_N9", x= XNBIP_N9 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "XNBIH_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "XNBIH_N9", x= XNBIH_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

addWorksheet(wb, "AER_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "AER_N9", x= AER_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "POH_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "POH_N9", x= POH_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "NEE_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "NEE_N9", x= NEE_N9 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPV_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPV_N9", x= MPV_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "MPEV_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "MPEV_N9", x= MPEV_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "SH_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "SH_N9", x= SH_N9, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "FSA_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "FSA_N9", x= FSA_N9 , startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")
addWorksheet(wb, "HAC_N9", gridLines = FALSE, tabColour = "white")
writeDataTable(wb, sheet = "HAC_N9", x= HAC_c, startRow = 1,
               rowNames = FALSE,startCol = 1,
               tableStyle = "TableStyleMedium9")

saveWorkbook(wb, "Pobreza por necesidades básicas insatisfechas 2010.xlsx")