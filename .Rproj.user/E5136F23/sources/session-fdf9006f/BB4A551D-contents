# Dimensión: Socioeconomía

# Nombre del Indicador--------------------------------------------------------
# Pobreza por necesidades básicas insatisfechas

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
hogar2022 <- read_sav("CPV_Hogar_2022_Nacional.sav") %>% as.data.table()
poblacion2022 <- read_sav("CPV_Población_2022_Nacional.sav") %>% as.data.table()
vivienda2022 <- read_sav("CPV_Vivienda_2022_Nacional.sav") %>% as.data.table()

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

# 5. Calcular indicadores --------------------------------------------------

# Generación de funciones
modSum<-function(x){
  if(all(is.na(x))){return(NA)}
  sum(x, na.rm = TRUE)}

# Con el objetivo de tener en la base unificada unicamente personas 
# en viviendas particulares ocupadas con personas presentes,se aplica los siguientes filtros:
# VO1 = Tipo de vivienda 
# V0201R = Condición de ocupación de vivienda particular (si es 1 = ocupada)

VHP2022<-VHP2022[(V01<=8 & V0201R==1), ]

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
#P17R = Nivel de instrucción más alto al que asiste o asistió 
#P18R = Grado, curso o año más alto que aprobó

VHP2022[,ANIOS:=NULL]
VHP2022[,ANIOS:=fcase (P17R==1, 0,
                       P17R==2, 0,
                       P17R==3, 0,
                       P17R==4 & P18R==0, 0,
                       P17R==4 & P18R==1, 0,
                       P17R==4 & P18R==2, 3, #Alfabetización Módulo 1y2 2do–3ro EGB
                       P17R==4 & P18R==3, 3,
                       P17R==4 & P18R==4, 5, #Alfabetización Módulos 3y4, 4to–5to EGB
                       P17R==4 & P18R==5, 5,
                       P17R==4 & P18R==6, 7, #Alfabetización Módulos 5y6, 6to–7mo EGB
                       P17R==4 & P18R %in% c(7:10),99,
                       P17R==5 & P18R!=99,0+P18R,
                       P17R==6 & P18R!=99,10+P18R,
                       P17R==7 & P18R!=99,13+P18R,
                       P17R==8 & P18R!=99,13+P18R,
                       P17R==9 & P18R!=99,13+P18R,
                       P17R==10 & P18R!=99,18+P18R,
                       P17R==11 & P18R!=99,20+P18R,
                       P17R==99 | P18R==99,99)] 

# Dependencia económica

# Identificación de perceptores de ingresos

# P03 = Años cumplidos 
# P22 = La semana pasada
# 1 Trabajó al menos una hora para generar un ingreso
# 2 Realizó algún trabajo ocasional (cachuelo o chaucha) por un pago
# 3 Atendió un negocio propio
# 4 Ayudó en algún negocio o empleo de algún miembro de su hogar
# 5 No trabajó, pero SÍ tiene un trabajo al que seguro va a volver (por vacaciones, enfermedad, etc.)
# 6 Hizo o ayudó en labores agrícolas, cría de animales o pesca
# 7 No trabajó

VHP2022[, y.temp:=0]
VHP2022[(P03>=10 & P22<7), y.temp:=1]

# Conteo de perceptores de ingresos por hogar

VHP2022[, den.temp:= modSum(y.temp), by = ID_HOG]

# Relación entre el número de miembros del hogar y el número de perceptores de ingresos
# Si en el hogar existen 0 perceptores de ingresos, se considera deficitario

# H1303 = Total de personas en el hogar
# P01 = Parentesco o relación con el representante del hogar

VHP2022[den.temp>0, z.temp:= H1303/den.temp]

# Resultado dimensión

VHP2022[,w.temp:=NULL]
VHP2022[(ANIOS<=3 & P01==1) & z.temp>3, w.temp:=1]
VHP2022[(ANIOS<=3 & P01==1) & z.temp<=3, w.temp:=0]
VHP2022[(ANIOS>3 & ANIOS<99 & P01==1) & z.temp>3, w.temp:=0]
VHP2022[(ANIOS>3 & ANIOS<99 & P01==1) & z.temp<=3, w.temp:=0]
VHP2022[den.temp==0 & (ANIOS<=3 & P01==1), w.temp:=1]
VHP2022[den.temp==0 & (ANIOS>3 & ANIOS<99 & P01==1), w.temp:=0]
VHP2022[(ANIOS==99 & P01==1), w.temp:=9]

VHP2022[,comp1:=NULL]
VHP2022[,comp1:= modSum(w.temp), by = ID_HOG] 

# Descomposición por indicadores ==============================#

# 1. Años de escolariodad del representante del hogar 
# El representante del hogar tiene 3 o menos años de escolaridad (Escolaridad del representante del hogar); y

VHP2022[,AER:=NULL]
VHP2022[(ANIOS<=3 & P01==1) ,AER:=1]
VHP2022[(ANIOS>3 & P01==1) ,AER:=0]

# 2. Número de personas por cada persona ocupada en el hogar 
# Existen más de 3 personas por cada persona ocupada en el hogar (Dependencia económica).

VHP2022[,POH:=NULL]
VHP2022[z.temp>3, POH:=1]
VHP2022[z.temp<=3, POH:=0]

#====================================================================#
# 2.- NIÑOS EN EDAD ESCOLAR (comp2)
#====================================================================#
# Se considera deficitario si:
# Existen en el hogar niños de 6 a 12 años que no asisten a clases

# Identificación de niños de 6 a 12 años que no asisten a clases

#P15=Asiste actualmente a educación regular o formal
# 1 Sí
# 2 No

VHP2022[, v.temp:=0]
VHP2022[P03 %in% c(6:12) & P15==1, v.temp:=0]
VHP2022[P03 %in% c(6:12) & P15==2, v.temp:=1]
VHP2022[P03 %in% c(6:12) & is.na(P15), v.temp:=NA]

# Conteo de niños de 6 a 12 años que no asisten a clases por hogar

VHP2022[, ASIST:=NULL]
VHP2022[, ASIST:= modSum(v.temp), by = ID_HOG]

# Resultado dimensión

VHP2022[, comp2:=0]
VHP2022[ASIST>0, comp2:= 1]
VHP2022[ASIST==0, comp2:= 0]
VHP2022[is.na(ASIST), comp2:= NA]

#====================================================================#
# 3.- CARACTERÍSTICAS FÍSICAS DE LA VIVIENDA (comp3)
#====================================================================#
# Se considera deficitario si:
# El material del piso de la vivienda es tierra u otros materiales; o
# El material de las paredes exteriores es caña no revestida, u otros materiales.

#V07 = Material predominante del piso
#7 Tierra
#8 Otro material

#V05 = Material predominante de las paredes exteriores
#7 Caña no revestida
#8 Otro material

# Resultado dimensión

VHP2022[, comp3:=0]
VHP2022[V07 %in% c(7,8) | V05 %in% c(7,8), comp3:=1]
VHP2022[is.na(V07) & is.na(V05), comp3:=NA]

# Descomposición por indicadores ==============================#

# El material del piso de la vivienda es tierra u otros materiales

vivienda2022[(V01<=8 & V0201R==1), MPV:=0]
vivienda2022[V07 %in% c(7,8), MPV:=1]
vivienda2022[is.na(V07), MPV:=NA]

# El material de las paredes exteriores es caña no revestida, u otros materiales.

vivienda2022[(V01<=8 & V0201R==1), MPEV:=0]
vivienda2022[V05 %in% c(7,8), MPEV:=1]
vivienda2022[is.na(V05), MPEV:=NA]

#====================================================================#
# 4.- DISPONIBILIDAD DE SERVICIOS BÁSICOS DE LA VIVIENDA (comp4)
#====================================================================#

#V09 = El agua que recibe la vivienda es
#V10 = El agua que recibe la vivienda proviene o es suministrada por
#V11 = El servicio higiénico de la vivienda es

# Resultado dimensión

VHP2022[(V01<=8 & V0201R==1), comp4:=0]
VHP2022[V10 %in% c(3:5) | V11 %in% c(4:7) | V09 %in% c(2:4), comp4:=1]
VHP2022[is.na(V10) & is.na(V11) & is.na(V09), comp4:=NA] 

# Descomposición por indicadores ==============================#

# La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado 
#conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina

vivienda2022[(V01<=8 & V0201R==1), SH:=0]
vivienda2022[ V11 %in% c(4:7), SH:=1]
vivienda2022[ is.na(V11), SH:=NA] 

# Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# (río, vertiente, acequia, canal, grieta o agua lluvia); o
# La vivienda no obtiene agua por tubería dentro de la vivienda.

vivienda2022[(V01<=8 & V0201R==1), FSA:=0]
vivienda2022[V10 %in% c(3:5) | V09 %in% c(2:4), FSA:=1]
vivienda2022[is.na(V10) & is.na(V09), FSA:=NA] 

#====================================================================#
# 5.- ESTADO DE HACINAMIENTO DEL HOGAR (comp5)
#====================================================================#
# Se considera deficitario si
# Existen en el hogar en promedio más de tres personas por cuarto utilizado para dormir.
# *Cuando en el hogar no existen dormitorios exclusivos para dormir, se asume que existe uno.

# H01 = Número de dormitorios
# H1303 = Total de personas en el hogar

# Relación personas por dormitorio

VHP2022[H01>0, PERDOR:=H1303/H01]
VHP2022[H01==0, PERDOR:=H1303]
VHP2022[is.na(H01), PERDOR:=NA]

# Resultado dimensión

VHP2022[PERDOR<=3, comp5:=0]
VHP2022[PERDOR>3, comp5:=1]
VHP2022[is.na(PERDOR), comp5:=NA]

#====================================================================#
# CÁLCULO DEL INDICADOR DE POBREZA POR NECESIDADES BÁSICAS INSATISFECHAS - NBI
#====================================================================#

# Un hogar se considera pobre por NBI cuando tiene al menos una NBI y en extremo
# pobre por NBI cuando tiene dos o más NBI.

# Conteo del número de carencias por hogar

VHP2022[comp1<9, knbi:= rowSums(cbind(comp1, comp2, comp3, comp4, comp5), na.rm =T)]
VHP2022[comp1>=9, knbi:= rowSums(cbind(comp2, comp3, comp4, comp5), na.rm = T)]

# Resultado pobreza por NBI

VHP2022[knbi>=1, nbi:=1]
VHP2022[knbi==0 & comp1<9, nbi:=0]

# Resultado pobreza extrema por NBI

VHP2022[knbi>=2, xnbi:=1]
VHP2022[knbi< 2 & comp1<9, xnbi:=0]

# Descomposición NBI por componentes

VHP2022[, compk:=NULL]
VHP2022[knbi==1 & comp1==1, compk:=1] # DEPENDENCIA ECONÓMICA DEL HOGAR 
VHP2022[knbi==1 & comp2==1, compk:=2] # NIÑOS EN EDAD ESCOLAR
VHP2022[knbi==1 & comp3==1, compk:=3] # CARACTERÍSTICAS FÍSICAS DE LA VIVIENDA
VHP2022[knbi==1 & comp4==1, compk:=4] # DISPONIBILIDAD DE SERVICIOS BÁSICOS DE LA VIVIENDA
VHP2022[knbi==1 & comp5==1, compk:=5] # ESTADO DE HACINAMIENTO DEL HOGAR
VHP2022[knbi > 1, compk:=6]           # 
VHP2022[knbi==0 & comp1<9, compk:=0]

# val_lab(VHP2022$compk) = num_lab(" 
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
NBIP_c <- VHP2022[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = canton]

# Pobreza por NBI Hogares
NBIH_c <- VHP2022[P01==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = canton]

# Pobreza extrema por NBI Personas
XNBIP_c <- VHP2022[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = canton]

# Pobreza extrema por NBI Hogares
XNBIH_c <- VHP2022[!is.na(xnbi) & P01==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = canton]

# Proxys
# 1. Años de escolariodad del representante del hogar 
AER_c <- VHP2022[P01==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_c <- VHP2022[P01==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 3. Niños en edad escolar por hogar 
NEE_c <- VHP2022[P01 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_c <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_c <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_c <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = canton]

SH_nac <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
)]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_c <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = canton]

FSA_nac <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
)]

# 8. Hacinamiento 
HAC_c <- VHP2022[P01==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = canton]

# Administración zonal  ----------------------------

# Pobreza por NBI Personas
NBIP_az <- VHP2022[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),                                            #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),                                           #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,                              # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100                                 # porcentaje de personas no pobres por NBI
), by = adm_zonal]

# Hogares
NBIH_az <- VHP2022[P01==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),                                             # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),                                          # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,                                       # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100                                   # porcentaje de hogares no pobres por NBI
), by = adm_zonal]

# Pobreza extrema por NBI Personas
XNBIP_az <- VHP2022[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),                                          #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),                                        #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,                           # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100                             # porcentaje de personas no pobres extremo por NBI
), by = adm_zonal]

# Hogares
XNBIH_az <- VHP2022[!is.na(xnbi) & P01==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),                                              #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),                                          #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,                           # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100                          # porcentaje de hogares no pobres extremo por NBI
), by = adm_zonal]

# Proxys

# 1. Años de escolariodad del representante del hogar 

AER_az <- VHP2022[P01==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 2. Número de personas por cada persona ocupada en el hogar 

POH_az <- VHP2022[P01==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 3. Niños en edad escolar por hogar 

NEE_az <- VHP2022[P01 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 4. El material del piso de la vivienda es tierra u otros materiales 

MPV_az <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.

MPEV_az <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado 
# conectado a pozo ciego, inodoro o escusado con descarga directa al mar, 
# río, lago o quebrada o tiene letrina

SH_az <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero 
# repartidor u otras fuentes o La vivienda no obtiene agua por tubería dentro de la vivienda.

FSA_az <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# 8. Hacinamiento 

HAC_az <- VHP2022[P01==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = adm_zonal]

# Parroquial  ----------------------------

# Pobreza por NBI Personas
NBIP_pr <- VHP2022[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = parroquia]

# Hogares
NBIH_pr <- VHP2022[P01==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = parroquia]

# Pobreza extrema por NBI Personas
XNBIP_pr <- VHP2022[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = parroquia]

# Hogares
XNBIH_pr <- VHP2022[!is.na(xnbi) & P01==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = parroquia]

# 1. Años de escolariodad del representante del hogar 
AER_pr <- VHP2022[P01==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_pr <- VHP2022[P01==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 3. Niños en edad escolar por hogar 
NEE_pr <- VHP2022[P01 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_pr <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_pr <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_pr <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_pr <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# 8. Hacinamiento 
HAC_pr <- VHP2022[P01==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = parroquia]

# Sector  ----------------------------

# Pobreza por NBI Personas
NBIP_s <- VHP2022[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = Sector_DMQ]

# Hogares
NBIH_s <- VHP2022[P01==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = Sector_DMQ]

# Pobreza extrema por NBI Personas
XNBIP_s <- VHP2022[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = Sector_DMQ]

# Hogares
XNBIH_s <- VHP2022[!is.na(xnbi) & P01==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = Sector_DMQ]

# 1. Años de escolariodad del representante del hogar 
AER_s <- VHP2022[P01==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_s <- VHP2022[P01==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 3. Niños en edad escolar por hogar 
NEE_s <- VHP2022[P01 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_s <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_s <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_s <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_s <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# 8. Hacinamiento 
HAC_s <- VHP2022[P01==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = Sector_DMQ]

# Grilla COD1000  ----------------------------

# Pobreza por NBI Personas
NBIP_1000 <- VHP2022[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = COD1000]

# Hogares
NBIH_1000 <- VHP2022[P01==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = COD1000]

# Pobreza extrema por NBI Personas
XNBIP_1000 <- VHP2022[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = COD1000]

# Hogares
XNBIH_1000 <- VHP2022[!is.na(xnbi) & P01==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = COD1000]

# 1. Años de escolariodad del representante del hogar 
AER_1000 <- VHP2022[P01==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_1000 <- VHP2022[P01==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 3. Niños en edad escolar por hogar 
NEE_1000 <- VHP2022[P01 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_1000 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_1000 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_1000 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_1000 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# 8. Hacinamiento 
HAC_1000 <- VHP2022[P01==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = COD1000]

# Grilla COD500  ----------------------------

# Pobreza por NBI Personas
NBIP_500 <- VHP2022[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = COD500]

# Hogares
NBIH_500 <- VHP2022[P01==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = COD500]

# Pobreza extrema por NBI Personas
XNBIP_500 <- VHP2022[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = COD500]

# Hogares
XNBIH_500 <- VHP2022[!is.na(xnbi) & P01==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = COD500]

# 1. Años de escolariodad del representante del hogar 
AER_500 <- VHP2022[P01==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_500 <- VHP2022[P01==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 3. Niños en edad escolar por hogar 
NEE_500 <- VHP2022[P01 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_500 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_500 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_500 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_500 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# 8. Hacinamiento 
HAC_500 <- VHP2022[P01==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = COD500]

# Grilla H3_N8  ----------------------------

# Pobreza por NBI Personas
NBIP_N8 <- VHP2022[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = H3_N8]

# Hogares
NBIH_N8 <- VHP2022[P01==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = H3_N8]

# Pobreza extrema por NBI Personas
XNBIP_N8 <- VHP2022[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = H3_N8]

# Hogares
XNBIH_N8 <- VHP2022[!is.na(xnbi) & P01==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = H3_N8]

# 1. Años de escolariodad del representante del hogar 
AER_N8 <- VHP2022[P01==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_N8 <- VHP2022[P01==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 3. Niños en edad escolar por hogar 
NEE_N8 <- VHP2022[P01 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_N8 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_N8 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_N8 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_N8 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# 8. Hacinamiento 
HAC_N8 <- VHP2022[P01==1 & !is.na(comp5), .(
  HAC = sum(comp5 == 1, na.rm = TRUE),  
  no_HAC = sum(comp5 == 0, na.rm = TRUE),  
  pc_HAC = (sum(comp5 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nHAC = (sum(comp5 == 0, na.rm = TRUE) / .N) * 100
), by = H3_N8]

# Grilla H3_N9  ----------------------------

# Pobreza por NBI Personas
NBIP_N9 <- VHP2022[!is.na(nbi), .(
  p_nbi = sum(nbi == 1, na.rm = TRUE),  #  Pobre por NBI
  nop_nbi = sum(nbi == 0, na.rm = TRUE),  #  No pobre por NBI
  pc_pnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres por NBI
  pc_npnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres por NBI
), by = H3_N9]

# Hogares
NBIH_N9 <- VHP2022[P01==1 & !is.na(nbi), .(
  h_nbi = sum(nbi == 1, na.rm = TRUE),  # Total de hogares pobres por NBI
  noh_nbi = sum(nbi == 0, na.rm = TRUE),  # Total de hogares no pobres por NBI
  pc_hnbi = (sum(nbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres por NBI
  pc_nhnbi = (sum(nbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres por NBI
), by = H3_N9]

# Pobreza extrema por NBI Personas
XNBIP_N9 <- VHP2022[!is.na(xnbi), .(
  p_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Pobre extreo por NBI
  nop_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  No pobre extremo por NBI
  pc_pxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de personas pobres extremo por NBI
  pc_npxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de personas no pobres extremo por NBI
), by = H3_N9]

# Hogares
XNBIH_N9 <- VHP2022[!is.na(xnbi) & P01==1, .(
  h_xnbi = sum(xnbi == 1, na.rm = TRUE),  #  Total de hogares pobres extremo por NBI
  noh_xnbi = sum(xnbi == 0, na.rm = TRUE),  #  Total de hogares no pobres extremo por NBI
  pc_hxnbi = (sum(xnbi == 1, na.rm = TRUE) / .N) * 100,  # porcentaje de hogares pobres extremo por NBI
  pc_nhxnbi = (sum(xnbi == 0, na.rm = TRUE) / .N) * 100  # porcentaje de hogares no pobres extremo por NBI
), by = H3_N9]

# 1. Años de escolariodad del representante del hogar 
AER_N9 <- VHP2022[P01==1 &!is.na(AER), .(
  AER = sum(AER == 1, na.rm = TRUE),  
  no_AER = sum(AER == 0, na.rm = TRUE),  
  pc_AER = (sum(AER == 1, na.rm = TRUE) / .N) * 100,  
  pc_nAER = (sum(AER == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 2. Número de personas por cada persona ocupada en el hogar 
POH_N9 <- VHP2022[P01==1 & !is.na(POH), .(
  POH = sum(POH == 1, na.rm = TRUE),  
  no_POH = sum(POH == 0, na.rm = TRUE),  
  pc_POH = (sum(POH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nPOH = (sum(POH == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 3. Niños en edad escolar por hogar 
NEE_N9 <- VHP2022[P01 ==1 & !is.na(comp2), .(
  NEE = sum(comp2 == 1, na.rm = TRUE),  
  no_NEE = sum(comp2 == 0, na.rm = TRUE),  
  pc_NEE = (sum(comp2 == 1, na.rm = TRUE) / .N) * 100,  
  pc_nNEE = (sum(comp2 == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 4. El material del piso de la vivienda es tierra u otros materiales 
MPV_N9 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPV), .(
  MPV = sum(MPV == 1, na.rm = TRUE),  
  no_MPV = sum(MPV == 0, na.rm = TRUE),  
  pc_MPV = (sum(MPV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPV = (sum(MPV == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 5. El material de las paredes exteriores es caña no revestida, u otros materiales.
MPEV_N9 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(MPEV), .(
  MPEV = sum(MPEV == 1, na.rm = TRUE),  
  no_MPEV = sum(MPEV == 0, na.rm = TRUE),  
  pc_MPEV = (sum(MPEV == 1, na.rm = TRUE) / .N) * 100,  
  pc_nMPEV = (sum(MPEV == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 6. La vivienda no tiene servicio higiénico o cuenta con inodoro o escusado conectado a pozo ciego, inodoro o escusado con descarga directa al mar, río, lago o quebrada o tiene letrina
SH_N9 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(SH), .(
  SH = sum(SH == 1, na.rm = TRUE),  
  no_SH = sum(SH == 0, na.rm = TRUE),  
  pc_SH = (sum(SH == 1, na.rm = TRUE) / .N) * 100,  
  pc_nSH = (sum(SH == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 7. Si el agua que obtiene la vivienda proviene de pozo, carro o tanquero repartidor u otras fuentes 
# O La vivienda no obtiene agua por tubería dentro de la vivienda.
FSA_N9 <- vivienda2022[(V01<=8 & V0201R==1) & !is.na(FSA), .(
  FSA = sum(FSA == 1, na.rm = TRUE),  
  no_FSA = sum(FSA == 0, na.rm = TRUE),  
  pc_FSA = (sum(FSA == 1, na.rm = TRUE) / .N) * 100,  
  pc_nFSA = (sum(FSA == 0, na.rm = TRUE) / .N) * 100
), by = H3_N9]

# 8. Hacinamiento 
HAC_N9 <- VHP2022[P01==1 & !is.na(comp5), .(
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

saveWorkbook(wb, "Pobreza por necesidades básicas insatisfechas.xlsx")