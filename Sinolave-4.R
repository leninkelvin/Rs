# Script for extracting data from the SINOLAVE database.
# In this instanceI want data extracted by dates in addition to data previously
# obtained. 
# 15 March 2022

# update, without prompts for permission/clarification
update.packages(ask = FALSE)

ptm <- proc.time() # Code to take the starting time of the process

#required libraries, without these noting works. 

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(pastecs) # for statistics extraction

# SUPER IMPORTANTE PARA TENER BIEN LAS FECHAS
Mexico$FECHA_INICIO_CUADRO_CLINICO = as.Date(Mexico$FECHA_INICIO_CUADRO_CLINICO, "%d/%m/%Y") 

#Mexico downloaded on the 3rd of March 2022. 
#Mexico = AllCSV

XXXV = filter(Mexico, TIPO_INFLUENZA_CONF1 %in% c(35)) # Influenza cases, first filter

mofo = filter(Mexico, TIPO_INFLUENZA_CONF1 != 35)
other = filter(mofo, RESULT_PRUEBA_RAPIDA == 1)

#other = filter(Mexico, TIPO_INFLUENZA_CONF1 != 35)

# Now, to separate by ages XXXV IV 

aaA = filter(XXXV, EDAD_ANO <= 19)
aaB = filter(XXXV, EDAD_ANO >= 20 & EDAD_ANO <= 39)
aaC = filter(XXXV, EDAD_ANO >= 40 & EDAD_ANO <= 59)
aaD = filter(XXXV, EDAD_ANO >= 60)

# Now, to separate by ages NOT XXXV IV + POSITIVE

bbA = filter(other, EDAD_ANO <= 19)
bbB = filter(other, EDAD_ANO >= 20 & EDAD_ANO <= 39)
bbC = filter(other, EDAD_ANO >= 40 & EDAD_ANO <= 59)
bbD = filter(other, EDAD_ANO >= 60)

DateI = '2020-01-01' # Starting date for the data extraction
DateF = '2022-03-01' # Final date for data extraction

#Week I

DateN = as.Date(DateI) + 6

aaaA = aaA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaA = filter(aaaA, MOTIVO_EGRESO %in% c(4))
aaaaaA = filter(aaaaA, DOSIS_VAC_COVID19 %in% c(1))
cA = filter(aaaaaA, MARCA_VAC_COVID19 == "CanSino")

aaaB = aaB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaB = filter(aaaB, MOTIVO_EGRESO %in% c(4))
aaaaaB = filter(aaaaB, DOSIS_VAC_COVID19 %in% c(1))
cB = filter(aaaaaB, MARCA_VAC_COVID19 == "CanSino")

aaaC = aaC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaC = filter(aaaC, MOTIVO_EGRESO %in% c(4))
aaaaaC = filter(aaaaC, DOSIS_VAC_COVID19 %in% c(1))
cC = filter(aaaaaC, MARCA_VAC_COVID19 == "CanSino")

aaaD = aaD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaD = filter(aaaD, MOTIVO_EGRESO %in% c(4))
aaaaaD = filter(aaaaD, DOSIS_VAC_COVID19 %in% c(1))
cD = filter(aaaaaD, MARCA_VAC_COVID19 == "CanSino")

bbbA = bbA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbA = filter(bbbA, MOTIVO_EGRESO %in% c(4))
bbbbbA = filter(bbbbA, DOSIS_VAC_COVID19 %in% c(1))
dA = filter(bbbbbA, MARCA_VAC_COVID19 == "CanSino")

bbbB = bbB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbB = filter(bbbB, MOTIVO_EGRESO %in% c(4))
bbbbbB = filter(bbbbB, DOSIS_VAC_COVID19 %in% c(1))
dB = filter(bbbbbB, MARCA_VAC_COVID19 == "CanSino")

bbbC = bbC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbC = filter(bbbC, MOTIVO_EGRESO %in% c(4))
bbbbbC = filter(bbbbC, DOSIS_VAC_COVID19 %in% c(1))
dC = filter(bbbbbC, MARCA_VAC_COVID19 == "CanSino")

bbbD = bbD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbD = filter(bbbD, MOTIVO_EGRESO %in% c(4))
bbbbbD = filter(bbbbD, DOSIS_VAC_COVID19 %in% c(1))
dD = filter(bbbbbD, MARCA_VAC_COVID19 == "CanSino")

SINOLAVE_D_C = data.frame(as.Date(DateI),count(aaaaA), count(aaaaaA), count(cA),count(aaaaB), count(aaaaaB), count(cB),
+ count(aaaaC), count(aaaaaC), count(cC),count(aaaaD), count(aaaaaD), count(cD),
+ count(bbbbA), count(bbbbbA), count(dA),count(bbbbB), count(bbbbbB), count(dB),
+ count(bbbbC), count(bbbbbC), count(dC),count(bbbbD), count(bbbbbD), count(dD))

gc()

while (DateI < DateF) {

#Week II

DateI = as.Date(DateN) + 1
DateN = as.Date(DateI) + 6

aaaA = aaA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaA = filter(aaaA, MOTIVO_EGRESO %in% c(4))
aaaaaA = filter(aaaaA, DOSIS_VAC_COVID19 %in% c(1))
cA = filter(aaaaaA, MARCA_VAC_COVID19 == "CanSino")

aaaB = aaB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaB = filter(aaaB, MOTIVO_EGRESO %in% c(4))
aaaaaB = filter(aaaaB, DOSIS_VAC_COVID19 %in% c(1))
cB = filter(aaaaaB, MARCA_VAC_COVID19 == "CanSino")

aaaC = aaC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaC = filter(aaaC, MOTIVO_EGRESO %in% c(4))
aaaaaC = filter(aaaaC, DOSIS_VAC_COVID19 %in% c(1))
cC = filter(aaaaaC, MARCA_VAC_COVID19 == "CanSino")

aaaD = aaD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaD = filter(aaaD, MOTIVO_EGRESO %in% c(4))
aaaaaD = filter(aaaaD, DOSIS_VAC_COVID19 %in% c(1))
cD = filter(aaaaaD, MARCA_VAC_COVID19 == "CanSino")

bbbA = bbA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbA = filter(bbbA, MOTIVO_EGRESO %in% c(4))
bbbbbA = filter(bbbbA, DOSIS_VAC_COVID19 %in% c(1))
dA = filter(bbbbbA, MARCA_VAC_COVID19 == "CanSino")

bbbB = bbB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbB = filter(bbbB, MOTIVO_EGRESO %in% c(4))
bbbbbB = filter(bbbbB, DOSIS_VAC_COVID19 %in% c(1))
dB = filter(bbbbbB, MARCA_VAC_COVID19 == "CanSino")

bbbC = bbC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbC = filter(bbbC, MOTIVO_EGRESO %in% c(4))
bbbbbC = filter(bbbbC, DOSIS_VAC_COVID19 %in% c(1))
dC = filter(bbbbbC, MARCA_VAC_COVID19 == "CanSino")

bbbD = bbD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbD = filter(bbbD, MOTIVO_EGRESO %in% c(4))
bbbbbD = filter(bbbbD, DOSIS_VAC_COVID19 %in% c(1))
dD = filter(bbbbbD, MARCA_VAC_COVID19 == "CanSino")

SINOLAVE_D_C_temp = data.frame(as.Date(DateI),count(aaaaA), count(aaaaaA), count(cA),count(aaaaB), count(aaaaaB), count(cB),
+ count(aaaaC), count(aaaaaC), count(cC),count(aaaaD), count(aaaaaD), count(cD),
+ count(bbbbA), count(bbbbbA), count(dA),count(bbbbB), count(bbbbbB), count(dB),
+ count(bbbbC), count(bbbbbC), count(dC),count(bbbbD), count(bbbbbD), count(dD))

SINOLAVE_D_C = rbind(SINOLAVE_D_C, SINOLAVE_D_C_temp) # adding observations to dataframe

gc()
}

names(SINOLAVE_D_C)[1] =  "Fecha Inicio" 
names(SINOLAVE_D_C)[2] =  "A 35 Egreso 4" 
names(SINOLAVE_D_C)[3] =  "A 35 Vacuna 1"
names(SINOLAVE_D_C)[4] =  "A 35 CanSino"  
names(SINOLAVE_D_C)[5] =  "B 35 Egreso 4"
names(SINOLAVE_D_C)[6] =  "B 35 Vacuna 1"
names(SINOLAVE_D_C)[7] =  "B 35 CanSino" 
names(SINOLAVE_D_C)[8] =  "C 35 Egreso 4"
names(SINOLAVE_D_C)[9] =  "C 35 Vacuna 1"
names(SINOLAVE_D_C)[10] = "C 35 CanSino"   
names(SINOLAVE_D_C)[11] = "D 35 Egreso 4"
names(SINOLAVE_D_C)[12] = "D 35 Vacuna 1"
names(SINOLAVE_D_C)[13] = "D 35 CanSino" 
names(SINOLAVE_D_C)[14] = "A no35 Egreso 4"
names(SINOLAVE_D_C)[15] = "A no35 Vacuna 1"
names(SINOLAVE_D_C)[16] = "A no35 CanSino" 
names(SINOLAVE_D_C)[17] = "B no35 Egreso 4"
names(SINOLAVE_D_C)[18] = "B no35 Vacuna 1"
names(SINOLAVE_D_C)[19] = "B no35 CanSino" 
names(SINOLAVE_D_C)[20] = "C no35 Egreso 4"
names(SINOLAVE_D_C)[21] = "C no35 Vacuna 1"
names(SINOLAVE_D_C)[22] = "C no35 CanSino" 
names(SINOLAVE_D_C)[23] = "D no35 Egreso 4"
names(SINOLAVE_D_C)[24] = "D no35 Vacuna 1"
names(SINOLAVE_D_C)[25] = "D no35 CanSino" 

write.table(SINOLAVE_D_C, file = "SINOLAVE_D_C.csv", sep = "\t", row.names = F)

proc.time() - ptm # Code to get the final time of the process and display the total time.

gc()

system("say Just finished!")
