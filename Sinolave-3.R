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

XXXV = filter(Mexico, TIPO_INFLUENZA_CONF1 %in% c(35)) # Influenza cases, first filter

other = filter(Mexico, TIPO_INFLUENZA_CONF1 != 35)

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
aaaaA = filter(aaaA, PACIENTE == "AMBULATORIO")
aaaaaA = filter(aaaA, PACIENTE == "HOSPITALIZADO")
aaaaaaA = filter(aaaaA, DOSIS_VAC_COVID19 %in% c(0))
aaaaaaaA = filter(aaaaaA, DOSIS_VAC_COVID19 %in% c(0))

aaaaaaaaA = filter(aaaaA, DOSIS_VAC_COVID19 %in% c(1))
aaaaaaaaaA = filter(aaaaaA, DOSIS_VAC_COVID19 %in% c(1))

aaaaaaaaaaA = filter(aaaaA, DOSIS_VAC_COVID19 %in% c(2))
aaaaaaaaaaaA = filter(aaaaaA, DOSIS_VAC_COVID19 %in% c(2))

aaaB = aaB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaB = filter(aaaB, PACIENTE == "AMBULATORIO")
aaaaaB = filter(aaaB, PACIENTE == "HOSPITALIZADO")
aaaaaaB = filter(aaaaB, DOSIS_VAC_COVID19 %in% c(0))
aaaaaaaB = filter(aaaaaB, DOSIS_VAC_COVID19 %in% c(0))

aaaaaaaaB = filter(aaaaB, DOSIS_VAC_COVID19 %in% c(1))
aaaaaaaaaB = filter(aaaaaB, DOSIS_VAC_COVID19 %in% c(1))

aaaaaaaaaaB = filter(aaaaB, DOSIS_VAC_COVID19 %in% c(2))
aaaaaaaaaaaB = filter(aaaaaB, DOSIS_VAC_COVID19 %in% c(2))

aaaC = aaC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaC = filter(aaaC, PACIENTE == "AMBULATORIO")
aaaaaC = filter(aaaC, PACIENTE == "HOSPITALIZADO")
aaaaaaC = filter(aaaaC, DOSIS_VAC_COVID19 %in% c(0))
aaaaaaaC = filter(aaaaaC, DOSIS_VAC_COVID19 %in% c(0))

aaaaaaaaC = filter(aaaaC, DOSIS_VAC_COVID19 %in% c(1))
aaaaaaaaaC = filter(aaaaaC, DOSIS_VAC_COVID19 %in% c(1))

aaaaaaaaaaC = filter(aaaaC, DOSIS_VAC_COVID19 %in% c(2))
aaaaaaaaaaaC = filter(aaaaaC, DOSIS_VAC_COVID19 %in% c(2))

aaaD = aaD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaD = filter(aaaD, PACIENTE == "AMBULATORIO")
aaaaaD = filter(aaaD, PACIENTE == "HOSPITALIZADO")
aaaaaaD = filter(aaaaD, DOSIS_VAC_COVID19 %in% c(0))
aaaaaaaD = filter(aaaaaD, DOSIS_VAC_COVID19 %in% c(0))

aaaaaaaaD = filter(aaaaD, DOSIS_VAC_COVID19 %in% c(1))
aaaaaaaaaD = filter(aaaaaD, DOSIS_VAC_COVID19 %in% c(1))

aaaaaaaaaaD = filter(aaaaD, DOSIS_VAC_COVID19 %in% c(2))
aaaaaaaaaaaD = filter(aaaaaD, DOSIS_VAC_COVID19 %in% c(2))

XXXV_PACIENTES = data.frame(count(aaaaaaA), count(aaaaaaaA), count(aaaaaaaaA), count(aaaaaaaaaA), count(aaaaaaaaaaA), count(aaaaaaaaaaaA),
+ count(aaaaaaB), count(aaaaaaaB), count(aaaaaaaaB), count(aaaaaaaaaB), count(aaaaaaaaaaB), count(aaaaaaaaaaaB), 
+ count(aaaaaaC), count(aaaaaaaC), count(aaaaaaaaC), count(aaaaaaaaaC), count(aaaaaaaaaaC), count(aaaaaaaaaaaC),
+ count(aaaaaaD), count(aaaaaaaD), count(aaaaaaaaD), count(aaaaaaaaaD), count(aaaaaaaaaaD), count(aaaaaaaaaaaD))

bbbA = bbA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbA = filter(bbbA, PACIENTE == "AMBULATORIO")
bbbbbA = filter(bbbA, PACIENTE == "HOSPITALIZADO")
bbbbbbA = filter(bbbbA, DOSIS_VAC_COVID19 %in% c(0))
bbbbbbbA = filter(bbbbbA, DOSIS_VAC_COVID19 %in% c(0))

bbbbbbbbA = filter(bbbbA, DOSIS_VAC_COVID19 %in% c(1))
bbbbbbbbbA = filter(bbbbbA, DOSIS_VAC_COVID19 %in% c(1))

bbbbbbbbbbA = filter(bbbbA, DOSIS_VAC_COVID19 %in% c(2))
bbbbbbbbbbbA = filter(bbbbbA, DOSIS_VAC_COVID19 %in% c(2))

bbbB = bbB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbB = filter(bbbB, PACIENTE == "AMBULATORIO")
bbbbbB = filter(bbbB, PACIENTE == "HOSPITALIZADO")
bbbbbbB = filter(bbbbB, DOSIS_VAC_COVID19 %in% c(0))
bbbbbbbB = filter(bbbbbB, DOSIS_VAC_COVID19 %in% c(0))

bbbbbbbbB = filter(bbbbB, DOSIS_VAC_COVID19 %in% c(1))
bbbbbbbbbB = filter(bbbbbB, DOSIS_VAC_COVID19 %in% c(1))

bbbbbbbbbbB = filter(bbbbB, DOSIS_VAC_COVID19 %in% c(2))
bbbbbbbbbbbB = filter(bbbbbB, DOSIS_VAC_COVID19 %in% c(2))

bbbC = bbC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbC = filter(bbbC, PACIENTE == "AMBULATORIO")
bbbbbC = filter(bbbC, PACIENTE == "HOSPITALIZADO")
bbbbbbC = filter(bbbbC, DOSIS_VAC_COVID19 %in% c(0))
bbbbbbbC = filter(bbbbbC, DOSIS_VAC_COVID19 %in% c(0))

bbbbbbbbC = filter(bbbbC, DOSIS_VAC_COVID19 %in% c(1))
bbbbbbbbbC = filter(bbbbbC, DOSIS_VAC_COVID19 %in% c(1))

bbbbbbbbbbC = filter(bbbbC, DOSIS_VAC_COVID19 %in% c(2))
bbbbbbbbbbbC = filter(bbbbbC, DOSIS_VAC_COVID19 %in% c(2))

bbbD = bbD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbD = filter(bbbD, PACIENTE == "AMBULATORIO")
bbbbbD = filter(bbbD, PACIENTE == "HOSPITALIZADO")
bbbbbbD = filter(bbbbD, DOSIS_VAC_COVID19 %in% c(0))
bbbbbbbD = filter(bbbbbD, DOSIS_VAC_COVID19 %in% c(0))

bbbbbbbbD = filter(bbbbD, DOSIS_VAC_COVID19 %in% c(1))
bbbbbbbbbD = filter(bbbbbD, DOSIS_VAC_COVID19 %in% c(1))

bbbbbbbbbbD = filter(bbbbD, DOSIS_VAC_COVID19 %in% c(2))
bbbbbbbbbbbD = filter(bbbbbD, DOSIS_VAC_COVID19 %in% c(2))

NO_XXXV_PACIENTES = data.frame(count(bbbbbbA), count(bbbbbbbA), count(bbbbbbbbA), count(bbbbbbbbbA), count(bbbbbbbbbbA), count(bbbbbbbbbbbA),
                               + count(bbbbbbB), count(bbbbbbbB), count(bbbbbbbbB), count(bbbbbbbbbB), count(bbbbbbbbbbB), count(bbbbbbbbbbbB), 
                               + count(bbbbbbC), count(bbbbbbbC), count(bbbbbbbbC), count(bbbbbbbbbC), count(bbbbbbbbbbC), count(bbbbbbbbbbbC),
                               + count(bbbbbbD), count(bbbbbbbD), count(bbbbbbbbD), count(bbbbbbbbbD), count(bbbbbbbbbbD), count(bbbbbbbbbbbD))

while (DateI < DateF) {

#Week II

DateI = as.Date(DateN) + 1
DateN = as.Date(DateI) + 6

aaaA = aaA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaA = filter(aaaA, PACIENTE == "AMBULATORIO")
aaaaaA = filter(aaaA, PACIENTE == "HOSPITALIZADO")
aaaaaaA = filter(aaaaA, DOSIS_VAC_COVID19 %in% c(0))
aaaaaaaA = filter(aaaaaA, DOSIS_VAC_COVID19 %in% c(0))

aaaaaaaaA = filter(aaaaA, DOSIS_VAC_COVID19 %in% c(1))
aaaaaaaaaA = filter(aaaaaA, DOSIS_VAC_COVID19 %in% c(1))

aaaaaaaaaaA = filter(aaaaA, DOSIS_VAC_COVID19 %in% c(2))
aaaaaaaaaaaA = filter(aaaaaA, DOSIS_VAC_COVID19 %in% c(2))

aaaB = aaB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaB = filter(aaaB, PACIENTE == "AMBULATORIO")
aaaaaB = filter(aaaB, PACIENTE == "HOSPITALIZADO")
aaaaaaB = filter(aaaaB, DOSIS_VAC_COVID19 %in% c(0))
aaaaaaaB = filter(aaaaaB, DOSIS_VAC_COVID19 %in% c(0))

aaaaaaaaB = filter(aaaaB, DOSIS_VAC_COVID19 %in% c(1))
aaaaaaaaaB = filter(aaaaaB, DOSIS_VAC_COVID19 %in% c(1))

aaaaaaaaaaB = filter(aaaaB, DOSIS_VAC_COVID19 %in% c(2))
aaaaaaaaaaaB = filter(aaaaaB, DOSIS_VAC_COVID19 %in% c(2))

aaaC = aaC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaC = filter(aaaC, PACIENTE == "AMBULATORIO")
aaaaaC = filter(aaaC, PACIENTE == "HOSPITALIZADO")
aaaaaaC = filter(aaaaC, DOSIS_VAC_COVID19 %in% c(0))
aaaaaaaC = filter(aaaaaC, DOSIS_VAC_COVID19 %in% c(0))

aaaaaaaaC = filter(aaaaC, DOSIS_VAC_COVID19 %in% c(1))
aaaaaaaaaC = filter(aaaaaC, DOSIS_VAC_COVID19 %in% c(1))

aaaaaaaaaaC = filter(aaaaC, DOSIS_VAC_COVID19 %in% c(2))
aaaaaaaaaaaC = filter(aaaaaC, DOSIS_VAC_COVID19 %in% c(2))

aaaD = aaD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaaD = filter(aaaD, PACIENTE == "AMBULATORIO")
aaaaaD = filter(aaaD, PACIENTE == "HOSPITALIZADO")
aaaaaaD = filter(aaaaD, DOSIS_VAC_COVID19 %in% c(0))
aaaaaaaD = filter(aaaaaD, DOSIS_VAC_COVID19 %in% c(0))

aaaaaaaaD = filter(aaaaD, DOSIS_VAC_COVID19 %in% c(1))
aaaaaaaaaD = filter(aaaaaD, DOSIS_VAC_COVID19 %in% c(1))

aaaaaaaaaaD = filter(aaaaD, DOSIS_VAC_COVID19 %in% c(2))
aaaaaaaaaaaD = filter(aaaaaD, DOSIS_VAC_COVID19 %in% c(2))

XXXV_PACIENTES_temp = data.frame(count(aaaaaaA), count(aaaaaaaA), count(aaaaaaaaA), count(aaaaaaaaaA), count(aaaaaaaaaaA), count(aaaaaaaaaaaA),
                            + count(aaaaaaB), count(aaaaaaaB), count(aaaaaaaaB), count(aaaaaaaaaB), count(aaaaaaaaaaB), count(aaaaaaaaaaaB), 
                            + count(aaaaaaC), count(aaaaaaaC), count(aaaaaaaaC), count(aaaaaaaaaC), count(aaaaaaaaaaC), count(aaaaaaaaaaaC),
                            + count(aaaaaaD), count(aaaaaaaD), count(aaaaaaaaD), count(aaaaaaaaaD), count(aaaaaaaaaaD), count(aaaaaaaaaaaD))

XXXV_PACIENTES = rbind(XXXV_PACIENTES, XXXV_PACIENTES_temp) # adding observations to dataframe

bbbA = bbA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbA = filter(bbbA, PACIENTE == "AMBULATORIO")
bbbbbA = filter(bbbA, PACIENTE == "HOSPITALIZADO")
bbbbbbA = filter(bbbbA, DOSIS_VAC_COVID19 %in% c(0))
bbbbbbbA = filter(bbbbbA, DOSIS_VAC_COVID19 %in% c(0))

bbbbbbbbA = filter(bbbbA, DOSIS_VAC_COVID19 %in% c(1))
bbbbbbbbbA = filter(bbbbbA, DOSIS_VAC_COVID19 %in% c(1))

bbbbbbbbbbA = filter(bbbbA, DOSIS_VAC_COVID19 %in% c(2))
bbbbbbbbbbbA = filter(bbbbbA, DOSIS_VAC_COVID19 %in% c(2))

bbbB = bbB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbB = filter(bbbB, PACIENTE == "AMBULATORIO")
bbbbbB = filter(bbbB, PACIENTE == "HOSPITALIZADO")
bbbbbbB = filter(bbbbB, DOSIS_VAC_COVID19 %in% c(0))
bbbbbbbB = filter(bbbbbB, DOSIS_VAC_COVID19 %in% c(0))

bbbbbbbbB = filter(bbbbB, DOSIS_VAC_COVID19 %in% c(1))
bbbbbbbbbB = filter(bbbbbB, DOSIS_VAC_COVID19 %in% c(1))

bbbbbbbbbbB = filter(bbbbB, DOSIS_VAC_COVID19 %in% c(2))
bbbbbbbbbbbB = filter(bbbbbB, DOSIS_VAC_COVID19 %in% c(2))

bbbC = bbC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbC = filter(bbbC, PACIENTE == "AMBULATORIO")
bbbbbC = filter(bbbC, PACIENTE == "HOSPITALIZADO")
bbbbbbC = filter(bbbbC, DOSIS_VAC_COVID19 %in% c(0))
bbbbbbbC = filter(bbbbbC, DOSIS_VAC_COVID19 %in% c(0))

bbbbbbbbC = filter(bbbbC, DOSIS_VAC_COVID19 %in% c(1))
bbbbbbbbbC = filter(bbbbbC, DOSIS_VAC_COVID19 %in% c(1))

bbbbbbbbbbC = filter(bbbbC, DOSIS_VAC_COVID19 %in% c(2))
bbbbbbbbbbbC = filter(bbbbbC, DOSIS_VAC_COVID19 %in% c(2))

bbbD = bbD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbbD = filter(bbbD, PACIENTE == "AMBULATORIO")
bbbbbD = filter(bbbD, PACIENTE == "HOSPITALIZADO")
bbbbbbD = filter(bbbbD, DOSIS_VAC_COVID19 %in% c(0))
bbbbbbbD = filter(bbbbbD, DOSIS_VAC_COVID19 %in% c(0))

bbbbbbbbD = filter(bbbbD, DOSIS_VAC_COVID19 %in% c(1))
bbbbbbbbbD = filter(bbbbbD, DOSIS_VAC_COVID19 %in% c(1))

bbbbbbbbbbD = filter(bbbbD, DOSIS_VAC_COVID19 %in% c(2))
bbbbbbbbbbbD = filter(bbbbbD, DOSIS_VAC_COVID19 %in% c(2))

NO_XXXV_PACIENTES_temp = data.frame(count(bbbbbbA), count(bbbbbbbA), count(bbbbbbbbA), count(bbbbbbbbbA), count(bbbbbbbbbbA), count(bbbbbbbbbbbA),
                               + count(bbbbbbB), count(bbbbbbbB), count(bbbbbbbbB), count(bbbbbbbbbB), count(bbbbbbbbbbB), count(bbbbbbbbbbbB), 
                               + count(bbbbbbC), count(bbbbbbbC), count(bbbbbbbbC), count(bbbbbbbbbC), count(bbbbbbbbbbC), count(bbbbbbbbbbbC),
                               + count(bbbbbbD), count(bbbbbbbD), count(bbbbbbbbD), count(bbbbbbbbbD), count(bbbbbbbbbbD), count(bbbbbbbbbbbD))

NO_XXXV_PACIENTES = rbind(NO_XXXV_PACIENTES, NO_XXXV_PACIENTES_temp) # adding observations to dataframe

}

names(XXXV_PACIENTES)[1] =  "35 A AMBULATORIO VACCINE 0" 
names(XXXV_PACIENTES)[2] =  "35 A HOSPITALIZADO VACCINE 0"
names(XXXV_PACIENTES)[3] =  "35 A AMBULATORIO VACCINE 1"  
names(XXXV_PACIENTES)[4] =  "35 A HOSPITALIZADO VACCINE 1"
names(XXXV_PACIENTES)[5] =  "35 A AMBULATORIO VACCINE 2"  
names(XXXV_PACIENTES)[6] =  "35 A HOSPITALIZADO VACCINE 2"
names(XXXV_PACIENTES)[7] =  "35 B AMBULATORIO VACCINE 0"  
names(XXXV_PACIENTES)[8] =  "35 B HOSPITALIZADO VACCINE 0"
names(XXXV_PACIENTES)[9] =  "35 B AMBULATORIO VACCINE 1"   
names(XXXV_PACIENTES)[10] = "35 B HOSPITALIZADO VACCINE 1" 
names(XXXV_PACIENTES)[11] = "35 B AMBULATORIO VACCINE 2"  
names(XXXV_PACIENTES)[12] = "35 B HOSPITALIZADO VACCINE 2"
names(XXXV_PACIENTES)[13] = "35 C AMBULATORIO VACCINE 0"  
names(XXXV_PACIENTES)[14] = "35 C HOSPITALIZADO VACCINE 0"
names(XXXV_PACIENTES)[15] = "35 C AMBULATORIO VACCINE 1"   
names(XXXV_PACIENTES)[16] = "35 C HOSPITALIZADO VACCINE 1" 
names(XXXV_PACIENTES)[17] = "35 C AMBULATORIO VACCINE 2"  
names(XXXV_PACIENTES)[18] = "35 C HOSPITALIZADO VACCINE 2"
names(XXXV_PACIENTES)[19] = "35 D AMBULATORIO VACCINE 0"   
names(XXXV_PACIENTES)[20] = "35 D HOSPITALIZADO VACCINE 0"
names(XXXV_PACIENTES)[21] = "35 D AMBULATORIO VACCINE 1"   
names(XXXV_PACIENTES)[22] = "35 D HOSPITALIZADO VACCINE 1" 
names(XXXV_PACIENTES)[23] = "35 D AMBULATORIO VACCINE 2"  
names(XXXV_PACIENTES)[24] = "35 D HOSPITALIZADO VACCINE 2"

names(NO_XXXV_PACIENTES)[1] =  "35 A AMBULATORIO VACCINE 0" 
names(NO_XXXV_PACIENTES)[2] =  "35 A HOSPITALIZADO VACCINE 0"
names(NO_XXXV_PACIENTES)[3] =  "35 A AMBULATORIO VACCINE 1"  
names(NO_XXXV_PACIENTES)[4] =  "35 A HOSPITALIZADO VACCINE 1"
names(NO_XXXV_PACIENTES)[5] =  "35 A AMBULATORIO VACCINE 2"  
names(NO_XXXV_PACIENTES)[6] =  "35 A HOSPITALIZADO VACCINE 2"
names(NO_XXXV_PACIENTES)[7] =  "35 B AMBULATORIO VACCINE 0"  
names(NO_XXXV_PACIENTES)[8] =  "35 B HOSPITALIZADO VACCINE 0"
names(NO_XXXV_PACIENTES)[9] =  "35 B AMBULATORIO VACCINE 1"   
names(NO_XXXV_PACIENTES)[10] = "35 B HOSPITALIZADO VACCINE 1" 
names(NO_XXXV_PACIENTES)[11] = "35 B AMBULATORIO VACCINE 2"  
names(NO_XXXV_PACIENTES)[12] = "35 B HOSPITALIZADO VACCINE 2"
names(NO_XXXV_PACIENTES)[13] = "35 C AMBULATORIO VACCINE 0"  
names(NO_XXXV_PACIENTES)[14] = "35 C HOSPITALIZADO VACCINE 0"
names(NO_XXXV_PACIENTES)[15] = "35 C AMBULATORIO VACCINE 1"   
names(NO_XXXV_PACIENTES)[16] = "35 C HOSPITALIZADO VACCINE 1" 
names(NO_XXXV_PACIENTES)[17] = "35 C AMBULATORIO VACCINE 2"  
names(NO_XXXV_PACIENTES)[18] = "35 C HOSPITALIZADO VACCINE 2"
names(NO_XXXV_PACIENTES)[19] = "35 D AMBULATORIO VACCINE 0"   
names(NO_XXXV_PACIENTES)[20] = "35 D HOSPITALIZADO VACCINE 0"
names(NO_XXXV_PACIENTES)[21] = "35 D AMBULATORIO VACCINE 1"   
names(NO_XXXV_PACIENTES)[22] = "35 D HOSPITALIZADO VACCINE 1" 
names(NO_XXXV_PACIENTES)[23] = "35 D AMBULATORIO VACCINE 2"  
names(NO_XXXV_PACIENTES)[24] = "35 D HOSPITALIZADO VACCINE 2"

write.table(XXXV_PACIENTES, file = "XXXV_PACIENTES.cvs", sep = "\t", row.names = F)
write.table(NO_XXXV_PACIENTES, file = "NO_XXXV_PACIENTES.cvs", sep = "\t", row.names = F)

proc.time() - ptm # Code to get the final time of the process and display the total time.

system("say Just finished!")
