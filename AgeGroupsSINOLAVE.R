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

XXXV = filter(Mexico, TIPO_INFLUENZA_CONF1 %in% c(35)) # Influenza cases, first filter

  death= filter(XXXV, MOTIVO_EGRESO %in% c(4)) # defunsion
  
    antigen = filter(XXXV, RESULT_PRUEBA_RAPIDA == 1)

#other = filter(Mexico, TIPO_INFLUENZA_CONF1 != 35)
    
# Now, to separate by ages XXXV IV 

aaA = filter(death, EDAD_ANO <= 1)
aaB = filter(death, EDAD_ANO >= 2 & EDAD_ANO <= 4)
aaC = filter(death, EDAD_ANO >= 18 & EDAD_ANO <= 19)

bbA = filter(antigen, EDAD_ANO <= 1)
bbB = filter(antigen, EDAD_ANO >= 2 & EDAD_ANO <= 4)
bbC = filter(antigen, EDAD_ANO >= 18 & EDAD_ANO <= 19)

DateI = '2020-01-01' # Starting date for the data extraction
DateF = '2022-03-21' # Final date for data extraction

#Week I

DateN = as.Date(DateI) + 6

aaaA = aaA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaB = aaB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
aaaC = aaC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))

bbbA = bbA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbB = bbB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
bbbC = bbC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))

XXXV_D = data.frame(as.Date(DateI), count(aaaA), count(aaaB), count(aaaC),count(bbbA), count(bbbB), count(bbbC))

while (DateI < DateF) {
  
  #Week II
  
  DateI = as.Date(DateN) + 1
  DateN = as.Date(DateI) + 6
  
  aaaA = aaA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  aaaB = aaB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  aaaC = aaC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  
  bbbA = bbA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  bbbB = bbB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  bbbC = bbC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))

  XXXV_D_temp = data.frame(as.Date(DateI), count(aaaA), count(aaaB), count(aaaC),count(bbbA), count(bbbB), count(bbbC))
  XXXV_D = rbind(XXXV_D, XXXV_D_temp) # adding observations to dataframe
  
  gc()
  
}

names(XXXV_D)[1] = "InicioSint" 
names(XXXV_D)[2] = "1yo_d" 
names(XXXV_D)[3] = "2-4yo_d" 
names(XXXV_D)[4] = "18-19yo_d" 
names(XXXV_D)[5] = "1yo_pos" 
names(XXXV_D)[6] = "2-4yo_pos"
names(XXXV_D)[7] = "18-19yo_pos"

write.table(XXXV_D, file = "Jovenes.cvs", sep = "\t", row.names = F)

proc.time() - ptm # Code to get the final time of the process and display the total time.

system("say Just finished!")