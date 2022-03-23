# This is a script to try and practice scripting
# It will contain commands to process the Mexican COVID-19 database
# Started on the 20th August 2021.

# update, without prompts for permission/clarification
update.packages(ask = FALSE)

ptm <- proc.time() # Code to take the starting time of the process

#required libraries, without these noting works. 

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
#library(gganimate)
#library(hrbrthemes)
#library(gifski)


DateI = '2020-01-05' #
DateF = '2022-03-13' #

# To extract POSITIVE cases we use CLASIFICACION_FINAL 1, 2, and 3
casospos = filter(x, CLASIFICACION_FINAL %in% c(1,2,3))

# Now, to separate by ages. Code is cp for casos positivos, then roman numerals for top age.

cpA = filter(casospos, EDAD == 0)
cpB = filter(casospos, EDAD >= 1 & EDAD <= 4)
cpC = filter(casospos, EDAD >= 5 & EDAD <= 9)
cpD = filter(casospos, EDAD >= 10 & EDAD <= 14)
cpE = filter(casospos, EDAD >= 15 & EDAD <= 19)

# To extract the "MUERTES" I use the negative of selecting those alive, ei. with a meaning less death date.
muertes = filter(casospos, FECHA_DEF != '9999-99-99')

# Now, to separate by deaths. Code is m for muertes, then roman numerals for top age.

mA = filter(muertes, EDAD == 0)
mB = filter(muertes, EDAD >= 1 & EDAD <= 4)
mC = filter(muertes, EDAD >= 5 & EDAD <= 9)
mD = filter(muertes, EDAD >= 10 & EDAD <= 14)
mE = filter(muertes, EDAD >= 15 & EDAD <= 19)

# Now, to separate by deaths. Code is h for hospitalizados, then roman numerals for top age.

# Extract HOSPITALIZADOS
hospitalizados = filter(casospos, TIPO_PACIENTE %in% c(2))

hA = filter(hospitalizados, EDAD == 0)
hB = filter(hospitalizados, EDAD >= 1 & EDAD <= 4)
hC = filter(hospitalizados, EDAD >= 5 & EDAD <= 9)
hD = filter(hospitalizados, EDAD >= 10 & EDAD <= 14)
hE = filter(hospitalizados, EDAD >= 15 & EDAD <= 19)

#Week I

DateN = as.Date(DateI) + 6

wI_cpA = cpA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_cpB = cpB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_cpC = cpC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_cpD = cpD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_cpE = cpE %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))

positivecases = data.frame(as.Date(DateI), count(wI_cpA), count(wI_cpB), count(wI_cpC), count(wI_cpD), count(wI_cpE))

wI_mA = mA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mB = mB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mC = mC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mD = mD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mE = mE %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))

muertes = data.frame(as.Date(DateI), count(wI_mA), count(wI_mB), count(wI_mC), count(wI_mD), count(wI_mE))

wI_hA = hA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hB = hB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hC = hC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hD = hD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hE = hE %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))

hospitalizados = data.frame(as.Date(DateI), count(wI_hA), count(wI_hB), count(wI_hC), count(wI_hD), count(wI_hE))

while (DateI < DateF) {
  
  #Week II
  
  DateI = as.Date(DateN) + 1
  DateN = as.Date(DateI) + 6
  
  wI_cpA = cpA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_cpB = cpB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_cpC = cpC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_cpD = cpD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_cpE = cpE %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  positivecasesP = data.frame(as.Date(DateI), count(wI_cpA), count(wI_cpB), count(wI_cpC), count(wI_cpD), count(wI_cpE))
  positivecases = rbind(positivecases, positivecasesP) # adding observations to dataframe
  
  wI_mA = mA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mB = mB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mC = mC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mD = mD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mE = mE %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  muertesP = data.frame(as.Date(DateI), count(wI_mA), count(wI_mB), count(wI_mC), count(wI_mD), count(wI_mE))
  muertes = rbind(muertes, muertesP) # adding observations to dataframe
  
  wI_hA = hA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hB = hB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hC = hC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hD = hD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hE = hE %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  hospitalizadosP = data.frame(as.Date(DateI), count(wI_hA), count(wI_hB), count(wI_hC), count(wI_hD), count(wI_hE))
  hospitalizados = rbind(hospitalizados, hospitalizadosP) # adding observations to dataframe
  
}

# Changing columns names to the age groups before saving. 

names(positivecases)[1] = "InicioSint" 
names(positivecases)[2] = "A" 
names(positivecases)[3] = "B" 
names(positivecases)[4] = "C" 
names(positivecases)[5] = "D" 
names(positivecases)[6] = "E" 

names(hospitalizados)[1] = "InicioSint" 
names(hospitalizados)[2] = "A" 
names(hospitalizados)[3] = "B" 
names(hospitalizados)[4] = "C" 
names(hospitalizados)[5] = "D" 
names(hospitalizados)[6] = "D" 

names(muertes)[1] = "InicioSint"
names(muertes)[2] = "A" 
names(muertes)[3] = "B" 
names(muertes)[4] = "C" 
names(muertes)[5] = "D" 
names(muertes)[6] = "E" 

write.table(positivecases, file = "positivecases-infantes.cvs", sep = "\t", row.names = F)
write.table(muertes, file = "muertes-infantes.cvs", sep = "\t", row.names = F)
write.table(hospitalizados, file = "hospitalizados-infantes.cvs", sep = "\t", row.names = F)

proc.time() - ptm # Code to get the final time of the process and display the total time.

system("say Just finished!")
