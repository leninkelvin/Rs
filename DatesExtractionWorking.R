# This is a script to try and practice scripting
# It will contain commands to process the Mexican COVID-19 database
# Started on the 20th August 2021.

ptm <- proc.time() # Code to take the starting time of the process

#required libraries

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
#library(gganimate)
#library(hrbrthemes)
#library(gifski)

DateI = '2020-01-05'
DateF = '2021-10-22'

# To extract POSITIVE cases we use CLASIFICACION_FINAL 1, 2, and 3
casospos = filter(x, CLASIFICACION_FINAL %in% c(1,2,3))

      # To extract POSITIVE cases we use CLASIFICACION_FINAL 1
      casospos1 = filter(x, CLASIFICACION_FINAL %in% c(1))
      
      # To extract POSITIVE cases we use CLASIFICACION_FINAL 2
      casospos2 = filter(x, CLASIFICACION_FINAL %in% c(2))
      
      # To extract POSITIVE cases we use CLASIFICACION_FINAL 3
      casospos3 = filter(x, CLASIFICACION_FINAL %in% c(3))

#AGE histogram composition for casospos datasaet
h= hist(casospos$EDAD, 
        main="Casos positivos extraidos con definicion 1, 2 y 3",
        xlab="Edad",
        breaks=120,
)

casospos_counts = as.data.frame(h$counts)

write.table(casospos_counts, file = "casospos_counts.cvs",
            sep = "\t", row.names = T)

# To extract NEGATIVE cases we use CLASIFICACION_FINAL 7
casosneg = filter(x, CLASIFICACION_FINAL %in% c(7))

#AGE histogram composition for casospos datasaet
h= hist(casosneg$EDAD, 
        main="Casos negativos extraidos con definicion 7",
        xlab="Edad",
        breaks=120,
)

casosneg_counts = as.data.frame(h$counts)

write.table(casosneg_counts, file = "casosneg_counts.cvs",
            sep = "\t", row.names = T)

# To extract the "MUERTES" I use the negative of selecting those alive, ei. with a meaning less death date.
muertes = filter(casospos, FECHA_DEF != '9999-99-99')

#AGE histogram composition for casospos datasaet
h= hist(muertes$EDAD, 
        main="Muertes extraidas con fecha de defuncion NOT 9999-99-99",
        xlab="Edad",
        breaks=120,
)

muertes_counts = as.data.frame(h$counts)

write.table(muertes_counts, file = "muertes_counts.cvs",
            sep = "\t", row.names = T)

# Extract HOSPITALIZADOS
hospitalizados = filter(casospos, TIPO_PACIENTE %in% c(2))

#AGE histogram composition for casospos datasaet
h = hist(hospitalizados$EDAD, 
         main="Casos hospitalizados (tipo de paciente) extraidos de casos positivos (1, 2, y 3)",
         xlab="Edad",
         breaks=120,
)

hospitalizados_counts = as.data.frame(h$counts)

write.table(hospitalizados_counts, file = "hospitalizados_counts.cvs",
            sep = "\t", row.names = T)

# To extract the "MUERTES" I use the negative of selecting those alive, ei. with a meaning less death date.
muertesneg = filter(casosneg, FECHA_DEF != '9999-99-99')

#AGE histogram composition for casospos datasaet
h = hist(muertesneg$EDAD, 
         main="Muertes negativas extraidas de casos negativos (7) y FECHA DEF NOT 9999-99-99",
         xlab="Edad",
         breaks=120,
)

muertesneg_counts = as.data.frame(h$counts)

write.table(muertesneg_counts, file = "muertesneg_counts.cvs",
            sep = "\t", row.names = T)

# Extract HOSPITALIZADOS
hospitalizadosneg = filter(casosneg, TIPO_PACIENTE %in% c(2))

#AGE histogram composition for casospos datasaet
h = hist(hospitalizadosneg$EDAD, 
         main="Hospitalizados negativos extraidas de casos negativos (7) y TIPO de PACIENTE 2",
         xlab="Edad",
         breaks=121,
)

hospitalizadosneg_counts = as.data.frame(h$counts)

write.table(hospitalizadosneg_counts, file = "hospitalizadosneg_counts.cvs",
            sep = "\t", row.names = T)

# Now, to separate by ages. Code is cp for casos positivos, then roman numerals for top age.

cpA = filter(casospos, EDAD <= 19)
cpB = filter(casospos, EDAD >= 20 & EDAD <= 39)
cpC = filter(casospos, EDAD >= 40 & EDAD <= 59)
cpD = filter(casospos, EDAD >= 60)

#positivecases = data.frame(count(cpA), count(cpB), count(cpC), count(cpD)) # putting the numbers in new dataframe

# Now, to separate by ages. Code is cn for casos negativos, then roman numerals for top age.

cnA = filter(casosneg, EDAD <= 19)
cnB = filter(casosneg, EDAD >= 20 & EDAD <= 39)
cnC = filter(casosneg, EDAD >= 40 & EDAD <= 59)
cnD = filter(casosneg, EDAD >= 60)

#negativecases = data.frame(count(cnA), count(cnB), count(cnC), count(cnD))

# Now, to separate by deaths. Code is m for muertes, then roman numerals for top age.

mA = filter(muertes, EDAD <= 19)
mB = filter(muertes, EDAD >= 20 & EDAD <= 39)
mC = filter(muertes, EDAD >= 40 & EDAD <= 59)
mD = filter(muertes, EDAD >= 60)

#muertes = data.frame(count(mA), count(mB), count(mC), count(mD))

# Now, to separate by deaths. Code is h for hospitalizados, then roman numerals for top age.

hA = filter(hospitalizados, EDAD <= 19)
hB = filter(hospitalizados, EDAD >= 20 & EDAD <= 39)
hC = filter(hospitalizados, EDAD >= 40 & EDAD <= 59)
hD = filter(hospitalizados, EDAD >= 60)

#hospitalizados = data.frame(count(hA), count(hB), count(hC), count(hD))

# Now, to separate by deaths. Code is m for muertes, then roman numerals for top age.

mnA = filter(muertesneg, EDAD <= 19)
mnB = filter(muertesneg, EDAD >= 20 & EDAD <= 39)
mnC = filter(muertesneg, EDAD >= 40 & EDAD <= 59)
mnD = filter(muertesneg, EDAD >= 60)

#muertes = data.frame(count(mA), count(mB), count(mC), count(mD))

# Now, to separate by deaths. Code is h for hospitalizados, then roman numerals for top age.

hnA = filter(hospitalizadosneg, EDAD <= 19)
hnB = filter(hospitalizadosneg, EDAD >= 20 & EDAD <= 39)
hnC = filter(hospitalizadosneg, EDAD >= 40 & EDAD <= 59)
hnD = filter(hospitalizadosneg, EDAD >= 60)

#hospitalizados = data.frame(count(hA), count(hB), count(hC), count(hD))

# Now,to extract ranges of data based on dates, the code is w for week, 
# roman numerals for the week, lower d or c (death or case) dash age group. 
# For example: wXXIX_dXIX means week 29, deaths max age 19 yo.

#Week I

DateN = as.Date(DateI) + 6

wI_cpA = cpA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_cpB = cpB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_cpC = cpC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_cpD = cpD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))

positivecases = data.frame(as.Date(DateI), count(wI_cpA), count(wI_cpB), count(wI_cpC), count(wI_cpD))

wI_mA = mA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mB = mB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mC = mC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mD = mD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))

muertes = data.frame(as.Date(DateI), count(wI_mA), count(wI_mB), count(wI_mC), count(wI_mD))

wI_hA = hA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hB = hB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hC = hC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hD = hD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))

hospitalizados = data.frame(as.Date(DateI), count(wI_hA), count(wI_hB), count(wI_hC), count(wI_hD))

wI_mnA = mnA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mnB = mnB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mnC = mnC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_mnD = mnD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))

muertesneg = data.frame(as.Date(DateI), count(wI_mnA), count(wI_mnB), count(wI_mnC), count(wI_mnD))

wI_hnA = hnA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hnB = hnB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hnC = hnC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
wI_hnD = hnD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))

hospitalizadosneg = data.frame(as.Date(DateI), count(wI_hnA), count(wI_hnB), count(wI_hnC), count(wI_hnD))

while (DateI < DateF) {
  
  #Week II
  
  DateI = as.Date(DateN) + 1
  DateN = as.Date(DateI) + 6
  
  wI_cpA = cpA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_cpB = cpB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_cpC = cpC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_cpD = cpD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  positivecasesP = data.frame(as.Date(DateI), count(wI_cpA), count(wI_cpB), count(wI_cpC), count(wI_cpD))
  positivecases = rbind(positivecases, positivecasesP) # adding observations to dataframe
  
  wI_mA = mA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mB = mB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mC = mC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mD = mD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  muertesP = data.frame(as.Date(DateI), count(wI_mA), count(wI_mB), count(wI_mC), count(wI_mD))
  muertes = rbind(muertes, muertesP) # adding observations to dataframe
  
  
  wI_hA = hA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hB = hB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hC = hC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hD = hD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  hospitalizadosP = data.frame(as.Date(DateI), count(wI_hA), count(wI_hB), count(wI_hC), count(wI_hD))
  hospitalizados = rbind(hospitalizados, hospitalizadosP) # adding observations to dataframe
  
  wI_mnA = mnA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mnB = mnB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mnC = mnC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_mnD = mnD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  muertesnegP = data.frame(as.Date(DateI), count(wI_mnA), count(wI_mnB), count(wI_mnC), count(wI_mnD))
  muertesneg = rbind(muertesneg, muertesnegP) # adding observations to dataframe 
  
  wI_hnA = hnA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hnB = hnB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hnC = hnC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hnD = hnD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  hospitalizadosnegP = data.frame(as.Date(DateI), count(wI_hnA), count(wI_hnB), count(wI_hnC), count(wI_hnD))
  hospitalizadosneg = rbind(hospitalizadosneg, hospitalizadosnegP) # adding observations to dataframe
}

# Changing columns names to the age groups before saving. 

names(positivecases)[1] = "InicioSint" 
names(positivecases)[2] = "A" 
names(positivecases)[3] = "B" 
names(positivecases)[4] = "C" 
names(positivecases)[5] = "D" 

#names(negativecases)[1] = "A" 
#names(negativecases)[2] = "B" 
#names(negativecases)[3] = "C" 
#names(negativecases)[4] = "D" 

names(muertes)[1] = "InicioSint"
names(muertes)[2] = "A" 
names(muertes)[3] = "B" 
names(muertes)[4] = "C" 
names(muertes)[5] = "D" 

names(hospitalizados)[1] = "InicioSint" 
names(hospitalizados)[2] = "A" 
names(hospitalizados)[3] = "B" 
names(hospitalizados)[4] = "C" 
names(hospitalizados)[5] = "D" 

names(muertesneg)[1] = "InicioSint"
names(muertesneg)[2] = "A" 
names(muertesneg)[3] = "B" 
names(muertesneg)[4] = "C" 
names(muertesneg)[5] = "D" 

names(hospitalizadosneg)[1] = "InicioSint" 
names(hospitalizadosneg)[2] = "A" 
names(hospitalizadosneg)[3] = "B" 
names(hospitalizadosneg)[4] = "C" 
names(hospitalizadosneg)[5] = "D" 

write.table(positivecases, file = "positivecases.cvs", sep = "\t", row.names = F)
write.table(hospitalizados, file = "hospitalizados.cvs", sep = "\t", row.names = F)
write.table(muertes, file = "muertes.cvs", sep = "\t", row.names = F)
write.table(muertesneg, file = "muertesneg.cvs", sep = "\t", row.names = F)
write.table(hospitalizadosneg, file = "hospitalizadosneg.cvs", sep = "\t", row.names = F)

rm(hospitalizadosP) # to remove dataframes
rm(muertesP) # to remove dataframes
rm(positivecasesP) # to remove dataframes
rm(hospitalizadosnegP) # to remove dataframes
rm(muertesnegP) # to remove dataframes

#ggplot(muertes, aes(x=InicioSint, y=A)) + geom_bar(stat = "identity") 
#ggplot(muertes, aes(x=InicioSint, y=B)) + geom_bar(stat = "identity") 
#ggplot(muertes, aes(x=InicioSint, y=C)) + geom_bar(stat = "identity") 
#ggplot(muertes, aes(x=InicioSint, y=D)) + geom_bar(stat = "identity")

proc.time() - ptm # Code to get the final time of the process and display the total time.

system("say Just finished!")