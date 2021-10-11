# This is a script to try and practice scripting
# It will contain commands to process the Mexican COVID-19 database
# Started on the 20th August 2021.

ptm <- proc.time() # Code to take the starting time of the process

#required libraries

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(gganimate)
library(hrbrthemes)
library(gifski)

DateI = '2020-01-05' # Starting date for the data extraction
DateF = '2021-10-03' # Final date for data extraction

x = filter(XXI_X_XXI, ENTIDAD_RES %in% c(22))

          # To extract POSITIVE cases we use CLASIFICACION_FINAL 1, 2, and 3 into casospos dataset
          casospos = filter(x, CLASIFICACION_FINAL %in% c(1,2,3))
            #AGE histogram composition for casospos datasaet
            hist(casospos$EDAD, 
            main="Casos positivos extraidos con definicion 1, 2 y 3",
            xlab="Edad",
            )
            
          # To extract NEGATIVE cases we use CLASIFICACION_FINAL 7 into casosneg dataset
          casosneg = filter(x, CLASIFICACION_FINAL %in% c(7))
            #AGE histogram composition for casosneg datasaet
            hist(casosneg$EDAD)
            
          # To extract the "MUERTES" I use the negative of selecting those alive, ei. with a meaning less death date.
          # Dataset muertes comes from casospositivos
          muertes = filter(casospos, FECHA_DEF != '9999-99-99')
            #AGE histogram composition for muertes datasaet
            hist(muertes$EDAD)
            
          # Extract HOSPITALIZADOS from casospositivos
          hospitalizados = filter(casospos, TIPO_PACIENTE %in% c(2))
          #AGE histogram composition for hospitalizados datasaet
          hist(hospitalizados$EDAD)          
          
          # To extract the "MUERTES" I use the negative of selecting those alive, ei. with a meaning less death date.
          muertesneg = filter(casosneg, FECHA_DEF != '9999-99-99')
          
          # Extract HOSPITALIZADOS
          hospitalizadosneg = filter(casosneg, TIPO_PACIENTE %in% c(2))

    # Now, to separate by ages. Code is cp for casos positivos, then roman numerals for top age.
    
    cpA = filter(casospos, EDAD <= 10)
    cpB = filter(casospos, EDAD >= 20 & EDAD <= 39)
    cpC = filter(casospos, EDAD >= 40 & EDAD <= 59)
    cpD = filter(casospos, EDAD >= 60)
    
    #positivecases = data.frame(count(cpA), count(cpB), count(cpC), count(cpD)) # putting the numbers in new dataframe
    
    # Now, to separate by ages. Code is cn for casos negativos, then roman numerals for top age.
    
    cnA = filter(casosneg, EDAD <= 10)
    cnB = filter(casosneg, EDAD >= 20 & EDAD <= 39)
    cnC = filter(casosneg, EDAD >= 40 & EDAD <= 59)
    cnD = filter(casosneg, EDAD >= 60)
    
    #negativecases = data.frame(count(cnA), count(cnB), count(cnC), count(cnD))
    
    # Now, to separate by deaths. Code is m for muertes, then roman numerals for top age.
    
    mA = filter(muertes, EDAD <= 10)
    mB = filter(muertes, EDAD >= 20 & EDAD <= 39)
    mC = filter(muertes, EDAD >= 40 & EDAD <= 59)
    mD = filter(muertes, EDAD >= 60)
    
    #muertes = data.frame(count(mA), count(mB), count(mC), count(mD))
    
    # Now, to separate by deaths. Code is h for hospitalizados, then roman numerals for top age.
    
    hA = filter(hospitalizados, EDAD <= 10)
    hB = filter(hospitalizados, EDAD >= 20 & EDAD <= 39)
    hC = filter(hospitalizados, EDAD >= 40 & EDAD <= 59)
    hD = filter(hospitalizados, EDAD >= 60)
    
    #hospitalizados = data.frame(count(hA), count(hB), count(hC), count(hD))
    
    # Now, to separate by deaths. Code is m for muertes, then roman numerals for top age.
    
    mnA = filter(muertesneg, EDAD <= 10)
    mnB = filter(muertesneg, EDAD >= 20 & EDAD <= 39)
    mnC = filter(muertesneg, EDAD >= 40 & EDAD <= 59)
    mnD = filter(muertesneg, EDAD >= 60)
    
    #muertes = data.frame(count(mA), count(mB), count(mC), count(mD))
    
    # Now, to separate by deaths. Code is h for hospitalizados, then roman numerals for top age.
    
    hnA = filter(hospitalizadosneg, EDAD <= 10)
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
  muertesneg = rbind(hospitalizados, hospitalizadosP) # adding observations to dataframe 
  
  wI_hnA = hnA %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hnB = hnB %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hnC = hnC %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  wI_hnD = hnD %>% filter(FECHA_SINTOMAS <= as.Date(DateN) & FECHA_SINTOMAS >= as.Date(DateI))
  
  hospitalizadosnegP = data.frame(as.Date(DateI), count(wI_hnA), count(wI_hnB), count(wI_hnC), count(wI_hnD))
  hospitalizadosneg = rbind(hospitalizados, hospitalizadosP) # adding observations to dataframe
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

rm(hospitalizadosP) # to remove dataframes
rm(muertesP) # to remove dataframes
rm(positivecasesP) # to remove dataframes
rm(hospitalizadosnegP) # to remove dataframes
rm(muertesnegP) # to remove dataframes

#ggplot(muertes, aes(x=InicioSint, y=A)) + geom_bar(stat = "identity") 
#ggplot(muertes, aes(x=InicioSint, y=B)) + geom_bar(stat = "identity") 
#ggplot(muertes, aes(x=InicioSint, y=C)) + geom_bar(stat = "identity") 
#ggplot(muertes, aes(x=InicioSint, y=D)) + geom_bar(stat = "identity")

write.table(hospitalizados, file = "hospitalizados.csv",
            sep = "\t", row.names = F)

write.table(hospitalizadosneg, file = "hospitalizadosneg.csv",
            sep = "\t", row.names = F)

write.table(muertes, file = "muertes.csv",
            sep = "\t", row.names = F)

write.table(muertesneg, file = "muertesneg.csv",
            sep = "\t", row.names = F)

write.table(positivecases, file = "positivecases.csv",
            sep = "\t", row.names = F)

proc.time() - ptm # Code to get the final time of the process and display the total time.