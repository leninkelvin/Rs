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

#cpA = filter(casospos, EDAD <= 19)
#cpB = filter(casospos, EDAD >= 20 & EDAD <= 39)
#cpC = filter(casospos, EDAD >= 40 & EDAD <= 59)
#cpD = filter(casospos, EDAD >= 60)

cpA_1 = filter(cpA, SEXO == 1)
write.table(cpA_1, file = "PosCas0_19_H.csv", sep = "\t", row.names = F)

cpA_2 = filter(cpA, SEXO == 2)
write.table(cpA_2, file = "PosCas0_19_M.csv", sep = "\t", row.names = F)

cpB_1 = filter(cpB, SEXO == 1)
cpB_11 = filter(cpB_1, EDAD >= 20 & EDAD <= 29)
cpB_12 = filter(cpB_1, EDAD >= 30 & EDAD <= 39)
write.table(cpB_11, file = "PosCas20_29_H.csv", sep = "\t", row.names = F)
write.table(cpB_12, file = "PosCas30_39_H.csv", sep = "\t", row.names = F)

cpB_2 = filter(cpB, SEXO == 2)
cpB_21 = filter(cpB_2, EDAD >= 20 & EDAD <= 29)
cpB_22 = filter(cpB_2, EDAD >= 30 & EDAD <= 39)
write.table(cpB_21, file = "PosCas20_29_M.csv", sep = "\t", row.names = F)
write.table(cpB_22, file = "PosCas30_39_M.csv", sep = "\t", row.names = F)

cpC_1 = filter(cpC, SEXO == 1)
write.table(cpC_1, file = "PosCas40_59_H.csv", sep = "\t", row.names = F)

cpC_2 = filter(cpC, SEXO == 2)
write.table(cpC_2, file = "PosCas40_59_M.csv", sep = "\t", row.names = F)

cpD_1 = filter(cpD, SEXO == 1)
write.table(cpD_1, file = "PosCas60+_H.csv", sep = "\t", row.names = F)

cpD_2 = filter(cpD, SEXO == 2)
write.table(cpD_2, file = "PosCas60+_M.csv", sep = "\t", row.names = F)
