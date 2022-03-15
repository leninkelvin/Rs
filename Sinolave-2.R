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

# DATASETS GENERATED:
# XXXV TIPO_INFLUENZA_CONF1 35
#   XXXV_I MOTIVO_EGRESO MEJORIA
#     XXXV_I_none SUBGROUP WITHOUT VACCINE
#     XXXV_I_I SUBGROUP WITH ONE DOSE
#       XXXV_I_II SUBGROUP WITH TWO DOSES
#
#   XXXV_IV MOTIVO_EGRESO DEFUNCION
#     XXXV_IV_none SUBGROUP WITHOUT VACCINE
#     XXXV_IV_I SUBGROUP WITH ONE DOSE
#     XXXV_IV_II SUBGROUP WITH TWO DOSES
#
# other opposite of XXXV
#   POS_other within other, which had an antigen test
#     EG_POS_other, previous with EGRESS 1
#       VAC_EG_POS_other_none
#       VAC_EG_POS_other_I
#       VAC_EG_POS_other_2
#   POS_other_IV
#     POS_other_IV_none
#     POS_other_IV_1
#     POS_other_IV_2

XXXV = filter(Mexico, TIPO_INFLUENZA_CONF1 %in% c(35)) # Influenza cases, first filter
XXXV$MOTIVO_EGRESO = as.factor(XXXV$MOTIVO_EGRESO) # needed for statistics
  summary(XXXV$MOTIVO_EGRESO)
  stat.desc(XXXV$MOTIVO_EGRESO) # descriptive statistics 

XXXV_I= filter(XXXV, MOTIVO_EGRESO %in% c(1)) #MEJORIA, second filter
  summary(XXXV_I$MOTIVO_EGRESO)#

  XXXV_I$PACIENTE = as.factor(XXXV_I$PACIENTE)
    summary(XXXV_I$PACIENTE) # AMBULATORIO O HOSPITALIZADO
  XXXV_I$DOSIS_VAC_COVID19 = as.factor(XXXV_I$DOSIS_VAC_COVID19)
    summary(XXXV_I$DOSIS_VAC_COVID19) # 0, 1 OR 2 DOSES
  XXXV_I$TIENE_INTUBACION_ENDOTRAQUEAL = as.factor(XXXV_I$TIENE_INTUBACION_ENDOTRAQUEAL)
    summary(XXXV_I$TIENE_INTUBACION_ENDOTRAQUEAL) # INTUBATION

    XXXV_I_none= filter(XXXV_I, DOSIS_VAC_COVID19 %in% c(0)) # NO VACCINATED
      summary(XXXV_I_none$PACIENTE)
      summary(XXXV_I_none$TIENE_INTUBACION_ENDOTRAQUEAL)

        I_none_edad = hist(XXXV_I_none$EDAD_ANO) # STAST FOR THIS GROUP
        I_none_edad[["breaks"]]
        I_none_edad[["counts"]]
        stat.desc(XXXV_I_none$EDAD_ANO)
        summary(XXXV_I_none$EDAD_ANO)

XXXV_I_I= filter(XXXV_I, DOSIS_VAC_COVID19 %in% c(1)) # una dosis
  summary(XXXV_I_I$PACIENTE) # AMBULATORIO O HOSPITALIZADO
  summary(XXXV_I_I$TIENE_INTUBACION_ENDOTRAQUEAL)
    XXXV_I_I$MARCA_VAC_COVID19 = as.factor(XXXV_I_I$MARCA_VAC_COVID19)
    summary(XXXV_I_I$MARCA_VAC_COVID19) # VACCINE BRAND

    XXXV_I_II= filter(XXXV_I, DOSIS_VAC_COVID19 %in% c(2)) # dos dosis
      summary(XXXV_I_II$PACIENTE)# AMBULATORIO O HOSPITALIZADO
      summary(XXXV_I_II$TIENE_INTUBACION_ENDOTRAQUEAL)
        XXXV_I_II$REF_VAC_COVID19 = as.factor(XXXV_I_II$REF_VAC_COVID19) # VACCINE
        # REINFORCEMENT
        summary(XXXV_I_II$REF_VAC_COVID19)
        XXXV_I_II$MARCA_VAC_COVID19 = as.factor(XXXV_I_II$MARCA_VAC_COVID19)
        summary(XXXV_I_II$MARCA_VAC_COVID19) # VACCINE BRAND

          I_II_edad = hist(XXXV_I_II$EDAD_ANO)
          I_II_edad[["breaks"]]
          I_II_edad[["counts"]]
          stat.desc(XXXV_I_II$EDAD_ANO)
          summary(XXXV_I_II$EDAD_ANO)
          
# NEXT DATA PROCESSING CHUNCK
          
XXXV_IV = filter(XXXV, MOTIVO_EGRESO %in% c(4)) # DEAD
  summary(XXXV_IV$MOTIVO_EGRESO)
XXXV_IV$PACIENTE = as.factor(XXXV_IV$PACIENTE)
  summary(XXXV_IV$PACIENTE)
XXXV_IV$DOSIS_VAC_COVID19 = as.factor(XXXV_IV$DOSIS_VAC_COVID19)
  summary(XXXV_IV$DOSIS_VAC_COVID19)
XXXV_IV$TIENE_INTUBACION_ENDOTRAQUEAL = as.factor(XXXV_IV$TIENE_INTUBACION_ENDOTRAQUEAL)
  summary(XXXV_IV$TIENE_INTUBACION_ENDOTRAQUEAL)
          
  XXXV_IV_none= filter(XXXV_IV, DOSIS_VAC_COVID19 %in% c(0))
    summary(XXXV_IV_none$PACIENTE)
    summary(XXXV_IV_none$TIENE_INTUBACION_ENDOTRAQUEAL)
          
    IV_none_edad = hist(XXXV_IV_none$EDAD_ANO)
    IV_none_edad[["breaks"]]  
    IV_none_edad[["counts"]]
    stat.desc(XXXV_IV_none$EDAD_ANO)
    summary(XXXV_IV_none$EDAD_ANO)
          
  XXXV_IV_I= filter(XXXV_IV, DOSIS_VAC_COVID19 %in% c(1))
    summary(XXXV_IV_I$PACIENTE)
    summary(XXXV_IV_I$TIENE_INTUBACION_ENDOTRAQUEAL)
            
    XXXV_IV_I$MARCA_VAC_COVID19 = as.factor(XXXV_IV_I$MARCA_VAC_COVID19)
    summary(XXXV_IV_I$MARCA_VAC_COVID19)
          
    XXXV_IV_II= filter(XXXV_IV, DOSIS_VAC_COVID19 %in% c(2))
    summary(XXXV_IV_II$PACIENTE)
    summary(XXXV_IV_II$TIENE_INTUBACION_ENDOTRAQUEAL)
            
    XXXV_IV_II$MARCA_VAC_COVID19 = as.factor(XXXV_IV_II$MARCA_VAC_COVID19)
    summary(XXXV_IV_II$MARCA_VAC_COVID19)
            
    XXXV_IV_II$REF_VAC_COVID19 = as.factor(XXXV_IV_II$REF_VAC_COVID19)
    summary(XXXV_IV_II$REF_VAC_COVID19)
            
    XXXV_IV_II$SEXO = as.factor(XXXV_IV_II$SEXO)
    summary(XXXV_IV_II$SEXO)
          
      IV_II_edad = hist(XXXV_IV_II$EDAD_ANO)
      IV_II_edad[["breaks"]]
      IV_II_edad[["counts"]]
      stat.desc(XXXV_IV_II$EDAD_ANO)
          
      XXXV_IV_II_REF_No = filter(XXXV_IV_II, REF_VAC_COVID19 == "No")
      XXXV_IV_II_REF_Si = filter(XXXV_IV_II, REF_VAC_COVID19 == "Si")
        summary(XXXV_IV_II_REF_No$SEXO)
        summary(XXXV_IV_II_REF_Si$SEXO)
        
        
other = filter(Mexico, TIPO_INFLUENZA_CONF1 != 35)
        
POS_other = filter(other, RESULT_PRUEBA_RAPIDA == 1)
        
EG_POS_other = filter(POS_other, MOTIVO_EGRESO %in% c(1))
EG_POS_other$PACIENTE = as.factor(EG_POS_other$PACIENTE)
  summary(EG_POS_other$PACIENTE)
  EG_POS_other$DOSIS_VAC_COVID19 = as.factor(EG_POS_other$DOSIS_VAC_COVID19)
  
  VAC_EG_POS_other_none = filter(EG_POS_other, DOSIS_VAC_COVID19 %in% c(0))
    summary(VAC_EG_POS_other_none$PACIENTE)
  VAC_EG_POS_other_1 = filter(EG_POS_other, DOSIS_VAC_COVID19 %in% c(1))
    summary(VAC_EG_POS_other_1$PACIENTE)

    VAC_EG_POS_other_1$MARCA_VAC_COVID19 = as.factor(VAC_EG_POS_other_1$MARCA_VAC_COVID19)
    summary(VAC_EG_POS_other_1$MARCA_VAC_COVID19)
    
    VAC_EG_POS_other_2= filter(EG_POS_other, DOSIS_VAC_COVID19 %in% c(2))
    summary(VAC_EG_POS_other_2$PACIENTE)
    
    VAC_EG_POS_other_2$MARCA_VAC_COVID19 = as.factor(VAC_EG_POS_other_2$MARCA_VAC_COVID19)
    summary(VAC_EG_POS_other_2$MARCA_VAC_COVID19)
    
POS_other_IV = filter(POS_other, MOTIVO_EGRESO %in% c(4))
POS_other_IV$PACIENTE = as.factor(POS_other_IV$PACIENTE)
  summary(POS_other_IV$PACIENTE)
  
  POS_other_IV$DOSIS_VAC_COVID19 = as.factor(POS_other_IV$DOSIS_VAC_COVID19)
  
  POS_other_IV_none= filter(POS_other_IV, DOSIS_VAC_COVID19 %in% c(0))
  POS_other_IV_none$PACIENTE = as.factor(POS_other_IV_none$PACIENTE)
  summary(POS_other_IV_none$PACIENTE)
  
  POS_other_IV_1= filter(POS_other_IV, DOSIS_VAC_COVID19 %in% c(1))
  POS_other_IV_1$PACIENTE = as.factor(POS_other_IV_1$PACIENTE)
  summary(POS_other_IV_1$PACIENTE)
  POS_other_IV_1$MARCA_VAC_COVID19 = as.factor(POS_other_IV_1$MARCA_VAC_COVID19)
  summary(POS_other_IV_1$MARCA_VAC_COVID19)
  
  POS_other_IV_2= filter(POS_other_IV, DOSIS_VAC_COVID19 %in% c(2))
  POS_other_IV_2$PACIENTE = as.factor(POS_other_IV_2$PACIENTE)
  summary(POS_other_IV_2$PACIENTE)
  
  POS_other_IV_2$MARCA_VAC_COVID19 = as.factor(POS_other_IV_2$MARCA_VAC_COVID19)
  summary(POS_other_IV_2$MARCA_VAC_COVID19)
  
  POS_other_IV_2_Si = filter(POS_other_IV_2, REF_VAC_COVID19 == "Si")

  DateI = '0001-01-20' # Starting date for the data extraction
  DateF = '0007-01-20' # Final date for data extraction
  
  #Week I
  
  DateN = as.Date(DateI) + 6
  XXXV_IV_none_DATES = XXXV_IV_none %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  
  DATA = data.frame(as.Date(DateI), count(XXXV_IV_none_DATES))
  
  while (DateI < DateF) {
    
    #Week II
    
    DateI = as.Date(DateN) + 1
    DateN = as.Date(DateI) + 6
    XXXV_IV_none_DATES = XXXV_IV_none %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    
    DATAP = data.frame(as.Date(DateI), count(XXXV_IV_none_DATES))
    DATA = rbind(DATA, DATAP) # adding observations to dataframe
  }
  
#Mexico$FECHA_INGRESO_UCI = as.factor(Mexico$FECHA_INGRESO_UCI) # Mexico is all the DB.
#  summary(Mexico$FECHA_INGRESO_UCI)
#Mexico$TIENE_INTUBACION_ENDOTRAQUEAL = as.factor(Mexico$TIENE_INTUBACION_ENDOTRAQUEAL)
#  summary(Mexico$TIENE_INTUBACION_ENDOTRAQUEAL)