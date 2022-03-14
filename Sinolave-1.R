# update, without prompts for permission/clarification
update.packages(ask = FALSE)

ptm <- proc.time() # Code to take the starting time of the process

#required libraries, without these noting works. 

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(pastecs)
#library(gganimate)
#library(hrbrthemes)
#library(gifski)

Mexico$FECHA_INGRESO_UCI = as.factor(Mexico$FECHA_INGRESO_UCI)
summary(Mexico$FECHA_INGRESO_UCI)
  Mexico$TIENE_INTUBACION_ENDOTRAQUEAL = as.factor(Mexico$TIENE_INTUBACION_ENDOTRAQUEAL)
  summary(Mexico$TIENE_INTUBACION_ENDOTRAQUEAL)

classXXXV = filter(Mexico, TIPO_INFLUENZA_CONF1 %in% c(35))
  classXXXV$MOTIVO_EGRESO = as.factor(classXXXV$MOTIVO_EGRESO)
  summary(classXXXV$MOTIVO_EGRESO)

subclass_I= filter(classXXXV, MOTIVO_EGRESO %in% c(1)) #MEJORIA
summary(subclass_I$MOTIVO_EGRESO)
  
  subclass_I$PACIENTE = as.factor(subclass_I$PACIENTE)
  summary(subclass_I$PACIENTE)
  subclass_I$DOSIS_VAC_COVID19 = as.factor(subclass_I$DOSIS_VAC_COVID19)
  summary(subclass_I$DOSIS_VAC_COVID19)
  subclass_I$TIENE_INTUBACION_ENDOTRAQUEAL = as.factor(subclass_I$TIENE_INTUBACION_ENDOTRAQUEAL)
  summary(subclass_I$TIENE_INTUBACION_ENDOTRAQUEAL)
    
    subclass_I_none= filter(subclass_I, DOSIS_VAC_COVID19 %in% c(0)) # no vacunado
      summary(subclass_I_none$PACIENTE)
      summary(subclass_I_none$TIENE_INTUBACION_ENDOTRAQUEAL)
      
      I_none_edad = hist(subclass_I_none$EDAD_ANO)
      I_none_edad[["breaks"]]
      I_none_edad[["counts"]]
      stat.desc(subclass_I_none$EDAD_ANO)
      summary(subclass_I_none$EDAD_ANO)
      
    subclass_I_I= filter(subclass_I, DOSIS_VAC_COVID19 %in% c(1)) # una dosis
      summary(subclass_I_I$PACIENTE)
      summary(subclass_I_I$TIENE_INTUBACION_ENDOTRAQUEAL)
      subclass_I_I$MARCA_VAC_COVID19 = as.factor(subclass_I_I$MARCA_VAC_COVID19)
      summary(subclass_I_I$MARCA_VAC_COVID19)
      
    subclass_I_II= filter(subclass_I, DOSIS_VAC_COVID19 %in% c(2)) # dos dosis
      summary(subclass_I_II$PACIENTE)
      summary(subclass_I_II$TIENE_INTUBACION_ENDOTRAQUEAL)
      subclass_I_II$REF_VAC_COVID19 = as.factor(subclass_I_II$REF_VAC_COVID19)
      summary(subclass_I_II$REF_VAC_COVID19)
      subclass_I_II$MARCA_VAC_COVID19 = as.factor(subclass_I_II$MARCA_VAC_COVID19)
      summary(subclass_I_II$MARCA_VAC_COVID19)
      
      I_II_edad = hist(subclass_I_II$EDAD_ANO)
      I_II_edad[["breaks"]]
      I_II_edad[["counts"]]
      stat.desc(subclass_I_II$EDAD_ANO)
      summary(subclass_I_II$EDAD_ANO)
      
subclass_IV = filter(classXXXV, MOTIVO_EGRESO %in% c(4))
summary(subclass_IV$MOTIVO_EGRESO)
  subclass_IV$PACIENTE = as.factor(subclass_IV$PACIENTE)
  summary(subclass_IV$PACIENTE)
  subclass_IV$DOSIS_VAC_COVID19 = as.factor(subclass_IV$DOSIS_VAC_COVID19)
  summary(subclass_IV$DOSIS_VAC_COVID19)
  subclass_IV$TIENE_INTUBACION_ENDOTRAQUEAL = as.factor(subclass_IV$TIENE_INTUBACION_ENDOTRAQUEAL)
  summary(subclass_IV$TIENE_INTUBACION_ENDOTRAQUEAL)

    subclass_IV_none= filter(subclass_IV, DOSIS_VAC_COVID19 %in% c(0))
      summary(subclass_IV_none$PACIENTE)
      summary(subclass_IV_II$TIENE_INTUBACION_ENDOTRAQUEAL)
      
      IV_none_edad = hist(subclass_IV_none$EDAD_ANO)
      IV_none_edad[["breaks"]]
      IV_none_edad[["counts"]]
      stat.desc(subclass_IV_none$EDAD_ANO)
      summary(subclass_IV_none$EDAD_ANO)
      
    subclass_IV_I= filter(subclass_IV, DOSIS_VAC_COVID19 %in% c(1))
      summary(subclass_IV_I$PACIENTE)
      summary(subclass_IV_I$TIENE_INTUBACION_ENDOTRAQUEAL)
      
      subclass_IV_I$MARCA_VAC_COVID19 = as.factor(subclass_IV_I$MARCA_VAC_COVID19)
      summary(subclass_IV_I$MARCA_VAC_COVID19)
      
    subclass_IV_II= filter(subclass_IV, DOSIS_VAC_COVID19 %in% c(2))
      summary(subclass_IV_II$PACIENTE)
      summary(subclass_IV_II$TIENE_INTUBACION_ENDOTRAQUEAL)
      
      subclass_IV_II$MARCA_VAC_COVID19 = as.factor(subclass_IV_II$MARCA_VAC_COVID19)
      summary(subclass_IV_II$MARCA_VAC_COVID19)
      
      subclass_IV_II$REF_VAC_COVID19 = as.factor(subclass_IV_II$REF_VAC_COVID19)
      summary(subclass_IV_II$REF_VAC_COVID19)
      
      subclass_IV_II$SEXO = as.factor(subclass_IV_II$SEXO)
      summary(subclass_IV_II$SEXO)
      
      IV_II_edad = hist(subclass_IV_II$EDAD_ANO)
      IV_II_edad[["breaks"]]
      IV_II_edad[["counts"]]
      stat.desc(subclass_IV_II$EDAD_ANO)
        
        subclass_IV_II_REF_No = filter(subclass_IV_II, REF_VAC_COVID19 == "No")
        subclass_IV_II_REF_Si = filter(subclass_IV_II, REF_VAC_COVID19 == "Si")
        summary(subclass_IV_II_REF_No$SEXO)
        summary(subclass_IV_II_REF_Si$SEXO)

      