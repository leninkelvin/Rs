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
#       XXXV_I_IV SUBGROUP WITH TWO DOSES
#
#   XXXV_IV MOTIVO_EGRESO DEFUNCION
#     XXXV_IV_none SUBGROUP WITHOUT VACCINE
#     XXXV_IV_I SUBGROUP WITH ONE DOSE
#     XXXV_IV_IV SUBGROUP WITH TWO DOSES
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

Mexico$FECHA_INICIO_CUADRO_CLINICO = as.Date(Mexico$FECHA_INICIO_CUADRO_CLINICO, "%d/%m/%Y") 
# SUPER IMPORTANTE PARA TENER BIEN LAS FECHAS

Mex_REF_si= filter(Mexico, REF_VAC_COVID19 == "Si")
Mex_REF_no= filter(Mexico, REF_VAC_COVID19 == "No")
summary(Mex_REF_si$REF_VAC_COVID19)
Mex_REF_si$REF_VAC_MARCA = as.factor(Mex_REF_si$REF_VAC_MARCA)
summary(Mex_REF_si$REF_VAC_MARCA)
summary(Mex_REF_no$REF_VAC_COVID19)


XXXV = filter(Mexico, TIPO_INFLUENZA_CONF1 %in% c(35)) # Influenza cases, first filter
XXXV$MOTIVO_EGRESO = as.factor(XXXV$MOTIVO_EGRESO) # needed for statistics
  summary(XXXV$MOTIVO_EGRESO)
  stat.desc(XXXV$MOTIVO_EGRESO) # descriptive statistics 
  
  other = filter(Mexico, TIPO_INFLUENZA_CONF1 != 35)
  
  # MORE DATA EXTRACTION
  XXXV_P_Amb = filter(XXXV, PACIENTE == "AMBULATORIO")					# 
  XXXV_P_Hosp = filter(XXXV, PACIENTE == "HOSPITALIZADO") 				#
	XXXV_P_Amb_none= filter(XXXV_P_Amb, DOSIS_VAC_COVID19 %in% c(0))	# 
	XXXV_P_Amb_I= filter(XXXV_P_Amb, DOSIS_VAC_COVID19 %in% c(1))		# 
	XXXV_P_Amb_II= filter(XXXV_P_Amb, DOSIS_VAC_COVID19 %in% c(2))		# 
	XXXV_P_Hosp_none= filter(XXXV_P_Hosp, DOSIS_VAC_COVID19 %in% c(0))	# 
	XXXV_P_Hosp_I= filter(XXXV_P_Hosp, DOSIS_VAC_COVID19 %in% c(1)) 	# 
	XXXV_P_Hosp_II= filter(XXXV_P_Hosp, DOSIS_VAC_COVID19 %in% c(2)) 	#
	
#XXXV_PACIENTES = data.frame(count(XXXV_P_Amb_none), count(XXXV_P_Amb_I), count(XXXV_P_Amb_II), count(XXXV_P_Hosp_none), count(XXXV_P_Hosp_I), count(XXXV_P_Hosp_II))

names(XXXV_PACIENTES)[1] = "35 Amb sinV" 
names(XXXV_PACIENTES)[2] = "35 Amb 1V" 
names(XXXV_PACIENTES)[3] = "35 Amb 2V"   
names(XXXV_PACIENTES)[4] = "35 Hosp sinV" 
names(XXXV_PACIENTES)[5] = "35 Hosp 1V" 
names(XXXV_PACIENTES)[6] = "35 Hops 2V" 

write.table(XXXV_PACIENTES, file = "XXXV_PACIENTES.cvs", sep = "\t", row.names = F)

  Other_P_Amb = filter(other, PACIENTE == "AMBULATORIO") 				# 
  Other_P_Hosp = filter(other, PACIENTE == "HOSPITALIZADO")				#
	Other_P_Amb_none= filter(Other_P_Amb, DOSIS_VAC_COVID19 %in% c(0)) 	# 
	Other_P_Amb_I= filter(Other_P_Amb, DOSIS_VAC_COVID19 %in% c(1))		# 
	Other_P_Amb_II= filter(Other_P_Amb, DOSIS_VAC_COVID19 %in% c(2))	# 
	Other_P_Hosp_none= filter(Other_P_Hosp, DOSIS_VAC_COVID19 %in% c(0))# 
	Other_P_Hosp_I= filter(Other_P_Hosp, DOSIS_VAC_COVID19 %in% c(1)) 	# 
	Other_P_Hosp_II= filter(Other_P_Hosp, DOSIS_VAC_COVID19 %in% c(2)) 	#  

#Other_PACIENTES = data.frame(count(Other_P_Amb_none), count(Other_P_Amb_I), count(Other_P_Amb_II), count(Other_P_Hosp_none), count(Other_P_Hosp_I), count(Other_P_Hosp_II))

names(Other_PACIENTES)[1] = "not35 Amb sinV" 
names(Other_PACIENTES)[2] = "not35 Amb 1V" 
names(Other_PACIENTES)[3] = "not35 Amb 2V"   
names(Other_PACIENTES)[4] = "not35 Hosp sinV" 
names(Other_PACIENTES)[5] = "not35 Hosp 1V" 
names(Other_PACIENTES)[6] = "not35 Hops 2V" 
 
write.table(Other_PACIENTES, file = "Other_PACIENTES.cvs", sep = "\t", row.names = F) 

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
    
      REF_VAC_EG_POS_other_2_si= filter(VAC_EG_POS_other_2, REF_VAC_COVID19 == "Si")
      REF_VAC_EG_POS_other_2_no= filter(VAC_EG_POS_other_2, REF_VAC_COVID19 == "No")
      summary(REF_VAC_EG_POS_other_2_si$REF_VAC_COVID19)
      summary(REF_VAC_EG_POS_other_2_no$REF_VAC_COVID19)
    
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

# Mexico$FECHA_INICIO_CUADRO_CLINICO = as.Date(Mexico$FECHA_INICIO_CUADRO_CLINICO, "%d/%m/%Y") 
# SUPER IMPORTANTE PARA TENER BIEN LAS FECHAS
  
  # Now, to separate by ages XXXV IV 
  
  aaA = filter(XXXV_IV_none, EDAD_ANO <= 19)
  aaB = filter(XXXV_IV_none, EDAD_ANO >= 20 & EDAD_ANO <= 39)
  aaC = filter(XXXV_IV_none, EDAD_ANO >= 40 & EDAD_ANO <= 59)
  aaD = filter(XXXV_IV_none, EDAD_ANO >= 60)
  
  bbA = filter(XXXV_IV_I, EDAD_ANO <= 19)
  bbB = filter(XXXV_IV_I, EDAD_ANO >= 20 & EDAD_ANO <= 39)
  bbC = filter(XXXV_IV_I, EDAD_ANO >= 40 & EDAD_ANO <= 59)
  bbD = filter(XXXV_IV_I, EDAD_ANO >= 60)
  
  ccA = filter(XXXV_IV_II, EDAD_ANO <= 19)
  ccB = filter(XXXV_IV_II, EDAD_ANO >= 20 & EDAD_ANO <= 39)
  ccC = filter(XXXV_IV_II, EDAD_ANO >= 40 & EDAD_ANO <= 59)
  ccD = filter(XXXV_IV_II, EDAD_ANO >= 60)
  
  # Now, to separate by ages NOT XXXV IV + POSITIVE
  
  ddA = filter(POS_other_IV_none, EDAD_ANO <= 19)
  ddB = filter(POS_other_IV_none, EDAD_ANO >= 20 & EDAD_ANO <= 39)
  ddC = filter(POS_other_IV_none, EDAD_ANO >= 40 & EDAD_ANO <= 59)
  ddD = filter(POS_other_IV_none, EDAD_ANO >= 60)
  
  eeA = filter(POS_other_IV_1, EDAD_ANO <= 19)
  eeB = filter(POS_other_IV_1, EDAD_ANO >= 20 & EDAD_ANO <= 39)
  eeC = filter(POS_other_IV_1, EDAD_ANO >= 40 & EDAD_ANO <= 59)
  eeD = filter(POS_other_IV_1, EDAD_ANO >= 60)
  
  ffA = filter(POS_other_IV_2, EDAD_ANO <= 19)
  ffB = filter(POS_other_IV_2, EDAD_ANO >= 20 & EDAD_ANO <= 39)
  ffC = filter(POS_other_IV_2, EDAD_ANO >= 40 & EDAD_ANO <= 59)
  ffD = filter(POS_other_IV_2, EDAD_ANO >= 60)
  
  DateI = '2020-01-01' # Starting date for the data extraction
  DateF = '2022-03-01' # Final date for data extraction

  #Week I
  
  DateN = as.Date(DateI) + 6
  
  # XXXV IV no vac
  XXXV_IV_none_DATES_a = aaA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #none age group A
  XXXV_IV_none_DATES_b = aaB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #none age group B
  XXXV_IV_none_DATES_c = aaC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #none age group C
  XXXV_IV_none_DATES_d = aaD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #none age group D
  
  XXXV_IV_none_XXX_a = data.frame(as.Date(DateI), count(XXXV_IV_none_DATES_a), count(XXXV_IV_none_DATES_b), count(XXXV_IV_none_DATES_c), count(XXXV_IV_none_DATES_d))
  
  # XXXV IV 1 vac
  XXXV_IV_I_DATES_a = bbA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group A
  XXXV_IV_I_DATES_b = bbB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group B
  XXXV_IV_I_DATES_c = bbC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group C
  XXXV_IV_I_DATES_d = bbD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group D
  
  XXXV_IV_I_XXX_a = data.frame(as.Date(DateI), count(XXXV_IV_I_DATES_a), count(XXXV_IV_I_DATES_b), count(XXXV_IV_I_DATES_c), count(XXXV_IV_I_DATES_d))
  
  # XXXV IV 2 vac
  
  XXXV_IV_II_DATES_a = ccA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #2 vac age group A
  XXXV_IV_II_DATES_b = ccB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #2 vac age group B
  XXXV_IV_II_DATES_c = ccC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #2 vac age group C
  XXXV_IV_II_DATES_d = ccD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #2 vac age group D
  
  XXXV_IV_II_XXX_a = data.frame(as.Date(DateI), count(XXXV_IV_II_DATES_a), count(XXXV_IV_II_DATES_b), count(XXXV_IV_II_DATES_c), count(XXXV_IV_II_DATES_d))
  
  #  NOT XXXV + POSITIVE IV no vac
  
  POS_other_IV_none_DATES_a = ddA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #none age group A
  POS_other_IV_none_DATES_b = ddB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #none age group B
  POS_other_IV_none_DATES_c = ddC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #none age group C
  POS_other_IV_none_DATES_d = ddD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #none age group D
  
  POS_other_IV_none_XXX_a = data.frame(as.Date(DateI), count(POS_other_IV_none_DATES_a), count(POS_other_IV_none_DATES_a), count(POS_other_IV_none_DATES_a), count(POS_other_IV_none_DATES_a))
  
  #  NOT XXXV + POSITIVE  IV 1 vac
  
  POS_other_IV_I_DATES_a = eeA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group A
  POS_other_IV_I_DATES_b = eeB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group B
  POS_other_IV_I_DATES_c = eeC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group C
  POS_other_IV_I_DATES_d = eeD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group D
  
  POS_other_IV_I_XXX_a = data.frame(as.Date(DateI), count(POS_other_IV_I_DATES_a), count(POS_other_IV_I_DATES_b), count(POS_other_IV_I_DATES_c), count(POS_other_IV_I_DATES_d))
  
  #  NOT XXXV + POSITIVE IV 2 vac
  
  POS_other_IV_II_DATES_a = ffA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #2 vac age group A
  POS_other_IV_II_DATES_b = ffB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #2 vac age group B
  POS_other_IV_II_DATES_c = ffC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #2 vac age group C
  POS_other_IV_II_DATES_d = ffD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
  #1 vac age group D
  
  POS_other_IV_II_XXX_a = data.frame(as.Date(DateI), count(POS_other_IV_II_DATES_a), count(POS_other_IV_II_DATES_b), count(POS_other_IV_II_DATES_c), count(POS_other_IV_II_DATES_d))
  
  while (DateI < DateF) {
    
    #Week II
    
    DateI = as.Date(DateN) + 1
    DateN = as.Date(DateI) + 6
    
    # XXXV IV no vac
    XXXV_IV_none_DATES_a = aaA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #none age group A
    XXXV_IV_none_DATES_b = aaB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))    
    #none age group B
    XXXV_IV_none_DATES_c = aaC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #none age group C
    XXXV_IV_none_DATES_d = aaD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #none age group D
    XXXV_IV_none_XXX_aP = data.frame(as.Date(DateI), count(XXXV_IV_none_DATES_a), count(XXXV_IV_none_DATES_b), count(XXXV_IV_none_DATES_c), count(XXXV_IV_none_DATES_d))
    XXXV_IV_none_XXX_a = rbind(XXXV_IV_none_XXX_a, XXXV_IV_none_XXX_aP) # adding observations to dataframe

    # XXXV IV 1 vac
    
    XXXV_IV_I_DATES_a = bbA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #1 age group A
    XXXV_IV_I_DATES_b = bbB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #1 age group B
    XXXV_IV_I_DATES_c = bbC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #1 age group C
    XXXV_IV_I_DATES_d = bbD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #1 age group D    
    XXXV_IV_I_XXX_aP = data.frame(as.Date(DateI), count(XXXV_IV_I_DATES_a), count(XXXV_IV_I_DATES_b), count(XXXV_IV_I_DATES_c), count(XXXV_IV_I_DATES_d))
    XXXV_IV_I_XXX_a = rbind(XXXV_IV_I_XXX_a, XXXV_IV_I_XXX_aP) # adding observations to dataframe
    
    # XXXV IV 2 vac
    XXXV_IV_II_DATES_a = ccA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #2 age group A
    XXXV_IV_II_DATES_b = ccB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #2 age group B
    XXXV_IV_II_DATES_c = ccC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))    
    #2 age group C
    XXXV_IV_II_DATES_d = ccD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #2 age group D   
    
    XXXV_IV_II_XXX_aP = data.frame(as.Date(DateI), count(XXXV_IV_II_DATES_a), count(XXXV_IV_II_DATES_b), count(XXXV_IV_II_DATES_c), count(XXXV_IV_II_DATES_d))
    XXXV_IV_II_XXX_a = rbind(XXXV_IV_II_XXX_a, XXXV_IV_II_XXX_aP) # adding observations to dataframe

    # NOT XXXV + POSITIVE IV no vac
    
    POS_other_IV_none_DATES_a = ddA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #none age group A
    POS_other_IV_none_DATES_b = ddB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))    
    #none age group B
    POS_other_IV_none_DATES_c = ddC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))    
    #none age group C
    POS_other_IV_none_DATES_d = ddD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))    
    #none age group D
    
    POS_other_IV_none_XXX_aP = data.frame(as.Date(DateI), count(POS_other_IV_none_DATES_a), count(POS_other_IV_none_DATES_b), count(POS_other_IV_none_DATES_c), count(POS_other_IV_none_DATES_b))
    POS_other_IV_none_XXX_a = rbind(POS_other_IV_none_XXX_a ,POS_other_IV_none_XXX_aP) # adding observations to dataframe

    # XXXV IV 1 vac
    
    POS_other_IV_I_DATES_a = eeA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #1 age group A
    POS_other_IV_I_DATES_b = eeB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #1 age group B
    POS_other_I_DATES_c = eeC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #1 age group C
    POS_other_I_DATES_d = eeD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #1 age group D    
    
    POS_other_IV_I_XXX_aP = data.frame(as.Date(DateI), count(POS_other_IV_I_DATES_a), count(POS_other_IV_I_DATES_b), count(POS_other_IV_I_DATES_c), count(POS_other_IV_I_DATES_d))
    POS_other_IV_I_XXX_a = rbind(POS_other_IV_I_XXX_a, POS_other_IV_I_XXX_aP) # adding observations to dataframe
    
    # XXXV IV 2 vac
    
    POS_other_II_DATES_a = ffA %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #2 age group A
    POS_other_II_DATES_b = ffB %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #2 age group B
    POS_other_II_DATES_c = ffC %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))    
    #2 age group C
    POS_other_II_DATES_d = ffD %>% filter(FECHA_INICIO_CUADRO_CLINICO <= as.Date(DateN) & FECHA_INICIO_CUADRO_CLINICO >= as.Date(DateI))
    #2 age group D
    
    
    POS_other_IV_II_XXX_aP = data.frame(as.Date(DateI), count(POS_other_II_DATES_a), count(POS_other_II_DATES_b), count(POS_other_II_DATES_c), count(POS_other_II_DATES_d))
    POS_other_IV_II_XXX_a = rbind(POS_other_IV_II_XXX_a, POS_other_IV_II_XXX_aP) # adding observations to dataframe

  }
  
  names(POS_other_IV_none_XXX_a)[1] = "FECHA_INICIO_CUADRO_CLINICO" 
  names(POS_other_IV_none_XXX_a)[2] = "A" 
  names(POS_other_IV_none_XXX_a)[3] = "B"   
  names(POS_other_IV_none_XXX_a)[4] = "C" 
  names(POS_other_IV_none_XXX_a)[5] = "D" 
  
  names(POS_other_IV_I_XXX_a)[1] = "FECHA_INICIO_CUADRO_CLINICO" 
  names(POS_other_IV_I_XXX_a)[2] = "A" 
  names(POS_other_IV_I_XXX_a)[3] = "B"   
  names(POS_other_IV_I_XXX_a)[4] = "C" 
  names(POS_other_IV_I_XXX_a)[5] = "D" 
  
  names(POS_other_IV_II_XXX_a)[1] = "FECHA_INICIO_CUADRO_CLINICO" 
  names(POS_other_IV_II_XXX_a)[2] = "A" 
  names(POS_other_IV_II_XXX_a)[3] = "B"   
  names(POS_other_IV_II_XXX_a)[4] = "C" 
  names(POS_other_IV_II_XXX_a)[5] = "D" 
  
  names(XXXV_IV_none_XXX_a)[1] = "FECHA_INICIO_CUADRO_CLINICO" 
  names(XXXV_IV_none_XXX_a)[2] = "A" 
  names(XXXV_IV_none_XXX_a)[3] = "B"   
  names(XXXV_IV_none_XXX_a)[4] = "C" 
  names(XXXV_IV_none_XXX_a)[5] = "D" 
  
  names(XXXV_IV_I_XXX_a)[1] = "FECHA_INICIO_CUADRO_CLINICO" 
  names(XXXV_IV_I_XXX_a)[2] = "A" 
  names(XXXV_IV_I_XXX_a)[3] = "B"   
  names(XXXV_IV_I_XXX_a)[4] = "C"
  names(XXXV_IV_I_XXX_a)[5] = "D"

  names(XXXV_IV_II_XXX_a)[1] = "FECHA_INICIO_CUADRO_CLINICO" 
  names(XXXV_IV_II_XXX_a)[2] = "A" 
  names(XXXV_IV_II_XXX_a)[3] = "B"   
  names(XXXV_IV_II_XXX_a)[4] = "C" 
  names(XXXV_IV_II_XXX_a)[5] = "D" 
  
  
  write.table(XXXV_IV_none_XXX_a, file = "XXXV_IV_none_XXX_a.cvs", sep = "\t", row.names = F)
  write.table(XXXV_IV_I_XXX_a, file = "XXXV_IV_I_XXX_a.cvs", sep = "\t", row.names = F)
  write.table(XXXV_IV_II_XXX_a, file = "XXXV_IV_II_XXX_a.cvs", sep = "\t", row.names = F)
  write.table(POS_other_IV_none_XXX_a, file = "POS_other_IV_none_XXX_a.cvs", sep = "\t", row.names = F)
  write.table(POS_other_IV_I_XXX_a, file = "POS_other_IV_I_XXX_a.cvs", sep = "\t", row.names = F)
  write.table(POS_other_IV_II_XXX_a, file = "POS_other_IV_II_XXX_a.cvs", sep = "\t", row.names = F)
  
  proc.time() - ptm # Code to get the final time of the process and display the total time.
  
  system("say Just finished!")
  
#Mexico$FECHA_INGRESO_UCI = as.factor(Mexico$FECHA_INGRESO_UCI) # Mexico is all the DB.
#  summary(Mexico$FECHA_INGRESO_UCI)
#Mexico$TIENE_INTUBACION_ENDOTRAQUEAL = as.factor(Mexico$TIENE_INTUBACION_ENDOTRAQUEAL)
#  summary(Mexico$TIENE_INTUBACION_ENDOTRAQUEAL)