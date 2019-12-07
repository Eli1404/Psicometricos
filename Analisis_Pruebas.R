rm (list = ls())
library(data.table)
library(tidyverse)
library(psych)

# Read psychometrical files -----------------------------------------------
temp = list.files(pattern="*.csv", recursive = T)
All = rbindlist(lapply(temp, fread), fill = TRUE)
All$condicion <- replace(as.character(All$condicion), All$condicion == "Ironía", "Ironia")

# Pschymetrical results ---------------------------------------------------
# SCL-90 ------------------------------------------------------------------
SCL <- All %>%
  group_by(participant) %>%
  filter(expName == "SCL-90", resp_react.response !="") %>%
  summarize(media = mean(resp_react.response, na.rm = T), D.T = sd(resp_react.response)) 
IG= round(rescale (SCL$media, mean = 50, sd = 10, df=T),1)
IGS <- data.frame(id = SCL$participant, SCL90 = IG$t.t.scale.x.....sd...mean.) 

# PEBL --------------------------------------------------------------------
GONOGO <- All %>%
  group_by(subnum) %>%
  filter(responded!= "", type !="practice") %>%
  rename(id = subnum) %>%
  summarize(gonogo = sum(corr))

TOL <- All %>%
  group_by(sub) %>%
  filter(success!= "") %>%
  rename(id = sub) %>%
  summarize(tol = sum(success))

DSPAN <- All %>%
  group_by(subnum) %>%
  filter(correct != "") %>%
  rename(id = subnum) %>%
  summarize(dspan = sum(correct))

NBACK <- All %>%
  group_by(subnum) %>%
  filter(corr1!= "") %>%
  rename(id = subnum) %>%
  summarize(nback = sum(corr1)+sum(corr2))

# WAIS - ICV and IRP ------------------------------------------------------
WAIS <- All %>%
  group_by(ID, prueba) %>%
  summarize(puntuacion = sum(puntuacion)) %>%
  spread(prueba, puntuacion) %>%
  rename(id = ID)

# RMET --------------------------------------------------------------------
RMET <- All %>%
  group_by(participant) %>%
  filter(expName == "rmet", key_resp_3.corr != "") %>%
  rename(id = participant) %>%
  summarize(rmet = sum(key_resp_3.corr))

# IRI ---------------------------------------------------------------------
IRI_p <- All %>%
  filter(expName == "IRI", Tipo == "Positivo") %>%
  group_by(participant, condicion) %>%
  summarise(pos = sum(resp_react.response))

IRI_n <- All %>%
  filter(expName == "IRI", Tipo == "Negativo") %>%
  group_by(participant, condicion) %>%
  summarise(neg = sum(resp_react.response))

IRI <- tibble(id = IRI_p$participant, 
              condition = IRI_p$condicion, 
              res = IRI_p$pos - IRI_n$neg) %>%
  spread(condition, res)

# AQ ----------------------------------------------------------------------
AQ_Task <- All %>%
  group_by(participant) %>%
  filter(expName == "AQ", key_resp_3.corr != "") %>%
  rename(id = participant) %>%
  summarize(AQ = sum(key_resp_3.corr)) 

# SSS ---------------------------------------------------------------------
SSS <- All %>%  
  group_by(participant, condicion) %>%
  filter(expName =="SSS", rating.response != "") %>%
  rename(id = participant) %>%
  summarize(SSS = round(mean(rating.response, na.rm = T), 2)) %>%
  spread(condicion, SSS) %>%
  mutate(SSS = sum(2:5))  

# MRI task  ---------------------------------------------------------------
Language_Task <- All %>%
  group_by(participant, condicion) %>%
  filter(key_resp_5.corr != "" & expName !="Prueba_1") %>%
  rename(id = participant) %>%
  summarize(Aciertos = sum(key_resp_5.corr, na.rm = T), Tiempo = round(mean(key_resp_5.rt, na.rm = T),2)) 
  gather(Aciertos, Tiempo, key = "variables", value = "valores")

postfRMI <- All %>%
  group_by(frase) %>%
  filter(expName =="Post_fMRI", rating.response != "") %>%
  summarize(postfMRI = round(mean(rating.response, na.rm = T), 2))

# Summary results ---------------------------------------------------------
Pruebas <- full_join(IGS, GONOGO, by = "id") %>%
  full_join(TOL, by = "id") %>%
  full_join(DSPAN, by = "id") %>%
  full_join(NBACK, by = "id") %>%
  full_join(WAIS, by = "id") %>%
  full_join(RMET, by = "id") %>%
  full_join(IRI, by = "id") %>%
  full_join(AQ_Task, by = "id") %>%
  full_join(SSS, by = "id")
