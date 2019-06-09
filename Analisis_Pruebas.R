rm (list = ls())
library(data.table)
library(tidyverse)

temp = list.files(pattern="*.csv", recursive = T)
All = rbindlist(lapply(temp, fread), fill = TRUE)
attach(All)

AQ_Task <- All %>%
  group_by(participant) %>%
  filter(expName == "AQ", key_resp_3.corr != "") %>%
  rename(ID = participant) %>%
  summarize(AQ = sum(key_resp_3.corr))

RMET <- All %>%
  group_by(participant) %>%
  filter(expName == "rmet", key_resp_3.corr != "") %>%
  rename(ID = participant) %>%
  summarize(rmet = sum(key_resp_3.corr))

DSPAN <- All %>%
  group_by(subnum) %>%
  filter(correct != "") %>%
  rename(ID = subnum) %>%
  summarize(dspan = sum(correct))

GONOGO <- All %>%
  group_by(subnum) %>%
  filter(responded!= "", type !="practice") %>%
  rename(ID = subnum) %>%
  summarize(gonogo = sum(corr))

NBACK <- All %>%
  group_by(subnum) %>%
  filter(corr1!= "") %>%
  rename(ID = subnum) %>%
  summarize(nback = sum(corr1)+sum(corr2))

TOL <- All %>%
  group_by(sub) %>%
  filter(success!= "") %>%
  rename(ID = sub) %>%
  summarize(tol = sum(success))

WAIS <- All %>%
  group_by(prueba, ID) %>%
  summarize(SE = sum(puntuacion), VB = sum(puntuacion), IN = sum(puntuacion),
            DC = sum(puntuacion), MT = sum(puntuacion), RV = sum(puntuacion))

Pruebas <- full_join(GONOGO, TOL, by = "ID") %>%
  full_join(DSPAN, by = "ID") %>%
  full_join(NBACK, by = "ID") %>%
  full_join(WAIS, by = "ID") %>%
  full_join(RMET, by = "ID") %>%
  full_join(AQ_Task, by = "ID")