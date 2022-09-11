
# MELATONIN - mixed models - AM - 08.12.2021

library ("nlme")
library("lme4")
library("lmerTest")
library("ggplot2")
library("dplyr")
library("MuMIn")
library("rcompanion")
library("performance")
library("Rmisc") 
library("sjPlot")
library("sjmisc")
library("sjlabelled")
library("MASS")
library ("car")
library("rcompanion")
library(tidyverse)
library(dplyr)
library(lsmeans)
library(sjstats) # eta sq and Cohen F effect size

setwd("C:\\Users\\adria\\Desktop") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\Melatonin_and_demographics_AM_08.12.2021.csv")

data_framee <- data.frame('id'=data$ID,'age'=data$age, 'sex'=data$sex_M_1_2_F, 'genotype'=data$genotype_1_carrier_1_1_non_carrier_2, 'sample_number'=data$Sample_number, 
                          'melatonin'=data$Melatonin_pg_mL, 'protocol'=data$protocol_1_SD_2_MN, 'time'=data$Time_Taken)

data_frame <- na.exclude(data_framee)

options(scipen=999)        

###################################################################################################################################################################
# Melatonin

hist(data_frame$melatonin)

melatonin <- lmer(melatonin ~ as.factor(sample_number) + age + sex + genotype + protocol  + (1|id), data=data_frame,  na.action = na.exclude)

hist(resid(melatonin))

anova (melatonin)

lsmeans_melatonin <- lsmeans(melatonin, ~sample_number)

lsmeans_melatonin 

write.csv(lsmeans_melatonin, 'C:\\Users\\adria\\Desktop\\melatonin.csv', row.names=F)
