# NAPS - GLMM - AM - 18.12.2021

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


setwd("C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\B_Sleep_Staging") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\B_Sleep_Staging\\Sleep_analysis_Naps_AM_14.09.2021.csv")

data_frame <- data.frame('id'=data$ID,'age'=data$age, 'sex'=data$sex_M_1_2_F, 'genotype'=data$genotype_1_carrier_1_1_non_carrier_2 ,'nap_number'=data$nap_number, 
                         'hours_of_sleep_baseline'=data$baseline_h_of_sleep,'SE'=data$SE, 'TST'=data$TST_min, 
                          'Wake_duration'=data$WAKE_duration_min, 'N1_duration'=data$N1_duration_min, 'N2_duration'=data$N2_duration_min, 'N3_duration'=data$N3_duration_min,
                         'REM_duration'=data$REM_duration_min, 'N1_latency'=data$Latency_to_N1_min, 'N2_latency'=data$Latency_to_N2_min, 'N3_latency'=data$Latency_to_N3_min,
                         'REM_latency'=data$Latency_to_REM_min, 'SE'=data$SE, 'WASO'=data$WASO_min, 'SE_logit'=data$SE_logit,
                         'Wake_to_TIB_logit'=data$WAKE_duration_percentage_TIB_logit, 'N1_to_TIB_logit'=data$N1_duration_percentage_TIB_logit,
                         'N2_to_TIB_logit'=data$N2_duration_percentage_TIB_logit, 'N3_to_TIB_logit'=data$N3_duration_percentage_TIB_logit, 'REM_to_TIB_logit'=data$REM_duration_percentage_TIB_logit)

options(scipen=999)
                      
###################################################################################################################################################################
# Total Sleep time - TST

TST <- lmer(TST  ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (TST)
eta_sq(TST)

lsmeans_TST <- lsmeans(TST, ~ as.factor(nap_number)*as.factor(genotype))

lsmeans_TST 

hist(resid(TST))

write.csv(lsmeans_TST, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\TST.csv', row.names=F)

###################################################################################################################################################################
# SE

SE <- lmer(SE_logit  ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (SE)
eta_sq(SE)

hist(resid(SE))

lsmeans_SE <- lsmeans(SE, ~ nap_number*genotype)

lsmeans_SE 

write.csv(lsmeans_SE, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_SE.csv', row.names=F)

###################################################################################################################################################################
# WASO

WASO <- lmer(WASO   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (WASO)
eta_sq(WASO)

hist(resid(WASO))

lsmeans_WASO <- lsmeans(WASO, ~ nap_number*genotype)

lsmeans_WASO 

write.csv(lsmeans_WASO, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_WASO.csv', row.names=F)

###################################################################################################################################################################

# Wake duration

Wake_duration <- lmer(Wake_duration  ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova(Wake_duration)

eta_sq(Wake_duration)

hist(resid(Wake_duration))

lsmeans_Wake_duration <- lsmeans(Wake_duration, ~ nap_number*genotype)

lsmeans_Wake_duration 

write.csv(lsmeans_Wake_duration, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_Wake_duration.csv', row.names=F)

###################################################################################################################################################################
# N1 duration

N1_duration <- lmer(N1_duration   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (N1_duration)
eta_sq(N1_duration)

hist(resid(N1_duration))

lsmeans_N1_duration <- lsmeans(N1_duration, ~ as.factor(nap_number)*as.factor(genotype))

lsmeans_N1_duration 

write.csv(lsmeans_N1_duration, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N1_duration.csv', row.names=F)

###################################################################################################################################################################
# N2 duration

N2_duration <- lmer(N2_duration  ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (N2_duration)
eta_sq(N2_duration)

hist(resid(N2_duration))

lsmeans_N2_duration <- lsmeans(N2_duration, ~ nap_number*genotype)

lsmeans_N2_duration 

write.csv(lsmeans_N2_duration, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N2_duration.csv', row.names=F)

###################################################################################################################################################################
# N3 duration

N3_duration <- lmer(N3_duration  ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)


anova (N3_duration)
eta_sq(N3_duration)

hist(resid(N3_duration))

lsmeans_N3_duration <- lsmeans(N3_duration, ~ nap_number*genotype)

lsmeans_N3_duration 

write.csv(lsmeans_N3_duration, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N3_duration.csv', row.names=F)

###################################################################################################################################################################
# REM duration

REM_duration <- lmer(REM_duration   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (REM_duration)
eta_sq(REM_duration)

hist(resid(REM_duration))

lsmeans_REM_duration <- lsmeans(REM_duration, ~ nap_number*genotype)

lsmeans_REM_duration 

write.csv(lsmeans_REM_duration, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_REM_duration.csv', row.names=F)

###################################################################################################################################################################
# Latency
###################################################################################################################################################################
# N1 latency

N1_latency <- lmer(N1_latency   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (N1_latency)
eta_sq(N1_latency)

hist(resid(N1_latency))

lsmeans_N1_latency <- lsmeans(N1_latency, ~ nap_number*genotype)

lsmeans_N1_latency 

write.csv(lsmeans_N1_latency, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N1_latency.csv', row.names=F)

###################################################################################################################################################################
# N2 latency

N2_latency <- lmer(N2_latency  ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (N2_latency)
eta_sq(N2_latency)

hist(resid(N2_latency))

lsmeans_N2_latency <- lsmeans(N2_latency, ~ nap_number*genotype)

lsmeans_N2_latency 

write.csv(lsmeans_N2_latency, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N2_latency.csv', row.names=F)


###################################################################################################################################################################
# N3 latency

N3_latency <- lmer(N3_latency  ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (N3_latency)
eta_sq(N3_latency)

hist(resid(N3_latency))

lsmeans_N3_latency <- lsmeans(N3_latency, ~ nap_number*genotype)

lsmeans_N3_latency 

write.csv(lsmeans_N3_latency, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N3_latency.csv', row.names=F)

###################################################################################################################################################################
# REM latency

REM_latency <- lmer(REM_latency   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (REM_latency)
eta_sq(REM_latency)

hist(resid(REM_latency))

lsmeans_REM_latency <- lsmeans(REM_latency, ~ nap_number*genotype)

lsmeans_REM_latency 

write.csv(lsmeans_REM_latency, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_REM_latency.csv', row.names=F)

###################################################################################################################################################################
# percentage of TIB
###################################################################################################################################################################
# WAKE to TIB

WAKE_to_TIB <- lmer(Wake_to_TIB_logit   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (WAKE_to_TIB)
eta_sq(WAKE_to_TIB)

hist(resid(WAKE_to_TIB))

lsmeans_WAKE_to_TIB <- lsmeans(WAKE_to_TIB, ~ nap_number*genotype)

lsmeans_WAKE_to_TIB 

write.csv(lsmeans_WAKE_to_TIB, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_WAKE_to_TIB.csv', row.names=F)


###################################################################################################################################################################
# WAKE to TIB

N1_to_TIB <- lmer(N1_to_TIB_logit   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (N1_to_TIB)
eta_sq(N1_to_TIB)

hist(resid(N1_to_TIB))

lsmeans_N1_to_TIB <- lsmeans(N1_to_TIB, ~ nap_number*genotype)

lsmeans_N1_to_TIB 

write.csv(lsmeans_N1_to_TIB, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N1_to_TIB.csv', row.names=F)

###################################################################################################################################################################
# WAKE to TIB

N2_to_TIB <- lmer(N2_to_TIB_logit   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (N2_to_TIB)
eta_sq(N2_to_TIB)

hist(resid(N2_to_TIB))

lsmeans_N2_to_TIB <- lsmeans(N2_to_TIB, ~ nap_number*genotype)

lsmeans_N2_to_TIB 

write.csv(lsmeans_N2_to_TIB, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N2_to_TIB.csv', row.names=F)


###################################################################################################################################################################
# WAKE to TIB

N3_to_TIB <- lmer(N3_to_TIB_logit   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (N3_to_TIB)
eta_sq(N3_to_TIB)

hist(resid(N3_to_TIB))

lsmeans_N3_to_TIB <- lsmeans(N3_to_TIB, ~ nap_number*genotype)

lsmeans_N3_to_TIB 

write.csv(lsmeans_N3_to_TIB, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_N3_to_TIB.csv', row.names=F)

###################################################################################################################################################################
# WAKE to TIB

REM_to_TIB <- lmer(REM_to_TIB_logit   ~  as.factor(nap_number) + age + sex + genotype + hours_of_sleep_baseline + nap_number*genotype + (1|id), data=data_frame,  na.action = na.exclude)

anova (REM_to_TIB)
eta_sq(REM_to_TIB)

hist(resid(REM_to_TIB))

lsmeans_REM_to_TIB <- lsmeans(REM_to_TIB, ~ nap_number*genotype)

lsmeans_REM_to_TIB 

write.csv(lsmeans_REM_to_TIB, 'C:\\Users\\adria\\Desktop\\NAPS\\LMS\\lsmeans_REM_to_TIB.csv', row.names=F)

