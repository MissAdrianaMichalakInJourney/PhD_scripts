
# KSS - by protocol - AM - 12.01.2022

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
library(lsmeans)
library(sjstats) # eta sq and Cohen F effect size


setwd("C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\C_KSS") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\C_KSS\\Lab_Sessions_KSS_Before_each_COGT_AM_14.09.2021.csv")

KSS_data <- data.frame('id'=data$ID,'age'=data$age, 'sex'=data$Sex_M_1_F_2,'years_of_education'=data$years_spent_in_education,
                       'protocol'= data$protocol_1_SD_2_MN, 'KSS_score'=data$KSS_score_before_COGT, 'hours_of_sleep_baseline'=data$TST_Baseline_min,
                       'SE_baseline'=data$SE_Baseline, 'session_number'=data$COGT_number, 'KSS_score_normalized'=data$normalized_KSS_score_before_COGT)

options(scipen=999)

KSS_data_ <- KSS_data %>%
  filter(!session_number==1)

###################################################################################################################################################################
# KSS - normalized

hist (KSS_data$KSS_score_normalized) 

KSS <- lmer(KSS_score_normalized  ~ as.factor(session_number) + age + sex + protocol + hours_of_sleep_baseline +
              protocol*session_number + (1|id), data=KSS_data_,  na.action = na.exclude)
            
anova (KSS)

eta_sq(KSS)

hist(resid(KSS))

shapiro.test(resid(KSS))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(KSS)

lms_KSS <- lsmeans(KSS, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_KSS

write.csv(lms_KSS, 'C:\\Users\\adria\\Desktop\\KSS\\LMS\\KSS_normalized_scores_by_protocol.csv', row.names=F)

###################################################################################################################################################################
# KSS - raw data

hist (KSS_data$KSS_score) 

KSS <- lmer(KSS_score  ~ as.factor(session_number) + age + sex  + protocol + hours_of_sleep_baseline +
              protocol*session_number + (1|id), data=KSS_data,  na.action = na.exclude)

anova (KSS)

eta_sq(KSS)

hist(resid(KSS))

shapiro.test(resid(KSS))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(KSS)

lms_KSS <- lsmeans(KSS, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_KSS

write.csv(lms_KSS, 'C:\\Users\\adria\\Desktop\\KSS\\LMS\\KSS_raw_scores_by_protocol.csv', row.names=F)


