
# SHQ - flare  models - by protocol - AM - 14.01.2022

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

setwd("C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\D_SHQ") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\D_SHQ\\SHQ_databases_AM_31.07.2021\\SHQ_outcomes_Flares_EASY_HARD_Final_AM_16.12.2021.csv")

data_SHQ <- data.frame('id'=data$Participant_ID,'age'=data$age, 'sex'=data$Sex_M_1_F_2,
                       'protocol'= data$protocol_1_SD_2_MN, 'session_number'=data$session_number, 'level_ID'=data$level_ID, 'level_1_easy_2_hard'=data$easy_level_1_hard_2,
                       'flare_ACC'=data$flare_accuracy, 'time_needed_to_complete'=data$time_needed_to_complete, 'years_of_education'=data$years_spent_in_education,
                       'hours_of_sleep_baseline'=data$TST_Baseline_min, 'flare_ACC_normalized'=data$normalized_flare_accuracy, 'normalized_time'=data$normalized_time_needed_to_complete)



flare_easy <-data_SHQ %>%
  filter(level_1_easy_2_hard==1) 

flare_easy

flare_hard <-data_SHQ %>%
  filter(level_1_easy_2_hard==2) 

flare_hard

options(scipen=999)

data_SHQ_1 <- data_SHQ %>%
  filter(!session_number==1)

flare_easy <-data_SHQ_1 %>%
  filter(level_1_easy_2_hard==1) 

flare_easy

flare_hard <-data_SHQ_1 %>%
  filter(level_1_easy_2_hard==2) 

flare_hard

#####################################################################################################

hist (data_SHQ$flare_ACC) 

lmer_flare_ACC_raw <- lmer(flare_ACC ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                         protocol*session_number  +(1|id), data=data_SHQ,  na.action = na.exclude)

anova (lmer_flare_ACC_raw)

eta_sq(lmer_flare_ACC_raw)

hist(resid(lmer_flare_ACC_raw))

shapiro.test(resid(lmer_flare_ACC_raw))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_flare_ACC_raw)

lms_flare_ACC_raw <- lsmeans(lmer_flare_ACC_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_ACC_raw

write.csv(lms_flare_ACC_raw, 'C:\\Users\\adria\\Desktop\\SHQ_lms_flare_ACC_easy_hard_raw.csv', row.names=F)

#####################################################################################################

hist (data_SHQ$time_needed_to_complete) 

lmer_time_needed_to_complete_raw <- lmer(log10(time_needed_to_complete) ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                                       protocol*session_number +(1|id),data=data_SHQ,  na.action = na.exclude)

anova (lmer_time_needed_to_complete_raw)

eta_sq(lmer_time_needed_to_complete_raw)

hist(resid(lmer_time_needed_to_complete_raw))

shapiro.test(resid(lmer_time_needed_to_complete_raw))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_time_needed_to_complete_raw)

lms_flare_time_needed_to_complete_raw <- lsmeans(lmer_time_needed_to_complete_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_time_needed_to_complete_raw 

write.csv(lms_flare_time_needed_to_complete_raw , 'C:\\Users\\adria\\Desktop\\SHQ_FLARE_lmer_time_needed_to_complete_easy_hard_raw.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

##########################################################################################################################################################################################################
# FLARE EASY

hist (data_SHQ$flare_ACC) 

lmer_flare_easy_ACC_raw <- lmer((flare_ACC)^3 ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                         protocol*session_number  +(1|id), data=flare_easy,  na.action = na.exclude)

anova (lmer_flare_easy_ACC_raw)

eta_sq(lmer_flare_easy_ACC_raw)

hist(resid(lmer_flare_easy_ACC_raw))

shapiro.test(resid(lmer_flare_easy_ACC_raw))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_flare_ACC_raw)

lms_flare_easy_ACC_raw <- lsmeans(lmer_flare_easy_ACC_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_easy_ACC_raw

write.csv(lms_flare_easy_ACC_raw, 'C:\\Users\\adria\\Desktop\\SHQ_lms_flare_ACC_easy_raw.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

#####################################################################################################

hist (data_SHQ$time_needed_to_complete) 

lmer_time_needed_to_complete_easy_raw <- lmer(log10(time_needed_to_complete) ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                                       protocol*session_number +(1|id), data=flare_easy,  na.action = na.exclude)

anova (lmer_time_needed_to_complete_easy_raw)

eta_sq(lmer_time_needed_to_complete_easy_raw)

hist(resid(lmer_time_needed_to_complete_easy_raw))

shapiro.test(resid(lmer_time_needed_to_complete_easy_raw))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_time_needed_to_complete_easy_raw)

lms_flare_time_needed_to_complete_easy_raw <- lsmeans(lmer_time_needed_to_complete_easy_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_time_needed_to_complete_easy_raw

write.csv(lms_flare_time_needed_to_complete_easy_raw, 'C:\\Users\\adria\\Desktop\\SHQ_FLARE_lmer_time_needed_to_complete_easy_raw.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

#####################################################################################################
# FLARE HARD 

hist (data_SHQ$flare_ACC) 

lmer_flare_ACC_hard_raw <- lmer(flare_ACC ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                         protocol*session_number +(1|id), data=flare_hard,  na.action = na.exclude)

anova (lmer_flare_ACC_hard_raw)

eta_sq(lmer_flare_ACC_hard_raw)

hist(resid(lmer_flare_ACC_hard_raw))

shapiro.test(resid(lmer_flare_ACC_hard_raw))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_flare_ACC_hard_raw)

lms_flare_ACC_hard_raw <- lsmeans(lmer_flare_ACC_hard_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_ACC_hard_raw

write.csv(lms_flare_ACC_hard_raw, 'C:\\Users\\adria\\Desktop\\SHQ_lms_flare_ACC_hard_raw.csv', row.names=F)

#####################################################################################################

hist (data_SHQ$time_needed_to_complete) 

lmer_time_needed_to_complete_hard_raw <- lmer(log10(time_needed_to_complete) ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                       protocol*session_number +(1|id), data=flare_hard,  na.action = na.exclude)

anova (lmer_time_needed_to_complete_hard_raw)

eta_sq(lmer_time_needed_to_complete_hard_raw)

hist(resid(lmer_time_needed_to_complete_hard_raw))

shapiro.test(resid(lmer_time_needed_to_complete_hard_raw))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_time_needed_to_complete_hard_raw)

lms_time_needed_to_complete_hard_raw <- lsmeans(lmer_time_needed_to_complete_hard_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_time_needed_to_complete_hard_raw

write.csv(lms_time_needed_to_complete_hard_raw, 'C:\\Users\\adria\\Desktop\\SHQ_FLARE_lmer_time_needed_to_complete_hard_raw.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

#####################################################################################################
# NORMALIZED
#####################################################################################################

hist (data_SHQ$flare_ACC_normalized) 

lmer_flare_ACC_normalized <- lmer(flare_ACC_normalized ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                             protocol*session_number +(1|id), data=data_SHQ_1,  na.action = na.exclude)

anova (lmer_flare_ACC_normalized)

eta_sq(lmer_flare_ACC_normalized)

hist(resid(lmer_flare_ACC_normalized))

shapiro.test(resid(lmer_flare_ACC_normalized))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_flare_ACC_normalized)

lms_flare_ACC_normalized <- lsmeans(lmer_flare_ACC_normalized, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_ACC_normalized

write.csv(lms_flare_ACC_normalized, 'C:\\Users\\adria\\Desktop\\SHQ_lms_flare_ACC_easy_hard_normalized.csv', row.names=F)

#####################################################################################################

hist (data_SHQ$normalized_time) 

lmer_time_needed_to_complete_normalized <- lmer(log10(normalized_time) ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                                           protocol*session_number +(1|id),data=data_SHQ_1,  na.action = na.exclude)

anova (lmer_time_needed_to_complete_normalized)

eta_sq(lmer_time_needed_to_complete_normalized)

hist(resid(lmer_time_needed_to_complete_normalized))

shapiro.test(resid(lmer_time_needed_to_complete_normalized))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_time_needed_to_complete_normalized)

lms_flare_time_needed_to_complete_normalized <- lsmeans(lmer_time_needed_to_complete_normalized, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_time_needed_to_complete_normalized 

write.csv(lms_flare_time_needed_to_complete_normalized , 'C:\\Users\\adria\\Desktop\\SHQ_FLARE_lmer_time_needed_to_complete_easy_hard_normalized.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)


##########################################################################################################################################################################################################
# FLARE EASY

hist (data_SHQ$flare_ACC_normalized) 

lmer_flare_easy_ACC_normalized <- lmer(flare_ACC_normalized ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                                  protocol*session_number +(1|id), data=flare_easy,  na.action = na.exclude)

anova (lmer_flare_easy_ACC_normalized)

eta_sq(lmer_flare_easy_ACC_normalized)

hist(resid(lmer_flare_easy_ACC_normalized))

shapiro.test(resid(lmer_flare_easy_ACC_normalized))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_flare_ACC_normalized)

lms_flare_easy_ACC_normalized <- lsmeans(lmer_flare_easy_ACC_normalized, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_easy_ACC_normalized

write.csv(lms_flare_easy_ACC_normalized, 'C:\\Users\\adria\\Desktop\\SHQ_lms_flare_ACC_easy_normalized.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

#####################################################################################################

hist (data_SHQ$normalized_time) 

lmer_time_needed_to_complete_easy_normalized <- lmer(log10(normalized_time) ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                                                protocol*session_number +(1|id), data=flare_easy,  na.action = na.exclude)

anova (lmer_time_needed_to_complete_easy_normalized)

eta_sq(lmer_time_needed_to_complete_easy_normalized)

hist(resid(lmer_time_needed_to_complete_easy_normalized))

shapiro.test(resid(lmer_time_needed_to_complete_easy_normalized))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_time_needed_to_complete_easy_normalized)

lms_flare_time_needed_to_complete_easy_normalized <- lsmeans(lmer_time_needed_to_complete_easy_normalized, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_time_needed_to_complete_easy_normalized

write.csv(lms_flare_time_needed_to_complete_easy_normalized, 'C:\\Users\\adria\\Desktop\\SHQ_FLARE_lmer_time_needed_to_complete_easy_normalized.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

#####################################################################################################
# FLARE HARD 

hist (data_SHQ$flare_ACC_normalized) 

lmer_flare_ACC_hard_normalized <- lmer(flare_ACC_normalized ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                  protocol*session_number +(1|id), data=flare_hard,  na.action = na.exclude)

anova (lmer_flare_ACC_hard_normalized)

eta_sq(lmer_flare_ACC_hard_normalized)

hist(resid(lmer_flare_ACC_hard_normalized))

shapiro.test(resid(lmer_flare_ACC_hard_normalized))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_flare_ACC_hard_normalized)

lms_flare_ACC_hard_normalized <- lsmeans(lmer_flare_ACC_hard_normalized, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_flare_ACC_hard_normalized

write.csv(lms_flare_ACC_hard_normalized, 'C:\\Users\\adria\\Desktop\\SHQ_lms_flare_ACC_hard_normalized.csv', row.names=F)

#####################################################################################################

hist (data_SHQ$time_needed_to_complete) 

lmer_time_needed_to_complete_hard_normalized <- lmer(log10(normalized_time) ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                protocol*session_number + (1|id), data=flare_hard,  na.action = na.exclude)

anova (lmer_time_needed_to_complete_hard_normalized)

eta_sq(lmer_time_needed_to_complete_hard_normalized)

hist(resid(lmer_time_needed_to_complete_hard_normalized))

shapiro.test(resid(lmer_time_needed_to_complete_hard_normalized))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_time_needed_to_complete_hard_normalized)

lms_time_needed_to_complete_hard_normalized <- lsmeans(lmer_time_needed_to_complete_hard_normalized, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_time_needed_to_complete_hard_normalized

write.csv(lms_time_needed_to_complete_hard_normalized, 'C:\\Users\\adria\\Desktop\\SHQ_FLARE_lmer_time_needed_to_complete_hard_normalized.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

