
# PVT - mixed models - AM - 12.01.2022

# Mixed Effect models - zawiera effecty stale jaki i effekty losowe; a method for analyzing data that are non independent, multilevel/hierarchical, longitudinal, or correlated.

# install.packages("tidyverse")

library ("nlme")
library("lme4")
library("lmerTest") # gives more comprehensive ANOVA output
library("ggplot2")
library("dplyr")
library("MuMIn") # R2 for the model
library("rcompanion")
library("performance")
library("Rmisc") 
library("sjPlot")
library("sjmisc")
library("sjlabelled")
library("MASS")
library ("car")
library("rcompanion")
library(lsmeans)
library(sjstats) # eta sq and Cohen F effect size

setwd("C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\A_PVT") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\A_PVT\\PVT_FINAL_06.11.2021.csv")

PVT <- data.frame ('id'= data$participant_ID,'age'= data$age,'sex'=data$Sex_M_1_F_2,'session_number'=data$session_number,'protocol'=data$protocol_1_SD_2_MN, 'date' = data$date, 
                   'time' = data$time, 'mean_RT_no_lapses'=data$mean_RT_no_lapses,'fastest_10%_RT'= data$fastest_10_RT, 'slowest_10%_RT_no_lapses'=data$slowest_10_RT_no_lapses,
                   'number_of_lapses'=data$number_of_lapses, 'median_RT_no_lapses'=data$median_RT_no_lapses, 'hours_of_sleep_baseline'=data$TST_Baseline_min,
                   'SE_baseline'=data$SE_Baseline, 'years_of_education'=data$years_spent_in_education,'normalized_mean_RT_no_lapses'=data$normalized_mean_RT_no_lapses,'normalized_fastest_10_RT'= data$normalized_fastest_10_RT, 
                   'normalized_slowest_10_RT_no_lapses'=data$normalized_slowest_10_RT_no_lapses, 'normalized_median_RT_no_lapses'=data$normalized_median_RT_no_lapses, 'normalized_no_of_lapses'=data$normalized_number_of_lapses)

PVT

PVT_ <- PVT %>%
  filter(!session_number==1)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEAN RT - no lapses

hist (PVT$normalized_mean_RT_no_lapses) 

lmer_mean_RT_no_lapses <- lmer(normalized_mean_RT_no_lapses ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                 protocol*session_number +(1|id), data=PVT_,  na.action = na.exclude)


anova (lmer_mean_RT_no_lapses)

eta_sq(lmer_mean_RT_no_lapses)

hist(resid(lmer_mean_RT_no_lapses))

lms_mean_RT_no_lapses <- lsmeans(lmer_mean_RT_no_lapses, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_mean_RT_no_lapses

write.csv(lms_mean_RT_no_lapses, 'C:\\Users\\adria\\Desktop\\PVT_normalized_mean_no_lapses_mixed_model.csv', row.names=F)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEAN RT - no lapses

hist (PVT$normalized_median_RT_no_lapses) 

lmer_median_RT_no_lapses <- lmer(normalized_median_RT_no_lapses ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                   protocol*session_number +(1|id), data=PVT_,  na.action = na.exclude)


anova (lmer_median_RT_no_lapses)

eta_sq(lmer_median_RT_no_lapses)

hist(resid(lmer_median_RT_no_lapses))

lms_median_RT_no_lapses <- lsmeans(lmer_median_RT_no_lapses, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_median_RT_no_lapses

write.csv(lms_median_RT_no_lapses, 'C:\\Users\\adria\\Desktop\\PVT_normalized_median_no_lapses_mixed_model.csv', row.names=F)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEAN RT - no lapses

hist (PVT$normalized_fastest_10_RT) 

lmer_10_fastest <- lmer(normalized_fastest_10_RT ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                          protocol*session_number+(1|id),  data=PVT_,  na.action = na.exclude)

anova (lmer_10_fastest)
eta_sq(lmer_10_fastest)

hist(resid(lmer_10_fastest))

lms_10_fastest <- lsmeans(lmer_10_fastest, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_10_fastest

write.csv(lms_10_fastest, 'C:\\Users\\adria\\Desktop\\PVT_normalized_10_fastest_mixed_model.csv', row.names=F)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEAN RT - no lapses

hist (PVT$normalized_slowest_10_RT_no_lapses) 

lmer_10_slowest <- lmer(normalized_slowest_10_RT_no_lapses ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                          protocol*session_number +(1|id),  data=PVT_,  na.action = na.exclude)

anova (lmer_10_slowest)

eta_sq(lmer_10_slowest)

hist(resid(lmer_10_slowest))

lms_10_slowest <- lsmeans(lmer_10_slowest, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_10_slowest

write.csv(lms_10_slowest, 'C:\\Users\\adria\\Desktop\\PVT_normalized_10_slowest_mixed_model.csv', row.names=F)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEAN RT - no lapses

hist (PVT$normalized_no_of_lapses) 

lmer_number_of_lapses <- lmer(log10(normalized_no_of_lapses) ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                                protocol*session_number +(1|id), data=PVT_, na.action = na.exclude)

anova (lmer_number_of_lapses)

eta_sq (lmer_number_of_lapses)

hist(resid(lmer_number_of_lapses))

lms_number_of_lapses <- lsmeans(lmer_number_of_lapses, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_number_of_lapses

write.csv(lms_number_of_lapses, 'C:\\Users\\adria\\Desktop\\PVT_lms_number_of_lapses_mixed_model.csv', row.names=F)

#################################################################################################################################################################
# RAW values
#################################################################################################################################################################

# MIXED MODEL - MEAN RT - no lapses

hist (PVT$mean_RT_no_lapses) 

lmer_mean_RT_no_lapses_raw <- lmer(mean_RT_no_lapses ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                 protocol*session_number +(1|id), data=PVT,  na.action = na.exclude)


anova (lmer_mean_RT_no_lapses_raw)

eta_sq(lmer_mean_RT_no_lapses_raw)

hist(resid(lmer_mean_RT_no_lapses_raw))

lms_mean_RT_no_lapses_raw <- lsmeans(lmer_mean_RT_no_lapses_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_mean_RT_no_lapses_raw

write.csv(lms_mean_RT_no_lapses_raw, 'C:\\Users\\adria\\Desktop\\PVT_mean_no_lapses_mixed_model_RAW.csv', row.names=F)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEDIAN RT - no lapses

hist (PVT$median_RT_no_lapses) 

lmer_median_RT_no_lapses_raw <- lmer(median_RT_no_lapses ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                   protocol*session_number +(1|id), data=PVT,  na.action = na.exclude)


anova (lmer_median_RT_no_lapses_raw)

eta_sq(lmer_median_RT_no_lapses_raw)

hist(resid(lmer_median_RT_no_lapses_raw))

lms_median_RT_no_lapses_raw <- lsmeans(lmer_median_RT_no_lapses_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_median_RT_no_lapses_raw

write.csv(lms_median_RT_no_lapses_raw, 'C:\\Users\\adria\\Desktop\\PVT_median_no_lapses_mixed_model_RAW.csv', row.names=F)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEAN RT - no lapses

hist (PVT$fastest_10._RT) 

lmer_10_fastest_raw <- lmer(fastest_10._RT ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                          protocol*session_number +(1|id),  data=PVT,  na.action = na.exclude)

anova (lmer_10_fastest_raw)
eta_sq(lmer_10_fastest_raw)

hist(resid(lmer_10_fastest_raw))

lms_10_fastest_raw <- lsmeans(lmer_10_fastest_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_10_fastest_raw

write.csv(lms_10_fastest_raw, 'C:\\Users\\adria\\Desktop\\PVT_10_fastest_mixed_model_RAW.csv', row.names=F)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEAN RT - no lapses

hist (PVT$slowest_10._RT_no_lapses) 

lmer_10_slowest_raw <- lmer(slowest_10._RT_no_lapses ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                          protocol*session_number +(1|id),  data=PVT,  na.action = na.exclude)

anova (lmer_10_slowest_raw)

eta_sq(lmer_10_slowest_raw)

hist(resid(lmer_10_slowest_raw))

lms_10_slowest_raw <- lsmeans(lmer_10_slowest_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_10_slowest_raw

write.csv(lms_10_slowest_raw, 'C:\\Users\\adria\\Desktop\\PVT_normalized_10_slowest_mixed_model_RAW.csv', row.names=F)

###########################################################################################################################################################

# MIXED MODEL - normalized - MEAN RT - no lapses

hist (PVT$number_of_lapses) 

lmer_number_of_lapses_raw <- lmer(number_of_lapses ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                                protocol*session_number +(1|id), data=PVT, na.action = na.exclude)

anova (lmer_number_of_lapses_raw)

eta_sq (lmer_number_of_lapses_raw)

hist(resid(lmer_number_of_lapses_raw))

lms_number_of_lapses_raw <- lsmeans(lmer_number_of_lapses_raw, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_number_of_lapses_raw

write.csv(lms_number_of_lapses_raw, 'C:\\Users\\adria\\Desktop\\PVT_lms_number_of_lapses_mixed_model_RAW.csv', row.names=F)

