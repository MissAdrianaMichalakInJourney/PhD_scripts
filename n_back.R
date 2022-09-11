
# n-back - mixed models - FINAL - AM - 20.12.2021

# Mixed Effect models - zawiera effecty stale jaki i effekty losowe; a method for analyzing data that are non independent, multilevel/hierarchical, longitudinal, or correlated.

# installr: install.packages("installr") 
# library(installr)
library ("nlme")
library("lme4")
library("lmerTest")
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

setwd("C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\n_back_databases_AM_31.07.2021") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\n_back_databases_AM_31.07.2021\\n_back_Final_AM_14.09.2021.csv")

n_back <- data.frame ('id'= data$participant_ID,'age'= data$age,'sex'=data$Sex_M_1_F_2,'genotype'=data$Genotype_1_e4_positive_2_e4_negative,'years_of_education'=data$years_spent_in_education,'session_number'=data$session_number,'protocol'=data$protocol_1_SD_2_MN, 'date' = data$date, 'time' = data$time,
                      'RT_one_back'=data$RT_ONE_back, 'accuracy_one_back'= data$accuracy_ONE_back, 'RT_two_back'=data$RT_TWO_back, 'accuracy_two_back'=data$accuracy_TWO_back,
                      'SE_baseline'=data$SE_Baseline, 'hours_of_sleep_baseline'=data$TST_Baseline_min, 'CCI'=data$CCI_Cognition_total,
                      'normalized_RT_one_back'=data$normalized_RT_ONE_back, 'normalized_RT_two_back'=data$normalized_RT_TWO_back, 'normalized_accuracy_one_back'=
                        data$normalized_accuracy_ONE_back, 'normalized_accuracy_TWO_back'=data$normalized_accuracy_TWO_back)

n_back_1 <- na.exclude(n_back)

options(scipen=999)

n_back_1_ <- n_back_1 %>%
  filter(!session_number==1)

############################################################################################################################################################
# Normalized
############################################################################################################################################################
# n-back - RT - ONE back

hist (n_back_1$normalized_RT_one_back) 

lmer_RT_ONE_back <- lmer(normalized_RT_one_back ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                           protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=n_back_1_,  na.action = na.exclude)

anova (lmer_RT_ONE_back)

eta_sq(lmer_RT_ONE_back)

hist(resid(lmer_RT_ONE_back))

shapiro.test(resid(lmer_RT_ONE_back))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_RT_ONE_back)

lms_RT_ONE_back <- lsmeans(lmer_RT_ONE_back, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_RT_ONE_back

write.csv(lms_RT_ONE_back, 'C:\\Users\\adria\\Desktop\\lms_RT_ONE_back_normalized.csv', row.names=F)

############################################################################################################################################################
# n-back - RT - TWO back

hist (n_back_1$normalized_RT_two_back) 

lmer_RT_TWO_back <- lmer(normalized_RT_two_back  ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                           protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=n_back_1_,  na.action = na.exclude)

anova (lmer_RT_TWO_back)

eta_sq(lmer_RT_TWO_back)

hist(resid(lmer_RT_TWO_back))

shapiro.test(resid(lmer_RT_TWO_back))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_RT_TWO_back)

lms_RT_TWO_back <- lsmeans(lmer_RT_TWO_back, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_RT_TWO_back

write.csv(lms_RT_TWO_back, 'C:\\Users\\adria\\Desktop\\lms_RT_TWO_back_normalized.csv', row.names=F)

############################################################################################################################################################
# n-back accuracy - ONE back

# transformation of the dependent value --> https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/ --> (1/x)^4

hist (n_back_1$normalized_accuracy_one_back) 

lmer_ACC_ONE_back <- lmer((normalized_accuracy_one_back^8) ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                            protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=n_back_1_,  na.action = na.exclude)

anova (lmer_ACC_ONE_back)

eta_sq(lmer_ACC_ONE_back)

hist(resid(lmer_ACC_ONE_back))

shapiro.test(resid(lmer_ACC_ONE_back))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_ACC_ONE_back)

lms_ACC_ONE_back <- lsmeans(lmer_ACC_ONE_back, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_ACC_ONE_back

write.csv(lms_ACC_ONE_back, 'C:\\Users\\adria\\Desktop\\lms_ACC_ONE_back_normalized.csv', row.names=F)

############################################################################################################################################################
# n-back accuracy - TWO back

hist (n_back_1$normalized_accuracy_TWO_back) 

lmer_ACC_TWO_back <- lmer((normalized_accuracy_TWO_back^5) ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                            protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=n_back_1_,  na.action = na.exclude)


anova (lmer_ACC_TWO_back)

eta_sq(lmer_ACC_TWO_back)

hist(resid(lmer_ACC_TWO_back))

shapiro.test(resid(lmer_ACC_TWO_back))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_ACC_TWO_back)

lms_ACC_TWO_back <- lsmeans(lmer_ACC_TWO_back, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_ACC_TWO_back

write.csv(lms_ACC_TWO_back, 'C:\\Users\\adria\\Desktop\\lms_ACC_TWO_back_normalized.csv', row.names=F)


############################################################################################################################################################
# RAW
############################################################################################################################################################
# n-back - RT - ONE back

hist (n_back_1$RT_one_back) 

lmer_RT_ONE_back <- lmer(RT_one_back ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                           protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=n_back_1,  na.action = na.exclude)

anova (lmer_RT_ONE_back)

eta_sq(lmer_RT_ONE_back)

hist(resid(lmer_RT_ONE_back))

shapiro.test(resid(lmer_RT_ONE_back))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_RT_ONE_back)

lms_RT_ONE_back <- lsmeans(lmer_RT_ONE_back, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_RT_ONE_back

write.csv(lms_RT_ONE_back, 'C:\\Users\\adria\\Desktop\\lms_RT_ONE_back_RAW.csv', row.names=F)

############################################################################################################################################################
# n-back - RT - TWO back

hist (n_back_1$RT_two_back) 

lmer_RT_TWO_back <- lmer(RT_two_back  ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                           protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=n_back_1,  na.action = na.exclude)

anova (lmer_RT_TWO_back)

eta_sq(lmer_RT_TWO_back)

hist(resid(lmer_RT_TWO_back))

shapiro.test(resid(lmer_RT_TWO_back))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_RT_TWO_back)

lms_RT_TWO_back <- lsmeans(lmer_RT_TWO_back, ~ as.factor(session_number)*genotype*protocol) # plot main effect of: genotype, protocol
lms_RT_TWO_back

write.csv(lms_RT_TWO_back, 'C:\\Users\\adria\\Desktop\\lms_RT_TWO_back_RAW.csv', row.names=F)

############################################################################################################################################################
# n-back accuracy - ONE back

# transformation of the dependent value --> https://www.r-bloggers.com/2020/01/a-guide-to-data-transformation/ --> (1/x)^4

hist (n_back_1$accuracy_one_back) 

lmer_ACC_ONE_back <- lmer(accuracy_one_back ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                            protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=n_back_1,  na.action = na.exclude)

anova (lmer_ACC_ONE_back)

eta_sq(lmer_ACC_ONE_back)

hist(resid(lmer_ACC_ONE_back))

shapiro.test(resid(lmer_ACC_ONE_back))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_ACC_ONE_back)

lms_ACC_ONE_back <- lsmeans(lmer_ACC_ONE_back, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_ACC_ONE_back

write.csv(lms_ACC_ONE_back, 'C:\\Users\\adria\\Desktop\\lms_ACC_ONE_back_RAW.csv', row.names=F)

############################################################################################################################################################
# n-back accuracy - TWO back

hist (n_back_1$accuracy_two_back) 

lmer_ACC_TWO_back <- lmer(accuracy_two_back ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                            protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=n_back_1,  na.action = na.exclude)


anova (lmer_ACC_TWO_back)

eta_sq(lmer_ACC_TWO_back)

hist(resid(lmer_ACC_TWO_back))

shapiro.test(resid(lmer_ACC_TWO_back))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_ACC_TWO_back)

lms_ACC_TWO_back <- lsmeans(lmer_ACC_TWO_back, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_ACC_TWO_back

write.csv(lms_ACC_TWO_back, 'C:\\Users\\adria\\Desktop\\lms_ACC_TWO_back_RAW.csv', row.names=F)

