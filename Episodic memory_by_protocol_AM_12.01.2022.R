
# Episodic memory - mixed models - AM - 12.01.2022

# Mixed Effect models - zawiera effecty stale jaki i effekty losowe; a method for analyzing data that are non independent, multilevel/hierarchical, longitudinal, or correlated.

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
library(lsmeans)
library(sjstats) # eta sq and Cohen F effect size

setwd("C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\C_Episodic Memory task\\Episodic memory_databases_AM_31.07.2021") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\C_Episodic Memory task\\Episodic memory_databases_AM_31.07.2021\\Episodic_Memory_Final_AM_16.12.2021.csv")

##############################################################################################################################################################

# updating the database with logit variables

Episodic_memory_1 <- data.frame ('id'= data$participant_ID,data$age,'age'= data$age,'sex'=data$Sex_M_1_F_2,'session_number'=data$session_number,'protocol'=data$protocol_1_SD_2_MN,
                                 'years_of_education'=data$years_spent_in_education, 'hours_of_sleep_baseline'=data$TST_Baseline_min, 
                                 'normalized_time_needed_to_complete_the_task_s'=data$Normalized_time_needed_to_complete_the_task_s, 'normalized_Recognition_Hits_logit'=data$Normalized_Recognition_Hits_logit,'normalized_Recognition_ms'=data$Normalized_Recognition_Hits_ms,
                                 'normalized_Recognition_Correct_Rejections_ms'=data$Normalized_Recognition_Correct_Rejections_ms, 'normalized_Recognition_Correct_Rejections_logit'=data$Normalized_Recognition_Correct_Rejection_logit, 'normalized_Source_Memory_Hit_logit'=data$Normalized_Source_Memory_Hit_logit,
                                  'normalized_Source_Memory_Hit_Reaction_Time_ms'=data$Normalized_Source_Memory_Hit_Reaction_Time_ms, 'Normalized_Recognition_False_Alarms_logit'=data$Normalized_Recognition_False_Alarms_logit,
                                
                                  'time_needed_to_complete_the_task_s'=data$time_needed_to_complete_the_task_s, 'Recognition_Hits_logit'=data$Recognition_Hits_logit,'Recognition_ms'=data$Recognition_Hits_ms,
                                 'Recognition_Correct_Rejections_ms'=data$Recognition_Correct_Rejections_ms, 'Recognition_Correct_Rejections_logit'=data$Recognition_Correct_Rejection_logit, 'Source_Memory_Hit_logit'=data$Source_Memory_Hit_logit,
                                 'Source_Memory_Hit_Reaction_Time_ms'=data$Source_Memory_Hit_Reaction_Time_ms, 'Recognition_False_Alarms_logit'=data$Recognition_False_Alarms_logit)

Episodic_memory <- na.omit(Episodic_memory_1)

Episodic_memory


Episodic_memory_ <- Episodic_memory %>%
  filter(!session_number==1)

options(scipen = 999)

############################################################################################################################################################
# MIXED MODELS - NORMALIZED
##############################################################################################################################################################

# Episodic Memory - time needed to complete

hist (Episodic_memory$normalized_time_needed_to_complete_the_task_s) 

lmer_time_needed_to_complete_the_task <- lmer(log10(normalized_time_needed_to_complete_the_task_s)  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                protocol*session_number  + (1|id), data=Episodic_memory_,  na.action = na.exclude)


anova (lmer_time_needed_to_complete_the_task )

eta_sq(lmer_time_needed_to_complete_the_task )

hist(resid(lmer_time_needed_to_complete_the_task ))

lms_time_needed_to_complete_the_task <- lsmeans(lmer_time_needed_to_complete_the_task , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_time_needed_to_complete_the_task 

write.csv(lms_time_needed_to_complete_the_task, 'C:\\Users\\adria\\Desktop\\EM_time_to_complete_NORMALIZED.csv', row.names=F)

##############################################################################################################################################################

# Episodic Memory - Recognition - HITS - normalized_Recognition_Hits_percentage

hist (Episodic_memory$normalized_Recognition_Hits_logit) 

lmer_normalized_Recognition_Hits_percentage <- lmer(sqrt(normalized_Recognition_Hits_logit)  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                      protocol*session_number  + (1|id), data=Episodic_memory_,  na.action = na.exclude)


anova (lmer_normalized_Recognition_Hits_percentage )

eta_sq(lmer_normalized_Recognition_Hits_percentage )

hist(resid(lmer_normalized_Recognition_Hits_percentage ))

shapiro.test(resid(lmer_normalized_Recognition_Hits_percentage ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Recognition_Hits_percentage )

lms_normalized_Recognition_Hits_percentage <- lsmeans(lmer_normalized_Recognition_Hits_percentage , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_normalized_Recognition_Hits_percentage

write.csv(lms_normalized_Recognition_Hits_percentage, 'C:\\Users\\adria\\Desktop\\EM_lms_Recognition_Hits_logit_NORMALIZED.csv', row.names=F)

##############################################################################################################################################################

# Episodic Memory - Recognition - normalized_Recognition_Hits_ms

hist (Episodic_memory$normalized_Recognition_ms) 

lmer_normalized_Recognition_Hits_ms <- lmer(log10(normalized_Recognition_ms)  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                              protocol*session_number  + (1|id), data=Episodic_memory_,  na.action = na.exclude)

anova (lmer_normalized_Recognition_Hits_ms )

eta_sq(lmer_normalized_Recognition_Hits_ms )

hist(resid(lmer_normalized_Recognition_Hits_ms ))

shapiro.test(resid(lmer_normalized_Recognition_Hits_ms ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Recognition_Hits_ms )

lms_normalized_Recognition_Hits_ms <- lsmeans(lmer_normalized_Recognition_Hits_ms , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_normalized_Recognition_Hits_ms 

write.csv(lms_normalized_Recognition_Hits_ms , 'C:\\Users\\adria\\Desktop\\EM_lms_Recognition_Hits_ms_NORMALIZED.csv', row.names=F)

############################################################################################################################################################

# Episodic Memory - Recognition - %

hist (Episodic_memory$normalized_Recognition_Correct_Rejections_ms) 

lmer_normalized_Recognition_Correct_Rejections_ms <- lmer(log10(normalized_Recognition_Correct_Rejections_ms)   ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                            protocol*session_number  + (1|id), data=Episodic_memory_,  na.action = na.exclude)
anova (lmer_normalized_Recognition_Correct_Rejections_ms )

eta_sq(lmer_normalized_Recognition_Correct_Rejections_ms )

hist(resid(lmer_normalized_Recognition_Correct_Rejections_ms ))

shapiro.test(resid(lmer_normalized_Recognition_Correct_Rejections_ms ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Recognition_Correct_Rejections_ms )

lsm_normalized_Recognition_Correct_Rejections_ms <- lsmeans(lmer_normalized_Recognition_Correct_Rejections_ms , ~ session_number*protocol) # plot main effect of: genotype, protocol
lsm_normalized_Recognition_Correct_Rejections_ms 

write.csv(lsm_normalized_Recognition_Correct_Rejections_ms , 'C:\\Users\\adria\\Desktop\\EM_Recognition_Correct_Rejections_ms_NORMALIZED.csv', row.names=F)

############################################################################################################################################################

# Episodic Memory - Recognition - normalized_logit

hist (Episodic_memory$normalized_Recognition_Correct_Rejections_logit) 

lmer_normalized_Recognition_Correct_Rejections_logit <- lmer(log10(normalized_Recognition_Correct_Rejections_logit)  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                            protocol*session_number  + (1|id), data=Episodic_memory_,  na.action = na.exclude)

anova (lmer_normalized_Recognition_Correct_Rejections_logit)

eta_sq(lmer_normalized_Recognition_Correct_Rejections_logit)

hist(resid(lmer_normalized_Recognition_Correct_Rejections_logit ))

shapiro.test(resid(lmer_normalized_Recognition_Correct_Rejections_logit ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Recognition_Correct_Rejections_logit )

lsm_normalized_Recognition_Correct_Rejections_logit <- lsmeans(lmer_normalized_Recognition_Correct_Rejections_logit , ~ session_number*protocol) # plot main effect of: genotype, protocol
lsm_normalized_Recognition_Correct_Rejections_logit

write.csv(lsm_normalized_Recognition_Correct_Rejections_logit , 'C:\\Users\\adria\\Desktop\\EM_Recognition_Correct_Rejections_HITS_logit_NORMALIZED.csv', row.names=F)

###########################################################################################################################################################

# Episodic Memory - False alarms - %

hist (Episodic_memory$Normalized_Recognition_False_Alarms_logit) 

lmer_normalized_False_Alarms_logit <- lmer(Normalized_Recognition_False_Alarms_logit  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                             protocol*session_number  + (1|id), data=Episodic_memory_,  na.action = na.exclude)
anova (lmer_normalized_False_Alarms_logit )

eta_sq(lmer_normalized_False_Alarms_logit)

hist(resid(lmer_normalized_False_Alarms_logit ))

shapiro.test(resid(lmer_normalized_False_Alarms_logit ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_False_Alarms_logit )

lsm_normalized_False_Alarms_logit <- lsmeans(lmer_normalized_Recognition_Correct_Rejections_logit , ~ session_number*protocol) # plot main effect of: genotype, protocol
lsm_normalized_False_Alarms_logit

write.csv(lsm_normalized_Recognition_Correct_Rejections_logit , 'C:\\Users\\adria\\Desktop\\EM_False_Alarms_logit_NORMALIZED.csv', row.names=F)

############################################################################################################################################################
# source memory
############################################################################################################################################################

# Episodic Memory - Recognition - normalized - IT IS IN % - LOGIT!

hist (Episodic_memory$normalized_Source_Memory_Hit_logit) 

lmer_normalized_Source_Memory_Hit_Count <- lmer(normalized_Source_Memory_Hit_logit  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                  protocol*session_number  + (1|id), data=Episodic_memory_,  na.action = na.exclude)
anova (lmer_normalized_False_Alarms_logit )
anova (lmer_normalized_Source_Memory_Hit_Count )

eta_sq(lmer_normalized_Source_Memory_Hit_Count )

hist(resid(lmer_normalized_Source_Memory_Hit_Count ))

shapiro.test(resid(lmer_normalized_Source_Memory_Hit_Count ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Source_Memory_Hit_Count )

lms_normalized_Source_Memory_Hit_Count <- lsmeans(lmer_normalized_Source_Memory_Hit_Count , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_normalized_Source_Memory_Hit_Count 

write.csv(lms_normalized_Source_Memory_Hit_Count  , 'C:\\Users\\adria\\Desktop\\EM_Source_Memory_Hit_Count_NORMALIZED.csv', row.names=F)

############################################################################################################################################################

# Episodic Memory - Source memory - normalized_Source_Memory_Hit_Reaction_Time_ms

hist (Episodic_memory$normalized_Source_Memory_Hit_Reaction_Time_ms) 

lmer_normalized_Source_Memory_Hit_Reaction_Time_ms <- lmer(log10(normalized_Source_Memory_Hit_Reaction_Time_ms)   ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                             protocol*session_number  + (1|id), data=Episodic_memory_,  na.action = na.exclude)

anova (lmer_normalized_Source_Memory_Hit_Reaction_Time_ms )

eta_sq(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms )

hist(resid(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms ))

shapiro.test(resid(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms )

lsm_normalized_Source_Memory_Hit_Reaction_Time_ms <- lsmeans(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms , ~ session_number*protocol) # plot main effect of: genotype, protocol
lsm_normalized_Source_Memory_Hit_Reaction_Time_ms 

write.csv(lsm_normalized_Source_Memory_Hit_Reaction_Time_ms  , 'C:\\Users\\adria\\Desktop\\EM_Source_Memory_Hit_Reaction_Time_ms_NORMALIZED.csv', row.names=F)

############################################################################################################################################################
# RAW DATA
############################################################################################################################################################
# MIXED MODELS
##############################################################################################################################################################

# Episodic Memory - time needed to complete

hist (Episodic_memory$time_needed_to_complete_the_task_s) 

lmer_time_needed_to_complete_the_task <- lmer(log10(time_needed_to_complete_the_task_s)  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                protocol*session_number  + (1|id), data=Episodic_memory,  na.action = na.exclude)


anova (lmer_time_needed_to_complete_the_task )

eta_sq(lmer_time_needed_to_complete_the_task )

hist(resid(lmer_time_needed_to_complete_the_task ))

lms_time_needed_to_complete_the_task <- lsmeans(lmer_time_needed_to_complete_the_task , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_time_needed_to_complete_the_task 

write.csv(lms_time_needed_to_complete_the_task, 'C:\\Users\\adria\\Desktop\\time_to_complete_RAW.csv', row.names=F)

##############################################################################################################################################################

# Episodic Memory - Recognition - HITS - normalized_Recognition_Hits_percentage

hist (Episodic_memory$Recognition_Hits_logit) 

lmer_normalized_Recognition_Hits_percentage <- lmer(log10(Recognition_Hits_logit)  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                      protocol*session_number  + (1|id), data=Episodic_memory,  na.action = na.exclude)



anova (lmer_normalized_Recognition_Hits_percentage )

eta_sq(lmer_normalized_Recognition_Hits_percentage )

hist(resid(lmer_normalized_Recognition_Hits_percentage ))

shapiro.test(resid(lmer_normalized_Recognition_Hits_percentage ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Recognition_Hits_percentage )

lms_normalized_Recognition_Hits_percentage <- lsmeans(lmer_normalized_Recognition_Hits_percentage , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_normalized_Recognition_Hits_percentage

write.csv(lms_normalized_Recognition_Hits_percentage, 'C:\\Users\\adria\\Desktop\\Recognition_Hits_logit_RAW.csv', row.names=F)

##############################################################################################################################################################

# Episodic Memory - Recognition - normalized_Recognition_Hits_ms

hist (Episodic_memory$Recognition_ms) 

lmer_normalized_Recognition_Hits_ms <- lmer(log10(Recognition_ms)   ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                              protocol*session_number  + (1|id), data=Episodic_memory,  na.action = na.exclude)
anova (lmer_normalized_Recognition_Hits_ms )

eta_sq(lmer_normalized_Recognition_Hits_ms )

hist(resid(lmer_normalized_Recognition_Hits_ms ))

shapiro.test(resid(lmer_normalized_Recognition_Hits_ms ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Recognition_Hits_ms )

lms_normalized_Recognition_Hits_ms <- lsmeans(lmer_normalized_Recognition_Hits_ms , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_normalized_Recognition_Hits_ms 

write.csv(lms_normalized_Recognition_Hits_ms , 'C:\\Users\\adria\\Desktop\\Recognition_Hits_ms_RAW.csv', row.names=F)

############################################################################################################################################################

# Episodic Memory - Recognition - %

hist (Episodic_memory$Recognition_Correct_Rejections_ms) 

lmer_normalized_Recognition_Correct_Rejections_ms <- lmer(log10(Recognition_Correct_Rejections_ms)    ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                            protocol*session_number  + (1|id), data=Episodic_memory,  na.action = na.exclude)

anova (lmer_normalized_Recognition_Correct_Rejections_ms )

eta_sq(lmer_normalized_Recognition_Correct_Rejections_ms )


hist(resid(lmer_normalized_Recognition_Correct_Rejections_ms ))

shapiro.test(resid(lmer_normalized_Recognition_Correct_Rejections_ms ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Recognition_Correct_Rejections_ms )

lsm_normalized_Recognition_Correct_Rejections_ms <- lsmeans(lmer_normalized_Recognition_Correct_Rejections_ms , ~ session_number*protocol) # plot main effect of: genotype, protocol
lsm_normalized_Recognition_Correct_Rejections_ms 

write.csv(lsm_normalized_Recognition_Correct_Rejections_ms , 'C:\\Users\\adria\\Desktop\\Recognition_Correct_Rejections_ms_RAW.csv', row.names=F)

############################################################################################################################################################

# Episodic Memory - Recognition - normalized_logit

hist (Episodic_memory$Recognition_Correct_Rejections_logit) 

lmer_normalized_Recognition_Correct_Rejections_logit <- lmer(log10(Recognition_Correct_Rejections_logit)   ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                            protocol*session_number  + (1|id), data=Episodic_memory,  na.action = na.exclude)

anova (lmer_normalized_Recognition_Correct_Rejections_logit )

eta_sq(lmer_normalized_Recognition_Correct_Rejections_logit )

hist(resid(lmer_normalized_Recognition_Correct_Rejections_logit ))

shapiro.test(resid(lmer_normalized_Recognition_Correct_Rejections_logit ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Recognition_Correct_Rejections_logit)

lsm_normalized_Recognition_Correct_Rejections_logit <- lsmeans(lmer_normalized_Recognition_Correct_Rejections_logit , ~ session_number*protocol) # plot main effect of: genotype, protocol
lsm_normalized_Recognition_Correct_Rejections_logit 

write.csv(lsm_normalized_Recognition_Correct_Rejections_logit , 'C:\\Users\\adria\\Desktop\\Recognition_Correct_Rejections_logit_RAW.csv', row.names=F)


############################################################################################################################################################
# source memory
############################################################################################################################################################

# Episodic Memory - Recognition - normalized - IT IS IN % - LOGIT!

hist (Episodic_memory$Source_Memory_Hit_logit) 

lmer_normalized_Source_Memory_Hit_Count <- lmer((Source_Memory_Hit_logit)^2  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                  protocol*session_number  + (1|id), data=Episodic_memory,  na.action = na.exclude)
anova (lmer_normalized_Source_Memory_Hit_Count )

eta_sq(lmer_normalized_Source_Memory_Hit_Count )

hist(resid(lmer_normalized_Source_Memory_Hit_Count ))

shapiro.test(resid(lmer_normalized_Source_Memory_Hit_Count ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Source_Memory_Hit_Count )

lms_normalized_Source_Memory_Hit_Count <- lsmeans(lmer_normalized_Source_Memory_Hit_Count , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_normalized_Source_Memory_Hit_Count 

write.csv(lms_normalized_Source_Memory_Hit_Count  , 'C:\\Users\\adria\\Desktop\\Source_Memory_Hit_logit_RAW.csv', row.names=F)

############################################################################################################################################################

# Episodic Memory - Source memory - normalized_Source_Memory_Hit_Reaction_Time_ms

hist (Episodic_memory$Source_Memory_Hit_Reaction_Time_ms) 

lmer_normalized_Source_Memory_Hit_Reaction_Time_ms <- lmer(log10(Source_Memory_Hit_Reaction_Time_ms)   ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                                             protocol*session_number  + (1|id), data=Episodic_memory,  na.action = na.exclude)
anova (lmer_normalized_Source_Memory_Hit_Reaction_Time_ms )

eta_sq(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms )

hist(resid(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms ))

shapiro.test(resid(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms )

lsm_normalized_Source_Memory_Hit_Reaction_Time_ms <- lsmeans(lmer_normalized_Source_Memory_Hit_Reaction_Time_ms , ~ session_number*protocol) # plot main effect of: genotype, protocol
lsm_normalized_Source_Memory_Hit_Reaction_Time_ms 

write.csv(lsm_normalized_Source_Memory_Hit_Reaction_Time_ms  , 'C:\\Users\\adria\\Desktop\\Source_Memory_Hit_Reaction_Time_ms_RAW.csv', row.names=F)

