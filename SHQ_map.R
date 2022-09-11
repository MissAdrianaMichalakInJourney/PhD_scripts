
# SHQ - wayfining models - AM - 23.12.2021

install.packages("tidyverse")

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
library(tidyverse)
library(dplyr)

setwd("C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\D_SHQ\\SHQ_databases_AM_31.07.2021") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\D_SHQ\\SHQ_databases_AM_31.07.2021\\SHQ_outcomes_Wayfinding_EASY_HARD_Final_AM_16.12.2021.csv")


data_SHQ <- data.frame('id'=data$Participant_ID,'age'=data$age, 'sex'=data$Sex_M_1_F_2,'genotype'=data$Genotype_1_e4_positive_2_e4_negative,
                       'protocol'= data$protocol_1_SD_2_MN, 'session_number'=data$session_number, 'years_of_education'=data$years_spent_in_education,
                       'level_ID'=data$level_ID, 'level_easy_1_hard_2'=data$level_1_easy_2_hard,'wayfinding_distance'=data$distance_normalized_to_t1_only, 
                    'wayfinding_duration'=data$normalized_map_duration_to_t1_only, 'hours_of_sleep_baseline'=data$TST_Baseline_min)


wayfinding_EASY <-data_SHQ %>%
  filter(level_easy_1_hard_2==1) 

wayfinding_HARD <-data_SHQ %>%
  filter(level_easy_1_hard_2==2)

options(scipen=999)

data_SHQ_1 <- data_SHQ %>%
  filter(!session_number==1)

wayfinding_EASY <-data_SHQ_1 %>%
  filter(level_easy_1_hard_2==1) 

wayfinding_EASY

wayfinding_HARD <-data_SHQ_1 %>%
  filter(level_easy_1_hard_2==2) 

wayfinding_HARD

############################################################################################################################################################

# MIXED MODEL - ONE main outcome variable - MAP_DISTANCE - MAP wayfinding DISTANCE

hist (data_SHQ$wayfinding_distance) 

lmer_SHQ_distance_hard_and_easy_levels <- lmer(log10(wayfinding_distance)  ~ as.factor(session_number) + age + sex + genotype + protocol + years_of_education + hours_of_sleep_baseline +
                                                 protocol*session_number + protocol*genotype + session_number*genotype + session_number*protocol*genotype +(1|id), data=data_SHQ_1,  na.action = na.exclude)


anova (lmer_SHQ_distance_hard_and_easy_levels)

eta_sq(lmer_SHQ_distance_hard_and_easy_levels)

hist(resid(lmer_SHQ_distance_hard_and_easy_levels))

shapiro.test(resid(lmer_SHQ_distance_hard_and_easy_levels))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_distance_hard_and_easy_levels)

lms_SHQ_distance_hard_and_easy_levels <- lsmeans(lmer_SHQ_distance_hard_and_easy_levels, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_SHQ_distance_hard_and_easy_levels

write.csv(lms_SHQ_distance_hard_and_easy_levels, 'C:\\Users\\adria\\Desktop\\SHQ_distance_hard_and_easy_levels.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

############################################################################################################################################################

# MIXED MODEL - ONE main outcome variable - MAP_DISTANCE - MAP wayfinding DISTANCE - normalized - EASY 

hist (wayfinding_EASY$wayfinding_distance) 

lmer_SHQ_distance_EASY_levels <- lmer(log10(wayfinding_distance)  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                        protocol*session_number  +(1|id), data=wayfinding_EASY,  na.action = na.exclude)

# two figures - MN and SD --> slide 14
# SD vs MN for e4+ and e4- --> slide 13

distance_easy <- anova (lmer_SHQ_distance_EASY_levels)
write.csv (distance_easy, 'C:\\Users\\adria\\Desktop\\SHQ_distance_easy.csv')

distance_easy_eta <- eta_sq(lmer_SHQ_distance_EASY_levels)
write.csv(distance_easy_eta, 'C:\\Users\\adria\\Desktop\\SHQ_distance_easy_eta.csv')

hist(resid(lmer_SHQ_distance_EASY_levels))

shapiro.test(resid(lmer_SHQ_distance_EASY_levels))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_distance_EASY_levels)

lms_SHQ_distance_EASY_levels <- lsmeans(lmer_SHQ_distance_EASY_levels, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_distance_EASY_levels

write.csv(lms_SHQ_distance_EASY_levels, 'C:\\Users\\adria\\Desktop\\SHQ_distance_EASY_levels.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

############################################################################################################################################################

# MIXED MODEL - ONE main outcome variable - MAP_DISTANCE - MAP wayfinding DISTANCE - normalized - HARD

hist (wayfinding_HARD$wayfinding_distance) 

lmer_SHQ_distance_HARD_levels <- lmer(log10(wayfinding_distance) ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                        protocol*session_number +(1|id),data=wayfinding_HARD,  na.action = na.exclude)

# two figures - MN and SD --> slide 14
# SD vs MN for e4+ and e4- --> slide 13

distance_hard <- anova (lmer_SHQ_distance_HARD_levels)
write.csv (distance_hard, 'C:\\Users\\adria\\Desktop\\SHQ_distance_hard.csv')

distance_hard_eta <- eta_sq(lmer_SHQ_distance_HARD_levels)
write.csv(distance_hard_eta, 'C:\\Users\\adria\\Desktop\\SHQ_distance_hard_eta.csv')


hist(resid(lmer_SHQ_distance_HARD_levels))

shapiro.test(resid(lmer_SHQ_distance_HARD_levels))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_distance_HARD_levels)

lms_SHQ_distance_HARD_levels <- lsmeans(lmer_SHQ_distance_HARD_levels, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_distance_HARD_levels

write.csv(lms_SHQ_distance_HARD_levels, 'C:\\Users\\adria\\Desktop\\SHQ_distance_HARD_levels.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

#########################################################################################################################################################

# MIXED MODEL - ONE main outcome variable - MAP_DISTANCE - MAP wayfinding DURATION 

hist (data_SHQ$wayfinding_duration) 

lmer_SHQ_duration_hard_and_easy_levels <- lmer(log10(wayfinding_duration)  ~ as.factor(session_number) + age + sex  + protocol + years_of_education + hours_of_sleep_baseline +
                                                 protocol*session_number  +(1|id),data=data_SHQ_1,  na.action = na.exclude)

# SD vs MN for e4+ and e4- --> slide 13

anova (lmer_SHQ_duration_hard_and_easy_levels)

eta_sq(lmer_SHQ_duration_hard_and_easy_levels)

hist(resid(lmer_SHQ_duration_hard_and_easy_levels))

shapiro.test(resid(lmer_SHQ_duration_hard_and_easy_levels))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_duration_hard_and_easy_levels)

lms_SHQ_duration_hard_and_easy_levels <- lsmeans(lmer_SHQ_duration_hard_and_easy_levels, ~ session_number*genotype*protocol) # plot main effect of: genotype, protocol
lms_SHQ_duration_hard_and_easy_levels

write.csv(lms_SHQ_duration_hard_and_easy_levels, 'C:\\Users\\adria\\Desktop\\Duration_hard_and_easy_levels.csv', row.names=F)

#########################################################################################################################################################

# MIXED MODEL - ONE main outcome variable - MAP_DISTANCE - MAP wayfinding DURATION - EASY 

hist (wayfinding_EASY$wayfinding_duration) 

lmer_SHQ_duration_EASY_levels <- lmer(log10(wayfinding_duration)  ~ as.factor(session_number) + age + sex + protocol + years_of_education + hours_of_sleep_baseline +
                                        protocol*session_number +(1|id), data=wayfinding_EASY,  na.action = na.exclude)

# two figures - MN and SD --> slide 14
# SD vs MN for e4+ and e4- --> slide 13

easy_duration <- anova (lmer_SHQ_duration_EASY_levels)
write.csv(easy_duration, 'C:\\Users\\adria\\Desktop\\easy_duration.csv')

easy_duration_eta <- eta_sq(lmer_SHQ_duration_EASY_levels)
write.csv(easy_duration_eta, 'C:\\Users\\adria\\Desktop\\easy_duration_eta.csv')

hist(resid(lmer_SHQ_duration_EASY_levels))

shapiro.test(resid(lmer_SHQ_duration_EASY_levels))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_duration_EASY_levels)

lms_SHQ_duration_EASY_levels <- lsmeans(lmer_SHQ_duration_EASY_levels, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_duration_EASY_levels

write.csv(lms_SHQ_duration_EASY_levels, 'C:\\Users\\adria\\Desktop\\SHQ_duration_EASY_levels.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

############################################################################################################################################################

# MIXED MODEL - ONE main outcome variable - MAP_DISTANCE - MAP wayfinding DURATION - HARD

hist (wayfinding_HARD$wayfinding_duration) 

lmer_SHQ_duration_HARD_levels <- lmer(log10(wayfinding_duration)  ~ as.factor(session_number) + age + sex ++ protocol + years_of_education + hours_of_sleep_baseline +
                                        protocol*session_number  +(1|id), data=wayfinding_HARD,  na.action = na.exclude)

# two figures - MN and SD --> slide 14
# SD vs MN for e4+ and e4- --> slide 13

hard_duration <- anova (lmer_SHQ_duration_HARD_levels)
write.csv(hard_duration, 'C:\\Users\\adria\\Desktop\\easy_duration.csv')

hard_duration_eta <- eta_sq(lmer_SHQ_duration_HARD_levels)
write.csv(hard_duration_eta, 'C:\\Users\\adria\\Desktop\\hard_duration_eta.csv')

hist(resid(lmer_SHQ_duration_HARD_levels))

shapiro.test(resid(lmer_SHQ_duration_HARD_levels))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_duration_HARD_levels)

lms_SHQ_duration_HARD_levels <- lsmeans(lmer_SHQ_duration_HARD_levels, ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_duration_HARD_levels

write.csv(lms_SHQ_duration_HARD_levels, 'C:\\Users\\adria\\Desktop\\SHQ_duration_HARD_levels.csv', row.names=F)

# write.csv(comparisson$contrasts, 'C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\A_COGT\\B_n_back\\mixed_models\\one_n_back_RT.csv', row.names=F)

