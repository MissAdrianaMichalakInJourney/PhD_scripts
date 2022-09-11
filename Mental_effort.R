
# PVT - mixed models - AM - 07.11.2021

# Mixed Effect models - zawiera effecty stale jaki i effekty losowe; a method for analyzing data that are non independent, multilevel/hierarchical, longitudinal, or correlated.

# install.packages("tidyverse")

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


setwd("C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\D_Mental_effort\\normalized_by_t1") # set directory

data <- read.csv(file="C:\\Users\\adria\\Desktop\\B_PhD_analysis\\C_Lab_sessions\\A_Databases\\D_Mental_effort\\normalized_by_t1\\Lab Sessions_Mental_effort_scale_AM_14.09.2021.csv")

mental_effort <- data.frame ('id'= data$ID,'age'= data$age,'sex'=data$Sex_M_1_F_2,'genotype'=data$Genotype_1_e4_positive_2_e4_negative,'session_number'=data$COGT_number,'protocol'=data$protocol_1_SD_2_MN, 
                             
                             'years_of_education'=data$years_of_education, 'PVT_mental_effort'=data$PVT, 'n_back_mental_effort'=data$n_back, 'SHQ_MAP_EASY_mental_effort'=data$SHQ_map_easy,
                             'SHQ_MAP_HARD_mental_effort'=data$SHQ_map_hard, 'SHQ_FLARE_EASY_meantal_effort'=data$SHQ_flare_easy, 'SHQ_FLARE_HARD_mental_effort'=data$SHQ_map_hard,
                             'Episodic_memory_ENCODING'=data$Episodic_memory_encoding, 'Episodic_memory_RECALL'=data$Episodic_memory_recall, 'How_cognitively_demanding_was_all_the_session'=data$how_cognitively_demanding_was_the_session_cm,
                             
                             'PVT_normalized'=data$normalized_PVT, 'n_back_normalized'=data$normalized_n_back, 'SHQ_Flare_Easy_normalized'=data$normalized_SHQ_flare_easy,
                             'SHQ_Flare_hard_normalized'=data$normalized_SHQ_flare_hard, 'SHQ_Map_easy_normalzied'=data$normalized_SHQ_map_easy, 'SHQ_Map_hard_normalized'=
                               data$normalized_SHQ_map_hard, 'EM_encoding_normalzied'=data$Normalized_Episodic_Memory_encoding, 'EM_recall_normalized'=
                               data$Normalized_Episodic_Memory_Recall, 'overall_normalized'=data$normalized_whole_session, 'hourse_of_sleep_Baseline'=data$TST_Baseline_min)


options(scipen=999)

mental_effort_1 <- mental_effort %>%
  filter(!session_number==1)


############################################################################################################################################################

# Mixed models - mental effort - PVT

hist (mental_effort$PVT_mental_effort) 

lmer_PVT_mental_efforts <- lmer(PVT_mental_effort ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_PVT_mental_efforts )

eta_sq(lmer_PVT_mental_efforts )

hist(resid(lmer_PVT_mental_efforts ))

shapiro.test(resid(lmer_PVT_mental_efforts ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_PVT_mental_efforts )

lms_PVT_mental_effort <- lsmeans(lmer_PVT_mental_efforts , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_PVT_mental_effort

write.csv(lms_PVT_mental_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_PVT.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - n-back

hist (mental_effort$n_back_mental_effort) 

lmer_n_back_mental_effort <- lmer(n_back_mental_effort ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_n_back_mental_effort )

eta_sq(lmer_n_back_mental_effort )

hist(resid(lmer_n_back_mental_effort ))

shapiro.test(resid(lmer_n_back_mental_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_n_back_mental_effort )

lms_n_back_mental_effort <- lsmeans(lmer_n_back_mental_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_n_back_mental_effort

write.csv(lms_n_back_mental_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_n_back.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - SHQ - flare - EASY

hist (mental_effort$SHQ_FLARE_EASY_meantal_effort) 

lmer_SHQ_FLARE_EASY_meantal_effort <- lmer(SHQ_FLARE_EASY_meantal_effort ~ as.factor(session_number) + age + sex + protocol + protocol*session_number + years_of_education + hourse_of_sleep_Baseline + (1|id) , data = mental_effort_1, na.action = na.exclude)

FLARE_EASY <- anova (lmer_SHQ_FLARE_EASY_meantal_effort )

write.csv(FLARE_EASY, 'C:\\Users\\adria\\Desktop\\FLARE_EASY.csv')

FLARE_EASY_ETA <- eta_sq(lmer_SHQ_FLARE_EASY_meantal_effort )
write.csv(FLARE_EASY_ETA, 'C:\\Users\\adria\\Desktop\\FLARE_EASY_ETA.csv')

hist(resid(lmer_SHQ_FLARE_EASY_meantal_effort ))

shapiro.test(resid(lmer_SHQ_FLARE_EASY_meantal_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_FLARE_EASY_meantal_effort )

lms_SHQ_FLARE_EASY_meantal_effort <- lsmeans(lmer_SHQ_FLARE_EASY_meantal_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_FLARE_EASY_meantal_effort

write.csv(lms_SHQ_FLARE_EASY_meantal_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_SHQ_flare_EASY.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - SHQ - flare - HARD

hist (mental_effort$SHQ_FLARE_HARD_mental_effort) 

lmer_SHQ_FLARE_HARD_mental_effort <- lmer(SHQ_FLARE_HARD_mental_effort ~ as.factor(session_number) + age + sex + protocol + protocol*session_number + years_of_education + hourse_of_sleep_Baseline + (1|id)  , data = mental_effort_1, na.action = na.exclude)

FLARE_HARD <- anova (lmer_SHQ_FLARE_HARD_mental_effort )

write.csv(FLARE_HARD, 'C:\\Users\\adria\\Desktop\\FLARE_HARD.csv')

FLARE_HARD_ETA <- eta_sq(lmer_SHQ_FLARE_HARD_mental_effort )
write.csv(FLARE_HARD_ETA, 'C:\\Users\\adria\\Desktop\\FLARE_HARD_ETA.csv')

hist(resid(lmer_SHQ_FLARE_HARD_mental_effort ))

shapiro.test(resid(lmer_SHQ_FLARE_HARD_mental_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_FLARE_HARD_mental_effort )

lms_SHQ_FLARE_HARD_mental_effort <- lsmeans(lmer_SHQ_FLARE_HARD_mental_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_FLARE_HARD_mental_effort

write.csv(lms_SHQ_FLARE_HARD_mental_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_SHQ_flare_HARD.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - SHQ - wayfinding - EASY

hist (mental_effort$SHQ_MAP_EASY_mental_effort) 

lmer_SHQ_MAP_EASY_mental_effort <- lmer(SHQ_MAP_EASY_mental_effort ~ as.factor(session_number) + age + sex + protocol + protocol*session_number + years_of_education + hourse_of_sleep_Baseline + (1|id) , data = mental_effort_1, na.action = na.exclude)

MAP_EASY <- anova (lmer_SHQ_MAP_EASY_mental_effort )
MAP_EASY
write.csv(MAP_EASY, 'C:\\Users\\adria\\Desktop\\MAP_EASY.csv')

MAP_EASY_ETA <- eta_sq(lmer_SHQ_MAP_EASY_mental_effort )
write.csv(MAP_EASY_ETA, 'C:\\Users\\adria\\Desktop\\MAP_EASY_ETA.csv')

hist(resid(lmer_SHQ_MAP_EASY_mental_effort ))

shapiro.test(resid(lmer_SHQ_MAP_EASY_mental_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_MAP_EASY_mental_effort )

lms_HQ_MAP_EASY_mental_effort <- lsmeans(lmer_SHQ_MAP_EASY_mental_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_HQ_MAP_EASY_mental_effort

write.csv(lms_HQ_MAP_EASY_mental_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_SHQ_MAP_EASY.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - SHQ - wayfinding - HARD

hist (mental_effort$SHQ_MAP_HARD_mental_effort) 

lmer_SHQ_MAP_HARD_mental_effort <- lmer(SHQ_MAP_HARD_mental_effort  ~ as.factor(session_number) + age + sex + protocol + protocol*session_number + years_of_education + hourse_of_sleep_Baseline + (1|id), data = mental_effort_1, na.action = na.exclude)

MAP_HARD <- anova (lmer_SHQ_MAP_HARD_mental_effort )
MAP_HARD
write.csv(MAP_HARD, 'C:\\Users\\adria\\Desktop\\MAP_HARD.csv')

MAP_HARD_ETA <- eta_sq(lmer_SHQ_MAP_HARD_mental_effort )
write.csv(MAP_HARD_ETA, 'C:\\Users\\adria\\Desktop\\MAP_HARD_ETA.csv')

hist(resid(lmer_SHQ_MAP_HARD_mental_effort ))

shapiro.test(resid(lmer_SHQ_MAP_HARD_mental_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_MAP_HARD_mental_effort )

lms_SHQ_MAP_HARD_mental_effort <- lsmeans(lmer_SHQ_MAP_HARD_mental_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_MAP_HARD_mental_effort 

write.csv(lms_SHQ_MAP_HARD_mental_effort , 'C:\\Users\\adria\\Desktop\\Mental_effort_SHQ_MAP_HARD.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - Episodic memory - Encoding

hist (mental_effort$Episodic_memory_ENCODING) 

lmer_Episodic_memory_ENCODING <- lmer(Episodic_memory_ENCODING ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_Episodic_memory_ENCODING )

eta_sq(lmer_Episodic_memory_ENCODING )

hist(resid(lmer_Episodic_memory_ENCODING ))

shapiro.test(resid(lmer_Episodic_memory_ENCODING ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_Episodic_memory_ENCODING )

lms_Episodic_memory_ENCODING <- lsmeans(lmer_Episodic_memory_ENCODING , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_Episodic_memory_ENCODING

write.csv(lms_Episodic_memory_ENCODING , 'C:\\Users\\adria\\Desktop\\Mental_effort_Episodic_Memory_Encoding.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - Episodic memory - Recall

hist (mental_effort$Episodic_memory_RECALL) 

lmer_Episodic_memory_RECALL <- lmer(Episodic_memory_RECALL ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_Episodic_memory_RECALL )

eta_sq(lmer_Episodic_memory_RECALL )

hist(resid(lmer_Episodic_memory_RECALL ))

shapiro.test(resid(lmer_Episodic_memory_RECALL ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_Episodic_memory_RECALL )

lms_Episodic_memory_RECALL <- lsmeans(lmer_Episodic_memory_RECALL , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_Episodic_memory_RECALL 

write.csv(lms_Episodic_memory_RECALL, 'C:\\Users\\adria\\Desktop\\Mental_effort_Episodic_Memory_Recall.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - overall

hist (mental_effort$How_cognitively_demanding_was_all_the_session) 

lmer_How_cognitively_demanding_was_all_the_session <- lmer(How_cognitively_demanding_was_all_the_session ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_How_cognitively_demanding_was_all_the_session )

eta_sq(lmer_How_cognitively_demanding_was_all_the_session )

hist(resid(lmer_How_cognitively_demanding_was_all_the_session ))

shapiro.test(resid(lmer_How_cognitively_demanding_was_all_the_session ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_How_cognitively_demanding_was_all_the_session )

lms_How_cognitively_demanding_was_all_the_session <- lsmeans(lmer_How_cognitively_demanding_was_all_the_session , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_How_cognitively_demanding_was_all_the_session

write.csv(lms_How_cognitively_demanding_was_all_the_session, 'C:\\Users\\adria\\Desktop\\Mental_effort_How_cognitively_demanding_was_all_the_session.csv', row.names=F)

##############################################################################################################################################################
# NORMALIZED to t1
##############################################################################################################################################################

# Mixed models - mental effort - PVT

hist (mental_effort$PVT_normalized) 

lmer_PVT_mental_efforts <- lmer(PVT_normalized ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_PVT_mental_efforts )

eta_sq(lmer_PVT_mental_efforts )

hist(resid(lmer_PVT_mental_efforts ))

shapiro.test(resid(lmer_PVT_mental_efforts ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_PVT_mental_efforts )

lms_PVT_mental_effort <- lsmeans(lmer_PVT_mental_efforts , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_PVT_mental_effort

write.csv(lms_PVT_mental_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_PVT_normalized.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - n-back

hist (mental_effort$n_back_normalized) 

lmer_n_back_mental_effort <- lmer(n_back_normalized ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_n_back_mental_effort )

eta_sq(lmer_n_back_mental_effort )

hist(resid(lmer_n_back_mental_effort ))

shapiro.test(resid(lmer_n_back_mental_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_n_back_mental_effort )

lms_n_back_mental_effort <- lsmeans(lmer_n_back_mental_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_n_back_mental_effort

write.csv(lms_n_back_mental_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_n_back_normalized.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - SHQ - flare - EASY

hist (mental_effort$SHQ_Flare_Easy_normalized) 

lmer_SHQ_FLARE_EASY_meantal_effort <- lmer(SHQ_Flare_Easy_normalized ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_SHQ_FLARE_EASY_meantal_effort )

eta_sq(lmer_SHQ_FLARE_EASY_meantal_effort )

hist(resid(lmer_SHQ_FLARE_EASY_meantal_effort ))

shapiro.test(resid(lmer_SHQ_FLARE_EASY_meantal_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_FLARE_EASY_meantal_effort )

lms_SHQ_FLARE_EASY_meantal_effort <- lsmeans(lmer_SHQ_FLARE_EASY_meantal_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_FLARE_EASY_meantal_effort

write.csv(lms_SHQ_FLARE_EASY_meantal_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_SHQ_flare_EASY_normalized.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - SHQ - flare - HARD

hist (mental_effort$SHQ_Flare_hard_normalized) 

lmer_SHQ_FLARE_HARD_mental_effort <- lmer(SHQ_Flare_hard_normalized ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_SHQ_FLARE_HARD_mental_effort )

eta_sq(lmer_SHQ_FLARE_HARD_mental_effort )

hist(resid(lmer_SHQ_FLARE_HARD_mental_effort ))

shapiro.test(resid(lmer_SHQ_FLARE_HARD_mental_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_FLARE_HARD_mental_effort )

lms_SHQ_FLARE_HARD_mental_effort <- lsmeans(lmer_SHQ_FLARE_HARD_mental_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_FLARE_HARD_mental_effort

write.csv(lms_SHQ_FLARE_HARD_mental_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_SHQ_flare_HARD_normalized.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - SHQ - wayfinding - EASY

hist (mental_effort$SHQ_Map_easy_normalzied) 

lmer_SHQ_MAP_EASY_mental_effort <- lmer(SHQ_Map_easy_normalzied ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_SHQ_MAP_EASY_mental_effort )

eta_sq(lmer_SHQ_MAP_EASY_mental_effort )

hist(resid(lmer_SHQ_MAP_EASY_mental_effort ))

shapiro.test(resid(lmer_SHQ_MAP_EASY_mental_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_MAP_EASY_mental_effort )

lms_HQ_MAP_EASY_mental_effort <- lsmeans(lmer_SHQ_MAP_EASY_mental_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_HQ_MAP_EASY_mental_effort

write.csv(lms_HQ_MAP_EASY_mental_effort, 'C:\\Users\\adria\\Desktop\\Mental_effort_SHQ_MAP_EASY_normalzied.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - SHQ - wayfinding - HARD

hist (mental_effort$SHQ_Map_hard_normalized) 

lmer_SHQ_MAP_HARD_mental_effort <- lmer(SHQ_Map_hard_normalized ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_SHQ_MAP_HARD_mental_effort )

eta_sq(lmer_SHQ_MAP_HARD_mental_effort )

hist(resid(lmer_SHQ_MAP_HARD_mental_effort ))

shapiro.test(resid(lmer_SHQ_MAP_HARD_mental_effort ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_SHQ_MAP_HARD_mental_effort )

lms_SHQ_MAP_HARD_mental_effort <- lsmeans(lmer_SHQ_MAP_HARD_mental_effort , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_SHQ_MAP_HARD_mental_effort 

write.csv(lms_SHQ_MAP_HARD_mental_effort , 'C:\\Users\\adria\\Desktop\\Mental_effort_SHQ_MAP_HARD_normalized.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - Episodic memory - Encoding

hist (mental_effort$EM_encoding_normalzied) 

lmer_Episodic_memory_ENCODING <- lmer(EM_encoding_normalzied ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_Episodic_memory_ENCODING )

eta_sq(lmer_Episodic_memory_ENCODING )

hist(resid(lmer_Episodic_memory_ENCODING ))

shapiro.test(resid(lmer_Episodic_memory_ENCODING ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_Episodic_memory_ENCODING )

lms_Episodic_memory_ENCODING <- lsmeans(lmer_Episodic_memory_ENCODING , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_Episodic_memory_ENCODING

write.csv(lms_Episodic_memory_ENCODING , 'C:\\Users\\adria\\Desktop\\Mental_effort_Episodic_Memory_Encoding_normalized.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - Episodic memory - Recall

hist (mental_effort$EM_recall_normalized) 

lmer_Episodic_memory_RECALL <- lmer(EM_recall_normalized ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_Episodic_memory_RECALL )

eta_sq(lmer_Episodic_memory_RECALL )

hist(resid(lmer_Episodic_memory_RECALL ))

shapiro.test(resid(lmer_Episodic_memory_RECALL ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_Episodic_memory_RECALL )

lms_Episodic_memory_RECALL <- lsmeans(lmer_Episodic_memory_RECALL , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_Episodic_memory_RECALL 

write.csv(lms_Episodic_memory_RECALL, 'C:\\Users\\adria\\Desktop\\Mental_effort_Episodic_Memory_Recall_normalzied.csv', row.names=F)

############################################################################################################################################################

# Mixed models - mental effort - overall

hist (mental_effort$overall_normalized) 

lmer_How_cognitively_demanding_was_all_the_session <- lmer(overall_normalized ~ as.factor(session_number) + age + sex + protocol + protocol*session_number +  (1|id) , data = mental_effort_1, na.action = na.exclude)

anova (lmer_How_cognitively_demanding_was_all_the_session )

eta_sq(lmer_How_cognitively_demanding_was_all_the_session )

hist(resid(lmer_How_cognitively_demanding_was_all_the_session ))

shapiro.test(resid(lmer_How_cognitively_demanding_was_all_the_session ))

# collinearity --> if VIF > 10 - that means that predictors are highly correlated 

check_collinearity(lmer_How_cognitively_demanding_was_all_the_session )

lms_How_cognitively_demanding_was_all_the_session <- lsmeans(lmer_How_cognitively_demanding_was_all_the_session , ~ session_number*protocol) # plot main effect of: genotype, protocol
lms_How_cognitively_demanding_was_all_the_session

write.csv(lms_How_cognitively_demanding_was_all_the_session, 'C:\\Users\\adria\\Desktop\\Mental_effort_How_cognitively_demanding_was_all_the_session_normalized.csv', row.names=F)

