
# ACTIGRAPHY data - obtained using - pyActigraphy - linear models -AM - 25.03.2021

library(nnet)
library ("nlme")
library("lme4")
library("lmerTest")
library(extrafont)
loadfonts(device = "win")
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
library(tidyverse)
library(dplyr)
library ("car")
library("arsenal")
library ("car")
library("rcompanion")
library (tab)
library(htmlTable)
library(webshot)


setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy") # set directory
  
data <- read.csv("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\Actigraphy_database_and_screening_variables_25.03.2020_AM.csv")

my_data <- data.frame('id'=data$participant_ID,'age'=data$Age, 'sex'=data$Sex_M_1_F_2, 'genotype'=data$Genotype_1_e4_positive_2_e4_negative,
                                'years_of_education'=data$years_spent_in_education, 'mACE'=data$mACE_total_score, 'CCI'=data$CCI_Cognition_total, 'PHQ_9'=data$PHQ_9, 
                                'GAD_7'=data$GAD_7,'ISI'= data$ISI,'ESS'=data$ESS, 'PSQI'=data$PSQI,'trail_making_B_time'=data$TM_Part_B_sec, 'number_of_days'=data$full_days,'Interdaily_stability'=data$Interdaily_stability, "Munich_chronotype"=data$MEQ_Total_score, 
                                'Interdaily_variability'=data$Intradaily.variability, 'Relative_Amplitude'=data$Relative_Amplitude, 'Rest_to_Activity'=data$Rest_to_activity, 
                                'Activity_to_Rest'=data$Activity_to_rest, 'Cosinor_Amplitude'=data$Amplitude, 'Cosinor_Acrophase'=data$Acrophase, 'Cosinor_Mesor'=data$Mesor)




# linear model - Interdaily Stability (IS)

Interdaily_Stability <- lm( Interdaily_stability ~ age + sex + genotype + ESS + PHQ_9 + PSQI + CCI + trail_making_B_time + Munich_chronotype + Rest_to_Activity, data = my_data, na.action=na.exclude)

summary(Interdaily_Stability)

tab_model(Interdaily_Stability)

check_collinearity(Interdaily_Stability)

qqnorm(resid(Interdaily_Stability))
qqline(resid(Interdaily_Stability))

res <- resid(Interdaily_Stability)

plot(Interdaily_Stability, which=1, col=c("blue")) 

plot(Interdaily_Stability, which=2, col=c("green")) 

plot(Interdaily_Stability, which=5, col=c("pink"))

# plot - CCI and PHQ-9 

ggplot(Interdaily_Stability, aes(x=Interdaily_stability, y=genotype)) +
  geom_point(shape=21, size=2, fill="yellow", color="#8a92a1") +
  ylab("Residuals") + xlab("Patient Health Questionnaire-9 (PHQ-9)") +
  geom_smooth(method=lm, color = "yellow", fill="#8a92a1", size=1, alpha=0.3) +   # Add linear regression line 
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = 'black'),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14))


# linear model - Relative_Amplitude

Relative_Amplitude <- lm(Relative_Amplitude ~ age + sex + genotype + ESS + PHQ_9 + PSQI + CCI + trail_making_B_time + Munich_chronotype, data = my_data, na.action=na.exclude)

summary(Relative_Amplitude)

tab_model(Relative_Amplitude)

check_collinearity(Relative_Amplitude)

qqnorm(resid(Relative_Amplitude))
qqline(resid(Relative_Amplitude))

res <- resid(Relative_Amplitude)

plot(Relative_Amplitude, which=1, col=c("blue")) 

plot(Relative_Amplitude, which=2, col=c("green")) 

plot(Relative_Amplitude, which=5, col=c("pink"))



# linear model - Activity to Rest

Activity_to_Rest <- lm(Activity_to_Rest ~ age + sex + genotype + ESS + PHQ_9 + PSQI + CCI + trail_making_B_time + Munich_chronotype, data = my_data, na.action=na.exclude)

summary(Activity_to_Rest)

tab_model(Activity_to_Rest)

check_collinearity(Activity_to_Rest)

qqnorm(resid(Activity_to_Rest))
qqline(resid(Activity_to_Rest))

res <- resid(Activity_to_Rest)

plot(Activity_to_Rest, which=1, col=c("blue")) 

plot(Activity_to_Rest, which=2, col=c("green")) 

plot(Activity_to_Rest, which=5, col=c("pink"))
