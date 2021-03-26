
# ACTIGRAPHY analysis - marging files obtained using pyActigraphy - AM - 26.03.2021

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

getwd() # get your directory to just double check that directory is the correct 

files <-dir(path = "C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\data\\data_actigraphy_pyActigraphy_AM_24.03.2021", recursive = TRUE,include.dirs = TRUE,pattern = "(.*)csv$") # change the directory 

print (files) # print out all file which have been found in a given directory 

setwd ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\data\\data_actigraphy_pyActigraphy_AM_24.03.2021")

#select file names to be merged, i.e., outcomes of the for loop above
srosia <- list.files (path="C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\data\\data_actigraphy_pyActigraphy_AM_24.03.2021", pattern = "APO")
print(srosia)

# lapply returns a list of the same length as X

daisy <- lapply(srosia,function(l) {read.csv(l)
})
daisy

# marging together all files 

outcomes_pyActigraphy_all <- do.call (rbind.data.frame, daisy)

write.csv (outcomes_pyActigraphy_all,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\pyActigraphy_all_AM_25.03.2021.csv",row.names=F)

#########################
# demographics - AM - 25.03.2021
###################################

data <- read.csv("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\Actigraphy_database_and_screening_variables_25.03.2020_AM.csv")

# variables for demographics table

data_demographics <- data.frame('id'=data$participant_ID,'age'=data$Age, 'Sex'=data$Sex_M_1_F_2, 'genotype'=data$Genotype_1_e4_positive_2_e4_negative,
                                'years_of_education'=data$years_spent_in_education, 'mACE'=data$mACE_total_score, 'CCI'=data$CCI_Cognition_total, 'PHQ_9'=data$PHQ_9, 
                                'GAD_7'=data$GAD_7,'ISI'= data$ISI,'ESS'=data$ESS, 'PSQI'=data$PSQI,'trail_making_B_time'=data$TM_Part_B_sec)


# demographics - split by genotypes

demographics <- split(data_demographics, data_demographics$genotype) 

table_one <- tableby(genotype ~ age + Sex + years_of_education + mACE + CCI + PHQ_9 + PSQI + GAD_7 + ESS + ISI + PSQI, numeric.stats=c("meansd"), 
                     data = data_demographics)

# e4+ - high-risk =1=male and e4- avarage risk =2=female

mylabels <- list(age = "age", sex ="sex", years_of_education="years of education",mACE= "mini-Addenbrooke's Cognitive Examination (mACE)", 
                 CCI = 'Cognitive Change Cndex (CCI)',PHQ_9 = 'Patient Health Questionnaire-9 (PHQ-9)', GAD_7= 'General Anxiety Disorder-7 (GAD-7)', ESS='Epworth Sleepiness Scale (ESS)',
                 ISI= 'Insomnia Severity Index',  PSQI='Pittsburgh Sleep Quality Index (PSQI)',numeric.stats=c("meansd"), data = data_demographics)

table_onee <- summary(table_one, labelTranslations = mylabels, text=TRUE)

#write2word(table_one, "C:\\Users\\adria\\Desktop\\PhD\\Rrr", title="My table in Word")

write2html(table_onee, "C:\\Users\\adria\\Desktop\\actigraphy_demographics.html")

webshot("C:\\Users\\adria\\Desktop\\actigraphy_demographics.html", "C:\\Users\\adria\\Desktop\\actigraphy_demographics.png")

# variables for demographics table

data_demographics <- data.frame('id'=data$participant_ID,'age'=data$Age, 'Sex'=data$Sex_M_1_F_2, 'genotype'=data$Genotype_1_e4_positive_2_e4_negative,
                                'years_of_education'=data$years_spent_in_education, 'mACE'=data$mACE_total_score, 'CCI'=data$CCI_Cognition_total, 'PHQ_9'=data$PHQ_9, 
                                'GAD_7'=data$GAD_7,'ISI'= data$ISI,'ESS'=data$ESS, 'PSQI'=data$PSQI,'trail_making_B_time'=data$TM_Part_B_sec)

############################################
# actigraphy variables - split by genotypes

data_acti <- read.csv("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\Actigraphy_database_and_screening_variables_25.03.2020_AM.csv")

# variables for demographics table

data_acti_ <- data.frame('genotype'=data_acti$Genotype_1_e4_positive_2_e4_negative,'number_of_days'=data_acti$full_days,'Interdaily_stability'=data_acti$Interdaily_stability, 
                         'Interdaily_variability'=data_acti$Intradaily.variability, 'Relative_amplitude'=data_acti$Relative_Amplitude, 'Rest_to_Activity'=data_acti$Rest_to_activity, 
                         'Activity_to_Rest'=data_acti$Activity_to_rest, 'Cosinor_Amplitude'=data_acti$Amplitude, 'Cosinor_Acrophase'=data_acti$Acrophase, 'Cosinor_Mesor'=data_acti$Mesor)


actigraphy_variables <- split(data_acti_, data_acti_$genotype) 

table_two <- tableby(genotype ~ number_of_days + Interdaily_stability + Interdaily_variability + Relative_amplitude + Rest_to_Activity + Activity_to_Rest +
                       Cosinor_Amplitude + Cosinor_Acrophase + Cosinor_Mesor, numeric.stats=c("meansd"), 
                     data = data_acti_)

# e4+ - high-risk =1=male and e4- avarage risk =2=female

mylabels_2 <- list( number_of_days = 'number of days', Interdaily_stability = 'Interdaily Stability (IS)', Interdaily_variability = 'Interdaily Variability (Iv)',
                 Relative_amplitude = 'Relative Amplitude (RA)', Rest_to_Activity='Rest to Activity (kRA)', Activity_to_Rest = 'Activity to Rest (kAR)',
                 Cosinor_Amplitude = 'Cosinor Amplitude', Cosinor_Acrophase = 'Cosinor Acrophase', Cosinor_Mesor = 'Cosinor Mesor', numeric.stats=c("meansd"), data = data_acti_)
  
table_two <- summary(table_two, labelTranslations = mylabels_2, text=TRUE)

#write2word(table_one, "C:\\Users\\adria\\Desktop\\PhD\\Rrr", title="My table in Word")

write2html(table_two, "C:\\Users\\adria\\Desktop\\actigraphy_variables.html")

webshot("C:\\Users\\adria\\Desktop\\actigraphy_variables.html", "C:\\Users\\adria\\Desktop\\actigraphy_variables.png")

