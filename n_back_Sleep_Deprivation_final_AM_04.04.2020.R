
# n-back task - Sleep deprivation - Adriana Michalak - 04.04.2020

# the task was implemented in the E-Prime sofware by AM using stimuli provided by the Liege group (10 diffrent sets of 75 one-back and 75 two-back stimuli)

# ANALYSES: analyses for ALL participants in given directory; OUTCOMES: outcomes are generated for ALL participants (merged single outcome files)

library(stringr)

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\n_back\\Data\\SleepDeprivation") #set directory

getwd()

files <-dir(path = "C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\n_back\\Data\\SleepDeprivation", recursive = TRUE,include.dirs = TRUE,pattern = "^APO(.*)csv$")
print (files)


for (k in 1:length(files))
{ 
  
  my.data <- read.csv(files[k], skip=1) # remove 1st row 
  
  # take series number, i.e, t1, t2, ect. 
  
  save.name1 <-sub("_t[[:digit:]]+","", files)  
  save.name <- sub(".csv","", save.name1)  
  form = sprintf('%s', save.name)
  
  # take ppt ID 
  
  save.name1 <-sub("[_APO[:digit:]]+","", files) 
  save.name <-sub(".csv", "",save.name1) 
  form1 = sprintf('%s', save.name)
  
  
  mean_one_back_RT <- (round(mean(my.data$Stimulus.RT[1:75]),digits=2))
  print (mean_one_back_RT)
  
  mean_two_back_RT <- (round(mean(my.data$Stimulus.RT[76:150]), digits=2))
  print (mean_two_back_RT)
  
  
  mean_one_back_ACC <- sum(my.data$Stimulus.ACC[1:75])
  print (mean_one_back_ACC)
  
  mean_two_back_ACC <- sum(my.data$Stimulus.ACC[76:150])
  print (mean_two_back_ACC)
  
  # creat database with variables of interest 
  
  variables1 <- data.frame('participant_ID'= form[k],'session_number'=form1[k],'protocol_1_SD_2_MN'=1,'date' = my.data$SessionDate, 'time' = my.data$SessionTime, 'RT_ONE_back'=mean_one_back_RT, 'RT_TWO_back'=mean_two_back_RT, 'accuracy_ONE_back'=mean_one_back_ACC, 'accuracy_TWO_back'=mean_two_back_ACC)
  print(variables1)
  
  variables_1 <- data.frame(variables1[c(1:1), c(1:9)])
  print (variables_1)
  
  variables <- na.omit(variables_1)
  
  form = files
  pathOut= ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\n_back\\Outcomes\\SleepDeprivation\\")
  write.csv(variables,paste(pathOut,file = form[k]),row.names=F)
  
}


###### merge all the outcomes into one large database 

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\n_back\\Outcomes\\SleepDeprivation\\") #set directory
#select file names to be merged, i.e., outcomes of the for loop above
srosia <- list.files (path="C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\n_back\\Outcomes\\SleepDeprivation\\", pattern = "APO")
print(srosia)

# lapply returns a list of the same length as X

daisy <- lapply(srosia,function(l) {read.csv(l)
})
daisy

# marging together all files 

outcomes_n_back_Sleep_Deprivation <- do.call (rbind.data.frame, daisy)

write.csv (outcomes_n_back_Sleep_Deprivation,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\n_back\\Outcomes\\n_back_Sleep_Deprivation_Final_16.12.2020.csv",row.names=F)
