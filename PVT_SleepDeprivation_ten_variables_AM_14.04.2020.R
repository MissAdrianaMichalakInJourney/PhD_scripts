
# Psychomotor Vigilance Task (PVT) - SleepDeprivation - Adriana Michalak - 14.04.2020 

# analysis follow Dinges --> https://academic.oup.com/sleep/article/34/5/581/2281465

# ANALYSES: analyses for ALL participants in given directory; OUTCOMES: outcomes are generated for ALL participants (merged single outcome files)

library(stringr)

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\PVT\\Data\\SleepDeprivation\\") #set directory

getwd()

files <-dir(path = "C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\PVT\\Data\\SleepDeprivation\\", recursive = TRUE,include.dirs = TRUE,pattern = "^APO(.*)csv$")
print (files)


for (k in 1:length(files))
{
  
  my.data <- read.csv(files[k])
  
  save.name1 <-sub("_t[[:digit:]]+","", files)  
  save.name <- sub(".csv","", save.name1)  
  form = sprintf('%s', save.name)
  
  save.name1 <-sub("[_APO[:digit:]]+","", files) 
  save.name <-sub(".csv", "",save.name1) 
  form1 = sprintf('%s', save.name)
  
  # pressing the wrong button or failing to release the button for 3 s or longer were counted as errors and excluded from the analysis
  
  # RT_without_errors <- subset(my.data, Stimulus.RT <= 3000) # I cannot figure it out using existing PVT designed in E-Prime
  
  # The following PVT outcome metrics were assessed and included in the analyses: 
  
  # (1) median RT
  
  median_RT <- median(my.data$Stimulus.RT)
  
  # (2) mean RT
  
  mean_RT <- round (mean (my.data$Stimulus.RT), digits =2)
  
  # (3) fastest 10% RT
  
  fastest_10_RT <- round(mean(my.data$Stimulus.RT[my.data$Stimulus.RT<=quantile(my.data$Stimulus.RT, 0.1, na.rm=TRUE)], na.rm=TRUE), digits=2)
  
  # (4) mean 1/RT (also called reciprocal response time or response speed)

  
  reciprocal_RT <- mean(1/(my.data$Stimulus.RT))
  print (reciprocal_RT)
  
  # 5) slowest 10% 1/RT
  
  slowest_10_RT <- round(mean(my.data$Stimulus.RT[my.data$Stimulus.RT>=quantile(my.data$Stimulus.RT, 0.9, na.rm=TRUE)], na.rm=TRUE),digits=2)
  
  slowest_10_reciprocal_RT <- (1/slowest_10_RT)
  
  # (6) number of lapses --> Lapses (errors of omission) were defined as RTs = 500 ms
  
  lapses <- subset (my.data, Stimulus.RT >=500)
  print (lapses)
  number_of_lapses <- nrow(lapses)
  print (number_of_lapses)
  
  # (7) lapse probability (i.e., number of lapses divided by the number of valid stimuli, excluding false starts)
  
  number_of_trials_1 <- subset(my.data, Stimulus.RT >= 100)
  
  number_of_trials <- nrow(number_of_trials_1)
  
  lapses_probability <- number_of_lapses/number_of_trials
  print (lapses_probability)
  
  # (8) number of false starts --> Responses without a stimulus or RTs < 100 ms were counted as false starts (errors of commission)
  
  false_alarm <- subset(my.data, Stimulus.RT <=100)
  number_of_false_starts <- nrow (false_alarm)
  print (number_of_false_starts)
  
  # (9) number of lapses and false starts
  
  number_of_lapses_and_false_starts <- number_of_lapses + number_of_false_starts
  
  # (10) performance score, defined as 1 minus the number of lapses and false starts divided by the number of valid stimuli (including false starts)
  
  number_of_trials_all <- nrow (my.data)
  print(number_of_trials_all)
  performance_score <- 1- (number_of_lapses_and_false_starts / number_of_trials_all)
  
  #creat data frame (df) with main outcome variables 
  
  RT1<- data.frame ('participant_ID'= form[k],'session_number'=form1[k],'protocol_1_SD_2_MN'=1, 'date' = my.data$SessionDate, 'time' = my.data$SessionTime,'median_RT'=median_RT,
                    'mean_RT'= mean_RT, 'fastest_10_RT'= fastest_10_RT, 'slowest_10_RT' = slowest_10_RT, 'reciprocal_10_slowest_RT'=slowest_10_reciprocal_RT, 'response_speed' = reciprocal_RT,
                    'number_of_lapses'=number_of_lapses, 'lapses_probability' = lapses_probability, 'number_of_false_starts'= number_of_false_starts, 
                    'number_of_lapses_and_false_starts'= number_of_lapses_and_false_starts, 'performance_score'= performance_score)
  print(RT1)    
  
  RT <- data.frame(RT1[c(1:1), c(1:16)])
  print (RT)
  
  form = files
  pathOut= ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\PVT\\Outcomes\\SleepDeprivation\\ten_variables\\")
  write.csv(RT,paste(pathOut,file = form[k]),row.names=F)
  
  
}



###### merge all the outcomes into one large database 
setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\PVT\\Outcomes\\SleepDeprivation\\ten_variables\\") #set directory
#select file names to be merged, i.e., outcomes of the for loop above
merge <- list.files (path="C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\PVT\\Outcomes\\SleepDeprivation\\ten_variables\\", pattern = "APO")
print(merge)

#
daisy <- lapply(merge,function(l) {read.csv(l)
})
daisy

#
outcomes.all.ppt_PVT_MultiNap <- do.call (rbind.data.frame, daisy)

write.csv (outcomes.all.ppt_PVT_MultiNap,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\PVT\\Outcomes\\PVT_SleepDeprivation_ten_variables_AM_16.12.2020.csv",row.names=F)

