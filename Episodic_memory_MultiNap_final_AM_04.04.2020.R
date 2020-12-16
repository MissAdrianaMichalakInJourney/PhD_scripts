
# Episodic Memory task - MultiNap - Adriana Michalak - 04.04.2020

# the Episodic memory task consists of encoding and recognition sessions 
# the data were collected using NeurOn platform https://www.neuropsychology.online/cognition

# ANALYSES: analyses for ALL participants in given directory; OUTCOMES: outcomes are generated for ALL participants (merged single outcome files)

library(stringr)
library (stringi)

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\EpisodicMemory\\Data\\MultiNap") #set directory

getwd()

files <-dir(path = "C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\EpisodicMemory\\Data\\MultiNap", recursive = TRUE,include.dirs = TRUE,pattern = "^APO(.*)csv$")
print (files)


for (k in 1:length(files))
{

  my_data <- read.csv(files[k], header = FALSE, skip=1)
  
  # take the ppt ID
  
  save.name1 <-sub("_t[[:digit:]]+","", files)  
  save.name <- sub(".csv","", save.name1)  
  form = sprintf('%s', save.name)
  
  # take series number, i.e, t1, t2, ect. 
  
  save.name1 <-sub("[_APO[:digit:]]+","", files) 
  save.name <-sub(".csv", "",save.name1) 
  form1 = sprintf('%s', save.name)
  
  # time and date 
  
  col2 = 2
  
  # take date 
  time_and_date1 <- (my_data[c(1:1), c(col2)])
  date<-stri_sub (time_and_date1,1,10)
  
  # take time 
  time_and_date <- (my_data[c(1:1), c(col2)])
  time<-stri_sub (time_and_date,12,16)
  
  # time taken to complete the task 
  
  col3 = 3
  
  time_needed_to_complete_the_task <- data.frame(my_data[c(1:1), c(col3)])
  names(time_needed_to_complete_the_task)[1] <- "time_needed_to_complete_the_task_s"

  print (time_needed_to_complete_the_task)
  
  
  # RECOGNITION MEMORY - ACCURACY
  
  # col 1 and 2
  col1=1;
  col2 = 2;
  
  recognition_memory_accuracy <- t(my_data[c(4:7), c(col1,col2)])
  
  recognition_memory_accuracy <- data.frame('participant_ID'=form[k],'session_number'=form1[k], 'date' = date, 'time'= time, 'protocol_1_SD_2_MN'=2, time_needed_to_complete_the_task, recognition_memory_accuracy)
  print (recognition_memory_accuracy)
  
  colnames(recognition_memory_accuracy) <- as.character(unlist(recognition_memory_accuracy[1,]))
  
  recognition_memory_accuracy = recognition_memory_accuracy[-1, ]
  
  names(recognition_memory_accuracy)[1] <- "participant_ID"
  names(recognition_memory_accuracy)[2] <- "session_number"
  names(recognition_memory_accuracy)[3] <- "date"
  names(recognition_memory_accuracy)[4] <- "time"
  names(recognition_memory_accuracy)[5] <- "protocol_1_SD_2_MN"
  names(recognition_memory_accuracy)[6] <- "time_needed_to_complete_the_task_s"
  names(recognition_memory_accuracy)[7] <- "Recognition_Hits_percentage"
  names(recognition_memory_accuracy)[8] <- "Recognition_Misses_percentage"
  names(recognition_memory_accuracy)[9] <- "Recognition_Correct_Rejections_percentage"
  names(recognition_memory_accuracy)[10] <-"Recognition_False_Alarms_percentage"
  
  row.names(recognition_memory_accuracy) <- NULL
  
  print (recognition_memory_accuracy)
  
  # RECOGNITION MEMORY - REACTION TIME
  
  col3 = 3;
  col4 = 4;
  
  recognition_memory_RT <- t(my_data[c(4:7), c(col3,col4)])
  
  recognition_memory_RT <- data.frame(recognition_memory_RT)
  
  colnames(recognition_memory_RT) <- as.character(unlist(recognition_memory_RT[1,]))
  
  recognition_memory_RT = recognition_memory_RT[-1, ]

  names(recognition_memory_RT)[1] <- "Recognition_Hits_Accuracy_ms"
  names(recognition_memory_RT)[2] <- "Recognition_Misses_ms"
  names(recognition_memory_RT)[3] <- "Recognition_Correct_Rejections_ms"
  names(recognition_memory_RT)[4] <- "Recognition_False_Alarms_ms"
  
  row.names(recognition_memory_RT) <- NULL
  
  print (recognition_memory_RT)
  
  
  # SOURCE MEMORY / POSITION - ACC and RT
  
  col1 = 1
  col2 = 2
  
  source_memory <- t(my_data[c(11:15), c(col1,col2)])
  
  source_memory <- data.frame(source_memory)
  
  colnames(source_memory) <- as.character(unlist(source_memory[1,]))
  
  source_memory = source_memory[-1, ]
  
  names(source_memory)[1] <- "Source_Memory_Hit_Count"
  names(source_memory)[2] <- "Source_MemoryHit_percentage"
  names(source_memory)[3] <- "Source_Memory_Hit_Reaction_Time_ms"
  names(source_memory)[4] <- "Source_Memory_Miss_percentage"
  names(source_memory)[5] <- "Source_Memory_Miss_Reaction_Time_ms"
  
  row.names(source_memory) <- NULL
  
  print (source_memory)

  # create big data frame - all variables 
  
  Episodic_memory <- data.frame(recognition_memory_accuracy, recognition_memory_RT, source_memory)
  print (Episodic_memory)
    
  form = files
  pathOut= ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\EpisodicMemory\\Outcomes\\MultiNap\\")
  write.csv(Episodic_memory, paste(pathOut,file = form[k]),row.names=F)
  
  #write.csv(Episodic_memory,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\EpisodicMemory\\Outcomes\\MultiNap\\", row.names = FALSE)

}

###### merge all the outcomes into one large database 

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\EpisodicMemory\\Outcomes\\MultiNap\\") #set directory

# select file names to be merged, i.e., outcomes of the for loop above

srosia <- list.files (path="C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\EpisodicMemory\\Outcomes\\MultiNap\\", pattern = "APO")

print(srosia)

# lapply returns a list of the same length as X

daisy <- lapply(srosia,function(l) {read.csv(l)
})
daisy

# merging together all files 

outcomes_Episodic_memory_MultiNap <- do.call (rbind.data.frame, daisy)

write.csv (outcomes_Episodic_memory_MultiNap,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\COGT\\EpisodicMemory\\Outcomes\\Episodic_Memory_Final_MultiNap_16.12.2020.csv",row.names=F)
