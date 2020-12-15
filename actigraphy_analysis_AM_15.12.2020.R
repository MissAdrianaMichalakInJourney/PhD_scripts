
# ACTIGRAPHY ANALYSIS - final - Adriana Michalak - 15.12.2020

# ANALYSES: analyses for ALL participants in a given directory; OUTCOMES: outcomes are generated for ALL participants (merged single outcome files)
# TYPE of ANALYSES: Non-Parametric Circadian Rhythm Analysis (NPCRA) (https://www.camntech.com/motionware-npcra/)
# USED DEVICE: MotionWatch 8 (CamNtech)
# REQUIRERD FILE FORMAT: .csv - extracted from MotionWear 8 (CamNtech) software
# 'ppt' = participant 

library(stringr)
library (stringi)

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\data") # set directory

getwd()

files <-dir(path = "C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\data", recursive = TRUE,include.dirs = TRUE,pattern = "^APO(.*)csv$") # search for files including string "APO"
print (files)

# the for loop will go through the same steps for all participants 

for (k in 1:length(files))
{
  
  my_data <- read.csv(files[k], header = FALSE)
  
  # take the ppt ID
  
  col2 = 2; # adjusted to your data format, i.e., it is possible to include more participant details while setting up the actiwatch, therefore, ID may be in a different row
  
  participant_ID <- data.frame(my_data[c(1:1), c(col2)])
  names(participant_ID)[1] <- "participant_ID"

  form = sprintf('%s', participant_ID)

  
  # col 1 and 2 - columns of interest
  
  # col1=2; # rounded values
  col3 = 3; # if you want to use unrounded values
  
  # extract values of interest - I'm following the Non-Parametric Circadian Rhythm Analysis (NPCRA) extracted directly from the MotionWear 8 software (https://www.camntech.com/motionware-npcra/)
  
  length_of_analysis <- data.frame(my_data[c(5:5), c(col3)])
  names(length_of_analysis)[1] <- "Length_of_analysis_sec"  # 24h = 86400 s
  
  L5_Average <- data.frame(my_data[c(6:6), c(col3)])
  names(L5_Average)[1] <- "L5_Average"
  
  L5_Start_Hour <- data.frame(my_data[c(7:7), c(col3)])
  names(L5_Start_Hour)[1] <- "L5_Start_Hour"
  
  M10_Average <- data.frame(my_data[c(8:8), c(col3)])
  names(M10_Average)[1] <- "M10_Average"
  
  M10_Start_Hour <- data.frame(my_data[c(9:9), c(col3)])
  names(M10_Start_Hour)[1] <- "M10_Start_Hour"
  
  RA_Relative_Amplitude <- data.frame(my_data[c(10:10), c(col3)])
  names(RA_Relative_Amplitude)[1] <- "RA_Relative_Amplitude"
  
  IS_Interdaily_Stability <- data.frame(my_data[c(11:11), c(col3)])
  names(IS_Interdaily_Stability)[1] <- "IS_Interdaily_Stability"
  
  IV_Intra_daily_Variability <- data.frame(my_data[c(12:12), c(col3)])
  names(IV_Intra_daily_Variability)[1] <- "IV_Intra_daily_Variability"
  
  
  # creat a nice data frame - the script will work well only if ALL the data files looks the same, e.g., RA infor is always in row 10, column 3 or IV is in row 12, column 3; if you add more information for some participants, e.g., name and last name, it can shift the arrangement of the rows
  
  actigraphy <- data.frame(participant_ID, length_of_analysis, L5_Average,  L5_Start_Hour, M10_Average, M10_Start_Hour, RA_Relative_Amplitude,
                           IS_Interdaily_Stability, IV_Intra_daily_Variability)
  
  print (actigraphy)
  
  # save the database to .csv file
  
  form = files
  pathOut= ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\outcomes\\")
  write.csv(actigraphy, paste(pathOut,file = form[k]),row.names=F)
  
}

###### merge all the outcomes into one large database 

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\outcomes\\") # set your directory
#select file names to be merged, i.e., outcomes of the for loop above
merge_all <- list.files (path="C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\outcomes\\", pattern = "APO") # change to your pattern or just remove the last bit, i.e., "pattern= "APO"

print(merge_all)

# lapply returns a list of the same length as X
to_combine <- lapply(merge_all,function(l) {read.csv(l)
})
to_combine

# merging together all files 
outcomes_all_ppt_actigraphy <- do.call (rbind.data.frame, to_combine)

write.csv (outcomes_all_ppt_actigraphy,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\Actigraphy_database_14.11.2020.csv",row.names=F)

##############################
# calculate basic stats 
##############################

setwd ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy") # # change the path

my.data.all <-read.csv("Actigraphy_database_14.11.2020.csv") 

actigraphy_all_ppt <- data.frame("MEAN_Length_of_analysis"=mean(my.data.all$Length_of_analysis_sec), "MEAN_L5_Average"= mean(my.data.all$L5_Average),"RANGE_L5_Start_Hour"=range(my.data.all$L5_Start_Hour), "MEAN_M10_Average"=mean(my.data.all$M10_Average),
                                 "RANGE_M10_Start_Hour"=range(my.data.all$M10_Start_Hour), "MEAN_RA_Relative_Amplitude"=mean(my.data.all$RA_Relative_Amplitude),"RANGE_RA_Relative_Amplitude"=range(my.data.all$RA_Relative_Amplitude), "SD_RA_Relative_Amplitude"=sd(my.data.all$RA_Relative_Amplitude),
                         "MEAN_IS_Interdaily_Stability"=mean(my.data.all$IS_Interdaily_Stability),"RANGE_IS_Interdaily_Stability"=range(my.data.all$IS_Interdaily_Stability),  "SD_IS_Interdaily_Stability"=sd(my.data.all$IS_Interdaily_Stability), "MEAN_IV_Intra_daily_Variability"=mean(my.data.all$IV_Intra_daily_Variability), "RANGE_IV_Intra_daily_Variability"=range(my.data.all$IV_Intra_daily_Variability), "SD_IV_Intra_daily_Variability"=sd(my.data.all$IV_Intra_daily_Variability))

write.csv (actigraphy_all_ppt,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\actigraphy\\Actigraphy_all_ppt_basic_outcomes_14.11.2020.csv",row.names=F) # change the path
