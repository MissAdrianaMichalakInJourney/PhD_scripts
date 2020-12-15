
# Extracting wayfinding duration + map viewing time from .json - SHQ - Sleep deprivation - Adriana Michalak - 15.05.2020

# install rjson package 

install.packages("rjson")

# load the package required to read JSON files

library("rjson")

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Data\\SHQ_SleepDeprivation") #set directory

getwd()

files <-dir(path = "C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Data\\SHQ_SleepDeprivation", recursive = TRUE,include.dirs = TRUE,pattern = "*.json")

print (files)

# specify which map levels are you using 

pattern <- "level006|level007|level008|level011|level012|level013|level016|level017|level018|level021|level022|level023"

check_if_wayfinding <- grepl (pattern,files) # matches? TRUE or FALSE
check_if_wayfinding

for (k in 1:length(files)) {
  
  if (check_if_wayfinding[k]==TRUE) {
    
    # give the input file to the function
    
    input_file <- fromJSON(file=files[k])
    
    # Convert JSON file to a data frame.
    json_data_frame <- as.data.frame(input_file)
    
    # adjust to your ID names 
    
    save.name1 <-sub(".json", "", files)
    save.name1
    save.name2 <- sub(" -.*", "", save.name1) # ppt ID and the t 
    save.name2
    save.name3 <- sub (".*/", "", save.name1)
    save.name3
    save.name4 <- sub ("_attempt.*", "", save.name3) # level ID 
    save.name4
    save.name <- paste(save.name2, save.name4, sep = "_") # file name
    save.name
    save.name5 <-  sub ("_t.*", "", save.name1) # ppt ID only 
    save.name5
    save.name6 <- sub (".*_t", "", save.name2) # t session only 
    save.name6
    
    wayfinding <- data.frame ('Participant_ID'= save.name5[k],'protocol_1_SD_2_MN'=2,'session_number'=save.name6[k], 'level_ID'= json_data_frame$meta.level_id, 'meta.duration'= json_data_frame$meta.duration, 
                              'map_viewing_duration'=json_data_frame$meta.map_view_duration)
    
    wayfinding
    
    form = sprintf('outcomes_%s', save.name)
    pathOut= ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Outcomes\\SHQ_SleepDeprivation\\Outcomes_wayfinding\\")
    
    filename <- paste(pathOut, form[k], ".csv")
    
    write.csv( wayfinding,filename, row.names = F) 
    
  }
}

###### marge all the outcomes into one large database 

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Outcomes\\SHQ_SleepDeprivation\\Outcomes_wayfinding\\") # set new directory

# select file names to be marged, i.e., outcomes from the for loop (look up)

select_files <- list.files (path="C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Outcomes\\SHQ_SleepDeprivation\\Outcomes_wayfinding\\", pattern = "outcomes") # search for the files with "outcome" in the name in the given directory

print(select_files) # print files which have been found in the comman given in the line above 

# lapply returns a list of the same length as X

daisy <- lapply(select_files,function(l) {read.csv(l)
})
daisy

# let's marge the files, i.e., outcomes generated for each participant

outcomes.all.ppt <- do.call (rbind.data.frame, daisy)

write.csv(outcomes.all.ppt,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Outcomes\\SHQ_outcomes_wayfinding_viewing_duration_SleepDeprivation_AM_15.12.2020.csv",row.names=F) # it will generate the .csv with the outcomes of interest for each participants, we will marge them below


