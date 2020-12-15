
# Sea Hero Quest (SHQ) - FLARE levels - MultiNap - Adriana Michalak - 15.12.2020

# the script is extracting exclusively SHQ flare levels only

install.packages("rjson")

# load the package required to read JSON files

library("rjson")

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Data\\SHQ_MultiNap") # set directory

getwd()

files <-dir(path = "C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Data\\SHQ_MultiNap", recursive = TRUE,include.dirs = TRUE,pattern = "*.json")

print (files)

pattern <- "level004|level009|level014|level019|level029|level024" # level004|level009|level014 easy levels; evel19|level29|level24 hard levels 


check_if_flare <- grepl (pattern,files) # matches? TRUE or FALSE
check_if_flare

for (k in 1:length(files)) {
  
  if (check_if_flare[k]==TRUE) {
    
    # give the input file to the function
    
    input_file <- fromJSON(file=files[k])
    
    # Convert JSON file to a data frame.
    json_data_frame <- as.data.frame(input_file)

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
    
    flares <- data.frame ('Participant_ID'= save.name5[k],'protocol_1_SD_2_MN'=2, 'session_number'=save.name6[k], 'level_ID'= json_data_frame$meta.level_id,'flare_accuracy'= json_data_frame$meta.flare_accuracy, 
                          'time_needed_to_complete'=json_data_frame$meta.duration)
    flares
    
    form = sprintf('outcomes_%s', save.name)
    pathOut= ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Outcomes\\SHQ_MultiNap\\Outcomes_flares\\")
    
    filename <- paste(pathOut, form[k], ".csv")
    
    write.csv(flares,filename, row.names = F) 
    
  }
}

###### marge all the outcomes into one large database 

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Outcomes\\SHQ_MultiNap\\Outcomes_flares\\") # set new directory

# select file names to be marged, i.e., outcomes from the for loop (look up)

select_files <- list.files (path="C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Outcomes\\SHQ_MultiNap\\Outcomes_flares\\", pattern = "outcomes") # search for the files with "outcome" in the name in the given directory

print(select_files) # print files which have been found in the comman given in the line above 

# lapply returns a list of the same length as X

daisy <- lapply(select_files,function(l) {read.csv(l)
})
daisy

# let's marge the files, i.e., outcomes generated for each participant

outcomes.all.ppt <- do.call (rbind.data.frame, daisy)

write.csv(outcomes.all.ppt,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\SHQ\\Outcomes\\SHQ_outcomes_flares_MultiNap_AM_15.12.2020.csv",row.names=F) # it will generate the .csv with the outcomes of interest for each participants, we will marge them below


