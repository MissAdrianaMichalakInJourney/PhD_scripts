
# Supermarket task - FINAL - averages - suitable for analysis of the data collected BEFORE the update in April 2019 - author: Adriana Michalak - 11.05.2020

# update in April 2019 - variables related to map location were updated to be in % instead of being in pixels (datafiles after the updated starts with "supermarket + specified ID")

# ANALYSES: analyses are done for ALL participants; OUTCOMES: outcomes are generated for ALL participants (merged single outcome .csv file) - avarages of all the trials are calculated


library(stringr)


setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\Supermarket_AM\\Data\\before_update") # set directory

getwd() # get your directory to just double check that directory is the correct one

files <-dir(path = "C:\\Users\\adria\\Desktop\\PhD\\Rrr\\Supermarket_AM\\Data\\before_update", recursive = TRUE,include.dirs = TRUE,pattern = "^supermarket(.*)csv$") # change the directory 

print (files) # print out all file which have been found in a given directory 

for (k in 1:length(files)) # the for loop is going through all the files which were found in the given directory 
{
  
  my.data.all <- read.csv(files[k]) # read the .csv file
  
  save.name1 <-sub("[_APO[:digit:]]+","", files) 
  save.name1
  save.name2 <-sub("[user/supermarket_]+", "",save.name1)
  save.name2
  save.name <-sub(".csv", "",save.name2) 
  save.name
  form1 = sprintf('%s', save.name)
  
  # remove practice rows 
  
  my.data <-my.data.all[- grep("practice.mp4", my.data.all$video), ] # grep - searches for matches to argument pattern; this line is removing practice trials 
  
  # analyse only trials 1-7 
  
  my.data <-my.data.all[grep("S1Trial", my.data.all$video), ] # grep function is searching for pattern "S1Trial" as in my project we run only trails 1-7
  
  # conversion from pixels to % 
  
  map.location.x <- ((my.data$map.location.x / my.data$map.width) * 100)
  map.location.x
  
  map.location.y <- (my.data$map.location.y / my.data$map.height) * 100
  map.location.y
  
  map.location.correct.x <- ((my.data$map.location.correct.x / my.data$map.width) * 100)
  map.location.correct.x
  
  map.location.correct.y <- ((my.data$map.location.correct.y / my.data$map.height) * 100)
  map.location.correct.y
  
  # variable 1 - egocentric correct
  
  EGO <- ifelse(my.data$egocentric.correct.quadrant==my.data$egocentric.quadrant.reponse,1,ifelse(my.data$egocentric.correct.quadrant!=my.data$egocentric.quadrant.reponse,0,NA))
  
  print (EGO) # outcome variable #1 - print the variable to see the outcomes 
  
  # variable 2 - heading direction correct
  
  heading.direction <- as.vector(my.data$heading.direction) # I need vectors to work on
  
  heading.direction.correct.direction <- as.vector(my.data$heading.direction.correct.direction) 
  
  typeof(heading.direction.correct.direction)
  
  HEADING <- ifelse(heading.direction==heading.direction.correct.direction,1,ifelse(heading.direction!=heading.direction.correct.direction,0,NA))
  
  print (HEADING) # outcome variable #2 - print the variable to see the outcomes 
  
  # variable 3 - allocentric response
  
  ALLO<- sqrt((map.location.x - map.location.correct.x)^2 + (map.location.y - map.location.correct.y)^2)
  
  print(ALLO) # outcome variable #3 - print the variable to see the outcomes 
  
  # variable 4 - border effect 
  
  BorderEffect <- {is.logical(my.data$map.width*0.28<map.location.x+my.data$map.width*0.5&my.data$map.width*0.72>map.location.x+my.data$map.width*0.5)&(my.data$map.height*0.28<map.location.y+my.data$map.height*0.5&my.data$map.height*0.72>map.location.y+my.data$map.height*0.5)}
  
  print(BorderEffect) #outcome variable #4 - print the variable to see the outcomes 
  
  # creat data frame (df) with main outcome variables 
  
  outcomes <- data.frame('ParticipantID'=my.data$Participant.ID,'egocentric correct'= EGO,'heading direction correct'=HEADING, 'allocentric response'=(round(ALLO,digits=2)), 'border effect'=BorderEffect)
  print (outcomes)
  
  # calculate averages 
  
  Border_TRUE <- str_count(outcomes$border.effect, "TRUE")
  Border_effect_TRUE <- sum(Border_TRUE)
  print(Border_effect_TRUE)
  
  Border_FALSE <- str_count(outcomes$border.effect, "FALSE")
  Border_effect_FALSE <- sum(Border_FALSE)
  print(Border_effect_FALSE)
  
  outcomes_averages <- data.frame ('ParticipantID'= form1[k],'egocentric correct'= sum(EGO),'heading direction correct'=sum(HEADING), 'allocentric response_M'=(round(mean(ALLO),digits=2)), 'allocentric response_SD'=(round(sd(ALLO),digits=2)),'border effect TRUE'=Border_effect_TRUE, 'border effect FALSE'=Border_effect_FALSE)
  print (outcomes_averages)
  
  # save the data frame with your outcomes + setting the names of outcome files 
  
  save.name <-sub(".*_", "", files)
  form = sprintf('outcomes_%s', save.name)
  pathOut= ("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\Supermarket_AM\\Outcomes\\before_update\\")
  write.csv(outcomes_averages,paste(pathOut,file = form[k]),row.names=F) # it will generate the .csv with the outcomes of interest for each participants, we will marge them below
  
}

###### marge all the outcomes into one large database 

setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\Supermarket_AM\\Outcomes\\before_update\\") # set new directory

# select file names to be marged, i.e., outcomes from the for loop (look up)

select_files <- list.files (path="C:\\Users\\adria\\Desktop\\PhD\\Rrr\\Supermarket_AM\\Outcomes\\before_update\\", pattern = "outcomes") # search for the files with "outcome" in the name in the given directory

print(select_files) # print files which have been found in the comman given in the line above 

# lapply returns a list of the same length as X

daisy <- lapply(select_files,function(l) {read.csv(l)
})
daisy

# let's marge the files, i.e., outcomes generated for each participant

outcomes.all.ppt <- do.call (rbind.data.frame, daisy)

write.csv (outcomes.all.ppt,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\Supermarket_AM\\Outcomes\\Supermarket_outcomes_averages_1_AM_11.05.2020.csv",row.names=F) # save the merged database under given directory and given name (last bit before .csv)

