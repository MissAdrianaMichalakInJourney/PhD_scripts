


setwd("C:\\Users\\adria\\Desktop\\PhD\\Rrr\\sleep\\") #set directory

getwd()

my.data.all <-read.csv("inter_scorer_agreement.csv") 

df <- data.frame('Tanias_stage'= my.data.all$Tania,'Adrianas_stage'= my.data.all$Adriana,'epoch_start_at_sec'= row.names(my.data.all), 'agreement'=my.data.all$check)

print (df)

df.new = df[seq(0, nrow(df), 30), ]
print (df.new)


write.csv (df.new,"C:\\Users\\adria\\Desktop\\PhD\\Rrr\\sleep\\new.csv.csv",row.names=F)

