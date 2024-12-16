#maak lijst met alle bestanden in dir
library(readr)

#definieer de folder die je wilt checken
path <- "O:/Groep Geuze/21-280_BOOSTCAMP"
#path <- "L:/onderzoeksarchief/19-452_CONTROL_BS"

#Zorg dat R dat als actieve pad maakt
setwd(path)

#Creeer variabele met alle bestanden in hoofdfolder (ongeacht welke subfolder)
allFiles <- list.files(path, pattern=NULL, all.files=FALSE,
                       full.names=TRUE, recursive = TRUE)

# 
# #Met datum ea info van de bestanden erbij. Duurt erg veel langer!
# allFiles <- file.info(list.files(path, pattern=NULL, all.files=FALSE,
#                                  full.names=TRUE, recursive = TRUE))

setwd(paste(path,"/B_Documentation",sep = ""))
write.csv(allFiles, file = "allFiles.csv", quote = FALSE)

