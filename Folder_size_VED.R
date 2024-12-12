rm(list=ls())

yourname <- "Fenne" ### type the name of your folder
yourpath <- "/Volumes/heronderzoek-9/MGGZ/" ### type the path to MGGZ directory on O:-drive

pathname <- paste(yourpath, yourname, sep="")

setwd(pathname)

totalsize <- sum(file.info(dir())$size) / 1000000
totalsize #size in GB
