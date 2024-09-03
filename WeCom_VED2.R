# clear workspace
rm(list=ls())

# excel reader 
### if not installed, run this line:
### install.packages("readxl")
library("readxl")

# read data
my_data <- read_excel("/Volumes/heronderzoek-6/MGGZ/Fenne/WeCom_LukasArtikel_ArmedOproep.xlsx", na = "NA")

# view data
my_data

lm1 <- lm(Impressies ~ Social_channel * Post_name, data = my_data)
summary(lm1)
