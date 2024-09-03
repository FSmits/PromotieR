# WeCom VED 09 sep 2024 - Lekker bezig Fenne

# clear workspace
rm(list=ls())

# excel reader 
### if not installed, run this line:
### install.packages("readxl")
library("readxl")

# read data
data <- read_excel("/Volumes/heronderzoek-6/MGGZ/Fenne/WeCom_LukasArtikel_ArmedOproep.xlsx", na = "NA")

# view data
data
