# clear workspace
rm(list=ls())

# excel reader 
### if not installed, run this line:
### install.packages("readxl")
### install.packages("ggplot2")
library("readxl")
library("ggplot2")

# read data
my_data <- as.data.frame( read_excel("/Volumes/heronderzoek-6/MGGZ/Fenne/WeCom_LukasArtikel_ArmedOproep.xlsx", na = "NA") )

# view data
my_data

# put data into long format
my_data_long <- gather(my_data, readout, numbers, Clicks:Reposts, factor_key=TRUE)
my_data_long

# plot bar chart
ggplot(data=my_data_long, aes(x=readout, y=numbers, fill=Post_name, group=Post_name ) ) +
  geom_bar(position="dodge", stat="identity", width=0.6) + 
  scale_colour_manual(values=c("#e09d5e","#136497")) + scale_fill_manual(values=c("#e09d5e","#136497")) +
  theme(panel.background = element_rect(fill="white",colour="white"), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap( . ~ Social_channel)

# sum data per post (1) or per social channel (2)
data_post   <- my_data[1:2,4:5] + my_data[3:4,4:5] 
data_social <- my_data[c(1,3),4:5] + my_data[c(2,4),4:5]
                     
# Fisher's Exact test to compare posts (1) and social channels (2)
stats::fisher.test(data_post)
stats::fisher.test(data_social)
