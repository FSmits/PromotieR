###---------------- SCRIPT TO PLOT VED PLANNING - MARCH 13 ------------------###

rm(list=ls())

# --------- Packages --------- #
### Run this line only once:
# install.packages("dplyr"); install.packages("ggplot2"); install.packages("ggpubr"); install.packages("stringr"); install.packages("reshape2"); 

library(dplyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(reshape2)

# ------ Prepare data -------- # 

minute <- 1
time_block <- c(5 * minute)

# Create Data
data <- data.frame(
  group=c("Intro", "Prijsuitreiking", "Filmpje", "London", "San Antonio"),
  value=c(time_block, time_block, 5*time_block, minute * 3 * time_block, 2*time_block)
)
data$group <- factor(data$group, levels = c("Intro", "Prijsuitreiking", "Filmpje", "London", "San Antonio"))


# ------ Plot figure -------- # 

# Basic piechart
plt_planning <- ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0, direction = -1) +
  annotate(x = 1.55, y = 0.5, label = "Start of meeting", geom = "text", size = 2, color = "gray12") +
  annotate(x = 1.55, y = 0.5, label = "Start of meeting", geom = "text", size = 2, color = "gray12") +
  annotate(x = 1.55, y = 0.5, label = "Start of meeting", geom = "text", size = 2, color = "gray12") +  
  theme_void() # remove background, grid, numeric labels

plt_planning + labs(
  title = "VED Planning",
  subtitle ="The world is run by those wiling to sit until the end of meetings",
  caption = "Voor meer datavisualisatie inspo: https://r-graph-gallery.com/") +
  scale_fill_manual(values=c("#6C5B7B","#C06C84","#F67280","#F8B195","#F8B222","#C2C185")) +
  theme(text = element_text(size = 10, color = "gray12"), 
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(size = 13),
        axis.title = element_blank(),axis.ticks = element_blank(), axis.text.y = element_blank(), legend.position = "bottom", 
        legend.title = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),panel.grid = element_blank(),panel.grid.major.x = element_blank() )
