# ------------------- Bewegen in NL ------------------- # 
#
# Script waarin figuren worden gemaakt die ons laten zien 
# hoe Nederland ervoor staat op het gebied van bewegen


# --------------------- Install libraries -------------------- # 
### Run this line only once:
# install.packages("dplyr"); install.packages("ggplot2");  

# Libraries
library(ggplot2)
library(dplyr)



# --------------------- Klaarmaken data -------------------- # 
df_bewegen <- data.frame(
  jaar = c(2019:2022, 2019:2022),
  perc = c(54, 53, 50, 49, 44, 43.5, 42, 39),
  Opleiding = c(rep("laag",4), rep("hoog", 4)))
df_bewegen$Opleiding <- factor(df_bewegen$Opleiding)
df_bewegen$jaar <- as.numeric(df_bewegen$jaar)


# ------------------------- Figuur ------------------------- #

# Plot
plotone <- df_bewegen %>%
  ggplot(aes(x=jaar, y=perc, group=Opleiding, color=Opleiding)) +
  geom_line(linewidth=1) +
  xlim(2019,2023) +
  labs(title = 'Bewegen in NL', x = 'Jaar', y = '% voldoet aan beweegnorm') +
  scale_color_manual(values = c("hoog" = "#e1700e", "laag" = "#146496")) +
  theme(panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_line(colour = "grey"),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

plotone



# -------- Deel 2 --------- #

newdata <- data.frame(matrix(c(2023,2023,48.0,51.0,"laag","hoog"), nrow=2,ncol=3))
colnames(newdata) <- colnames(df_bewegen)
df_bewegen <- rbind(df_bewegen,newdata)
df_bewegen$perc <- as.numeric(df_bewegen$perc)
df_bewegen$jaar <- as.numeric(df_bewegen$jaar)

# Plot again
plottwo <- df_bewegen %>%
  ggplot(aes(x=jaar, y=perc, group=Opleiding, color=Opleiding)) +
  geom_line(linewidth=1) +
  xlim(2019,2023) +
  labs(title = 'Bewegen in NL 2023', x = 'Jaar', y = '%') +
  scale_color_manual(values = c("hoog" = "#e1700e", "laag" = "#146496")) +
  theme(panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_line(colour = "grey"),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

library(ggpubr)
ggarrange(plotone,plottwo,nrow=2,ncol=1)


