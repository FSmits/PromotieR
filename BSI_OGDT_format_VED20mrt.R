######## Voorbereidend werk; installeren en inladen e.d. #############
#to do voordat je dit script runt: verwijder overtollige data (dus iedereen die T2 voor 2022 heeft gedaan) en check of alle deelnemercodes hetzelfde zijn (ODTB01XX)

### Voeg de volgende 2 regels toe als iets significant blijkt: 
#significantie toevoegen
#geom_point(x=1.5, y= 4, shape = "*", col = "black", size= 10)+


# installeer benodigde packages
# install.packages("ggplot2"); install.packages("readxl"); install.packages("dplyr"); install.packages("tidyverse"); install.packages("devtools"); install.packages("cli"); install.packages("ggpubr")

#Packages in library
library(ggplot2); library(ggpubr); library(readxl); library(tidyverse); library(tidyr); library(ggsignif)
library(dplyr); library(devtools); devtools::install_github("AWKruijt/JT-RCI")

# read data
# bestandslocatie = "/Volumes/heronderzoek/MGGZ/WO/Therapie evaluaties/2022/OGDT/ODGT_22_ruwedata_20221114.xlsx"
OGDT_21           <- data.frame(matrix(data = NA, nrow = 100, ncol = 4) )
colnames(OGDT_21) <- c("ID","year","Session","BSI_TOT")
OGDT_21$ID        <- rep(paste("OGDT", 11:20, sep = ""), 5*2)
OGDT_21$year      <- rep(rep(2018:2022, each = 10 ), 2)
OGDT_21$Session   <- rep(1:2, each = 10*5)
OGDT_21$BSI_TOT[OGDT_21$Session == 1]   <- sample(53:212, 50, replace = TRUE)
OGDT_21$BSI_TOT[OGDT_21$Session == 2]   <- sample(25:159, 50, replace = TRUE)


####### herberekenen BSI scores #####
OGDT_21$BSI_TOT <- OGDT_21$BSI_TOT/53


#### START BSI BEREKENINGEN ####

############# Plotten van de BSI ##########################
BSI_indiv_change <- OGDT_21[ , c("ID", "Session", "BSI_TOT")]

##### Normscores BSI #####
TOT_bev_m  = 0.42
TOT_bev_sd = 0.40




#### TOT ####
TOT_indiv_change <- BSI_indiv_change[ , c("ID", "year", "Session", "BSI_TOT")] 

# reshape to wide format
TOT_indiv_change_wide <- reshape(TOT_indiv_change, direction= "wide", idvar = "ID", timevar = "Session", v.names = "BSI_TOT")

TOT_per_deelnemer <-TOT_indiv_change_wide %>%
  dplyr::mutate(is_increasing = `BSI_TOT.1` < `BSI_TOT.2`) %>%
  tidyr::gather("Session", "BSI_TOT", 2:3) %>%
  na.omit(TOT_indiv_change) %>%
  ggplot(aes(x = Session, y = BSI_TOT)) +
  
  #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
  geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
  #significantie toevoegen
  geom_point(x=1.5, y= 4, shape = "*", col = "black", size= 10) +
  
  geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
  geom_point(x=1, y= mean(TOT_indiv_change$BSI_TOT[TOT_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
  geom_point(x=2, y= mean(TOT_indiv_change$BSI_TOT[TOT_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
  
  # individuele lijn toevoegen
  geom_point() +
  geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
  scale_colour_manual(values = c("black", "red"))+                                 
  labs(x ="Sessie", y= "totaalscore")+
  ylim(-0.3,4)+
  
  #normscores toevoegen
  geom_rect(aes(xmin= 0, xmax=3, ymin = TOT_bev_m - TOT_bev_sd, ymax = TOT_bev_m+TOT_bev_sd), fill = "#146496", alpha = 0.002) +
  geom_hline(yintercept = seq(from=TOT_bev_m - TOT_bev_sd, to=TOT_bev_m + TOT_bev_sd, by = TOT_bev_sd),
             linetype= "dotdash", colour= "darkviolet", size= .3)+               
  
  #grafiek opmaken
  theme(panel.background = element_rect(fill="white",colour="white"), 
        panel.border = element_blank(),  panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  facet_wrap( . ~ year)
# #Jaartal als titel
# ggtitle("2022")


TOT_per_deelnemer









library(gapminder)
library(gganimate)
library(gifski)

## standard ggplot2
myPlot <- ggplot(OGDT_21, aes(x = as.factor(Session), y = BSI_TOT))  +
  geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75)
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Session', y = 'BSI totaal') +
  transition_states(as.factor(year)) +
  ease_aes('linear')

animate(myPlot, duration = 5, fps = 20, renderer = gifski_renderer())
anim_save("output.gif")







#------------------------------------------------------------------------------#
#                               MOVIE
#------------------------------------------------------------------------------#

map_dir <- 'figures' # directory for temporary maps
outfile <- 'static/movie.gif' # movie file (this could be several different file types)
movie_speed <- 5 # movie play speed

df <- OGDT_21

# make maps ---------------------------------------------------------------
if(!dir.exists(map_dir)){dir.create(map_dir)}

# start and end times
t0 <- min(df$year)
t1 <- max(df$year)

# time vector
# tseq = seq.POSIXt(t0, t1+31*60*60*24, by = 'month')
tseq = unique(df$year)

# make and save maps
for(it in 1:(length(tseq)-1)){
  
  # # subset within timestep
  # it0 = tseq[it]
  # it1 = tseq[it+1]
  # idf = df[df$time >= it0 & df$time <= it1,]
  
  # open map file
  png(paste0(map_dir,'/', as.character(it)), 
      width=6, height=6, unit="in", res=175, pointsize=10)
  
  df_plot <- df %>%
    filter(year == it)
  myPlot <- ggplot(df_plot, aes(x = as.factor(Session), y = BSI_TOT))  +
    geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75)
  
  # close and save plot
  dev.off()
  
}

# write system command
cmd = paste0('convert -antialias -delay 1x', movie_speed, ' ', map_dir, '/*.png ', outfile)

# execute system command
system(cmd)

# remove temporary files
unlink(map_dir, recursive = T)





# ------- niet nodig: --------
# 
# #### COG ####
# COG_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_COG")] 
# 
# COG_per_deelnemer <- COG_indiv_change %>%
#   tidyr::spread(Session, BSI_COG) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_COG", 2:3) %>%
#   na.omit(COG_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_COG)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn) + 2x geom_points voor de groepsgemiddelden aan het eind
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(COG_indiv_change$BSI_COG[COG_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(COG_indiv_change$BSI_COG[COG_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "cognitieve problemen")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = COG_bev_m-COG_bev_sd, ymax = COG_bev_m+COG_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=COG_bev_m-COG_bev_sd, to=COG_bev_m+COG_bev_sd, by = COG_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# 
# #### SOM ####
# SOM_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_SOM")] 
# 
# SOM_per_deelnemer <- SOM_indiv_change %>%
#   tidyr::spread(Session, BSI_SOM) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_SOM", 2:3) %>%
#   na.omit(SOM_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_SOM)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(SOM_indiv_change$BSI_SOM[SOM_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(SOM_indiv_change$BSI_SOM[SOM_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "somatische klachten")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = SOM_bev_m-SOM_bev_sd, ymax = SOM_bev_m+SOM_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=SOM_bev_m-SOM_bev_sd, to=SOM_bev_m+SOM_bev_sd, by = SOM_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# #### INT ######
# INT_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_INT")] 
# 
# INT_per_deelnemer <- INT_indiv_change %>%
#   tidyr::spread(Session, BSI_INT) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_INT", 2:3) %>%
#   na.omit(INT_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_INT)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(INT_indiv_change$BSI_INT[INT_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(INT_indiv_change$BSI_INT[INT_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "interpersoonlijke klachten")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = INT_bev_m-INT_bev_sd, ymax = INT_bev_m+INT_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=INT_bev_m-INT_bev_sd, to=INT_bev_m+INT_bev_sd, by = INT_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# #### DEP #####
# DEP_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_DEP")] 
# 
# DEP_per_deelnemer <- DEP_indiv_change %>%
#   tidyr::spread(Session, BSI_DEP) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_DEP", 2:3) %>%
#   na.omit(DEP_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_DEP)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(DEP_indiv_change$BSI_DEP[DEP_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(DEP_indiv_change$BSI_DEP[DEP_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "depressieve stemming")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = DEP_bev_m-DEP_bev_sd, ymax = DEP_bev_m+DEP_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=DEP_bev_m - DEP_bev_sd, to=DEP_bev_m + DEP_bev_sd, by = DEP_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# #### ANG ####
# ANG_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_ANG")] 
# 
# ANG_per_deelnemer <-ANG_indiv_change %>%
#   tidyr::spread(Session, BSI_ANG) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_ANG", 2:3) %>%
#   na.omit(ANG_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_ANG)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(ANG_indiv_change$BSI_ANG[ANG_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(ANG_indiv_change$BSI_ANG[ANG_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "angst")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = ANG_bev_m-ANG_bev_sd, ymax = ANG_bev_m+ANG_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=ANG_bev_m - ANG_bev_sd, to=ANG_bev_m + ANG_bev_sd, by = ANG_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# #### HOS ####
# HOS_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_HOS")] 
# 
# HOS_per_deelnemer <-HOS_indiv_change %>%
#   tidyr::spread(Session, BSI_HOS) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_HOS", 2:3) %>%
#   na.omit(HOS_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_HOS)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(HOS_indiv_change$BSI_HOS[HOS_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(HOS_indiv_change$BSI_HOS[HOS_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "hostiliteit")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = HOS_bev_m - HOS_bev_sd, ymax = HOS_bev_m+HOS_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=HOS_bev_m - HOS_bev_sd, to=HOS_bev_m + HOS_bev_sd, by = HOS_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# #### FOB ####
# FOB_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_FOB")] 
# 
# FOB_per_deelnemer <- FOB_indiv_change %>%
#   tidyr::spread(Session, BSI_FOB) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_FOB", 2:3) %>%
#   na.omit(FOB_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_FOB)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(FOB_indiv_change$BSI_FOB[FOB_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(FOB_indiv_change$BSI_FOB[FOB_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "fobische angst")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = FOB_bev_m - FOB_bev_sd, ymax = FOB_bev_m+FOB_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=FOB_bev_m - FOB_bev_sd, to=FOB_bev_m + FOB_bev_sd, by = FOB_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# #### PAR ####
# PAR_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_PAR")] 
# 
# PAR_per_deelnemer <- PAR_indiv_change %>%
#   tidyr::spread(Session, BSI_PAR) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_PAR", 2:3) %>%
#   na.omit(PAR_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_PAR)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(PAR_indiv_change$BSI_PAR[PAR_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(PAR_indiv_change$BSI_PAR[PAR_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "parano?de gedachten")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = PAR_bev_m - PAR_bev_sd, ymax = PAR_bev_m+PAR_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=PAR_bev_m - PAR_bev_sd, to=PAR_bev_m + PAR_bev_sd, by = PAR_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# #### PSY ####
# PSY_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_PSY")] 
# 
# PSY_per_deelnemer <- PSY_indiv_change %>%
#   tidyr::spread(Session, BSI_PSY) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_PSY", 2:3) %>%
#   na.omit(PSY_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_PSY)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn) + 2x geom_points voor de groepsgemiddelden aan het eind
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(COG_indiv_change$BSI_COG[COG_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(COG_indiv_change$BSI_COG[COG_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "psychoticisme")+
#   ylim(-0.3,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = PSY_bev_m - PSY_bev_sd, ymax = PSY_bev_m+PSY_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=PSY_bev_m - PSY_bev_sd, to=PSY_bev_m + PSY_bev_sd, by = PSY_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
# #### AANV ####
# Aanv_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_aanv")] 
# 
# Aanv_per_deelnemer <- Aanv_indiv_change %>%
#   tidyr::spread(Session, BSI_aanv) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_aanv", 2:3) %>%
#   na.omit(Aanv_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_aanv)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(Aanv_indiv_change$BSI_aanv[Aanv_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(Aanv_indiv_change$BSI_aanv[Aanv_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+
#   labs(x ="Sessie", y= "aanvullende items")+
#   ylim(-0.3,4)+
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black") ) 
# 
# 
#           #### AAS ####
# AAS_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_AAS")] 
# 
# AAS_per_deelnemer <-AAS_indiv_change %>%
#   tidyr::spread(Session, BSI_AAS) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_AAS", 2:3) %>%
#   na.omit(AAS_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_AAS)) +
#   
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#  
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(AAS_indiv_change$BSI_AAS[AAS_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(AAS_indiv_change$BSI_AAS[AAS_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "aantal aanwezige symptomen")+
#   ylim(0,55)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = AAS_bev_m - AAS_bev_sd, ymax = AAS_bev_m+AAS_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=AAS_bev_m - AAS_bev_sd, to=AAS_bev_m + AAS_bev_sd, by = AAS_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
#           #### EAS ####
# EAS_indiv_change <- BSI_indiv_change[ , c("ID", "Session", "BSI_EAS")] 
# 
# EAS_per_deelnemer <-EAS_indiv_change %>%
#   tidyr::spread(Session, BSI_EAS) %>%
#   dplyr::mutate(is_increasing = `1` < `2`) %>%
#   tidyr::gather("Session", "BSI_EAS", 2:3) %>%
#   na.omit(EAS_indiv_change) %>%
#   ggplot(aes(x = Session, y = BSI_EAS)) +
#   #algemene plots: boxplot en gemiddelde lijn (blauwe stippellijn)
#   geom_boxplot(fill = "#e1700e", alpha = 0.5, col = "#e1700e", show.legend = F, lwd = .75) +
#   #significantie toevoegen
#   geom_point(x=1.5, y= 4, shape = "*", col = "black", size= 10)+
#   
#   geom_smooth(method = "lm", se=FALSE, linetype = 'dashed', color="#146496", aes(group=1))+ 
#   geom_point(x=1, y= mean(EAS_indiv_change$BSI_EAS[EAS_indiv_change$Session==1]), shape = 15, col = "#146496", size= 3)+
#   geom_point(x=2, y= mean(EAS_indiv_change$BSI_EAS[EAS_indiv_change$Session==2]), shape = 15, col = "#146496", size= 3)+
#   
#   # individuele lijn toevoegen
#   geom_point() +
#   geom_line(aes(group = ID, col = is_increasing), alpha =.25, size = .7, show.legend = F) + 
#   scale_colour_manual(values = c("black", "red"))+                                 
#   labs(x ="Sessie", y= "ernst aanwezige symptomen")+
#       ylim(0,4)+
#   
#   #normscores toevoegen
#   geom_rect(aes(xmin= 0, xmax=3, ymin = EAS_bev_m - EAS_bev_sd, ymax = EAS_bev_m+EAS_bev_sd), fill = "#146496", alpha = 0.002) +
#   geom_hline(yintercept = seq(from=EAS_bev_m - EAS_bev_sd, to=EAS_bev_m + EAS_bev_sd, by = EAS_bev_sd),
#              linetype= "dotdash", colour= "darkviolet", size= .3)+               
#   
#   #grafiek opmaken
#   theme(panel.background = element_rect(fill="white",colour="white"), 
#         panel.border = element_blank(),  panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 
# 
#                           #### Bereken de RCI #####
# 
# ##### COG BSI ######
# COG_indiv_change_wide <- COG_indiv_change
# COG_indiv_change_wide <- COG_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_COG, names_prefix = "S")%>%
#   na.omit(COG_indiv_change_wide)
# 
# COG_indiv_change_wide$S1 <- as.numeric(COG_indiv_change_wide$S1) 
# COG_indiv_change_wide$S2 <- as.numeric(COG_indiv_change_wide$S2) 
# COG_indiv_change_wide <- as.data.frame(COG_indiv_change_wide)
# 
# COG_RCI <- JTRCI(data = COG_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = COG_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Cognitief")
# 
# ##### SOM BSI ######
# SOM_indiv_change_wide <- SOM_indiv_change
# SOM_indiv_change_wide <- SOM_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_SOM, names_prefix = "S")%>%
#   na.omit(SOM_indiv_change_wide)
# 
# SOM_indiv_change_wide$S1 <- as.numeric(SOM_indiv_change_wide$S1) 
# SOM_indiv_change_wide$S2 <- as.numeric(SOM_indiv_change_wide$S2) 
# SOM_indiv_change_wide <- as.data.frame(SOM_indiv_change_wide)
# 
# SOM_RCI<- JTRCI(data = SOM_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = SOM_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Somatisch")
# 
# 
# ##### INT BSI ######
# INT_indiv_change_wide <- INT_indiv_change
# INT_indiv_change_wide <- INT_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_INT, names_prefix = "S")%>%
#   na.omit(INT_indiv_change_wide)
# 
# INT_indiv_change_wide$S1 <- as.numeric(INT_indiv_change_wide$S1) 
# INT_indiv_change_wide$S2 <- as.numeric(INT_indiv_change_wide$S2) 
# INT_indiv_change_wide <- as.data.frame(INT_indiv_change_wide)
# 
# INT_RCI <- JTRCI(data = INT_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = INT_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Interpersoonlijke problemen")
# ##### DEP BSI ######
# DEP_indiv_change_wide <- DEP_indiv_change
# DEP_indiv_change_wide <- DEP_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_DEP, names_prefix = "S")%>%
#   na.omit(DEP_indiv_change_wide)
# 
# DEP_indiv_change_wide$S1 <- as.numeric(DEP_indiv_change_wide$S1) 
# DEP_indiv_change_wide$S2 <- as.numeric(DEP_indiv_change_wide$S2) 
# DEP_indiv_change_wide <- as.data.frame(DEP_indiv_change_wide)
# 
# DEP_RCI<- JTRCI(data = DEP_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = DEP_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Depressieve klachten")
# ##### ANG BSI ######
# ANG_indiv_change_wide <- ANG_indiv_change
# ANG_indiv_change_wide <- ANG_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_ANG, names_prefix = "S")%>%
#   na.omit(ANG_indiv_change_wide)
# 
# ANG_indiv_change_wide$S1 <- as.numeric(ANG_indiv_change_wide$S1) 
# ANG_indiv_change_wide$S2 <- as.numeric(ANG_indiv_change_wide$S2) 
# ANG_indiv_change_wide <- as.data.frame(ANG_indiv_change_wide)
# 
# ANG_RCI <- JTRCI(data = ANG_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = ANG_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Angstklachten")
# 
# ##### HOS BSI ######
# HOS_indiv_change_wide <- HOS_indiv_change
# HOS_indiv_change_wide <- HOS_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_HOS, names_prefix = "S")%>%
#   na.omit(HOS_indiv_change_wide)
# 
# HOS_indiv_change_wide$S1 <- as.numeric(HOS_indiv_change_wide$S1) 
# HOS_indiv_change_wide$S2 <- as.numeric(HOS_indiv_change_wide$S2) 
# HOS_indiv_change_wide <- as.data.frame(HOS_indiv_change_wide)
# 
# HOS_RCI <- JTRCI(data = HOS_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = HOS_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Hostiliteit")
# 
# ##### FOB BSI #####
# FOB_indiv_change_wide <- FOB_indiv_change
# FOB_indiv_change_wide <- FOB_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_FOB, names_prefix = "S")%>%
#   na.omit(FOB_indiv_change_wide)
# 
# FOB_indiv_change_wide$S1 <- as.numeric(FOB_indiv_change_wide$S1) 
# FOB_indiv_change_wide$S2 <- as.numeric(FOB_indiv_change_wide$S2) 
# FOB_indiv_change_wide <- as.data.frame(FOB_indiv_change_wide)
# 
# FOB_RCI <-JTRCI(data = FOB_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = FOB_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Fobie")
# 
# ##### PAR BSI ######
# PAR_indiv_change_wide <- PAR_indiv_change
# PAR_indiv_change_wide <- PAR_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_PAR, names_prefix = "S")%>%
#   na.omit(PAR_indiv_change_wide)
# 
# PAR_indiv_change_wide$S1 <- as.numeric(PAR_indiv_change_wide$S1) 
# PAR_indiv_change_wide$S2 <- as.numeric(PAR_indiv_change_wide$S2) 
# PAR_indiv_change_wide <- as.data.frame(PAR_indiv_change_wide)
# 
# PAR_RCI <- JTRCI(data = PAR_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = PAR_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI parano?de klachten")
# 
# ##### PSY BSI ######
# PSY_indiv_change_wide <- PSY_indiv_change
# PSY_indiv_change_wide <- PSY_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_PSY, names_prefix = "S")%>%
#   na.omit(PSY_indiv_change_wide)
# 
# PSY_indiv_change_wide$S1 <- as.numeric(PSY_indiv_change_wide$S1) 
# PSY_indiv_change_wide$S2 <- as.numeric(PSY_indiv_change_wide$S2) 
# PSY_indiv_change_wide <- as.data.frame(PSY_indiv_change_wide)
# 
# PSY_RCI <- JTRCI(data = PSY_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = PSY_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI psychotische klachten")
# ##### AANV BSI ######
# Aanv_indiv_change_wide <- Aanv_indiv_change
# Aanv_indiv_change_wide <- Aanv_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_aanv, names_prefix = "S")%>%
#   na.omit(Aanv_indiv_change_wide)
# 
# Aanv_indiv_change_wide$S1 <- as.numeric(Aanv_indiv_change_wide$S1) 
# Aanv_indiv_change_wide$S2 <- as.numeric(Aanv_indiv_change_wide$S2) 
# Aanv_indiv_change_wide <- as.data.frame(Aanv_indiv_change_wide)
# 
# Aanv_RCI<- JTRCI(data = Aanv_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = .8, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Aanvullende klachten")
# 
# ##### TOT BSI ######
# TOT_indiv_change_wide <- TOT_indiv_change
# TOT_indiv_change_wide <- TOT_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_TOT, names_prefix = "S")%>%
#   na.omit(TOT_indiv_change_wide)
# 
# TOT_indiv_change_wide$S1 <- as.numeric(TOT_indiv_change_wide$S1) 
# TOT_indiv_change_wide$S2 <- as.numeric(TOT_indiv_change_wide$S2) 
# TOT_indiv_change_wide <- as.data.frame(TOT_indiv_change_wide)
# 
# TOT_RCI <-JTRCI(data = TOT_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = TOT_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI Totaal aantal klachten")
# 
# ##### AAS BSI ######
# AAS_indiv_change_wide <- AAS_indiv_change
# AAS_indiv_change_wide <- AAS_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_AAS, names_prefix = "S")%>%
#   na.omit(AAS_indiv_change_wide)
# 
# AAS_indiv_change_wide$S1 <- as.numeric(AAS_indiv_change_wide$S1) 
# AAS_indiv_change_wide$S2 <- as.numeric(AAS_indiv_change_wide$S2) 
# AAS_indiv_change_wide <- as.data.frame(AAS_indiv_change_wide)
# 
# AAS_RCI <-JTRCI(data = AAS_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = AAS_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI aantal aanwezige symptomen")
# 
# ##### EAS BSI ######
# EAS_indiv_change_wide <- EAS_indiv_change
# EAS_indiv_change_wide <- EAS_indiv_change_wide %>% 
#   pivot_wider(names_from = Session, values_from = BSI_EAS, names_prefix = "S")%>%
#   na.omit(EAS_indiv_change_wide)
# 
# EAS_indiv_change_wide$S1 <- as.numeric(EAS_indiv_change_wide$S1) 
# EAS_indiv_change_wide$S2 <- as.numeric(EAS_indiv_change_wide$S2) 
# EAS_indiv_change_wide <- as.data.frame(EAS_indiv_change_wide)
# 
# EAS_RCI <- JTRCI(data = EAS_indiv_change_wide, pre = "S1", post = "S2", ppid= "ID",
#       reliability = EAS_rel, indextype = "RCI", 
#       higherIsBetter = F, table = T, plot = T,
#       xlab = "score voor", ylab = "score na", plottitle = "BSI ernst aanwezige symptomen")
# 
#                           #### Check of groep gemiddeld genomen verbeterd is ####
# 
# ### COG significant veranderd? ####
# SD_COG <- sd(COG_indiv_change_wide$S1)
# critical_value_COG <- 1.96*sqrt(2*((SD_COG*sqrt(1-COG_rel))^2))
# delta_COG <- mean(COG_indiv_change_wide$S2)- mean(COG_indiv_change_wide$S1)
# RCI_COG_sign <- delta_COG >= critical_value_COG | delta_COG <= - critical_value_COG
# 
# ### SOM significant veranderd? ####
# SD_SOM <- sd(SOM_indiv_change_wide$S1)
# critical_value_SOM <- 1.96*sqrt(2*((SD_SOM*sqrt(1-SOM_rel))^2))
# delta_SOM <- mean(SOM_indiv_change_wide$S2)- mean(SOM_indiv_change_wide$S1)
# RCI_SOM_sign <- delta_SOM >= critical_value_SOM | delta_SOM <= - critical_value_SOM
# 
# 
# ### DEP significant veranderd? ####
# SD_DEP <- sd(DEP_indiv_change_wide$S1)
# critical_value_DEP <- 1.96*sqrt(2*((SD_DEP*sqrt(1-DEP_rel))^2))
# delta_DEP <- mean(DEP_indiv_change_wide$S2)- mean(DEP_indiv_change_wide$S1)
# RCI_DEP_sign <- delta_DEP >= critical_value_DEP | delta_DEP <= - critical_value_DEP
# 
# ### INT significant veranderd? ####
# SD_INT <- sd(INT_indiv_change_wide$S1)
# critical_value_INT <- 1.96*sqrt(2*((SD_INT*sqrt(1-INT_rel))^2))
# delta_INT <- mean(INT_indiv_change_wide$S2)- mean(INT_indiv_change_wide$S1)
# RCI_INT_sign <- delta_INT >= critical_value_INT| delta_INT <= - critical_value_INT
# 
# 
# ### ANG significant veranderd? ####
# SD_ANG <- sd(ANG_indiv_change_wide$S1)
# critical_value_ANG <- 1.96*sqrt(2*((SD_ANG*sqrt(1-ANG_rel))^2))
# delta_ANG <- mean(ANG_indiv_change_wide$S2)- mean(ANG_indiv_change_wide$S1)
# RCI_ANG_sign <- delta_ANG >= critical_value_ANG | delta_ANG <= - critical_value_ANG
# 
# 
# ### HOS significant veranderd? ####
# SD_HOS <- sd(HOS_indiv_change_wide$S1)
# critical_value_HOS <- 1.96*sqrt(2*((SD_HOS*sqrt(1-HOS_rel))^2))
# delta_HOS <- mean(HOS_indiv_change_wide$S2)- mean(HOS_indiv_change_wide$S1)
# RCI_HOS_sign <- delta_HOS >= critical_value_HOS | delta_HOS <= - critical_value_HOS
# 
# 
# ### FOB significant veranderd? ####
# SD_FOB <- sd(FOB_indiv_change_wide$S1)
# critical_value_FOB <- 1.96*sqrt(2*((SD_FOB*sqrt(1-FOB_rel))^2))
# delta_FOB <- mean(FOB_indiv_change_wide$S2)- mean(FOB_indiv_change_wide$S1)
# RCI_FOB_sign <- delta_FOB >= critical_value_FOB | delta_FOB <= - critical_value_FOB
# 
# 
# ### PAR significant veranderd? ####
# SD_PAR <- sd(PAR_indiv_change_wide$S1)
# critical_value_PAR <- 1.96*sqrt(2*((SD_PAR*sqrt(1-PAR_rel))^2))
# delta_PAR <- mean(PAR_indiv_change_wide$S2)- mean(PAR_indiv_change_wide$S1)
# RCI_PAR_sign <- delta_PAR >= critical_value_PAR | delta_PAR <= - critical_value_PAR
# 
# 
# ### PSY significant veranderd? ####
# SD_PSY <- sd(PSY_indiv_change_wide$S1)
# critical_value_PSY <- 1.96*sqrt(2*((SD_PSY*sqrt(1-PSY_rel))^2))
# delta_PSY <- mean(PSY_indiv_change_wide$S2)- mean(PSY_indiv_change_wide$S1)
# RCI_PSY_sign <- delta_PSY >= critical_value_PSY | delta_PSY <= - critical_value_PSY
# 
# ### TOT significant veranderd? ####
# SD_TOT <- sd(TOT_indiv_change_wide$S1)
# critical_value_TOT <- 1.96*sqrt(2*((SD_TOT*sqrt(1-TOT_rel))^2))
# delta_TOT <- mean(TOT_indiv_change_wide$S2)- mean(TOT_indiv_change_wide$S1)
# RCI_TOT_sign <- delta_TOT >= critical_value_TOT | delta_TOT <= - critical_value_TOT
# 
# ### AAS significant veranderd? ####
# SD_AAS <- sd(AAS_indiv_change_wide$S1)
# critical_value_AAS <- 1.96*sqrt(2*((SD_AAS*sqrt(1-AAS_rel))^2))
# delta_AAS <- mean(AAS_indiv_change_wide$S2)- mean(AAS_indiv_change_wide$S1)
# RCI_AAS_sign <- delta_AAS >= critical_value_AAS | delta_AAS <= - critical_value_AAS
# 
# ### EAS significant veranderd? ####
# SD_EAS <- sd(EAS_indiv_change_wide$S1)
# critical_value_EAS <- 1.96*sqrt(2*((SD_EAS*sqrt(1-EAS_rel))^2))
# delta_EAS <- mean(EAS_indiv_change_wide$S2)- mean(EAS_indiv_change_wide$S1)
# RCI_EAS_sign <- delta_EAS >= critical_value_EAS | delta_EAS <= - critical_value_EAS
# 
#                           #### rapportage ####
# 
# print(paste("COG klachten op groepsniveau significant veranderd?", (RCI_COG_sign)))
# print(paste("SOM klachten op groepsniveau significant veranderd?", (RCI_SOM_sign)))
# print(paste("DEP klachten op groepsniveau significant veranderd?", (RCI_DEP_sign))) 
# print(paste("INT klachten op groepsniveau significant veranderd?", (RCI_INT_sign)))
# print(paste("ANG klachten op groepsniveau significant veranderd?", (RCI_ANG_sign)))
# print(paste("HOS klachten op groepsniveau significant veranderd?", (RCI_HOS_sign)))
# print(paste("FOB klachten op groepsniveau significant veranderd?", (RCI_FOB_sign)))
# print(paste("PAR klachten op groepsniveau significant veranderd?", (RCI_PAR_sign)))
# print(paste("PSY klachten op groepsniveau significant veranderd?", (RCI_PSY_sign)))
# print(paste("TOT klachten op groepsniveau significant veranderd?", (RCI_TOT_sign)))
# print(paste("AAS klachten op groepsniveau significant veranderd?", (RCI_AAS_sign)))
# print(paste("EAS klachten op groepsniveau significant veranderd?", (RCI_EAS_sign)))
# 
# 
# BSI_per_subschaal <- ggarrange(COG_per_deelnemer, SOM_per_deelnemer, DEP_per_deelnemer,
#                                INT_per_deelnemer, ANG_per_deelnemer, HOS_per_deelnemer,
#                                FOB_per_deelnemer, PAR_per_deelnemer, PSY_per_deelnemer,
#                                Aanv_per_deelnemer,
#                                ncol = 3, nrow = 4, labels="AUTO")
# BSI_totaalscores<- ggarrange(TOT_per_deelnemer, AAS_per_deelnemer,EAS_per_deelnemer,
#                                            ncol = 3, nrow =1, labels="AUTO")
# 
# BSI_RCI_klachten <- ggarrange(COG_RCI, SOM_RCI, DEP_RCI, INT_RCI, ANG_RCI, 
#                               HOS_RCI, FOB_RCI, PAR_RCI, PSY_RCI, Aanv_RCI,
#                               ncol= 2, nrow= 5, labels="AUTO")
# BSI_RCI_totaalscores <- ggarrange(TOT_RCI, AAS_RCI, EAS_RCI, 
#                                   ncol=3, nrow =1, labels="AUTO")
# 
# ##### Ruwe data voor in de tabel ####
# 
# ##### OGDT DESCRIPTIVES ####
# SOM_S1 <-summary(OGDT_22$BSI_SOM[OGDT_22$Session == '1'])
# sd_SOM_S1 <- sd(OGDT_22$BSI_SOM[OGDT_22$Session == '1'])
# SOM_S2 <- summary(OGDT_22$BSI_SOM[OGDT_22$Session == '2'])
# sd_SOM_S2 <- sd(OGDT_22$BSI_SOM[OGDT_22$Session == '2'])
# print(paste("RC Index SOM:", critical_value_SOM, "verschilscore", delta_SOM))
# 
# COG_S1 <-summary(OGDT_22$BSI_COG[OGDT_22$Session == '1'])
# sd_COG_S1 <- sd(OGDT_22$BSI_COG[OGDT_22$Session == '1'])
# COG_S2 <- summary(OGDT_22$BSI_COG[OGDT_22$Session == '2'])
# sd_COG_S2 <- sd(OGDT_22$BSI_COG[OGDT_22$Session == '2'])
# print(paste("RC Index COG:", critical_value_COG, "verschilscore", delta_COG))
# 
# INT_S1 <-summary(OGDT_22$BSI_INT[OGDT_22$Session == '1'])
# sd_INT_S1 <- sd(OGDT_22$BSI_INT[OGDT_22$Session == '1'])
# INT_S2 <- summary(OGDT_22$BSI_INT[OGDT_22$Session == '2'])
# sd_INT_S1 <- sd(OGDT_22$BSI_INT[OGDT_22$Session == '2'])
# print(paste("RC Index INT:", critical_value_INT, "verschilscore", delta_INT))
# 
# DEP_S1 <-summary(OGDT_22$BSI_DEP[OGDT_22$Session == '1'])
# sd_DEP_S1 <- sd(OGDT_22$BSI_DEP[OGDT_22$Session == '1'])
# DEP_S2 <- summary(OGDT_22$BSI_DEP[OGDT_22$Session == '2'])
# sd_DEP_S2 <- sd(OGDT_22$BSI_DEP[OGDT_22$Session == '2'])
# print(paste("RC Index DEP:", critical_value_DEP, "verschilscore", delta_DEP))
# 
# ANG_S1 <-summary(OGDT_22$BSI_ANG[OGDT_22$Session == '1'])
# sd_ANG_S1 <- sd(OGDT_22$BSI_ANG[OGDT_22$Session == '1'])
# ANG_S2 <- summary(OGDT_22$BSI_ANG[OGDT_22$Session == '2'])
# sd_ANG_S2 <- sd(OGDT_22$BSI_ANG[OGDT_22$Session == '2'])
# print(paste("RC Index ANG:", critical_value_ANG, "verschilscore", delta_ANG))
# 
# HOS_S1 <-summary(OGDT_22$BSI_HOS[OGDT_22$Session == '1'])
# sd_HOS_S1 <- sd(OGDT_22$BSI_HOS[OGDT_22$Session == '1'])
# HOS_S2 <- summary(OGDT_22$BSI_HOS[OGDT_22$Session == '2'])
# sd_HOS_S2 <- sd(OGDT_22$BSI_HOS[OGDT_22$Session == '2'])
# print(paste("RC Index HOS:", critical_value_HOS, "verschilscore", delta_HOS))
# 
# FOB_S1 <-summary(OGDT_22$BSI_FOB[OGDT_22$Session == '1'])
# sd_FOB_S1 <- sd(OGDT_22$BSI_FOB[OGDT_22$Session == '1'])
# FOB_S2 <- summary(OGDT_22$BSI_FOB[OGDT_22$Session == '2'])
# sd_FOB_S2 <- sd(OGDT_22$BSI_FOB[OGDT_22$Session == '2'])
# print(paste("RC Index FOB:", critical_value_FOB, "verschilscore", delta_FOB))
# 
# PAR_S1 <-summary(OGDT_22$BSI_PAR[OGDT_22$Session == '1'])
# sd_PAR_S1 <- sd(OGDT_22$BSI_PAR[OGDT_22$Session == '1'])
# PAR_S2 <- summary(OGDT_22$BSI_PAR[OGDT_22$Session == '2'])
# sd_PAR_S2 <- sd(OGDT_22$BSI_PAR[OGDT_22$Session == '2'])
# print(paste("RC Index PAR:", critical_value_PAR, "verschilscore", delta_PAR))
# 
# PSY_S1 <-summary(OGDT_22$BSI_PSY[OGDT_22$Session == '1'])
# sd_PSY_S1 <- sd(OGDT_22$BSI_PSY[OGDT_22$Session == '1'])
# PSY_S2 <- summary(OGDT_22$BSI_PSY[OGDT_22$Session == '2'])
# sd_PSY_S2 <- sd(OGDT_22$BSI_PSY[OGDT_22$Session == '2'])
# print(paste("RC Index PSY:", critical_value_PSY, "verschilscore", delta_PSY))
# 
# aanv_S1 <-summary(OGDT_22$BSI_aanv[OGDT_22$Session == '1'])
# sd_aanv_S1 <- sd(OGDT_22$BSI_aanv[OGDT_22$Session == '1'])
# aanv_S2 <- summary(OGDT_22$BSI_aanv[OGDT_22$Session == '2'])
# sd_aanv_S2 <- sd(OGDT_22$BSI_aanv[OGDT_22$Session == '2'])
# 
# TOT_S1 <- summary(OGDT_22$BSI_TOT[OGDT_22$Session == '1'])
# sd_TOT_S1 <- sd(OGDT_22$BSI_TOT[OGDT_22$Session == '1'])
# TOT_S2 <- summary(OGDT_22$BSI_TOT[OGDT_22$Session == '2'])
# sd_TOT_S1 <- sd(OGDT_22$BSI_TOT[OGDT_22$Session == '2'])
# print(paste("RC Index TOT:", critical_value_TOT, "verschilscore", delta_TOT))
# 
# AAS_S1 <- summary(OGDT_22$BSI_AAS[OGDT_22$Session == '1'])
# sd_AAS_S1 <- sd(OGDT_22$BSI_AAS[OGDT_22$Session == '1'])
# AAS_S2 <- summary(OGDT_22$BSI_AAS[OGDT_22$Session == '2'])
# sd_AAS_S2 <- sd(OGDT_22$BSI_AAS[OGDT_22$Session == '2'])
# print(paste("RC Index AAS:", critical_value_AAS, "verschilscore", delta_AAS))
# 
# EAS_S1 <- summary(OGDT_22$BSI_EAS[OGDT_22$Session == '1'])
# sd_EAS_S1 <- sd(OGDT_22$BSI_EAS[OGDT_22$Session == '1'])
# EAS_S2 <- summary(OGDT_22$BSI_EAS[OGDT_22$Session == '2'])
# sd_EAS_S2 <- sd(OGDT_22$BSI_EAS[OGDT_22$Session == '2'])
# print(paste("RC Index EAS:", critical_value_EAS, "verschilscore", delta_EAS))





# SOM_bev_m  = 0.31
# SOM_bev_sd = 0.45
# COG_bev_m  = 0.6
# COG_bev_sd = 0.6
# INT_bev_m  = 0.57
# INT_bev_sd = 0.65
# DEP_bev_m  = 0.39
# DEP_bev_sd = 0.54
# ANG_bev_m  = 0.39
# ANG_bev_sd = 0.45
# HOS_bev_m  = 0.38
# HOS_bev_sd = 0.43
# FOB_bev_m  = 0.28
# FOB_bev_sd = 0.4
# PAR_bev_m  = 0.5
# PAR_bev_sd = 0.58
# PSY_bev_m  = 0.33
# PSY_bev_sd = 0.47
# TOT_bev_m  = 0.42
# TOT_bev_sd = 0.40
# AAS_bev_m  = 15.91
# AAS_bev_sd = 11.85
# EAS_bev_m  = 1.25
# EAS_bev_sd = 0.34
# 
# COG_rel = 0.8
# SOM_rel = 0.849
# INT_rel = 0.8
# DEP_rel = 0.82
# ANG_rel = 0.814
# HOS_rel =0.812
# FOB_rel = 0.833
# PAR_rel = 0.8
# PSY_rel = 0.8
# TOT_rel = 0.948
# AAS_rel = 0.948
# EAS_rel = 0.948





# #Bereken het aantal items van de BSI dat niet gelijk staat aan 0 om de AAS te berekenen 
# BSI_AAS = count=rowSums(OGDT_22[BSI_1:BSI_53]!=0)
# OGDT_22$BSI_AAS <- BSI_AAS
# OGDT_22$BSI_EAS = OGDT_22$BSI_tot/BSI_AAS  #bereken de ernst van aantal symptomen en plaats op de EAS-kolom in het bestand
# 
# #subschalen van de BSI berekenen
# OGDT_22$BSI_COG <- OGDT_22$BSI_COG/6   #bereken cognitieve klachten
# OGDT_22$BSI_SOM <- OGDT_22$BSI_SOM/7   #bereken somatische klachten
# OGDT_22$BSI_INT <- OGDT_22$BSI_INT/4   #bereken interpesoonlijke klachten
# OGDT_22$BSI_DEP <- OGDT_22$BSI_DEP/6   #bereken depressieve klachten
# OGDT_22$BSI_ANG <- OGDT_22$BSI_ANG/6   #bereken angst klachten
# OGDT_22$BSI_HOS <- OGDT_22$BSI_HOS/5   #bereken hostiliteit klachten
# OGDT_22$BSI_FOB <- OGDT_22$BSI_FOB/6   #bereken fobische klachten
# OGDT_22$BSI_PAR <- OGDT_22$BSI_PAR/5   #bereken paranoide klachten
# OGDT_22$BSI_PSY <- OGDT_22$BSI_PSY/5   #bereken psychotische klachten
# OGDT_22$BSI_aanv <- OGDT_22$BSI_aanv/4 #bereken aanvullende klachten





# #convert demographics naar numerics
# OGDT_22$Session <- factor(OGDT_22$`Consent-splitter`)
# OGDT_22$Age    <- as.numeric(OGDT_22$Age)
# OGDT_22$Sexe   <- as.numeric(OGDT_22$Sexe)
# 
# #### DEMOGRAPHICS ####
# geslacht <- OGDT_22$Sexe[OGDT_22$Session == '1']
# leeftijd <- OGDT_22$Age[OGDT_22$Session == '1']
# Man <- sum(geslacht==1)
# Vrouw <- sum(geslacht==2)
# 
# # Pas de aantallen tussen haakjes aan a.d.h.v. Man/vrouw variabele hierboven!
# 
# Demographics_sekse <- pie(table(geslacht), labels=c('Man (10)','Vrouw (0)'), col=c('#ADD8E6','#F5A761'),  main='Man-vrouw verhouding')
# Demographics_leeftijd <- boxplot(leeftijd, horizontal=T, main= "spreiding leeftijd deelnemers", col="#ADD8E6")
# Demographics_leeftijd <- stripchart(leeftijd, method = "jitter",add = TRUE, pch = 20, col = '#FF5733')

