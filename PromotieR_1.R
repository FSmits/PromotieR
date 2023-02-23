# Promotie R script 1

rm(list=ls())

# ----- Packages ----- 
### Run this line only once:
install.packages("dplyr"); install.packages("ggplot2"); install.packages("stringr"); install.packages("reshape2"); 

library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)

# ------ Hier data van Xandra --------

# temp data:
SPSS.users  <- c(3,2,3,4,1,3)
R.users     <- SPSS.users*2
df          <- as.data.frame(cbind(SPSS.users,R.users))
df$domein   <- c("lichaamsfuncties", "dagelijks functioneren", "zingeving", "meedoen", "kwaliteit van leven", "mentaal welbevinden")
plot_df     <- reshape2::melt(df)
  
# toch omdraaien kolommen en rijen voor plot


# ------ Het figuur -------

# Voorbeeldscript van R gallery:  (link: https://r-graph-gallery.com/web-circular-barplot-with-R-and-ggplot2.html)

# Make the plot
plt <- ggplot(plot_df) +
  geom_hline( aes(yintercept = y), data.frame(y=0:max(plot_df$value)), color="lightgrey" ) +
  geom_col( aes(x=str_wrap(domein,5), y=value, fill=domein), position = "dodge2", show.legend = TRUE, alpha = .9) +
  xlab(" ") + ylab("Waarde") + 
  coord_polar() +
  facet_wrap( . ~ variable)

# Add labels & Change colors
plt + labs(
  title = "Positive Health Web: SPSS vs. R",
  subtitle ="If you strive for a happier life, you might want to start to use R.",
  caption = "Voor meer datavisualisatie inspo: https://r-graph-gallery.com/") +
  scale_fill_manual(values=c("#6C5B7B","#C06C84","#F67280","#F8B195","#F8B222","#C2C185")) +
  theme(text = element_text(size = 10, color = "gray12"), 
        plot.title = element_text(face = "bold", size = 15, hjust = .05),
        plot.subtitle = element_text(size = 13, hjust = .05),
        axis.title = element_blank(),axis.ticks = element_blank(), axis.text.y = element_blank(),legend.position = "bottom", 
        panel.background = element_rect(fill = "white", color = "white"),panel.grid = element_blank(),panel.grid.major.x = element_blank() )


  
