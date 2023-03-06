# Promotie R script 1

rm(list=ls())

# --------- Packages --------- 
### Run this line only once:
# install.packages("dplyr"); install.packages("ggplot2"); install.packages("ggpubr"); install.packages("stringr"); install.packages("reshape2"); 

library(dplyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(reshape2)

# ------ Prepare data --------

# create data frame
SPSS.users  <- c(3,2,3,4,1,3)
set.seed(123); R.users     <- sample(5:10, 6) # run together
df          <- as.data.frame(cbind(SPSS.users,R.users))
df$R.users[df$R.users < 7] <- 7

# add domain
df$Domein   <- c("Lichaamsfuncties", "Dagelijks functioneren", "Zingeving", "Meedoen", "Kwaliteit van leven", "Mentaal welbevinden")

# create data frame for plot
plot_df     <- reshape2::melt(df, id = "Domein")


# ------ Het figuur -------

# Voorbeeldscript van R gallery:  (link: https://r-graph-gallery.com/web-circular-barplot-with-R-and-ggplot2.html)

scale_range <- seq(0, 10, 2)
# Make the plot
plt <- ggplot(plot_df) +
  geom_hline( aes(yintercept = y), data.frame(y=scale_range), color="lightgrey" ) +
  geom_col( aes(x=str_wrap(Domein,5), y=value, fill=Domein), position = "dodge2", show.legend = TRUE, alpha = .9) +
  xlab(" ") + ylab("Waarde") + 
  coord_polar() +
  # Annotate custom scale inside plot
  annotate(x = 6.5, y = 2.3, label = "2", geom = "text", size = 2, color = "gray12") +
  annotate(x = 6.5, y = 4.3, label = "4", geom = "text", size = 2, color = "gray12") +
  annotate(x = 6.5, y = 6.3, label = "6", geom = "text", size = 2, color = "gray12") +
  annotate(x = 6.5, y = 8.3, label = "8", geom = "text", size = 2, color = "gray12") +
  annotate(x = 6.5, y = 10.3, label = "10", geom = "text", size = 2, color = "gray12") +
  facet_wrap( . ~ variable)

# Add labels & Change colors
plt + labs(
  title = "Positive Health Web: SPSS vs. R",
  subtitle ="If you strive for a happier life, you might want to start to use R.",
  caption = "Voor meer datavisualisatie inspo: https://r-graph-gallery.com/") +
  scale_fill_manual(values=c("#6C5B7B","#C06C84","#F67280","#F8B195","#F8B222","#C2C185")) +
  theme(text = element_text(size = 10, color = "gray12"), 
        plot.title = element_text(face = "bold", size = 15, hjust = 0),
        plot.subtitle = element_text(size = 13, hjust = 0),
        axis.title = element_blank(),axis.ticks = element_blank(), axis.text.y = element_blank(), legend.position = "bottom", 
        panel.background = element_rect(fill = "white", color = "white"),panel.grid = element_blank(),panel.grid.major.x = element_blank() )

# Vragen
# Wie scoren beter op 'kwaliteit van leven'?
# Hoe hoog is scoren R users op 'dagelijks functioneren'?
# Op welk domein is het verschil tussen SPSS-gebruikers en R-gebruikers het grootst?
# Bonus: veranderen de plot kleuren in de kleuren van het EC



