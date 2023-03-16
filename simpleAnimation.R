library(gapminder)
library(gganimate)
library(gifski)

EC <- data.frame(
  year <- rep(rep((2000:2023), 5)),
  group <- rep(c("Elbert", "seniors", "phd's", "research assistents", "interns"), each = 24)
)
colnames(EC) <- c("year", "group")

EC$productivity[EC$group == "Elbert"]   <- c(seq(from=1,to=4,by=0.5), seq(from=3,to=7,by=1), rep(c(5,6,7), 4)) 
EC$productivity[EC$group == "seniors"]   <- c(seq(from=1,to=4,by=1), seq(from=7,to=3,by=-1), seq(from=2,to=7,by=0.5), seq(from=5,to=8,by=1))
EC$productivity[EC$group == "phd's"]   <- seq(from=1,to=13,by=0.5)
EC$productivity[EC$group == "research assistents"]   <- c(seq(from=1,to=8,by=1), rep(c(8,9), 8))
EC$productivity[EC$group == "interns"]   <- c(seq(from=3,to=8,by=0.25), 7.5, 7, 6.5)

#[EC$group!="phd's",]
## standard ggplot2
myPlot <- ggplot(EC, aes(x = group, y = productivity)) +
  geom_bar(stat = "summary") +
  scale_colour_manual(values = country_colors) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'Group', y = 'Productiviteit') +
  transition_time(year) +
  ease_aes('linear')

animate(myPlot, duration = 15, fps = 5, renderer = gifski_renderer())
anim_save("output.gif")




