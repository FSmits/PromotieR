library(gapminder)
library(gganimate)
library(gifski)

EC <- data.frame(
  year <- rep(rep((2000:2023), 5)),
  group <- rep(c("Elbert", "senior's", "phd's", "research assistents", "interns"), each = 24)
)
colnames(EC) <- c("year", "group")

EC$productivity[EC$group == "Elbert"]   <- sample(2:8, 24, replace = TRUE)
EC$productivity[EC$group == "Seniors"]   <- sample(2:6, 24, replace = TRUE)
EC$productivity[EC$group == "phd's"]   <- sample(6:9, 24, replace = TRUE)
EC$productivity[EC$group == "research assistents"]   <- sample(1:10, 24, replace = TRUE)
EC$productivity[EC$group == "interns"]   <- sample(3:7, 24, replace = TRUE)


## standard ggplot2
myPlot <- ggplot(EC, aes(x = group, y = niveau)) +
  geom_bar(stat = "summary") +
  scale_colour_manual(values = country_colors) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'group', y = 'Productiviteit') +
  transition_time(year) +
  ease_aes('linear')

animate(myPlot, duration = 5, fps = 20, renderer = gifski_renderer())
anim_save("output.gif")




