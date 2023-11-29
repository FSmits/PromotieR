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
  opleiding = c("l", "l", "l", "l", "h", "h", "h", "h"))
df_bewegen$opleiding <- factor(df_bewegen$opleiding)


# ------------------------- Figuur ------------------------- #

# Plot
df_bewegen %>%
  ggplot(aes(x=jaar, y=perc, group=opleiding, color=opleiding)) +
  geom_line(linewidth=2) +
  labs(title = 'Bewegen in NL', x = 'Jaar', y = 'Percentage dat aan beweegnorm voldoet') +
  scale_color_manual(values = c("h" = "#e1700e", "l" = "#146496")) +
  theme(panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_line(colour = "grey"),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# ----------------------- Statistiek ----------------------- #

# Verschillen laag en hoogopgeleiden significant van elkaar als het gaat om 
# het percentage dat voldoet aan de beweegrichtlijn in 2022?
#
# Gebruik voor het beantwoorden van de vraag het dataframe 'data_bewegen_2022'
# Dit dataframe bevat 2 kolommen: 
#         1: opleiding (l = laag, h = hoog)
#         2: richtlijn behaald (0 = nee, 1 = ja)


# De data
# ------------------------------- #
# Set a seed for reproducibility
set.seed(123)

# Number of participants
n_participants <- 100

# Generate data
opleiding <- sample(c('h', 'l'), n_participants, replace = TRUE, prob = c(0.55, 0.45))
richtlijn_behaald <- ifelse(opleiding == 'l', rbinom(sum(opleiding == 'l'), 1, 0.8), rbinom(sum(opleiding == 'h'), 1, 0.4))
data_bewegen_2022 <- data.frame(opleiding = rep(opleiding, each = 1), richtlijn_behaald = c(richtlijn_behaald))

# Display the DataFrame
head(data_bewegen_2022)

# Frequentie tabel
FrequentieTabel <- table(data_bewegen_2022$opleiding, data_bewegen_2022$richtlijn_behaald)
FrequentieTabel

# De toets
# ------------------------------- #
# Ga hieronder zelf verder

