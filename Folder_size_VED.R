### Clear environment
rm(list=ls())

### Install libraries
#install.packages("pacman")
#install.packages("ggplot2")
#install.packages("gridExtra")
library(pacman)
library(ggplot2)
library(gridExtra)


### Set name
your_name <- "Xandra"


### Get path name
# mac users: option + right click on MGGZ folder > copy as path name
# windows users: shift + right click on MGGZ folder > copy as path
mggz_path <- "/Users/aplas2/Networkshares/heronderzoek/MGGZ/" ### paste your path to MGGZ directory on O:-drive between the ""


### Get folder information --> TAKES ±7 MINUTES
colleagues <- c("Bastiaan", "Xandra", "Danny", "Amber", "Sofie", "Lisette", 
                "Antoin", "Rosanne", "Sophie", "Margreet", "Elbert", "Lukas", 
                "Frank", "Remco", "Karlijn", "Martine", "Remko")

# Dataframe for results
results <- data.frame(
  Name = character(),
  Number_of_Files = integer(),
  Total_Size_GB = numeric(),
  stringsAsFactors = FALSE
)

p_load(fs,tidyfst)
sys_time_print({
  # Loop over colleagues
  for (your_name in colleagues) {
    
    # Create path
    path_name <- paste(mggz_path, your_name, sep = "")
    
    # Controleer of de map bestaat
    if (dir.exists(path_name)) {
      
      files <- list.files(path_name, full.names = TRUE, recursive = TRUE)
      vect_size <- sapply(files, file.size)
      
      # Calc values
      number_of_files <- length(files)
      total_size_gb <- sum(vect_size, na.rm = TRUE) / 10^9
      
      # Save results dataframe
      results <- rbind(results, data.frame(
        Name = your_name,
        Number_of_Files = number_of_files,
        Total_Size_GB = total_size_gb,
        stringsAsFactors = FALSE
      ))
    } else {
      # Folder does not exsist
      results <- rbind(results, data.frame(
        Name = your_name,
        Number_of_Files = NA,
        Total_Size_GB = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
})


### Create figure
# Plot for number of files
results$Color <- ifelse(results$Name == your_name, "orange", "grey")
mean_files <- mean(results$Number_of_Files, na.rm = TRUE)
your_files <- results$Number_of_Files[results$Name == your_name]
percentage_files <- round((your_files / sum(results$Number_of_Files, na.rm = TRUE)) * 100, 2)

plot_files <- ggplot(results, aes(x = reorder(Name, -Number_of_Files), y = Number_of_Files, fill = Color)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Number_of_Files), vjust = -0.5) +
  scale_fill_identity() +
  geom_hline(yintercept = mean_files, linetype = "dashed", color = "red") +
  labs(title = paste("Aantal bestanden per persoon (", percentage_files, "% van totaal)", sep = ""),
       x = "Collega's", y = "Aantal bestanden") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for file size
mean_size <- mean(results$Total_Size_GB, na.rm = TRUE)
your_size <- results$Total_Size_GB[results$Name == your_name]
percentage_size <- round((your_size / sum(results$Total_Size_GB, na.rm = TRUE)) * 100, 2)

plot_size <- ggplot(results, aes(x = reorder(Name, -Total_Size_GB), y = Total_Size_GB, fill = Color)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Total_Size_GB, 2)), vjust = -0.5) +
  scale_fill_identity() +
  geom_hline(yintercept = mean_size, linetype = "dashed", color = "red") +
  labs(title = paste("Totale grootte per persoon (", percentage_size, "% van totaal)", sep = ""),
       x = "Collega's", y = "Totale grootte (GB)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots
grid.arrange(plot_files, plot_size, nrow = 2)



