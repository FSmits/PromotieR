
# Clear workspace
rm(list=ls())


select_pt <- function() {
  print("You will do a study with a stress task. This script will predict the effect of the stress task on the participating patient. Imagine a patient and give three characteristics: gender, age and diagnosis. ")
  
  gender    <- readline("Enter a gender (male/female): ")
  age       <- readline("Enter an age (18-88 yrs): ")
  diagnosis <- readline("Enter diagnosis (PTSD/anxiety/depression/OCD): ")
  
  data <- c(gender, age, diagnosis)
  
  outcomes <- c("Patient will shows proper stress response", "Patient will show no stress response", "Patient will drop out because of stress task")
  idx <- sample(1:length(outcomes), 1)
  
  
  data <- c(data,outcomes[idx])
  
  data
  
}

select_pt()





# ---- oud -----
education <- readline("Enter the education level (high school/college/graduate): ")
gender2   <- readline("Enter the gender (male/female): ")
age2      <- readline("Enter the age: ")

#education; gender2; age2
