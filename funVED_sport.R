# SCRIPT OM EEN PERSOONLIJK WORKOUT PLAN TE MAKEN

# SELECTEER JOUW VOORKEUREN

# Wat wil je trainen? 
#       A) Full body 
#       B) Armen
#       C) Benen
training_focus <- "A" #typ A, B of C tussen de aanhalingstekens

# Op welk niveau wil je trainen? 
#       A) Rustig 
#       B) Gemiddeld
#       C) Intensief

training_niveau <- "B" #typ A, B of C tussen de aanhalingstekens


# MAAK WORKOUT PLAN
oefeningen <- data.frame(full_body = c("Plank", "Burpees", "Push-Ups", "Jump Squats", "Mountain Climber"),
                         armen = c("Push-Ups", "Tricep Dips", "Arm Circles", "Plank Up-and-Down", "Prayer Pulses"),
                         benen = c("Jump Squats", "Wall Sit", "Schaatssprongen", "Alternating Jump Lunges", "Squat Pulses"))

if (training_focus == "A") {
  if (training_niveau == "A") {
    writeLines(paste("Jij wil je in de funVED focussen op het hele lichaam.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "full_body"], "\n-", oefeningen[2, "full_body"], "\n-", oefeningen[3, "full_body"], "\n-", oefeningen[4, "full_body"], "\n-", oefeningen[5, "full_body"]))
    writeLines("\nOmdat jij wil sporten op een rustig niveau wissel \njij deze oefeningen af met het kleine loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 1.")
    
  }else if (training_niveau == "B"){
    writeLines(paste("Jij wil je in de funVED focussen op het hele lichaam.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "full_body"], "\n-", oefeningen[2, "full_body"], "\n-", oefeningen[3, "full_body"], "\n-", oefeningen[4, "full_body"], "\n-", oefeningen[5, "full_body"]))
    writeLines("\nOmdat jij wil sporten op een gemiddeld niveau wissel \njij deze oefeningen af met het grote loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 2.")
    
  }else if (training_niveau == "C"){
    writeLines(paste("Jij wil je in de funVED focussen op het hele lichaam.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "full_body"], "\n-", oefeningen[2, "full_body"], "\n-", oefeningen[3, "full_body"], "\n-", oefeningen[4, "full_body"], "\n-", oefeningen[5, "full_body"]))
    writeLines("\nOmdat jij wil sporten op een intensief niveau wissel \njij deze oefeningen af met het grote loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 3.")
    }
}

if (training_focus == "B") {
  if (training_niveau == "A") {
    writeLines(paste("Jij wil je in de funVED focussen op de armen.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "armen"], "\n-", oefeningen[2, "armen"], "\n-", oefeningen[3, "armen"], "\n-", oefeningen[4, "armen"], "\n-", oefeningen[5, "armen"]))
    writeLines("\nOmdat jij wil sporten op een rustig niveau wissel \njij deze oefeningen af met het kleine loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 1.")
    
  }else if (training_niveau == "B"){
    writeLines(paste("Jij wil je in de funVED focussen op de armen.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "armen"], "\n-", oefeningen[2, "armen"], "\n-", oefeningen[3, "armen"], "\n-", oefeningen[4, "armen"], "\n-", oefeningen[5, "armen"]))
    writeLines("\nOmdat jij wil sporten op een gemiddeld niveau wissel \njij deze oefeningen af met het grote loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 2.")
    
  }else if (training_niveau == "C"){
    writeLines(paste("Jij wil je in de funVED focussen op de armen.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "armen"], "\n-", oefeningen[2, "armen"], "\n-", oefeningen[3, "armen"], "\n-", oefeningen[4, "armen"], "\n-", oefeningen[5, "armen"]))
    writeLines("\nOmdat jij wil sporten op een intensief niveau wissel \njij deze oefeningen af met het grote loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 3.")
  }
}

if (training_focus == "C") {
  if (training_niveau == "A") {
    writeLines(paste("Jij wil je in de funVED focussen op de benen.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "benen"], "\n-", oefeningen[2, "benen"], "\n-", oefeningen[3, "benen"], "\n-", oefeningen[4, "benen"], "\n-", oefeningen[5, "benen"]))
    writeLines("\nOmdat jij wil sporten op een rustig niveau wissel \njij deze oefeningen af met het kleine loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 1.")
    
  }else if (training_niveau == "B"){
    writeLines(paste("Jij wil je in de funVED focussen op de benen.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "benen"], "\n-", oefeningen[2, "benen"], "\n-", oefeningen[3, "benen"], "\n-", oefeningen[4, "benen"], "\n-", oefeningen[5, "benen"]))
    writeLines("\nOmdat jij wil sporten op een gemiddeld niveau wissel \njij deze oefeningen af met het grote loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 2.")
    
  }else if (training_niveau == "C"){
    writeLines(paste("Jij wil je in de funVED focussen op de benen.\n\nDit zijn jouw oefeningen:\n-", oefeningen[1, "benen"], "\n-", oefeningen[2, "benen"], "\n-", oefeningen[3, "benen"], "\n-", oefeningen[4, "benen"], "\n-", oefeningen[5, "benen"]))
    writeLines("\nOmdat jij wil sporten op een intensief niveau wissel \njij deze oefeningen af met het grote loop rondje.")
    writeLines("Voor het laatste spel wordt jouw vermenigvuldigingsfactor 3.")
  }
}

# EINDE CODE
