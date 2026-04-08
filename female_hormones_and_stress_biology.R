library(shiny)
library(ggplot2)

# ----- Functies -----

# Hormonale cycli
simulate_hormones <- function(day, leeftijd, categorie) {
  d <- day
  
  jitter_val <- function(base, amplitude = 0.05) {
    base * (1 + runif(1, -amplitude, amplitude))
  }
  
  # Categorie-effecten
  if (categorie == "Hormonale anticonceptie") {
    return(data.frame(
      hormoon = c("Oestrogeen", "Progesteron", "FSH", "LH"),
      waarde = c(jitter_val(60), jitter_val(60), jitter_val(10), jitter_val(5))
    ))
  }
  
  # Leeftijdseffecten
  if (leeftijd == "Menopauze (51+)") {
    return(data.frame(
      hormoon = c("Oestrogeen", "Progesteron", "FSH", "LH"),
      waarde = c(jitter_val(10), jitter_val(5), jitter_val(70), jitter_val(50))
    ))
  }
  
  # Normale cyclische basis
  FSH_base <- 15 + 10 * exp(-((d - 3)/3)^2) + 5 * exp(-((d - 14)/1)^2)
  LH_base <- 10 + 100 * exp(-((d - 14)/1)^2)
  Oest_base <- 40 + 50 * exp(-((d - 13)/5)^2)
  Prog_base <- 20 + 80 / (1 + exp(-(d - 14)/2))
  
  # Perimenopauze
  if (leeftijd == "Perimenopauze (40-50)") {
    FSH <- FSH_base * 1.5
    LH <- LH_base * 1.2
    Oest <- Oest_base * (0.7 + 0.3*sin(2*pi*d/7))
    Prog <- Prog_base * 0.6
  } else {
    FSH <- FSH_base
    LH <- LH_base
    Oest <- Oest_base
    Prog <- Prog_base
  }
  
  data.frame(
    hormoon = c("Oestrogeen", "Progesteron", "FSH", "LH"),
    waarde = c(Oest, Prog, FSH, LH)
  )
}

# Stress-hormonen (HPA-HPG interactie)
simulate_stress_hormones <- function(day, leeftijd, categorie, base_df) {
  # base_df: output van simulate_hormones()
  # Stressfactor: 0 = geen stress, 100 = max reductie (~50%)
  stress_factor <- 1 - 0.005 * day  # tijdelijke placeholder, kan input$stress zijn
  # Voor elk hormoon behalve Cortisol, pas reductie toe
  stress_val <- base_df
  stress_val$waarde <- stress_val$waarde * (1 - 0.005 * day)
  
  # Cortisol toevoegen, stijgt met stress
  Cortisol <- 20 + 80 * (day / 100) + runif(1, -5, 5)
  stress_val <- rbind(stress_val, data.frame(hormoon="Cortisol", waarde=Cortisol))
  
  return(stress_val)
}

# ----- Shiny UI -----
ui <- fluidPage(
  titlePanel("Vrouwelijke hormonen en stress"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("day", "Dag van de cyclus:", min = 1, max = 28, value = 1),
      selectInput("leeftijd", "Leeftijdsfase:", 
                  choices = c("Vruchtbare jaren", "Perimenopauze (40-50)", "Menopauze (51+)")),
      selectInput("categorie", "Categorie:", 
                  choices = c("Niks", "Hormonale anticonceptie")),
      sliderInput("stress", "Stressniveau:", min = 0, max = 100, value = 20)
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotOutput("hormonePlot")),
        column(6, plotOutput("stressPlot"))
      )
    )
  )
)

# ----- Shiny server -----
server <- function(input, output) {
  
  output$hormonePlot <- renderPlot({
    df <- simulate_hormones(input$day, input$leeftijd, input$categorie)
    
    ggplot(df, aes(x = hormoon, y = waarde, fill = hormoon)) +
      geom_bar(stat = "identity") +
      ylim(0, 110) +
      theme_minimal() +
      labs(title = paste("Hormonale waarden - dag", input$day))
  })
  
  output$stressPlot <- renderPlot({
    # 1️⃣ base van linker plot
    base_df <- simulate_hormones(input$day, input$leeftijd, input$categorie)
    
    # 2️⃣ pas stress aan
    df <- base_df
    stress_factor <- 1 - 0.005 * input$stress
    df$waarde <- df$waarde * stress_factor
    
    # 3️⃣ voeg Cortisol toe
    Cortisol <- 20 + 80 * (input$stress / 100) + runif(1, -5, 5)
    df <- rbind(df, data.frame(hormoon="Cortisol", waarde=Cortisol))
    
    ggplot(df, aes(x=hormoon, y=waarde, fill=hormoon)) +
      geom_bar(stat="identity") +
      ylim(0,200) +
      theme_minimal() +
      labs(title = paste("Stress-effecten - stressniveau", input$stress))
  })
}

# ----- Run Shiny App -----
shinyApp(ui = ui, server = server)