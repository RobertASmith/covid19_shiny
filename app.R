rm(list=ls())

library(shiny)
library(tidyverse)

# source simulation function
source("./functions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Flattening the Curve: R-Shiny App"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
         numericInput("SI_pop",
                     "Population",
                     min = 1,
                     max = 100000,
                     step = 1000,
                     value = 10000),
         
         sliderInput("SI_days",
                     "Model Duration in Days",
                     min = 1,
                     max = 365,
                     value = 100),
         
         sliderInput("SI_ttQ",
                     "Time to Quarantine",
                     min = 1,
                     max = 30,
                     value = 15),
         
         sliderInput("SI_encounters",
                     "Number of encounters",
                     min = 1,
                     max = 100,
                     value = 15),
         
         sliderInput("SI_trans_prob",
                     "Transition Probability",
                     min = 0,
                     max = 1,
                     value = 0.05),
         
         sliderInput("SI_n_initial",
                     "Initial Number",
                     min = 1,
                     max = 1000,
                     value = 1),
         
         actionButton(inputId = "run_model",     # id of action button, used in server
                      label   = "Run model")     # action button label (on button)
         
      ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
        navbarPage("Tabs:",
         tabPanel("Results",plotOutput("infectedPlot")),
         tabPanel("Methods",includeMarkdown("d_sparkes.Rmd"))
        ) # close navbarpage
         ) # close main panel
   ) # close side bar layout 
) # close UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # RUN SIMULATION WITH INPUTS
  observeEvent(input$run_model,       # when action button pressed ...
               ignoreNULL = F, {
                 

  results <- sim_function(n_population = input$SI_pop,
                          n_days = input$SI_days,
                          time_to_quarantine = input$SI_ttQ,
                          n_encounters = input$SI_encounters,
                          transmission_probability = input$SI_trans_prob,
                          n_initial = input$SI_n_initial )
  
  results <- as.data.frame(results)
  
  
  # PLOT
  
  output$infectedPlot <- renderPlot({
    
    ggplot(data = results, aes(x = day, y = n_newly_infected, fill = "red")) +
      geom_line() +
      ylim(c(0, input$SI_pop)) +
      theme_bw() +
      labs(title    = "Flattening the curve",
           subtitle = "Simulating the incidence of viral infection",
           caption  = "ShinyApp by RASmith, University of Sheffield") +
      xlab("Duration in Days") +
      ylab("Number Infected") +
      geom_area(aes(col="red"))+ 
      guides(colour = "none", fill = "none")

  })
 

  
               }) # Observe Event End 
} # server end

# Run the application 
shinyApp(ui = ui, server = server)

