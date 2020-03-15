
library(shiny)

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
  
  # SIMULATION FUNCTION
  sim_function <- function(
    n_population = 10000,
    n_days = 100,
    time_to_quarantine = 14,
    n_encounters = 50,
    transmission_probability = 0.05,
    n_initial = 1
  ){
    condition_matrix<- matrix(nrow = n_population,
                              ncol = n_days,
                              0)
    condition_matrix[1:n_initial, 1] <- 1
    condition_counts <- matrix(nrow = 3, ncol = n_days, NA)
    current_count <- c(table(condition_matrix[, 1])[c("-1", "0", "1")])
    current_count <- replace_na(current_count, 0)
    condition_counts[, 1] <- current_count
    colnames(condition_counts) <- 1:n_days
    rownames(condition_counts) <- c(-1, 0, 1)
    
    for(jj in 2:ncol(condition_matrix)){
      prop_infected <- mean(condition_matrix[, jj-1] == 1)
      
      # Number of encounters with infected
      expected_encounters_with_infected <- n_encounters * prop_infected
      n_encounters_with_infected <- rbinom(n_population, n_encounters, prop_infected)
      
      # Whether individual contracted virus
      prop_did_not_contract <- (1 - transmission_probability) ^ n_encounters_with_infected
      contracted <- rbinom(n_population, 1, 1-prop_did_not_contract)
      condition_matrix[, jj] <- contracted
      
      # Plus anyone who already had it
      condition_matrix[, jj][condition_matrix[, jj-1] == 1] <- 1
      condition_matrix[, jj][condition_matrix[, jj-1] == -1] <- -1
      
      # Go under quarantine
      condition_recognized <- rowSums(condition_matrix[, 1:jj] == 1) >= time_to_quarantine
      condition_matrix[condition_recognized, jj] <- -1
      
      current_count <- c(table(condition_matrix[, jj])[c("-1", "0", "1")])
      current_count <- replace_na(current_count, 0)
      condition_counts[, jj] <- current_count
    }
    
    n_infected_by_day <- colSums(condition_counts[-2, ])
    n_newly_infected <- c(0, colSums(condition_matrix[, -1] == 1 & condition_matrix[, -ncol(condition_matrix)] == 0))
    
    tibble(day = 1:n_days,
           n_infected_by_day,
           n_newly_infected) %>%
      return()
  }
  
  
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

