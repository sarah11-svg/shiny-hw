library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)

prizes <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv"
)

prizes <- prizes %>%
  mutate(prize_year = as.numeric(prize_year))

ui <- fluidPage(
  
  titlePanel("TidyTuesday Prize"),
  
  tabsetPanel(
    tabPanel("Summary",
             h3("Dataset Summary"),
             verbatimTextOutput("summary"),
             hr(),
             h4("Full Dataset"),
             DTOutput("table")
    ),
    
    tabPanel("Explore by Prize Genre",
             sidebarLayout(
               sidebarPanel(
                 selectInput("genre", "Prize Genre:",
                             choices = sort(unique(prizes$prize_genre)),
                             selected = unique(prizes$prize_genre)[1]),
                 
                 sliderInput("yearRange", "Year Range:",
                             min = min(prizes$prize_year, na.rm = TRUE),
                             max = max(prizes$prize_year, na.rm = TRUE),
                             value = c(min(prizes$prize_year, na.rm = TRUE),
                                       max(prizes$prize_year, na.rm = TRUE)))
               ),
               
               mainPanel(
                 plotOutput("genrePlot"),
                 hr(),
                 DTOutput("genreTable")
               )
             )
    ),
    
    tabPanel("Demographics",
             fluidRow(
               column(6,
                      h4("Ethnicity Distribution"),
                      plotOutput("ethnicityPlot")
               ),
               column(6,
                      h4("Roles Distribution"),
                      plotOutput("rolePlot")
               )
             )
    ),
    
    tabPanel("Institutions and Countries",
             sidebarLayout(
               sidebarPanel(
                 selectInput("institution", "Select Institution:",
                             choices = sort(unique(prizes$degree_institution)),
                             selected = unique(prizes$degree_institution)[1])
               ),
               
               mainPanel(
                 h4("Country of Residence Distribution"),
                 plotOutput("countryPlot"),
                 hr(),
                 h4("Institution Summary"),
                 verbatimTextOutput("instSummary")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  output$summary <- renderPrint({
    summary(prizes)
  })
  
  output$table <- renderDT({
    datatable(prizes)
  })
  
  output$genrePlot <- renderPlot({
    filtered <- prizes %>%
      filter(prize_genre == input$genre,
             prize_year >= input$yearRange[1],
             prize_year <= input$yearRange[2])
    
    ggplot(filtered, aes(x = prize_year)) +
      geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7) +
      labs(title = paste("Awards Over Time –", input$genre),
           x = "Prize Year",
           y = "Count")
  })
  
  output$genreTable <- renderDT({
    prizes %>%
      filter(prize_genre == input$genre) %>%
      datatable()
  })
  
  output$ethnicityPlot <- renderPlot({
    counts <- prizes %>% count(ethnicity_macro)
    
    ggplot(counts, aes(x = ethnicity_macro, y = n, fill = ethnicity_macro)) +
      geom_col() +
      labs(title = "Ethnicity Distribution") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$rolePlot <- renderPlot({
    role_counts <- prizes %>% count(person_role)
    
    ggplot(role_counts, aes(x = person_role, y = n, fill = person_role)) +
      geom_col() +
      labs(title = "Roles Distribution")
  })
  
  output$countryPlot <- renderPlot({
    counts <- prizes %>% count(uk_residence)
    
    ggplot(counts, aes(x = uk_residence, y = n, fill = uk_residence)) +
      geom_col() +
      labs(title = "UK Residence Distribution")
  })
  
  output$instSummary <- renderPrint({
    prizes %>%
      filter(degree_institution == input$institution) %>%
      summary()
  })
}

shinyApp(ui, server)
