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
  mutate(
    prize_year = as.numeric(prize_year)
  )

ui <- fluidPage(
  
  titlePanel("TidyTuesday Prize Awards Explorer"),
  
  tabsetPanel(
    tabPanel("Overview",
             h3("Dataset Summary"),
             verbatimTextOutput("summary"),
             hr(),
             h4("Full Dataset"),
             DTOutput("table")
    ),
    
    tabPanel("Explore by Prize Category",
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
                 plotlyOutput("genrePlot"),
                 hr(),
                 DTOutput("genreTable")
               )
             )
    ),
    
    tabPanel("People & Demographics",
             fluidRow(
               column(6,
                      h4("Ethnicity Distribution"),
                      plotlyOutput("ethnicityPlot")
               ),
               column(6,
                      h4("Roles Distribution"),
                      plotlyOutput("rolePlot")
               )
             )
    ),
    
    tabPanel("Institutions & Countries",
             sidebarLayout(
               sidebarPanel(
                 selectInput("institution", "Select Institution:",
                             choices = sort(unique(prizes$degree_institution)),
                             selected = unique(prizes$degree_institution)[1])
               ),
               
               mainPanel(
                 h4("Country of Residence Distribution"),
                 plotlyOutput("countryPlot"),
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
  
  output$genrePlot <- renderPlotly({
    
    filtered <- prizes %>%
      filter(prize_genre == input$genre,
             prize_year >= input$yearRange[1],
             prize_year <= input$yearRange[2])
    
    p <- ggplot(filtered, aes(x = prize_year)) +
      geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7) +
      labs(title = paste("Awards Over Time –", input$genre),
           x = "Prize Year",
           y = "Count")
    
    ggplotly(p)
  })
  
  
  output$genreTable <- renderDT({
    prizes %>%
      filter(prize_genre == input$genre) %>%
      datatable()
  })
  
  output$ethnicityPlot <- renderPlotly({
    counts <- prizes %>% count(ethnicity_macro)
    
    p <- ggplot(counts, aes(x = ethnicity_macro, y = n, fill = ethnicity_macro)) +
      geom_col() +
      labs(title = "Ethnicity Distribution") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  
  output$rolePlot <- renderPlotly({
    role_counts <- prizes %>% count(person_role)
    
    p <- ggplot(role_counts, aes(x = person_role, y = n, fill = person_role)) +
      geom_col() +
      labs(title = "Roles Distribution")
    
    ggplotly(p)
  })
  
  
  output$countryPlot <- renderPlotly({
    counts <- prizes %>% count(uk_residence)
    
    p <- ggplot(counts, aes(x = uk_residence, y = n, fill = uk_residence)) +
      geom_col() +
      labs(title = "UK Residence Distribution")
    
    ggplotly(p)
  })
  
  
  output$instSummary <- renderPrint({
    prizes %>%
      filter(degree_institution == input$institution) %>%
      summary()
  })
}

shinyApp(ui, server)
