
## Loading Libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)

## Loading Helper functions
source("dashboard_helper.R")

ui <- dashboardPage(skin = "black",
                    ## header
                    dashboardHeader(title = "Hotels"),
                    
                    ## Side bar
                    dashboardSidebar(
                      
                      selectInput("v_location",
                                  "Choose Location",
                                  choices = all_location,
                                  multiple = TRUE,
                                  selected = "Bang Tao Beach"),
                      
                      uiOutput("v_hotel"),
                      
                      dateRangeInput("v_daterange", "Date range:",
                                     start = min(tidy_reviews$date),
                                     end   = max(tidy_reviews$date),
                                     min   = min(tidy_reviews$date),
                                     max   = max(tidy_reviews$date))
                    ),
                    
                    
                    ## Body
                    dashboardBody(
                                  
                        shinyjs::useShinyjs(),
                        
                        fluidPage(
                          
                          verbatimTextOutput("value_1"),
                          
                          verbatimTextOutput("value_2"),
                          
                                  fluidRow(
                                    box(width = 5, status = "primary", title = "Top Words", solidHeader = TRUE,
                                        footer = "Shows top 10 words depending on selected filters.",
                                        withSpinner(plotOutput("top_words"), type = 3, color.background = "#0B08CFAA")),
                                    box(width = 7, status = "primary", title = "Word Cloud", solidHeader = TRUE,
                                        footer = "Shows a word cloud of postive (green) and negative (red) words.",
                                        withSpinner(plotOutput("word_cloud"), type = 3, color.background = "#0B08CFAA"))
                                  ),
                                  
                                  fluidRow(
                                    box(width = 6, status = "info", title = "Bing Sentiment Analysis", solidHeader = TRUE,
                                        footer = "Shows a trend of sentiments across review timeline.",
                                        withSpinner(plotlyOutput("sentiment_bing"), type = 3, color.background = "#0B08CFAA")),
                                    box(width = 6, status = "info", title = "Afinn Sentiment Analysis", solidHeader = TRUE,
                                        footer = "Shows a trend of sentiments across review timeline.",
                                        withSpinner(plotlyOutput("sentiment_afinn"), type = 3, color.background = "#0B08CFAA"))
                                  )
)))

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    pick_hotel(input$v_location)
  
  })
  
  output$v_hotel <- renderUI({
    selectInput("v_hotel","Select Hotel", choices = filtered_data(), multiple = TRUE) 
    
  })
  
  tidy_review_1 <- reactive({
    
    #req(input$v_location, input$v_hotel, input$v_daterange[1], input$v_daterange[2])
    
    validate(
      need(input$v_location != "", "Please choose a location!"),
      need(input$v_hotel != "", "Please choose a hotel!")
    )
    
    tidy_reviews %>% 
      filter(location %in% input$v_location,
             hotel_restaurant_name %in% input$v_hotel,
             between(date,input$v_daterange[1], input$v_daterange[2]))
  }) %>% bindEvent(input$v_location, input$v_hotel, input$v_daterange[1], input$v_daterange[2])
  
  
  tidy_review_2 <- reactive({
    
    #req(input$v_location, input$v_hotel)
    
    validate(
      need(input$v_location != "", "Please choose a location!"),
      need(input$v_hotel != "", "Please choose a hotel!")
    )
    
    tidy_reviews %>% 
      filter(location %in% input$v_location,
             hotel_restaurant_name %in% input$v_hotel) %>% 
      summarize(min_date = min(date),
                max_date = max(date)) %>% 
      mutate(min_date = paste("Earliest Review Date for the selected Location and Hotel: ", as.character(min_date)),
             max_date = paste("Latest Review Date for the selected Location and Hotel: ", as.character(max_date)))
  }) %>% bindEvent(input$v_location, input$v_hotel)
  
  
  output$value_1 <- renderText({ tidy_review_2()$min_date })
  
  output$value_2 <- renderText({ tidy_review_2()$max_date })
  
  output$top_words <- renderPlot({
    
    req(tidy_review_1())
    
    tidy_review_1() %>% 
      count(word, sort = TRUE) %>% 
      slice_max(order_by =  n, n = 10, with_ties = F) %>% 
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill = "midnightblue") +
      coord_flip() +
      labs(
        title = "Top words used in reviews",
        x = NULL,
        y = "Count of Occurence"
      )
  }) %>% bindCache(input$v_location, input$v_hotel, input$v_daterange[1], input$v_daterange[2])
  
  output$word_cloud <- renderPlot({
    
    req(tidy_review_1())
    
    tidy_review_1() %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("red", "green"),
                       max.words = 50, scale=c(3,.5))
  }) %>% bindCache(input$v_location, input$v_hotel, input$v_daterange[1], input$v_daterange[2])
  
  output$sentiment_bing <- renderPlotly({
    
    req(tidy_review_1())
    
    p_1 <- tidy_review_1() %>%
      mutate(date = round_date(date,"month")) %>% 
      inner_join(get_sentiments("bing")) %>%
      count(date,sentiment) %>% 
      ggplot(aes(date, n, col = sentiment)) +
      geom_line(size = 1.2, alpha = 0.8) +
      labs(
        title = "Trend of Sentiments - Bing",
        x = NULL,
        y = "# of sentiments",
        col = "Sentiments"
      )
    
    ggplotly(p_1)
    
    
  }) %>% bindCache(input$v_location, input$v_hotel, input$v_daterange[1], input$v_daterange[2])
  
  output$sentiment_afinn <- renderPlotly({
    
    req(tidy_review_1())
    
   p_2 <- tidy_review_1() %>%
      mutate(date = round_date(date,"month")) %>% 
      inner_join(get_sentiments("afinn")) %>% 
      count(date, wt = sum(value)) %>% 
      ggplot(aes(date, n)) +
      geom_line(size = 1.2) +
      labs(
        title = "Trend of Sentiments - Afinn",
        x = NULL,
        y = "Sum of sentiment scores"
      )
   
   ggplotly(p_2)
   
  }) %>% bindCache(input$v_location, input$v_hotel, input$v_daterange[1], input$v_daterange[2])
   
}

# Run the application 
shinyApp(ui = ui, server = server)

