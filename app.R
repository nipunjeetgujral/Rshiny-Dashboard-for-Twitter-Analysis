# libraries ####
  # library to attach to twitter API
  library(twitteR)
  
  # library to manipulate data
  library(dplyr)
  library(lubridate)
  library(tidytext)
  library(stringr)
  library(tm)
  
  # library to visualize data
  library(wordcloud)
  library(RColorBrewer)
  library(ggplot2)
  library(plotly)

  # shiny related
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(crosstalk)

# authentication ####
authentication()
# ui ####
ui <- dashboardPage(
  # title ####
    dashboardHeader(title = "Twitter Use Analysis"),
  # menu ####
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Charts",
        tabName = "charts",
        icon = icon("stats", lib = "glyphicon")
      ),
      menuItem(
        "Table",
        tabName = "table1",
        icon = icon("th-large", lib = "glyphicon")
      ),
      menuItem(
        "Strata Analysis",
        tabName = "table2",
        icon = icon("list", lib = "glyphicon")
      ),
    # text input ####
      textInput(
        inputId = "twitter_handle",
        label = "Enter a Twitter Handle",
        width = 300,
        value = ""
      ),
    # slider input ####
      sliderInput(
        inputId = "tweet_num",
        label = "Numebr of Tweets",
        min = 0, max = 1000,
        value = 50
      ),
    # action button ####
      actionButton(
        inputId = "click",
        label = "submit"
      )
    )
  ),
  # body ####
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "charts",
          fluidPage(
            box(plotlyOutput(outputId = "tweets_per_day")),
            box(plotlyOutput(outputId = "tweets_per_hour")),
            box(plotlyOutput(outputId = "ratio_vs_time")),
            box(plotlyOutput(outputId = "fav_vs_retweet")),
            box(plotlyOutput(outputId = "metric_histograms")),
            box(plotlyOutput(outputId = "tweet_source"))
          )
        ),
        tabItem(
          tabName = "table1",
          h2("Indexed Tweets Connected to Original Data Set", align = "center"),
          plotlyOutput(outputId = "x2"),
          DT::dataTableOutput(outputId = "x1")
        ),
        tabItem(
          tabName = "table2",
          h2("Word Frequency per Group", align = "center"),
          plotlyOutput(outputId = "mean_table"),
          fluidPage(
            DT::dataTableOutput(outputId = "word_frequency_table1"),
            DT::dataTableOutput(outputId = "word_frequency_table2"),
            DT::dataTableOutput(outputId = "word_frequency_table3"),
            DT::dataTableOutput(outputId = "word_frequency_table4")
          )
        )
      )
    )
  )

# server ####
server <- function(input, output, ...){
  observeEvent(input$click, {
  # definning user handle and sample size ####
    handle <- isolate({as.character(input$twitter_handle)})
    sample_size <- isolate({input$tweet_num})
    
  # pulling tweets ####
    temp_tweets <- reactive({
      pull_tweets(handle, sample_size, lower = .85, upper = 1.15)
    })
  
  # analysing tweets and time ####
    output$tweets_per_day <- renderPlotly({
      tweets_per_day(temp_tweets())
    })
  
    output$tweets_per_hour <- renderPlotly({
      tweets_per_hour(temp_tweets())
    })
    
    output$ratio_vs_time <- renderPlotly({
      ratio_vs_time(temp_tweets())
    })
  
  # variable distribution ####
    output$fav_vs_retweet <- renderPlotly({
      fav_vs_retweet(temp_tweets())
    })
    
    output$metric_histograms <- renderPlotly({
      distributions(temp_tweets())
    })
  # tweet source ####
    output$tweet_source <- renderPlotly({
      tweet_source(temp_tweets())
    })
  # indexed ratio ####
    m <- temp_tweets() %>% 
        tibble::rownames_to_column()
    d <- SharedData$new(m, ~rowname)
    
    output$x2 <- renderPlotly({
      s <- input$x1_rows_selected
      if (!length(s)) {
        p <- d %>%
          plot_ly(x = ~index, y = ~ratio, mode = "markers", color = I('steelblue'), name = 'Unfiltered') %>%
          layout(showlegend = TRUE) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
      } else if (length(s)) {
        pp <- m %>%
          plot_ly() %>% 
          add_trace(x = ~index, y = ~ratio, mode = "markers", color = I('steelblue'), name = 'Unfiltered') %>%
          layout(showlegend = TRUE)
        
        # selected data
        pp <- add_trace(pp, data = m[s, , drop = FALSE], x = ~index, y = ~ratio, mode = "markers",
                        color = I('red'), marker = list(size = 10), name = 'Filtered')
      }
    })
    output$x1 <- DT::renderDataTable({
      m2 <- m[d$selection(),]
      dt <- DT::datatable(m)
      if (NROW(m2) == 0) {
        dt
      } else {
        DT::formatStyle(dt, "rowname", target = "row",
                        color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                        backgroundColor = DT::styleEqual(m2$rowname, rep("blue", length(m2$rowname))))
      }
    })
  # word frequency chart ####
    output$mean_table <- renderPlotly({
      ratio_vs_index2(temp_tweets(), upper = 1.15, lower = .85)
    })
  })
}
    
# initiate server ####
shinyApp(ui = ui,
         server = server)