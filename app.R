############ SO THAT I CAN VISUALIZE THE STOCKS THAT I GET NOTIFIED ABOUT #########
library(shiny)
library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
source('algoFuncs.R')
id <- readRDS('pricesIntraday.rds') %>%
    rename(time = t, currentPrice = c) %>%
    mutate(
        date = floor_date(time, unit = 'day')
    ) %>%
    unique()
tickers <- unique(id$ticker) %>% sort()
datesToUse <- unique(floor_date(id$time, unit = 'day')) %>% sort()

# Define UI for application that draws a histogram
ui <- fluidPage(
    
  
        # Show a plot of the generated distribution----
        mainPanel(
            tabsetPanel(
                tabPanel('Intraday'
                         , sidebarPanel(
                             selectInput('tickerSelect', label = 'Ticker'
                                         , choices = tickers)
                             , dateRangeInput('dailyDateSelect', label = 'Date Range'
                                              , start = datesToUse[length(datesToUse)]
                                              , end = datesToUse[length(datesToUse)]
                                              # , min = datesToUse[1]
                                              , max = datesToUse[length(datesToUse)])
                             , numericInput('divergencePeriodsInput', label = 'Divergence Periods'
                                            , value = 30
                                            , min = 5
                                            , max = 60)
                             , actionButton('plotButtonIntraday', label = 'Plot')
                         )
                         , plotlyOutput("timePlot")
                         , plotlyOutput("priceActionPlot")
                         , plotlyOutput("indicatorsPlot")
                         # , verbatimTextOutput('checkText'))
                )
                , tabPanel('Daily'
                           , plotlyOutput("dailyPlot")
                           , plotlyOutput("priceActionPlotDaily")
                           , plotlyOutput("indicatorsPlotDaily"))
            )
        )
    
)

# Define server logic required to create charts and tables----
server <- function(input, output) {
    # output$checkText <- renderText({input$dailyDateSelect[1]})
    observeEvent(input$plotButtonIntraday, {
        # Intraday calculations----
        tdf <<- id %>% 
            filter(ticker == isolate(input$tickerSelect)) %>%
            filter(date >= isolate(input$dailyDateSelect[1]) & date <= isolate(input$dailyDateSelect[2]))
        indicators <- divergingStrategy(tdf, periods = isolate(input$divergencePeriodsInput))
        tdf %<>% 
            left_join(indicators[,c('ticker', 'time', 'divergence')], by = c('ticker', 'time')) %>%
            mutate(
                dcolor = case_when(
                    grepl('buy', x = divergence) ~ 'buy'
                    , grepl('sell', x = divergence) ~ 'sell'
                    , T ~ 'hold'
                )
            )
        output$timePlot <- renderPlotly({
            p <- ggplot(ttdf, aes(x = time, y = currentPrice)) +
                geom_point(aes(color = dcolor)) +
                geom_line() +
                labs(title = paste0('Price of ', isolate(input$tickerSelect))
                     , y = 'Price'
                     , x = 'Time')
            ggplotly(p)
        })
        output$priceActionPlot <- renderPlotly({
            p <- ggplot(indicators, aes(x = time, y = pLessMin)) +
                geom_line() +
                labs(title = paste0('pLessMin of ', isolate(input$tickerSelect))
                     , y = 'pLessMin'
                     , x = 'Time')
            ggplotly(p)
        })
        output$indicatorsPlot <- renderPlotly({
            p <- ggplot(indicators, aes(x = time, y = rsiChangeMin)) +
                geom_line() +
                labs(title = paste0('Change of RSI for ', isolate(input$tickerSelect))
                     , y = paste0('Change in RSI (', 20, ')')
                     , x = 'Time')
            ggplotly(p)
        })
        
        
    })
    
    # Daily calculations----
    observeEvent(input$plotButtonDaily, {
        
        ddf <<- readRDS(paste0('./firmSpecificData/',input$tickerSelect, '.rds')) %>%
            filter(ticker == isolate(input$tickerSelect)) %>%
            filter(date >= isolate(input$dateSelect[1]) & date <= isolate(input$dateSelect[2]))
        indicatorsDaily <- divergingStrategy(ddf %>% rename(currentPrice = adjusted, time = date))
        ddf %<>% left_join(indicatorsDaily[,c('ticker', 'time', 'divergence')]
                           , by = c('ticker' = 'ticker', 'date' = 'time')) %>%
            mutate(
                plotColor = case_when(
                    grepl('buy', x = divergence) ~ 'buy'
                    , grepl('sell', x = divergence) ~ 'sell'
                    , T ~ 'hold')
            )
        output$dailyPlot <- renderPlotly({
            p <- ggplot(ddf, aes(x = date, y = adjusted)) +
                geom_point(aes(color = plotColor)) +
                geom_line() +
                labs(title = paste0('Price of ', isolate(input$tickerSelect))
                     , y = 'Closing Price'
                     , x = 'Date')
            ggplotly(p)
        })
        
        output$priceActionPlotDaily <- renderPlotly({
            p <- ggplot(indicatorsDaily, aes(x = time, y = pLessMin)) +
                geom_line() +
                labs(title = paste0('pLessMin of ', isolate(input$tickerSelect))
                     , y = 'pLessMin'
                     , x = 'Date')
            ggplotly(p)
        })
        
        output$indicatorsPlotDaily <- renderPlotly({
            p <- ggplot(indicatorsDaily, aes(x = time, y = rsi)) +
                geom_line() +
                labs(title = paste0('RSI for ', isolate(input$tickerSelect))
                     , y = paste0('RSI (', 20, ')')
                     , x = 'Date')
            ggplotly(p)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
