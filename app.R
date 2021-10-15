############ SO THAT I CAN VISUALIZE THE STOCKS THAT I GET NOTIFIED ABOUT #########
library(shiny)
library(shinyalert) # To create pop up messages
library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(quantmod)
source('algoFuncs.R')
id <- readRDS('pricesIntraday.rds') %>%
    rename(time = t, currentPrice = c) %>%
    mutate(
        date = floor_date(time, unit = 'day')
    ) %>%
    unique()
tickers <- unique(id$ticker) %>% sort()
datesToUse <- unique(floor_date(id$time, unit = 'day')) %>% sort()

dailyTickers <- list.files('./firmSpecificData') %>%
    gsub('\\.rds', '', .)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyalert() # Set up shiny alert
    # Application title----
    , titlePanel("Stock analysis")
    
    # Sidebar----
    , sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition = "input.tabSelected=='Intraday'"
                             , selectInput('tickerSelectId', label = 'Ticker'
                                           , choices = tickers)
                             , dateInput('dateSelectId', label = 'Date'
                                         , value = datesToUse[length(datesToUse)]
                                         , min = min(datesToUse) %>% as.character())
                             , numericInput('divergencePeriodsId', label = 'Divergence Periods'
                                            , value = 30
                                            , min = 5
                                            , max = 60)
                             , actionButton('plotButtonId', label = 'Plot')
                             )
            , conditionalPanel(condition = "input.tabSelected=='Daily'"
                             , selectInput('tickerSelectD', label = 'Ticker'
                                           , choices = dailyTickers)
                             , dateRangeInput('dateSelectD', label = 'Date Range'
                                              , start = floor_date(Sys.Date() - 365, unit = 'year')
                                              )
                             , numericInput('divergencePeriodsInputD', label = 'Divergence Periods'
                                            , value = 30
                                            , min = 5
                                            , max = 60)
                             , actionButton('plotButtonD', label = 'Plot')
                             , textInput('newTicker', 'New Ticker')
                             , actionButton('newDataButton', 'Get Daily Data for New Ticker')
            )
        ),
        
        
        # Show a plot of the generated distribution----
        mainPanel(
            tabsetPanel(id = 'tabSelected'
                , tabPanel('Intraday'
                         , plotlyOutput("timePlot")
                         , plotlyOutput("priceActionPlot")
                         , plotlyOutput("indicatorsPlot")
                         , DT::dataTableOutput('tempData')
                         # , verbatimTextOutput('checkText'))
                )
                , tabPanel('Daily'
                           , plotlyOutput("dailyPlot")
                           , plotlyOutput("priceActionPlotDaily")
                           , plotlyOutput("indicatorsPlotDaily"))
            )
        )
    )
)

# Define server logic required to create charts and tables----
server <- function(input, output) {
    # output$checkText <- renderText({input$dateSelectId[1]})
    # observeEvent(input$tickerSelectId,{
    #     daties <- id %>% filter(ticker == isolate(input$tickerSelectId)) %>%
    #         pull(date) %>%
    #         unique() %>%
    #         sort()
    #     minDate <- min(daties, na.rm = T) %>% as.character()
    #     maxDate <- max(daties, na.rm = T) %>% as.character()
    #     updateDateInput(session, "dateSelectId"
    #                     , value = maxDate
    #                     , min = minDate)
    # })
    # 
    # observeEvent(input$dateSelectId,{
    #     updatedTickerChoices <- id %>% 
    #         filter(date == input$dateSelectId) %>%
    #         pull(ticker) %>%
    #         unique() %>%
    #         sort()
    #         
    #     updateSelectInput(session, 'tickerSelectId'
    #                       , choices = updatedTickerChoices)
    # })
                   
    # Intraday calculations----
    observeEvent(input$plotButtonId, {
        tdf <<- id %>% 
            filter(ticker == isolate(input$tickerSelectId)) %>%
            filter(date == isolate(input$dateSelectId))

        output$tempData <- renderDataTable({DT::datatable(tdf)})
        indicators <- divergingStrategy(tdf, periods = isolate(input$divergencePeriodsId))
        tdf %<>% 
            left_join(indicators[,c('ticker', 'time', 'divergence', 'rsi')], by = c('ticker', 'time')) %>%
            mutate(
                Action = case_when(
                    grepl('buy', x = divergence) ~ 'Buy'
                    , grepl('sell', x = divergence) ~ 'Sell'
                    , T ~ 'Hold'
                )
                , Action = factor(Action, levels = c('Buy', 'Sell', 'Hold'))
            ) %>%
            rename(Price = currentPrice, RSI = rsi)
        markerText <- tdf %>%
            filter(Action != 'Hold') %>%
            select(time, Price, Action, divergence)
        markerSymbols <- c('circle', 'x', '0')
        markerSymbols %<>% setNames(c('Buy', 'Sell', 'Hold'))
        markerColors <- c('green', 'red', 'blue')
        markerColors %<>% setNames(c('Buy', 'Sell', 'Hold'))

        output$timePlot <- renderPlotly({
            plot_ly(tdf, x = ~time, y = ~Price
                    , type = 'scatter', mode = 'lines'
                    , colors = markerColors, symbols = markerSymbols
                    , hoverinfo = 'text'
                    , text = ~paste0('</br> Time: ', time
                                     , '</br> Price: ', Price)) %>%
                add_trace(data = markerText, x = ~time, y = ~Price
                          , type = 'scatter', mode = 'markers'
                          , color = ~Action, symbol = ~Action
                          , hoverinfo = 'text'
                          , text = ~paste0('</br> Time: ', time, '</br> Price: ', Price, '</br> Divergence: ', divergence)
                ) %>%
                layout(title = paste0('Minutely Closing Prices for ', isolate(input$tickerSelectId))
                       , showlegend = F
                       , xaxis = list(range = list(min(tdf$time), max(tdf$time))
                                      , title = '')
                )
        })
        output$priceActionPlot <- renderPlotly({
            plot_ly(indicators, x = ~time, y = ~pLessMin
                    , type = 'scatter', mode = 'lines'
            ) %>%
                layout(title = paste0('Current Price Minus the Low Price During the Last '
                                      , isolate(input$divergencePeriodsInputD), ' Periods')
                       , xaxis = list(range = list(min(tdf$time), max(tdf$time))
                                      , title = '')
                       , yaxis = list(title = 'Difference in Price')
                )
        })
        output$indicatorsPlot <- renderPlotly({
            plot_ly(indicators, x = ~time, y = ~rsiChangeMin
                    , type = 'scatter', mode = 'lines'
            ) %>%
                layout(title = paste0('Current RSI Minus the RSI When Price was the Lowest')
                       , xaxis = list(range = list(min(tdf$time), max(tdf$time))
                                      , title = '')
                       , yaxis = list(title = 'Difference in RSI')
                )
        })
        
    })
    # Daily calculations----
    observeEvent(input$newDataButton, {
        t <- isolate(input$newTicker)
        if(!t %in% dailyTickers){
            tryCatch({
                tdaily <- getSymbols(t, auto.assign = F) %>%
                    as.data.frame() %>%
                    rownames_to_column() %>%
                    rename(date = rowname) %>%
                    mutate(
                        date = ymd(date)
                        , ticker = tolower(t)
                    )
                names(tdaily) <- gsub('^.*\\.', '', names(tdaily)) %>% tolower()
                write_rds(tdaily, paste0('./firmSpecificData/',t, '.rds'), compress = 'gz')
                dailyTickers <<- list.files('./firmSpecificData') %>%
                    gsub('\\.rds', '', .)
                updateSelectInput(inputId = 'tickerSelectD'
                                  , choices = dailyTickers)
                shinyalert('Yay!', paste0('The daily data for ', t, ' has been added.')
                           , type = 'success')
            }, error = function(e){
                cat('Problem getting data for', t)
                shinyalert('Oops!'
                           , paste0('Something went wrong trying to get the data for ', t)
                           , type = 'error')
            })
        }else{
            shinyalert('Unneccessary'
                       , paste0('We already have the data for ', t)
                       , type = 'info')
        }
    })
    
    observeEvent(input$plotButtonD,{
        

        ddf <<- readRDS(paste0('./firmSpecificData/',input$tickerSelectD, '.rds')) %>%
            filter(date >= isolate(input$dateSelectD[1]) & date <= isolate(input$dateSelectD[2]))
        indicatorsDaily <- divergingStrategy(ddf %>% rename(currentPrice = adjusted, time = date))
        ddf %<>% left_join(indicatorsDaily[,c('ticker', 'time', 'divergence', 'rsi')]
                           , by = c('ticker' = 'ticker', 'date' = 'time')) %>%
            mutate(
                Action = case_when(
                    grepl('buy', x = divergence) ~ 'Buy'
                    , grepl('sell', x = divergence) ~ 'Sell'
                    , T ~ 'Hold'
                    )
                , Action = factor(Action, levels = c('Buy', 'Sell', 'Hold'))
            ) %>%
            rename(Price = adjusted, Date = date, RSI = rsi)
        markerText <- ddf %>%
            filter(Action != 'Hold') %>%
            select(Date, Price, Action, divergence)
        markerSymbols <- c('circle', 'x', '0')
        markerSymbols %<>% setNames(c('Buy', 'Sell', 'Hold'))
        markerColors <- c('green', 'red', 'blue')
        markerColors %<>% setNames(c('Buy', 'Sell', 'Hold'))
        
        output$dailyPlot <- renderPlotly({
            plot_ly(ddf, x = ~Date, y = ~Price
                    , type = 'scatter', mode = 'lines'
                    , colors = markerColors, symbols = markerSymbols
                    , hoverinfo = 'text'
                    , text = ~paste0('</br> Date: ', Date
                                     , '</br> Price: ', Price)) %>%
                add_trace(data = markerText, x = ~Date, y = ~Price
                          , type = 'scatter', mode = 'markers'
                          , color = ~Action, symbol = ~Action
                          , hoverinfo = 'text'
                          , text = ~paste0('</br> Date: ', Date, '</br> Price: ', Price, '</br> Divergence: ', divergence)
                          ) %>%
                layout(title = paste0('Daily Adjusted Closing Prices for ', isolate(input$tickerSelectD))
                       , showlegend = F
                       , xaxis = list(range = list(min(ddf$Date), max(ddf$Date))
                                      , title = '')
                       )
        })
        
        
        output$priceActionPlotDaily <- renderPlotly({
            plot_ly(indicatorsDaily, x = ~time, y = ~pLessMin
                    , type = 'scatter', mode = 'lines'
                    ) %>%
                layout(title = paste0('Current Price Minus the Low Price During the Last '
                                      , isolate(input$divergencePeriodsInputD), ' Periods')
                       , xaxis = list(range = list(min(ddf$Date), max(ddf$Date))
                                    , title = '')
                       , yaxis = list(title = 'Difference in Price')
                       )
        })
        
        output$indicatorsPlotDaily <- renderPlotly({
            plot_ly(indicatorsDaily, x = ~time, y = ~rsiChangeMin
                    , type = 'scatter', mode = 'lines'
            ) %>%
                layout(title = paste0('Current RSI Minus the RSI When Price was the Lowest')
                       , xaxis = list(range = list(min(ddf$Date), max(ddf$Date))
                                      , title = '')
                       , yaxis = list(title = 'Difference in RSI')
                )
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
