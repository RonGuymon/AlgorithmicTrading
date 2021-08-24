library(tidyverse)
library(lubridate)
library(magrittr)
library(quantmod)
library(zoo)
library(googlesheets4)
library(here) # Sets the working directory regardless of the machine I'm on
source('algoFuncs.R')
# Alerts----
# Buy based on simple moving average crossing
# Sell based on simple moving average crossing (stop quote)
# Set new stop quote because of steep increase to lock in gains
# Buy based on short-term momentum

# Read in data from Google Sheet----
## Stock data from google sheet----
# https://cran.r-project.org/web/packages/googlesheets/README.html

# gToken <- gs4_auth() # Run this the first time to get the oAuth information
# saveRDS(gToken, "gToken.rds") # Save the oAuth information for non-interactive use
# suppressWarnings(readRDS("gToken.rds")) # Needs an interactive environment
suppressWarnings(gs4_auth(email = Sys.getenv('googleSheetsEmail')
                          , token = "gToken.rds")) # Auto refreshes stale oAuth token
# allSheets <- gs4_find(gToken) # Read in the metadata for all files in the account. Only need to run the first time to get the sheet key.
# System environment variable: stockSheetsKey
suppressMessages(stocks <- read_sheet(Sys.getenv('stockSheetsKey'), sheet = 'stocks'
                                      , col_types = 'cddd'))
stocks %<>%
  mutate(
    type = ifelse(is.na(qty), 'watch', 'own')
  )
suppressMessages(alerts <- read_sheet(Sys.getenv('stockSheetsKey'), sheet = 'alerts'
                                      , col_types = 'cc'))
## Intraday data----
id <- readRDS('pricesIntraday.rds') %>%
  mutate(
    ticker = tolower(ticker)
  ) %>%
  rename(time = t, currentPrice = c)

### Calculate gainers----
# Calculate moving averages and estimate current slope
idm <- id  %>%
  arrange(ticker, time) %>%
  # filter(time >= ymd_hms('2021-07-30 11:00:00')) %>%
  select(ticker, time, currentPrice)
idm <- movingAverage(allDaily = idm, n = 3, w = 6, priceCol = 'currentPrice')

gainers <- data.frame()
for(t in unique(idm$ticker)){
  cat(t, '\r')
  tdf <- idm %>% filter(ticker == !! t)
  slopeEstimate <- slopeCalcs(tdf, nobs = 60)
  tg <- data.frame(ticker = t
                   , time = max(tdf$time)
                   , currentPrice = tdf[nrow(tdf),] %>% pull(currentPrice)
                   , buySell = tdf$buySell[nrow(tdf)]
                   , slopeEstimate = slopeEstimate
                   , rollingCv_20 = tdf$rollingCv_20[nrow(tdf)]
                   , ad_smaNarrow = tdf$ad_smaNarrow[nrow(tdf)]
                   , pctChange = tdf$pctChange[nrow(tdf)]
                   , stage = tdf$stage[nrow(tdf)]
  )
  gainers %<>% bind_rows(tg)
  rm(tdf, slopeEstimate, tg)
}

gainers %<>% arrange(desc(slopeEstimate)) %>%
  mutate(
    time = Sys.time()
  ) %>%
  left_join(stocks[,c('ticker', 'type', 'stop')], by = 'ticker')


## Put the gainers data on the google Sheet----
write_sheet(gainers, ss = Sys.getenv('stockSheetsKey'), sheet = 'gainers')

# ## Visualize the data for one stock----
# tickerToShow <- 'body'
# pd <- id %>%
#   movingAverage(n = 3, w = 6, priceCol = 'currentPrice') %>%
#   filter(ticker == tickerToShow) %>%
#   filter(time >= Sys.Date()) %>%
#   pivot_longer(cols = c(currentPrice, sma_narrow, sma_wide), names_to = 'type', values_to = 'price')
# ggplot(pd, aes(x = time, y = price, color = type)) +
#   geom_line() +
#   labs(title = tickerToShow)




### Create alert messages for intraday trades----
alertMessages <- data.frame()
#### Surging----
# If stock price on gainer list with positive slope
# is increasing by 3% or more 
# and has been a buy for at least 5 periods
surging <- gainers %>%
  filter(slopeEstimate >= .01 & pctChange > .01 & buySell == 'buy' & stage >= 3)

if(nrow(surging) > 0 & alerts %>% filter(alertType == 'surging') %>% pull(yesOrNo) == 'yes'){
  surging %<>%
    mutate(
      msg = paste('[', ticker, round(slopeEstimate, 3), round(pctChange, 2), stage, ']', sep = ' ') %>% 
        gsub('[ ', '[', ., fixed = T) %>% gsub(' ]', ']', ., fixed = T)
    )
  msg <- paste(surging$msg, collapse =', ')
  tempMsg <- data.frame(msg = paste('Surging stocks (ticker, slope, pctChange, stage): ', msg))
  alertMessages %<>% bind_rows(tempMsg)
  rm(msg, tempMsg)
}
rm(surging)
#### Increase stop quote----
# If price rises to more than 10% of stop quote then increase the stop quote to 5% less than the current price---
increaseStopQuote <- gainers %>%
  filter(!is.na(stop)) %>%
  mutate(
    pctAboveStop = (currentPrice - stop) / stop
    , pctAboveStop = round(pctAboveStop, 3)
  ) %>%
  filter(pctAboveStop > .1)
if(nrow(increaseStopQuote) > 0 & alerts %>% filter(alertType == 'increase stop') %>% pull(yesOrNo) == 'yes'){
  increaseStopQuote %<>%
    mutate(
      msg = paste0(ticker, ' to ', round(currentPrice*.95, 2))
    )
  msg <- paste(increaseStopQuote$msg, collapse = ', ')
  tempMsg <- data.frame(msg = paste0('Increase stop price of ', msg))
  alertMessages %<>% bind_rows(tempMsg)
  rm(msg, tempMsg)
}
rm(increaseStopQuote)

# Daily data----
if(hour(Sys.time()) < 8){
  # This only needs to be run once a day, or until the latest data is added
  suppressMessages(gd <- read_sheet(Sys.getenv('stockSheetsKey'), sheet = 'gainersDaily'
                                    , col_types = 'cDdcddddddd'))
  spyCheck <- getSymbols('spy', auto.assign = F) %>% 
    as.data.frame() %>%
    rownames_to_column() %>%
    rename(date = rowname) %>%
    mutate(
      date = ymd(date)
      , ticker = 'spy'
    )
  if(max(spyCheck$date, na.rm = T) != max(gd$date, na.rm = T)){
    daily <- data.frame()
    stocksToGet <- unique(c(stocks$ticker, 'spy', 'gc=f')) # gc=f is gold
    for(t in stocksToGet){
      cat(t, '\n')
      tdaily <- getSymbols(t, auto.assign = F) %>% 
        as.data.frame() %>%
        rownames_to_column() %>%
        rename(date = rowname) %>%
        mutate(
          date = ymd(date)
          , ticker = t
        )
      names(tdaily) <- gsub('^.*\\.', '', names(tdaily)) %>% tolower()
      daily %<>% bind_rows(tdaily)
      rm(tdaily)
    }
    
    # Calculate moving averages and estimate current slope
    dailym <- daily  %>%
      drop_na() %>%
      arrange(ticker, date) %>%
      select(ticker, date, adjusted)
    dailym <- movingAverage(allDaily = dailym, n = 10, w = 20
                            , priceCol = 'adjusted', dateCol = 'date')
    
    gainersDaily <- data.frame()
    for(t in unique(dailym$ticker)){
      cat(t, '\r')
      tdf <- dailym %>% filter(ticker == !! t)
      slopeEstimate <- slopeCalcs(df = tdf, nobs = 60, priceCol = 'adjusted')
      
      # 60-day beta calc
      if(t == 'spy'){
        beta <- 1
      }else{
        bdf <- dailym %>% 
          filter(ticker %in% c(t, 'spy')) %>%
          filter(date >= Sys.Date()-60) %>%
          select(ticker, date, adjusted) %>%
          group_by(ticker) %>%
          mutate(
            return = (adjusted - dplyr::lag(adjusted))/dplyr::lag(adjusted)
          ) %>%
          ungroup() %>%
          select(-adjusted) %>%
          pivot_wider(names_from = 'ticker', values_from = 'return') %>%
          drop_na() %>%
          select(date, !!t, spy)
        corSpy <- cor(bdf[,2:3])[1,2]
        beta <- corSpy*sd(bdf[,2][[1]])/sd(bdf[,3][[1]])
      }
      tg <- data.frame(ticker = t
                       , date = max(tdf$date)
                       , adjusted = tdf[nrow(tdf),] %>% pull(adjusted)
                       , buySell = tdf$buySell[nrow(tdf)]
                       , slopeEstimate = slopeEstimate
                       , rollingCv_20 = tdf$rollingCv_20[nrow(tdf)]
                       , beta = beta
                       , ad_smaNarrow = tdf$ad_smaNarrow[nrow(tdf)]
                       , pctChange = tdf$pctChange[nrow(tdf)]
                       , stage = tdf$stage[nrow(tdf)]
      )
      gainersDaily %<>% bind_rows(tg)
      rm(tdf, slopeEstimate, tg, bdf, corSpy, beta)
    }
    gainersDaily %<>% 
      mutate(
        stageForWeight = ifelse(buySell == 'sell', -1*stage, stage)
        , weightedScore = (slopeEstimate + ad_smaNarrow + pctChange + 1/stageForWeight)/4
      ) %>%
      mutate(
        across(.cols = c(slopeEstimate:pctChange, weightedScore), .fns = round, 3)
      ) %>%
      arrange(desc(weightedScore)) %>%
      select(-stageForWeight)
    
    # ## Visualizations----
    # tickerToGet <- 'tsco'
    # dailym %>%
    #   filter(ticker == tickerToGet) %>%
    #   filter(date >= Sys.Date()-60) %>%
    #   pivot_longer(cols = adjusted:sma_wide, names_to = 'type', values_to = 'price') %>%
    #   ggplot(aes(x = date, y = price, color = type)) + geom_line() +
    #   labs(title = tickerToGet)
    
    ## Put the gainers data on the google Sheet----
    write_sheet(gainersDaily, ss = Sys.getenv('stockSheetsKey'), sheet = 'gainersDaily')
    
    ### Create alert messages for daily trades----
    #### Sell a stock that I'm holding----
    # If stock price with a stop quote is a sell then recommend the top of the gainers list
    ownSell <- stocks %>%
      left_join(gainersDaily[,c('ticker', 'buySell')], by = 'ticker') %>%
      filter(type == 'own' & buySell == 'sell')
    
    if(nrow(ownSell) > 0 & alerts %>% filter(alertType == 'daily') %>% pull(yesOrNo) == 'yes'){
      msg <- paste(ownSell$ticker, collapse =', ')
      replacements <- paste(gainersDaily$ticker[1:nrow(ownSell)], collapse = ', ')
      tempMsg <- data.frame(msg = paste0('Time to sell these stocks: ', msg
                                         , '. Consider replacing with these stocks: ', replacements))
      alertMessages %<>% bind_rows(tempMsg)
      rm(msg, tempMsg)
    }
  }
}

# Send messages----
if(nrow(alertMessages) > 0){
  library(twilio)
  Sys.getenv('TWILIO_SID')
  Sys.getenv('TWILIO_TOKEN')
  trialNumber <- '+14438154134'
  from_number <- '+14438154134'
  to_number <- '+17703099523'
  for(m in 1:nrow(alertMessages)){
    my_message <- tw_send_message(
      to = to_number
      , from = from_number
      , body = alertMessages$msg[m]
    )
  }
}


