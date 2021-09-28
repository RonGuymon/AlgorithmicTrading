########### UPDTAES THE GOOGLE SHEET AND SENDS ALERTS #########
cat(as.character(Sys.time()))
library(tidyverse)
library(lubridate)
library(magrittr)
library(quantmod)
library(zoo)
library(googlesheets4)
library(here) # Sets the working directory regardless of the machine I'm on
source('algoFuncs.R')
source('getListOfCompanies.R')
source('Tokens.R')
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
  .[!duplicated(.[,c('time', 'ticker')]),] %>%
  arrange(ticker, time) %>%
  filter(time >= Sys.Date())
# Create returns from the beginning of the day
maxPeriods <- idm %>% group_by(ticker) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  pull(n) %>% 
  max(na.rm = T)
maxPeriods <- maxPeriods - 1
rtnWindows <- c(1,3,5,10,15,20,25,30,60,90,120,150,180,210,240)
rtnWindows <- unique(sort(c(maxPeriods, rtnWindows))) %>% .[. <= maxPeriods]
gains <- data.frame()
for(rw in rtnWindows){
  cat(rw, '\n')
  tdf <- idm %>%
    group_by(ticker) %>%
    mutate(
      newReturn = (currentPrice - dplyr::lag(currentPrice, rw))/dplyr::lag(currentPrice,rw)
      , newReturn = round(newReturn,4)
    ) %>%
    filter(time == max(time, na.rm = T)) %>%
    ungroup() %>%
    select(ticker, newReturn) %>%
    unique()
  names(tdf)[ncol(tdf)] <- paste0('return_', rw)
  if(rw == min(rtnWindows)){
    gains <- tdf
  }else{
    gains %<>% left_join(tdf, by = 'ticker')
  }
}


for(ts in unique(idm$ticker)){
  cat(ts, '\n')
  tryCatch({
    tdf <- idm %>% filter(ticker == ts) %>%arrange(time)
    if(nrow(tdf) < 16){
      rsiWindow <- nrow(tdf)-2
    }else{
      rsiWindow <- 14
    }
    rsi <- tdf %>% mutate(
      rsi = TTR::RSI(currentPrice, n = rsiWindow)
    ) %>%
      filter(time == max(time, na.rm = T)) %>%
      pull(rsi)
    gains[which(gains$ticker == ts),'rsi_14'] <- rsi
    openPrice <- tdf %>%
      filter(time == min(time)) %>%
      pull(currentPrice)
    gains[which(gains$ticker == ts),'openPrice'] <- openPrice
  }, error = function(e){
    cat('Problem calculating RSI and/or openPrice for', ts, '\n')
  })
}

gains %<>%
  mutate(
    time = as.character(Sys.time())
  ) %>%
  relocate(time, .after = ticker) %>%
  relocate(openPrice, .after = time) %>%
  relocate(rsi_14, .after = openPrice) %>%
  arrange(desc(rsi_14))


## Put the gainers data on the google Sheet----
write_sheet(gains, ss = Sys.getenv('stockSheetsKey'), sheet = 'gainers')

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
# #### Surging----
# # If stock price on gainer list with positive slope
# # is increasing by 3% or more 
# # and has been a buy for at least 5 periods
# surging <- gains %>%
#   filter(slopeEstimate >= .01 & pctChange > .01 & buySell == 'buy' & stage >= 3)
# 
# if(nrow(surging) > 0 & alerts %>% filter(alertType == 'surging') %>% pull(yesOrNo) == 'yes'){
#   surging %<>%
#     mutate(
#       msg = paste('[', ticker, round(slopeEstimate, 3), round(pctChange, 2), stage, ']', sep = ' ') %>% 
#         gsub('[ ', '[', ., fixed = T) %>% gsub(' ]', ']', ., fixed = T)
#     )
#   msg <- paste(surging$msg, collapse =', ')
#   tempMsg <- data.frame(msg = paste('Surging stocks (ticker, slope, pctChange, stage): ', msg))
#   alertMessages %<>% bind_rows(tempMsg)
#   rm(msg, tempMsg)
# }
# rm(surging)
# #### Increase stop quote----
# # If price rises to more than 10% of stop quote then increase the stop quote to 5% less than the current price---
# increaseStopQuote <- gainers %>%
#   filter(!is.na(stop)) %>%
#   mutate(
#     pctAboveStop = (currentPrice - stop) / stop
#     , pctAboveStop = round(pctAboveStop, 3)
#   ) %>%
#   filter(pctAboveStop > .1)
# if(nrow(increaseStopQuote) > 0 & alerts %>% filter(alertType == 'increase stop') %>% pull(yesOrNo) == 'yes'){
#   increaseStopQuote %<>%
#     mutate(
#       msg = paste0(ticker, ' to ', round(currentPrice*.95, 2))
#     )
#   msg <- paste(increaseStopQuote$msg, collapse = ', ')
#   tempMsg <- data.frame(msg = paste0('Increase stop price of ', msg))
#   alertMessages %<>% bind_rows(tempMsg)
#   rm(msg, tempMsg)
# }
# rm(increaseStopQuote)
# 


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


