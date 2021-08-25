# Functions for algorithmic trading ----
# Libraries----
library(tidyverse)
library(lubridate)
library(magrittr)
# library(xml2)
library(rvest)
library(httr)
# Function: get a list of movers----
# This function pulls the movers from the finviz.com home page
getMovers <- function(){
  html <- read_html('https://finviz.com')
  tableNodes <- xml_find_all(html, "//table")
  tdf <- rvest::html_table(tableNodes[[9]], fill = T)
  df <- tdf[3:nrow(tdf), c(1:4,6)]
  names(df) <- tdf[2,c(1:4,6)] %>% tolower()
  df %<>%
    mutate(
      last = gsub('[^0-9.]', '', last) %>% as.numeric(last)
      , change = gsub('%', '', change) %>% as.numeric(.)
      , change = change * .01
      , volume = as.numeric(volume)
    ) %>%
    filter(ticker != 'Ticker')
  rm(tdf)
  return(df)
}
# a <- getMovers()

# Function: get a list of trending tickers----
# This function pulls the movers from yahoo's list of trending stocks
# Returns the table sorted in descending order of percent change
getTrendingYahoo <- function(){
  urlContents <- rvest::read_html('https://finance.yahoo.com/trending-tickers')
  t <- urlContents %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]]
  
  t %<>% .[,1:8] # Keep only the first eight columns. The last ones are charts.
  names(t) <- c('ticker', 'company', 'lastPrice', 'marketTime', 'dollarChange'
                , 'pctChange', 'volume', 'marketCap')
  
  df <- t %>%
    mutate(
      lastPrice = gsub(',', '', lastPrice) %>% as.numeric(.)
      , dollarChange = gsub(',', '', dollarChange) %>% as.numeric(.)
      , pctChange = gsub('%', '', pctChange) %>% as.numeric()
      , volumeMultiplier = case_when(
        grepl('M', ignore.case = T, x = volume) ~ 1000000
        , grepl('B', ignore.case = T, x = volume) ~ 1000000000
        , grepl('T', ignore.case = T, x = volume) ~ 1000000000000
        , T ~ 1
      )
      , volume = gsub('[^0-9\\.]', '', volume) %>% as.numeric()
      , volumeMils = round(volume*volumeMultiplier/1000000,2)
      , marketCapMultiplier = case_when(
        grepl('M', ignore.case = T, x = marketCap) ~ 1000000
        , grepl('B', ignore.case = T, x = marketCap) ~ 1000000000
        , grepl('T', ignore.case = T, x = marketCap) ~ 1000000000000
        , T ~ 1
      )
      , marketCap = gsub('[^0-9\\.]', '', marketCap) %>% as.numeric()
      , marketCapMils = round(marketCap*marketCapMultiplier/1000000,2)
      , type = 'trending'
    ) %>%
    select(-volume, -volumeMultiplier, -marketCap, -marketCapMultiplier) %>%
    arrange(desc(pctChange))
  return(df)
}
# Function: get a list of gaining tickers----
# This function pulls the gainers from yahoo's list of gaining stocks
# Returns the table sorted in descending order of percent change
getGainersYahoo <- function(){
  urlContents <- rvest::read_html('https://finance.yahoo.com/gainers')
  t <- urlContents %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]]
  
  t %<>% .[,1:9] # Keep only the first eight columns. The last ones are charts.
  names(t) <- c('ticker', 'company', 'lastPrice', 'dollarChange', 'pctChange'
                , 'volume', 'avgVolume', 'marketCap', 'peRatio')
  
  df <- t %>%
    mutate(
      lastPrice = gsub(',', '', lastPrice) %>% as.numeric(.)
      , dollarChange = gsub(',', '', dollarChange) %>% as.numeric(.)
      , pctChange = gsub('%|,', '', pctChange) %>% as.numeric()
      , volumeMultiplier = case_when(
        grepl('M', ignore.case = T, x = volume) ~ 1000000
        , grepl('B', ignore.case = T, x = volume) ~ 1000000000
        , grepl('T', ignore.case = T, x = volume) ~ 1000000000000
        , T ~ 1
      )
      , volume = gsub('[^0-9\\.]', '', volume) %>% as.numeric()
      , avgVolume = gsub('[^0-9\\.]', '', avgVolume) %>% as.numeric()
      , volumeMils = round(volume*volumeMultiplier/1000000,2)
      , avgVolumeMils = round(avgVolume*volumeMultiplier/1000000,2)
      , marketCapMultiplier = case_when(
        grepl('M', ignore.case = T, x = marketCap) ~ 1000000
        , grepl('B', ignore.case = T, x = marketCap) ~ 1000000000
        , grepl('T', ignore.case = T, x = marketCap) ~ 1000000000000
        , T ~ 1
      )
      , marketCap = gsub('[^0-9\\.]', '', marketCap) %>% as.numeric()
      , marketCapMils = round(marketCap*marketCapMultiplier/1000000,2)
      , peRatio = gsub(',', '', peRatio) %>% as.numeric()
      , type = 'gainer'
    ) %>%
    select(-volume, -volumeMultiplier, -avgVolume, -marketCap, -marketCapMultiplier) %>%
    arrange(desc(pctChange))
  return(df)
}
# Function: get Current Info for Stocks----
# This function gets some key information, like current price and change, from finviz.
# stocks <- c('SPY', 'GOOGL', 'TSCO', 'MRNA', 'TMUS', 'RCL', 'TRIP')
# stocks <- a %>% filter(signal %in% c('Top Gainers', 'New High')) %>% pull(ticker)
getCurrentInfo <- function(stocks = c('F', 'GOOGL')){
  allStock <- data.frame()
  for (s in stocks) {
    cat('Getting', s, '\n')
    tryCatch({
      # Get data from finviz website
      url <- paste0("http://finviz.com/quote.ashx?t=", s)
      html <- read_html(url)
      tableNodes <- xml_find_all(html, "//table")
      
      # Parse the data and put it into a dataframe
      tdf <- rvest::html_table(tableNodes[[8]], fill = T)
      df <- data.frame()
      for(c in 1:6){
        tdf2 <- tdf[,(c*2-1):(c*2)]
        names(tdf2) <- c('metric', 'value')
        df %<>% bind_rows(tdf2)
      }
      
      # There are 72 metrics. Choose the ones of interest.
      df %<>% 
        mutate(
          metric = gsub('[^a-zA-Z ]', '', metric) %>% gsub(' ', '_', .) %>% tolower()
        ) %>%
        filter(metric %in% c('price', 'beta', 'market_cap', 'current_ratio'
                             , 'rel_volume', 'volume', 'change')) %>%
        mutate(
          value = gsub(',', '', value)
          , valueMultiplier = case_when(
            grepl('B', x = value) ~ 1000
            , grepl('T', x = value) ~ 1000000
            , grepl('%', x = value) ~ .01
            , T ~ 1
          )
          , value = gsub('B|M|%', '', value)
          , value = as.numeric(value)
          , value = value*valueMultiplier
        ) %>%
        select(-valueMultiplier) %>%
        t() %>% 
        as.data.frame()
      names(df) <- df[1,]
      df %<>%
        slice(2) %>%
        mutate(
          across(.cols = market_cap:change, as.numeric)
          , ticker = s
          , time = Sys.time()
        )
      allStock %<>% bind_rows(df)
      rm(tdf, df, html, tableNodes)
      
    }, error = function(e){
      cat('There was a problem with', s, '\n')
    })
  }
  rownames(allStock) <- seq(1:nrow(allStock))
  rm(df, tdf)
  return(allStock)
}
# b <- getCurrentInfo(stocks = stocks)

# Function 2: Get current Info for Stocks----
# This function scrapes stock price data from the Yahoo Finance website
# ticker <- 'GOTU'
getCurrentInfoYahoo <- function(ticker = 'TSLA'){
  url <- paste0('https://finance.yahoo.com/quote/',ticker,'?p=',ticker)
  urlContents <- rvest::read_html(url)
  
  t <- urlContents %>%
    html_elements('#Lead-4-QuoteHeader-Proxy') %>% # The hashtag symbol searches by id. You can use a period to search by class
    html_nodes('span') %>% # 
    html_text()
  # Extract current price info
  changeDetect <- grepl('%)', x = t, fixed = T) # Vector of whether the text contains %). I'm using this as a reference point to get the item before it
  changeDetectLoc <- match(T, changeDetect)
  currentPrice <- t[changeDetectLoc-1] %>% gsub('[^0-9\\.]', '', .) %>%as.numeric()
  changes <- t[changeDetectLoc]
  pctChange <- gsub('^.*\\(', '', changes) %>% gsub('%.*$', '', .) %>% as.numeric()
  dollarChange = gsub(' .*$', '', changes) %>% as.numeric()
  # # Extract after hours price info
  # if(T %in% grepl('After hours', ignore.case = T, x = t)){
  #   afterHoursPrice <- t[changeDetectLoc+2] %>% as.numeric()
  #   changesAfterHours <- t[changeDetectLoc+3]
  #   pctChangeAh <- gsub('^.*\\(', '', changesAfterHours) %>% gsub('%.*$', '', .) %>% as.numeric()
  #   dollarChangeAh <- gsub(' .*$', '', changesAfterHours) %>% as.numeric()
  # }else{
  #   afterHoursPrice <- NA
  #   pctChangeAh <- NA
  #   dollarChangeAh <- NA
  # }
  # Put everything together into a dataframe
  df <- data.frame(ticker = ticker
                   , time = Sys.time()
                   , currentPrice = currentPrice
                   # , pctChange = pctChange
                   # , dollarChange = dollarChange
                   # , currentPriceAh = afterHoursPrice
                   # , pctChangeAh = pctChangeAh
                   # , dollarChangeAh = dollarChangeAh
                   )
  return(df)
}
# getCurrentInfoYahoo(ticker = 'googl')
# Function 3: Get current Info for Stocks using Finnhub----
# I had to sign up for a finnhub account
# Documentation: finnhub.io/docs/api/quote
# api key: c4e04eiad3ieqvqh3130
# sandbox api key: sandbox_c4e04eiad3ieqvqh313g
# webhook secret: c4e04eiad3ieqvqh3140
getCurrentInfoFinnhub <- function(ticker = 'TSLA', apikey = finnHubApiKey, colsToKeep = c('c', 't')){
  # I had to sign up for a finnhub account
  # Documentation: finnhub.io/docs/api/quote
  # c = current price
  # d = change
  # dp = % change
  # h = high price of the day
  # l = low price of the day
  # o = open price of the day
  # pc = previous close price
  # t = time in my time zone
  ticker <- toupper(ticker)
  url <- paste0('https://finnhub.io/api/v1/quote?symbol=',ticker,'&token=', apikey)
  urlContents <- httr::GET(url)
  df <- rawToChar(urlContents$content) %>% 
    jsonlite::fromJSON() %>%
    as.data.frame() %>%
    mutate(
      t = as.POSIXct(t, origin = '1970-01-01')
      , ticker = ticker
    ) %>%
    select(ticker, t, c, d, dp, h, l, o, pc)
  df <- df[,which(names(df) %in% c('ticker', colsToKeep))]
  return(df)
}
# getCurrentInfoFinnhub(ticker = 'pfe')
# Social Sentiment----
getSentimentFinnhub <- function(ticker = 'TSLA', apikey = finnHubApiKey){
  # This function uses Finnhub
  ticker <- toupper(ticker)
  url <- paste0('https://finnhub.io/api/v1/stock/social-sentiment?symbol=',ticker,'&token=', apikey)
  urlContents <- httr::GET(url)
  df <- rawToChar(urlContents$content) %>% 
    jsonlite::fromJSON() %>%
    .$reddit %>%
    as.data.frame() %>%
    mutate(
      ticker = ticker
    )
  return(df)
}
# getSentimentFinnhub(ticker = 'XM')
# Earnings Calendar----
earningsCalendar <- function(startDate = as.character(Sys.Date()), endDate = as.character(Sys.Date()+5), apikey = finnHubApiKey){
  # This function uses Finnhub
  url <- paste0('https://finnhub.io/api/v1/calendar/earnings?from=',startDate,'&to=',endDate,'&token=', apikey)
  urlContents <- httr::GET(url)
  df <- rawToChar(urlContents$content) %>% 
    jsonlite::fromJSON() %>%
    as.data.frame()
  names(df) <- gsub('^.*\\.', '', names(df))
  df %<>% rename(ticker = symbol)
  return(df)
}
# earningsCalendar()
# Earnings Surpises----
epsSurprise <- function(ticker = 'MRNA', apikey = finnHubApiKey){
  # This function uses Finnhub
  url <- paste0('https://finnhub.io/api/v1/stock/earnings?symbol=',ticker,'&token=', apikey)
  urlContents <- httr::GET(url)
  urlContents$content
  df <- rawToChar(urlContents$content) %>% 
    jsonlite::fromJSON() %>%
    as.data.frame() %>%
    rename(ticker = symbol)
  return(df)
}
# Moving Average Calculations----
library(zoo)
movingAverage <- function(allDaily, n = 2, w = 5, priceCol = 'Adjusted', dateCol = 'time'){
  # allDaily contains the stock data from yahoo with Adjusted Closing price. It should already be sorted correctly.
  # n is the number of days for the narrow window and should be less than w
  # w is the number of days for the wide window and should be greater than n
  # priceCol is the name of the column that contains prices
  # returns the same allDaily dataframe with the moving average calculations
  colIndex <- match(priceCol, names(allDaily))
  originalName <- names(allDaily)[colIndex]
  names(allDaily)[colIndex] <- 'Adjusted'
  
  colIndexTime <- match(dateCol, names(allDaily))
  originalNameTime <- names(allDaily[colIndexTime])
  names(allDaily)[colIndexTime] <- 'timestamp'
  allDaily %<>%
    dplyr::arrange(ticker, timestamp) %>%
    group_by(ticker) %>%
    mutate(
      sma_narrow = rollmean(Adjusted, k = n, align = 'right', fill = NA)
      , sma_wide = rollmean(Adjusted, k = w, align = 'right', fill = NA)
      , rollingCv_20 = rollapply(Adjusted, width = 20, FUN = sd, na.rm = T, partial = T)/Adjusted
      , ad_smaNarrow = (Adjusted - sma_narrow)/Adjusted
    ) %>%
    ungroup() %>%
    filter(!is.na(sma_wide)) %>%
    mutate(
      buySell = ifelse(sma_narrow > sma_wide, 'buy', 'sell')
      , pctChangeTime = case_when(
        buySell != dplyr::lag(buySell) ~ timestamp
        , is.na(dplyr::lag(buySell)) ~ timestamp
      )
      , pctChangeTime = na.locf(pctChangeTime, na.rm = T)
    ) %>%
    group_by(ticker, pctChangeTime) %>%
    mutate(
      pctChangePrice = ifelse(timestamp == min(timestamp, na.rm = T), Adjusted, NA)
      , pctChangePrice = na.locf(pctChangePrice, na.rm = T)
      , pctChange = (Adjusted - pctChangePrice)/pctChangePrice
      , stage = 1
      , stage = cumsum(stage)
      , pctChangeStage = pctChange/stage
    ) %>%
    ungroup() %>%
    select(-pctChangeTime, -pctChangePrice) %>%
    filter(!is.na(buySell))
  names(allDaily)[colIndex] <- originalName
  names(allDaily)[colIndexTime] <- originalNameTime
  return(allDaily)
}

# Slope calculations----
slopeCalcs <- function(df, nobs = 30, pval = .05, priceCol = 'currentPrice'){
  # Uses the last nobs periods to estimate the slope of the most recent observation
  # Allows for third order effects
  # Scales by the price in the first observation
  # df is a dataframe with ticker and currentPrice for one ticker
  # nobs is the maximum number of observations to use for calculating the slope
  # pval is the significance level to include a coefficient
  # Returns the estimated slope for the last observation
  
  colIndexPrice <- match(priceCol, names(df))
  originalNamePrice <- names(df)[colIndexPrice]
  names(df)[colIndexPrice] <- 'currentPrice'
  
  tryCatch({
    # Get most recent rows of data
    if(nrow(df) < nobs){
      startRowId <- 1
    }else{
      startRowId <- nrow(df)-nobs+1
    }
    # Filter data and create model with third order
    df <- df[startRowId:nrow(df),] %>%
      mutate(
        row = row_number()
        , firstPrice = ifelse(row == 1, currentPrice, NA)
        , firstPrice = na.locf(firstPrice)
        , currentPrice = currentPrice / firstPrice
      )
    lm1 <- lm(currentPrice ~ row + I(row^2) + I(row^3), data = df)
    # summary(lm1)
    # Extract significant coefficients
    coeffs <- summary(lm1)$coefficients %>% as.data.frame()
    names(coeffs) <- c('estimate', 'se', 't', 'p')
    coeffs %<>%
      mutate(
        estimate = ifelse(p <= pval, estimate, 0)
      )
    # Combine into a formula for calculating the derivative
    lm1Formula <- paste0('~', coeffs$estimate[1], '+'
                         , coeffs$estimate[2], '*x+'
                         , coeffs$estimate[3], '*x^2+'
                         , coeffs$estimate[4], '*x^3') %>%
      gsub('+-', '-', ., fixed = T) %>%
      gsub('++', '+', ., fixed = T)
    
    # Calculate the derivative
    dx <- deriv(formula(lm1Formula), 'x')
    # Evaluate the slope for the last observation
    x <- nrow(df)
    slopeForLastObs <- eval(dx)
    slopeForLastObs <- attributes(slopeForLastObs)[[1]][1]
  }, error = function(e){
    cat('Problem calculating slope for', unique(df$ticker))
    slopeForLastObs <<- NA
  })
  return(slopeForLastObs[[1]])
}
# Return----
returnPct <- function(prices, n = 30, pct = T){
  # prices is a vector of prices
  # n is the length of the return
  # pct is a logical value indicating whether you want the percentage return. If false, it will return the dollar change in price.
  startingPrice <- prices[length(prices)-n+1]
  mostRecentPrice <- prices[length(prices)]
  rtn <- mostRecentPrice - startingPrice
  if(pct == T){
    rtn <- rtn/startingPrice
  }
  return(rtn)
}
# Signals----

## Backtest Trading Strategy 1 ----
# Buy when the narrow window simple moving average is greater than the wide window sma
# , and sells at the closing price when the narrow window sma is less than the wide window sma
# at the closing adjusted price
tradeStratReturn1 <- function(buySell){
  # Calculates the difference in return between the buy and hold and this strategy for each stock.
  # buySell is a dataframe that includes: ticker, date, Adjusted, n (number of days for narrow simple moving average), w (number of days for wide sma)
  # and a buySell column indicating whether the stock should be bought or sold
  # n is the number of days for the narrow window simple moving average 
  # Returns: a dataframe that includes ticker, n, w, bhTotalReturn (buy and hold total return as a percentage of the earliest adjusted price)
  # , sTotalReturn (trading strategy total return as a percentage of the earliest adjusted price)
  # , and stratVsBh (sTotalReturn - bhTotalReturn)
  buySell %>%
    group_by(ticker) %>%
    mutate(
      # Buy and hold return benchmark
      pricePaid = ifelse(date == min(date), Adjusted, NA)) %>%
    ungroup() %>%
    mutate(
      pricePaid = na.locf(pricePaid)
    ) %>%
    group_by(ticker) %>%
    mutate(
      bhTotalReturn = (Adjusted - pricePaid)
      # Return based on this strategy
      , rowNumber = 1
      , rowNumber = cumsum(rowNumber)
      , pricePaids = case_when(
        rowNumber == 1 ~ Adjusted
        , buySell == 'buy' & dplyr::lag(buySell) == 'sell' ~ dplyr::lag(Adjusted)
        , buySell == 'sell' ~ 0
      )
      , pricePaids = na.locf(pricePaids)
      , pricePaids = case_when(
        buySell == 'buy' & dplyr::lead(buySell) == 'sell' ~ pricePaids
        , buySell == 'buy' & rowNumber == max(rowNumber) ~ pricePaids
      )
      , sTotalReturn = Adjusted - pricePaids
      , sTotalReturn = ifelse(is.na(sTotalReturn), 0, sTotalReturn)
      , sTotalReturn = cumsum(sTotalReturn)
    ) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(
      bhTotalReturn = round(bhTotalReturn/pricePaid, 3)
      , sTotalReturn = round(sTotalReturn/pricePaid, 3)
      , stratVsBh = sTotalReturn - bhTotalReturn
    ) %>%
    select(ticker, n, w, bhTotalReturn, sTotalReturn, stratVsBh)
}
# ## Backtest Trading Strategy 2 ----
# # Buy when the narrow window simple moving average is greater than the wide window sma
# # , and sells when the price gets below 98% (or some other percentage) of the maximum price during a buy period
# tradeStratReturn2 <- function(buySell, pct = .98){
#   # Calculates the difference in return between the buy and hold and this strategy for each stock.
#   # buySell is a dataframe that includes: ticker, date, Adjusted, n (number of days for narrow simple moving average), w (number of days for wide sma)
#   # and a buySell column indicating whether the stock should be bought or sold
#   # n is the number of days for the narrow window simple moving average 
#   # Returns: a dataframe that includes ticker, n, w, bhTotalReturn (buy and hold total return as a percentage of the earliest adjusted price)
#   # , sTotalReturn (trading strategy total return as a percentage of the earliest adjusted price)
#   # , and stratVsBh (sTotalReturn - bhTotalReturn)
#   buySell %>%
#     group_by(ticker) %>%
#     mutate(
#       # Buy and hold return benchmark
#       pricePaid = ifelse(date == min(date), Adjusted, NA)) %>%
#     ungroup() %>%
#     mutate(
#       pricePaid = na.locf(pricePaid)
#     ) %>%
#     group_by(ticker) %>%
#     mutate(
#       bhTotalReturn = (Adjusted - pricePaid)
#       # Return based on this strategy
#       , rowNumber = 1
#       , rowNumber = cumsum(rowNumber)
#       , pricePaids = case_when(
#         rowNumber == 1 ~ Adjusted
#         , buySell == 'buy' & dplyr::lag(buySell) == 'sell' ~ dplyr::lag(Adjusted)
#       )
#       , stopPrice = case_when(
#         buySell == 'buy' & dplyr::lag(Adjusted) > Adjusted ~ dplyr::lag(Adjusted)*pct
#         , buySell == 'buy' ~ Adjusted*pct
#         , buySell == 'buy' & 
#       )
#       , sTotalReturn = case_when(
#         buySell == 'buy' & Adjusted < stopPrice ~ stopPrice - pricePaids
#       )
#       , pricePaids = na.locf(pricePaids)
#       , pricePaids = case_when(
#         buySell == 'buy' & dplyr::lead(buySell) == 'sell' ~ pricePaids
#         , buySell == 'buy' & rowNumber == max(rowNumber) ~ pricePaids
#       )
#       , sTotalReturn = Adjusted - pricePaids
#       , sTotalReturn = ifelse(is.na(sTotalReturn), 0, sTotalReturn)
#       , sTotalReturn = cumsum(sTotalReturn)
#     ) %>%
#     filter(date == max(date)) %>%
#     ungroup() %>%
#     mutate(
#       bhTotalReturn = round(bhTotalReturn/pricePaid, 3)
#       , sTotalReturn = round(sTotalReturn/pricePaid, 3)
#       , stratVsBh = sTotalReturn - bhTotalReturn
#     ) %>%
#     select(ticker, n, w, bhTotalReturn, sTotalReturn, stratVsBh)
# }
