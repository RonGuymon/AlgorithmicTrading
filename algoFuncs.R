# Functions for algorithmic trading ----
# Libraries----
library(tidyverse)
library(lubridate)
library(magrittr)
# library(xml2)
library(rvest)
library(httr)
library(gridExtra)
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
getSocialSentimentFinnhub <- function(ticker = 'TSLA', apikey = finnHubApiKey){
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
# News Sentiment----
getNewsSentimentFinnhub <- function(ticker = 'TSLA', apikey = finnHubApiKey){
  # This function uses Finnhub
  ticker <- toupper(ticker)
  url <- paste0('https://finnhub.io/api/v1/news-sentiment?symbol=',ticker,'&token=', apikey)
  urlContents <- httr::GET(url)
  df <- rawToChar(urlContents$content) %>% 
    jsonlite::fromJSON() %>% 
    unlist() %>%
    t() %>%
    as.data.frame() %>%
    rename(ticker = symbol)
  return(df)
}
# Company News----
getCompanyNewsFinnhub <- function(ticker = 'TSLA', fromDate = Sys.Date()-364, toDate = Sys.Date(), apikey = finnHubApiKey){
  # This function uses Finnhub
  ticker <- toupper(ticker)
  url <- paste0('https://finnhub.io/api/v1/company-news?symbol=',ticker,'&from=',fromDate,'&to=',toDate,'&token=', apikey)
  urlContents <- httr::GET(url)
  df <- rawToChar(urlContents$content) %>% 
    jsonlite::fromJSON() %>%
    as.data.frame() %>%
    mutate(
      datetime = as.POSIXct(datetime, origin = '1970-01-01')
    )
  return(df)
}

# Market News----
getMarketNewsFinnhub <- function(category = 'general', apikey = finnHubApiKey){
  # This function uses Finnhub
  # Category can be one of four: general, forex, crypto, merger
  url <- paste0('https://finnhub.io/api/v1/news?category=',category,'&token=', apikey)
  urlContents <- httr::GET(url)
  df <- rawToChar(urlContents$content) %>% 
    jsonlite::fromJSON() %>%
    as.data.frame() %>%
    mutate(
      datetime = as.POSIXct(datetime, origin = '1970-01-01')
    )
  return(df)
}

# Peers----
getPeersFinnhub <- function(ticker = 'xm', apikey = finnHubApiKey){
  # This function uses Finnhub
  ticker <- toupper(ticker)
  url <- paste0('https://finnhub.io/api/v1/stock/peers?symbol=',ticker,'&token=', apikey)
  urlContents <- httr::GET(url)
  df <- rawToChar(urlContents$content) %>% 
    jsonlite::fromJSON() %>%
    as.data.frame() %>%
    mutate(
      ticker = ticker
    )
  names(df) <- c('peerTicker', 'ticker')
  df <- df[,c('ticker', 'peerTicker')]
  return(df)
}

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
# Mean Reversion Signals----
mr_sansIndicators <- function(df, closingPriceCol = 'adjusted'
                              , dateCol = 'date', tickerCol = 'ticker'
                              , p = 6, viz = F, returns = F){
  # https://www.youtube.com/watch?v=XVEbnqLk0-Q&t=373s&ab_channel=TheTransparentTrader
  # Buy when the close is the lowest over the period, sell when it is the highest over the period
  # df is a dataframe with at least two columns: closing price and date
  # closingPriceCol is the name of the column that has the closing price
  # dateCol is the name of the column that has the date
  # tickerCol is the name of the column with the ticker symbol
  # viz is a logical value indicating whether you want to return a plot
  # returns is a logical value indicatin whether you want to return the abnormal returns for the period, defined as the return from the strategy vs. buy & hold during the period.
  # Returns the signal for the most recent period, and 
  
  # Location and original name of closing price column
  cpi <- which(names(df) == closingPriceCol)
  cpo <- names(df)[cpi]
  
  # Location and original name of date column
  di <- which(names(df) == dateCol)
  do <- names(df)[di]
  
  # Location and original name of ticker column
  ti <- which(names(df) == tickerCol)
  if(length(ti) == 0){
    ticker <- 'Unspecified Ticker'
    to <- 'No col'
  }else{
    ticker <- unique(df[,ti])[1]
    to <- names(df)[ti]
  }
  
  
  dfa <- data.frame(cp = df[,cpi])
  for(i in 1:p){
    dfa$cp1 <- data.frame(cp = c(rep(NA, i), dfa[1:(nrow(dfa)-i),'cp'])) %>% pull(cp)
    names(dfa)[ncol(dfa)] <- paste0('cp_lag', i)
  }
  dfa$minVal <- apply(dfa,1,FUN = min, na.rm = T)
  dfa$maxVal <- apply(dfa,1,FUN = max, na.rm = T)
  dfa %<>% mutate(
    signal = case_when(
      cp == minVal ~ 'buy'
      , cp == maxVal ~ 'sell'
      , T ~ NA_character_
    )
  )
  dfa$date <- df[,di]
  returnList <- list()
  returnList$signal <- dfa[nrow(dfa), 'signal']
  returnList$currentRegime <- dfa %>% filter(!is.na(signal)) %>% .[nrow(.), 'signal']

  # Calculate abnormal return - no short sales
  if(returns == T){
    buyHoldReturn <- (dfa[nrow(dfa),'cp'] - dfa[1,'cp'])/dfa[1,'cp']
    
    dfa %<>% mutate(
      lastSignal = signal
      , lastSignal = na.locf(lastSignal, na.rm = F)
      , regimeSignal = case_when(
        signal != dplyr::lag(lastSignal) ~ signal
        , signal == lastSignal & is.na(dplyr::lag(signal)) & is.na(dplyr::lag(lastSignal)) ~ signal
        , T ~ NA_character_)
      , regimeDate = ifelse(!is.na(regimeSignal), date, NA)
      , regimePrice = ifelse(!is.na(regimeSignal), cp, NA)
      , regimeSignal = na.locf(regimeSignal, na.rm = F)
      , regimeDate = na.locf(regimeDate, na.rm = F)
      , regimePrice = na.locf(regimePrice, na.rm = F)
    ) %>%
      select(-lastSignal) %>%
      group_by(regimeSignal, regimeDate) %>%
      mutate(
        return = ifelse(date == max(date) & regimeSignal == 'buy',
                        cp - regimePrice,
                        NA)
      ) %>%
      ungroup() %>%
      mutate(
        return = case_when(
          regimeSignal == 'sell' & dplyr::lag(regimeSignal == 'buy') ~ (cp - dplyr::lag(regimePrice))/cp
          , date == max(date, na.rm = T) ~ return)
      )
    basePrice <- dfa %>% filter(regimeSignal == 'buy') %>% .[1,'cp'] %>% .[[1]]
    strategyReturn <- sum(dfa$return, na.rm = T)
    abnormalReturn <- strategyReturn - buyHoldReturn
    returnList$buyHoldReturn <- buyHoldReturn
    returnList$strategyReturn <- strategyReturn
    returnList$abnormalReturn <- abnormalReturn
  }
  
  # Create a plot if viz = T
  if(viz == T){
    if(returns == F){
      subtitle <- ''
    }else{
      subtitle <- paste0('Abnormal return = ', round(abnormalReturn, 2), '(', round(strategyReturn,2), ' - ', round(buyHoldReturn, 2), ')')
    }
    dfl <- dfa %>%
      mutate(
        regimeSignal = ifelse(regimeSignal == dplyr::lag(regimeSignal), NA_character_, regimeSignal)
      ) %>%
      ggplot(aes(x = date, y = cp)) +
      geom_point(aes(color = regimeSignal)) + 
      geom_line() +
      labs(title = paste0('Signal Chart for ', ticker, ' Using sansIndicators Strategy')
           , x = 'Date'
           , y = 'Closing Price'
           , subtitle = subtitle)
    returnList$viz <- dfl
  }
  
  return(returnList)
  
}
mr_sansIndicators1 <- function(df, closingPriceCol = 'adjusted', dateCol = 'date'
                               , tickerCol = 'ticker', p = 6, smaMultiplier = 2
                               , viz = F, returns = F){
  # https://www.youtube.com/watch?v=XVEbnqLk0-Q&t=373s&ab_channel=TheTransparentTrader
  # Buy when the close is the lowest over the period and above the sma, sell when it is the highest over the period
  # df is a dataframe with at least two columns: closing price and date
  # closingPriceCol is the name of the column that has the closing price
  # dateCol is the name of the column that has the date
  # p is the number of periods
  # smaMultiplier is multiplied by p for calculating the sma
  # viz is a logical value indicating whether you want to return a plot
  # returns is a logical value indicatin whether you want to return the abnormal returns for the period, defined as the return from the strategy vs. buy & hold during the period.
  # Returns the signal for the most recent period, and 
  
  # Location and original name of closing price column
  cpi <- which(names(df) == closingPriceCol)
  cpo <- names(df)[cpi]
  
  # Location and original name of date column
  di <- which(names(df) == dateCol)
  do <- names(df)[di]
  
  # Location and original name of ticker column
  ti <- which(names(df) == tickerCol)
  if(length(ti) == 0){
    ticker <- 'Unspecified Ticker'
    to <- 'No col'
  }else{
    ticker <- unique(df[,ti])[1]
    to <- names(df)[ti]
  }
  
  dfa <- data.frame(cp = df[,cpi])
  for(i in 1:p){
    dfa$cp1 <- data.frame(cp = c(rep(NA, i), dfa[1:(nrow(dfa)-i),'cp'])) %>% pull(cp)
    names(dfa)[ncol(dfa)] <- paste0('cp_lag', i)
  }
  dfa$minVal <- apply(dfa,1,FUN = min, na.rm = T)
  dfa$maxVal <- apply(dfa,1,FUN = max, na.rm = T)
  dfa$sma <- TTR::SMA(dfa$cp, n = round(p*smaMultiplier))
  dfa %<>% mutate(
    signal = case_when(
      cp == minVal & cp > sma ~ 'buy'
      , cp == maxVal & cp < sma ~ 'sell'
      , T ~ NA_character_
    )
  )
  dfa$date <- df[,di]
  returnList <- list()
  returnList$signal <- dfa[nrow(dfa), 'signal']
  returnList$currentRegime <- dfa %>% filter(!is.na(signal)) %>% .[nrow(.), 'signal']

  # Calculate abnormal return - no short sales
  if(returns == T){
    buyHoldReturn <- (dfa[nrow(dfa),'cp'] - dfa[1,'cp'])/dfa[1,'cp']
    
    dfa %<>% mutate(
      lastSignal = signal
      , lastSignal = na.locf(lastSignal, na.rm = F)
      , regimeSignal = case_when(
        signal != dplyr::lag(lastSignal) ~ signal
        , signal == lastSignal & is.na(dplyr::lag(signal)) & is.na(dplyr::lag(lastSignal)) ~ signal
        , T ~ NA_character_)
      , regimeDate = ifelse(!is.na(regimeSignal), date, NA)
      , regimePrice = ifelse(!is.na(regimeSignal), cp, NA)
      , regimeSignal = na.locf(regimeSignal, na.rm = F)
      , regimeDate = na.locf(regimeDate, na.rm = F)
      , regimePrice = na.locf(regimePrice, na.rm = F)
    ) %>%
      select(-lastSignal) %>%
      group_by(regimeSignal, regimeDate) %>%
      mutate(
        return = ifelse(date == max(date) & regimeSignal == 'buy',
                        (cp - regimePrice)/cp,
                        NA)
      ) %>%
      ungroup() %>%
      mutate(
        return = case_when(
          regimeSignal == 'sell' & dplyr::lag(regimeSignal == 'buy') ~ cp - dplyr::lag(regimePrice)
          , date == max(date, na.rm = T) ~ return)
      )
    basePrice <- dfa %>% filter(regimeSignal == 'buy') %>% .[1,'cp'] %>% .[[1]]
    strategyReturn <- sum(dfa$return, na.rm = T) 
    abnormalReturn <- strategyReturn - buyHoldReturn
    returnList$buyHoldReturn <- buyHoldReturn
    returnList$strategyReturn <- strategyReturn
    returnList$abnormalReturn <- abnormalReturn
  }
  
  # Create a plot if viz = T
  if(viz == T){
    if(returns == F){
      subtitle <- ''
    }else{
      subtitle <- paste0('Abnormal return = ', round(abnormalReturn, 2), '(', round(strategyReturn,2), ' - ', round(buyHoldReturn, 2), ')')
    }
    dfl <- dfa %>%
      select(date, cp, sma) %>%
      pivot_longer(cols = c('cp', 'sma'), names_to = 'type', values_to = 'price') %>%
      ggplot(aes(x = date, y = price, color = type)) +
      geom_line() +
      geom_text(data = dfa[,c('date', 'cp', 'signal')]
                , aes(x = date, y = cp, label = signal, color = signal)) +
      guides(color = F) +
      labs(title = paste0('Signal Chart for ', ticker, ' Using sansIndicators1 Strategy')
           , x = 'Date'
           , y = 'Closing Price'
           , subtitle = subtitle)
    returnList$viz <- dfl
  }
  
  return(returnList)
    
}
mr_tb12 <- function(df, closingPriceCol = 'adjusted', dateCol = 'date', tickerCol = 'ticker'
                    , rsi_p = 2, rsi_min = 20, sma_p = 10, ema_p = 200, viz = F, returns = F){
  # https://www.youtube.com/watch?v=1pjK2wdSqE4&ab_channel=TradingStrategyGuides
  # Tim Black's strategy #12
  # Buy when the rsi_2period < 10, price > sma_10, and price > ema_200
  # Sell when price > sma_10
  # Usually cash out within first three days
  # df is a dataframe with at least two columns: closing price and date
  # closingPriceCol is the name of the column that has the closing price
  # dateCol is the name of the column that has the date
  # rsi_p is the number of periods for the relative strength index
  # sma_p is the number of periods for the simple moving average
  # ema_p is the number of periods for the exponential moving average
  # viz is a logical value indicating whether you want to return a plot
  # returns is a logical value indicatin whether you want to return the abnormal returns for the period, defined as the return from the strategy vs. buy & hold during the period.
  # Returns the signal for the most recent period, and 
  
  # Location and original name of closing price column
  cpi <- which(names(df) == closingPriceCol)
  cpo <- names(df)[cpi]
  names(df)[cpi] <- 'cp'
  
  # Location and original name of date column
  di <- which(names(df) == dateCol)
  do <- names(df)[di]
  
  # Location and original name of ticker column
  ti <- which(names(df) == tickerCol)
  if(length(ti) == 0){
    ticker <- 'Unspecified Ticker'
    to <- 'No col'
  }else{
    ticker <- unique(df[,ti])[1]
    to <- names(df)[ti]
  }
  if(nrow(df) < ema_p){
    ema_p <- round(nrow(df)/2)
  }
  df %<>% drop_na()
  df$rsi <- TTR::RSI(df[,cpi], n = rsi_p)
  df$sma <- TTR::SMA(df[,cpi], n = sma_p)
  df$ema <- TTR::EMA(df[,cpi], n = ema_p)
  
  dfa <- df %>%
    mutate(
      buy = case_when(
        rsi < rsi_min & cp < sma & cp > ema ~ 'buy'
      )
      , sell = case_when(
        cp > sma ~ 'sell'
      )
      , signal = case_when(
        buy == 'buy' ~ 'buy'
        , sell == 'sell' ~ 'sell'
      )
      , signal = na.locf0(signal)
      , signal = case_when(
        is.na(signal) ~ NA_character_
        , signal == 'sell' & dplyr::lag(signal) == 'sell' ~ NA_character_
        , buy == 'buy' & dplyr::lag(buy) == 'buy' ~ NA_character_
        , signal == 'buy' & dplyr::lag(signal) == 'buy' ~ NA_character_
        , T ~ signal
      )
      , regimeDate = ifelse(signal == 'buy', date, NA)
      , regimeDate = na.locf0(regimeDate)
      , regimeDate = ifelse(is.na(signal), NA, regimeDate)
      , regimePrice = ifelse(signal == 'buy', cp, NA)
      , regimePrice = na.locf0(regimePrice)
      , regimePrice = ifelse(is.na(signal), NA, regimePrice)
      , return = ifelse(signal == 'sell', (cp - regimePrice)/cp, NA)
      ) %>%
    select(-buy, -sell)
  returnList <- list()
  returnList$signal <- dfa$signal[nrow(dfa)]
  returnList$data <- dfa
  
  # Calculate abnormal return - no short sales
  if(returns == T){
    buyHoldReturn <- (dfa[nrow(dfa),'cp'] - dfa[1,'cp'])/dfa[1,'cp']
    strategyReturn <- sum(dfa$return, na.rm = T) 
    abnormalReturn <- strategyReturn - buyHoldReturn
    
    returnList$buyHoldReturn <- buyHoldReturn
    returnList$strategyReturn <- strategyReturn
    returnList$abnormalReturn <- abnormalReturn
  }
  
  # Create a plot if viz = T
  if(viz == T){
    if(returns == F){
      subtitle <- ''
    }else{
      subtitle <- paste0('Abnormal return = ', round(abnormalReturn, 2), '(', round(strategyReturn,2), ' - ', round(buyHoldReturn, 2), ')')
    }
    dfl <- dfa %>%
      select(date, cp, sma, ema) %>%
      pivot_longer(cols = c('cp', 'sma', 'ema'), names_to = 'type', values_to = 'price') %>%
      ggplot(aes(x = date, y = price, color = type)) +
      geom_line() +
      labs(title = paste0('Signal Chart for ', ticker, ' Using mr_tb12 Strategy')
           , x = 'Date'
           , y = 'Closing Price'
           , subtitle = subtitle) +
      guides(color = F)
    if('buy' %in% dfa$signal){
      dfl <- dfl +
        geom_text(data = dfa[,c('date', 'cp', 'signal')]
                  , aes(x = date, y = cp, label = signal, color = signal))
    } 
    rsi <- dfa %>%
      select(date, rsi) %>%
      ggplot(aes(x = date, y = rsi)) +
      geom_line()
    plt <- grid.arrange(dfl, rsi, nrow = 2)
    returnList$viz <- plt
  }
  
  return(returnList)
  
}


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
