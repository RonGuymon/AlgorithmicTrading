########## GET INTRADAY STOCK DATA ##########
# The intent is to run this multiple times every day
# Adapted from the website below on 7/16/21
# https://stackoverflow.com/questions/40245464/web-scraping-of-key-stats-in-yahoo-finance-with-r
library(tidyverse)
library(magrittr)
library(lubridate)
library(rvest)
library(googlesheets4)
library(here)
source('algoFuncs.R')
source('Tokens.R')
# Read in sheet with stocks that are owned and being watched----
# stocks <- read.csv('stocks.csv') %>%
#   mutate(
#     type = ifelse(is.na(qty), 'watch', 'own')
#   )

# https://cran.r-project.org/web/packages/googlesheets/README.html

# gToken <- gs4_auth() # Run this the first time to get the oAuth information
# saveRDS(gToken, "gToken.rds") # Save the oAuth information for non-interactive use
# suppressWarnings(readRDS("gToken.rds")) # Needs an interactive environment
suppressWarnings(gs4_auth(email = Sys.getenv('googleSheetsEmail')
                          , token = "gToken.rds")) # Auto refreshes stale oAuth token
# allSheets <- gs4_find(gToken) # Read in the metadata for all files in the account. Only need to run the first time to get the sheet key.
suppressMessages(stocks <- read_sheet(Sys.getenv('stockSheetsKey'), sheet = 1
                                       , col_types = 'cddd'))
stocks %<>%
  mutate(
    type = ifelse(is.na(qty), 'watch', 'own')
  )

# Get trending and gainers----
tryCatch({
  trending <- getTrendingYahoo()
}, error = function(e){
  trending <<- data.frame(ticker = NA
                         , pctChange = NA
                         , type = 'trending')
  cat('Problem with getting trending stocks')
})

tryCatch({
  gainers <- getGainersYahoo() 
}, error = function(e){
  gainers <<- data.frame(ticker = NA
                         , pctChange = NA
                        , type = 'gainer')
  cat('Problem with getting gaining stocks')
})


# Combine list of stocks: own, watch, movers----
stockWatch <- trending[1:5,] %>% 
  bind_rows(gainers[1:5,]) %>%
  arrange(desc(pctChange)) %>%
  filter(pctChange > 0) %>%
  filter(!ticker %in% stocks$ticker) %>%
  select(ticker, pctChange, type) %>%
  bind_rows(stocks) %>%
  arrange(desc(qty), type) %>%
  filter(!is.na(ticker)) %>%
  mutate(
    ticker = tolower(ticker)
  )

# Read in existing data----
prices <- readRDS('pricesIntraday.rds') %>%
  mutate(
    ticker = tolower(ticker)
  )
# Get current information and combine with existing----
# prices <- data.frame()
stocksToGet <- c(unique(prices$ticker), stockWatch$ticker) %>% 
  unique() %>%
  .[1:55] %>%
  .[which(!is.na(.))]

bt <- Sys.time()
for(t in stocksToGet){
  cat(t, '\n')
  tp <- getCurrentInfoFinnhub(ticker = t, apikey = Sys.getenv('finnHubApiKey'))
  prices %<>% bind_rows(tp)
}
et <- Sys.time()
difftime(et, bt)
prices %<>% dplyr::filter(!is.na(ticker))

# Get rid of prices that are after hours----
# pricesToDelete <- prices %>%
#   mutate(
#     hourMin = hour(time)*100 + minute(time)
#   ) %>%
#   filter(hourMin < 700 | hourMin > 1430)
# prices %<>%
#   mutate(
#     hourMin = hour(time)*100 + minute(time)
#   ) %>%
#   filter(hourMin > 700 & hourMin < 1430)
## Write data to file---- 
# Once a day remove the prices for stocks that I'm not interested in watching for the long term
if(hour(Sys.time()) > 14 | hour(Sys.time()) < 6 | wday(Sys.time()) %in% c(1, 7)){
  longTermStocks <- stockWatch %>% filter(type %in% c('own', 'watch')) %>% pull(ticker)
  # Archive prices of stocks that I'm just watching for the day
  shortTermPrices <- readRDS('shortTermPrices.rds')
  stPricesToArchive <- prices %>% 
    dplyr::filter(!ticker %in% longTermStocks) %>%
    bind_rows(shortTermPrices) %>%
    arrange(ticker, time)
  write_rds(stPricesToArchive, 'shortTermPrices.rds', compress = 'gz')
  # Keep intraday prices for stocks that I want to watch long term
  prices %<>% filter(ticker %in% longTermStocks)
}
write_rds(prices, 'pricesIntraday.rds', compress = 'gz')


