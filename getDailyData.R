############## GET DAILY DATA ###############
# This only needs to be run once a day, or until the latest data is added
# Libraries----
cat(as.character(Sys.time()))
library(tidyverse)
library(lubridate)
library(magrittr)
library(quantmod)
library(zoo)
library(googlesheets4)
library(fredr)
library(here) # Sets the working directory regardless of the machine I'm on
source('algoFuncs.R')
source('getListOfCompanies.R')
source('Tokens.R')

# Read in a list of existing data files----
existingDataFolder <- './firmSpecificData/'
existingData <- list.files(existingDataFolder)

# Get FRED economic data----
fredData <- data.frame()
fredr_set_key(Sys.getenv('fredApiKey'))
fred_series <- c('VIXCLS' #VIX, which is a volatility index fund
                 , 'VXVCLS' #VIX for S&P100
                 , 'UNRATE' # Unemployment rate (weekly)
                 , 'T10YIE' # Ten year breakeven inflation rate
                 , 'EFFR' # Effective federal funds rate
                 , 'DCOILWTICO' # Crude oil prices
)
for(fs in fred_series){
  cat(fs, '\n')
  tryCatch({
    tfred <- fredr(series_id = fs
                  , observation_start = ymd('2010-01-01')
                  # , observation_end = Sys.Date()
    ) %>%
      rename(ticker = series_id, adjusted = value) %>%
      na.omit()
    # Check to see if data exists and if it does, only replace it if the new data has more recent dates
    fredFileName <- paste0(fs, '_fred.rds')
    if(fredFileName %in% existingData){
      tfredExisting <- readRDS(paste0(existingDataFolder, fredFileName))
      if(max(tfredExisting$date, na.rm = T) < max(tfred$date, na.rm = T)){
        write_rds(tfred, paste0(existingDataFolder, fredFileName), compress = 'gz')
      }
      rm(tfredExisting)
    }else{
      write_rds(tfred, paste0(existingDataFolder, fredFileName), compress = 'gz')
    }
  }, error = function(e){
    cat('Problem with ', fs, '\n')
  })
}





# Refresh token so that I can get data from the Google Sheet----
# https://cran.r-project.org/web/packages/googlesheets/README.html

# gToken <- gs4_auth() # Run this the first time to get the oAuth information
# saveRDS(gToken, "gToken.rds") # Save the oAuth information for non-interactive use
# suppressWarnings(readRDS("gToken.rds")) # Needs an interactive environment
suppressWarnings(gs4_auth(email = Sys.getenv('googleSheetsEmail')
                          , token = "gToken.rds")) # Auto refreshes stale oAuth token
# allSheets <- gs4_find(gToken) # Read in the metadata for all files in the account. Only need to run the first time to get the sheet key.

# Read in stocks Google sheet to see what stocks I'm interested in watching----
suppressMessages(stocks <- read_sheet(Sys.getenv('stockSheetsKey'), sheet = 'stocks'
                                      , col_types = 'cddd'))
stocks %<>%
  mutate(
    type = ifelse(is.na(qty), 'watch', 'own')
  )

# Read in gainersDaily Google sheet to see the daily momentum and returns----
suppressMessages(gd <- read_sheet(Sys.getenv('stockSheetsKey'), sheet = 'gainersDaily'
                                  # , col_types = 'cDdcddddddd'
))


testSpy <- getSymbols('spy', auto.assign = F) 
testSpy %<>% as.data.frame() %>%
  rownames_to_column() %>%
  rename(date = rowname) %>%
  mutate(
    date = ymd(date)
    , ticker = 'spy'
  )
names(testSpy) <- gsub('^.*\\.', '', names(testSpy)) %>% tolower()
histSpy <- read_rds(paste0(existingDataFolder, 'spy.rds'))
if(max(testSpy$date, na.rm = T) > max(histSpy$date, na.rm = T)){
  write_rds(testSpy, paste0(existingDataFolder,'spy.rds'), compress = 'gz')
  rm(histSpy, testSpy)
  nd <- getNASDAQ()
  sp <- getSP500()
  dj <- getDJ()
  allIndices <- bind_rows(nd, sp, dj) %>%
    arrange(desc(pct_chg)) %>%
    dplyr::distinct(ticker) %>%
    pull(ticker)
  stocksToGet <- unique(c(stocks$ticker, allIndices, 'gc=f') %>% tolower()) # gc=f is gold
  rm(nd, sp, dj, allIndices)
  daily <- data.frame()
  stgl <- length(stocksToGet)
  progNum <- 0
  for(t in stocksToGet){
    progNum <- progNum + 1
    cat(t, progNum, '\n')
    tryCatch({
      tdaily <- getSymbols(t, auto.assign = F) 
      Sys.sleep(2)
      
      tdaily2 <- tdaily %>% 
        as.data.frame() %>%
        rownames_to_column() %>%
        rename(date = rowname) %>%
        mutate(
          date = ymd(date)
          , ticker = t
        )
      names(tdaily2) <- gsub('^.*\\.', '', names(tdaily2)) %>% tolower()
      # Check to see if data exists and if it does, only replace it if the new data has more recent dates
      if(paste0(t, '.rds') %in% existingData){
        tdailyExisting <- readRDS(paste0(existingDataFolder, t, '.rds'))
        if(max(tdailyExisting$date, na.rm = T) < max(tdaily2$date, na.rm = T)){
          write_rds(tdaily2, paste0(existingDataFolder,t, '.rds'), compress = 'gz')
        }
        rm(tdailyExisting)
      }else{
        write_rds(tdaily2, paste0(existingDataFolder,t, '.rds'), compress = 'gz')
      }
      tdf <- tdaily2 %>%
        arrange(date)
      tdf <- na.omit(tdf)
      nrows <- nrow(tdf)
      maxDate <- max(tdf$date, na.rm = T)
      returnDateCalcs <- c(1,7,14,28,56,182,364)
      for(rdc in returnDateCalcs){
        voldy <- T
        while(voldy == T){
          neededDate <- maxDate - rdc
          # cat('neededDate=', rdc, '\n')
          if(neededDate < min(tdf$date, na.rm = T)){
            break
          }else if(neededDate %in% tdf$date){
            voldy <- F
          }else{
            rdc <- rdc+1
            # cat(' newNeededDate =', rdc, '\n')
          }
        }
        if(paste0('return_', rdc) %in% names(tdf)){
          next()
        }
        tdf2 <- tdf %>% 
          select(date, adjusted) %>%
          rename(adjusted2 = adjusted)
        tdf %<>% 
          mutate(
            jc = neededDate
          ) %>%
          left_join(tdf2, by = c('jc' = 'date')) %>%
          mutate(
            return = (adjusted - adjusted2)/adjusted2
          ) %>%
          select(-jc, -adjusted2)
        names(tdf)[ncol(tdf)] <- paste0('return_', rdc)
      }
      tdaily3 <- tdf[nrow(tdf),which(grepl('ticker|date|adjusted|return', names(tdf)))]
      daily %<>% bind_rows(tdaily3)
      rm(tdf, tdaily, tdaily2, tdaily3)
    }, error = function(e){
      cat('Problem with', t, progNum, '\n')
    })
  }
  
  dailyLong <- daily %>%
    pivot_longer(cols = contains('return'), names_to = 'window', values_to = 'return') %>%
    group_by(ticker) %>%
    na.omit() %>%
    mutate(
      weight = gsub('return_', '', window) %>% as.numeric()
      , weight = 1/weight
      , weightProduct = return*weight
    ) %>%
    summarise(wreturn = sum(weightProduct)/sum(weight)) %>%
    ungroup()
  
  daily %<>% 
    left_join(dailyLong, by = 'ticker') %>%
    arrange(desc(wreturn)) %>%
    relocate(wreturn, .after = ticker) %>%
    mutate(
      across(contains('return'), round, 4)
    )
  
  
  ## Put the gainers data on the google Sheet----
  write_sheet(daily, ss = Sys.getenv('stockSheetsKey'), sheet = 'gainersDaily')

}

