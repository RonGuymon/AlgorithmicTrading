################# MACHINE LEARNING TO PREDICT DAILY GAINS #############
# The data is gathered using a different script
library(tidyverse)
library(lubridate)
library(magrittr)
library(TTR)
library(caret)
library(xgboost)
library(zoo)
library(googlesheets4)
library(here) # Sets the working directory regardless of the machine I'm on
source('algoFuncs.R')
source('getListOfCompanies.R')
source('Tokens.R')

# Read in a list of existing data files----
existingDataFolder <- './firmSpecificData/'
existingData <- list.files(existingDataFolder)

# Prepare FRED data since it will be used with all firm specific models----
fredFiles <- existingData[which(grepl('fred', existingData))]
fredData <- data.frame()
for(ff in fredFiles){
  tfred <- readRDS(paste0(existingDataFolder, ff))
  if(as.numeric(tfred$date[2]-tfred$date[1]) > 5){
    tfred <- read_rds(paste0(existingDataFolder, 'T10YIE_fred.rds')) %>%
      select(date) %>%
      full_join(tfred, by = 'date') %>%
      mutate(
        ticker = na.locf(ticker, na.rm = F)
        , adjusted = na.locf(adjusted, na.rm = F)
      ) %>% 
      arrange(desc(date)) %>%
      mutate(
        ticker = na.locf(ticker, na.rm = F)
        , adjusted = na.locf(adjusted, na.rm = F)
      ) %>%
      arrange(date)
  }
  fredData %<>% bind_rows(tfred)
}
rm(tfred, fredFiles)

# Read in other stocks that will be used for all models----
otherStocks <- c('spy', 'gc=f')
otherData <- data.frame()
for(os in otherStocks){
  tos <- read_rds(paste0(existingDataFolder, os, '.rds')) %>%
    mutate(
      adjusted = na.locf(adjusted, na.rm = F)
    )
  otherData %<>% bind_rows(tos)
  rm(tos)
}

marketStocks <- bind_rows(fredData, otherData) %>%
  select(date, ticker, adjusted) %>%
  arrange(ticker, date) %>%
  mutate(
    return_1 = (adjusted - dplyr::lag(adjusted))/dplyr::lag(adjusted)
    , sma_10 = TTR::SMA(adjusted, n = 10)
    , sma_20 = TTR::SMA(adjusted, n = 20)
    , ema_20 = TTR::EMA(adjusted, n = 20)
    , ema_200 = TTR::EMA(adjusted, n = 200)
  ) %>%
  ungroup()


marketStocksWide <- data.frame()
for(tk in unique(marketStocks$ticker)){
  tdf <- marketStocks %>% filter(ticker == tk) %>% select(-ticker)
  names(tdf)[2:ncol(tdf)] <- paste0(names(tdf)[2:ncol(tdf)], '_', tk)
  if(nrow(marketStocksWide) == 0){
    marketStocksWide <- tdf
  }else{
    marketStocksWide %<>% full_join(tdf, by = 'date')
  }
}

marketStocksWide[,2:ncol(marketStocksWide)] %<>% lapply(zoo::na.locf, na.rm = F)
stocksToModel <- existingData[which(grepl('_fred', existingData) == F)] %>%
  .[!. %in% c('spy', 'gc=f')]
totalNum <- length(stocksToModel)
dataForGoogleSheet <- data.frame()
iter <- 0
# Identify the date to predict----
dateToPredict <- max(marketStocksWide$date, na.rm = T) + 1
if(wday(dateToPredict) == 1){
  dateToPredict <- dateToPredict + 1
}else if(wday(dateToPredict) == 7){
  dateToPredict <- dateToPredict + 2
}
# Load an existing model or create a new model?----
if(wday(Sys.Date()) == 7){
  loadExistingModel <- F
}else{
  loadExistingModel <- F # If T, will not create a new model
}
existingModelPath <- './firmSpecificModels/'
# Loop through each ticker, create a model, and a prediction----
for(stm in stocksToModel){
  iter <- iter + 1
  cat(stm, iter, paste0(round((iter/totalNum)*100), '%'), '\n')
  tryCatch({
    
    # Gather data----
    df <- read_rds(paste0(existingDataFolder, stm)) %>%
      select(ticker, date, open, high, low, close, volume, adjusted)
    predictionRow <- df[nrow(df),] %>%
      mutate(
        date = dateToPredict
      )
    df %<>% bind_rows(predictionRow)
    
    # Calculate returns----
    returnWindows <- c(1, 3, 5, 10, 14, 21)
    for(rw in returnWindows){
      if(rw == 1){
        df %<>% mutate(
          newReturn = (close - open)/open
        )
      }else{
        df %<>%
          mutate(
            newReturn = (adjusted - dplyr::lag(adjusted, rw)) / dplyr::lag(adjusted, rw)
          )
      }
      names(df)[ncol(df)] <- paste0('return_', rw)
    }
    
    # Calculate simple moving averages (SMA)----
    smaWindows <- c(5, 10, 14, 21)
    for(ma in smaWindows){
      df %<>%
        mutate(
          newSma = TTR::SMA(adjusted, n = ma)
        )
      names(df)[ncol(df)] <- paste0('sma_', ma)
    }
    
    # Calculate exponential moving averages (EMA)----
    emaWindows <- c(20, 50, 100, 200)
    for(ema in emaWindows){
      df %<>%
        mutate(
          newEma = TTR::EMA(adjusted, n = ema)
        )
      names(df)[ncol(df)] <- paste0('ema_', ema)
    }
    
    # Calculate relative strength indices (RSI)----
    rsiWindows <- c(2, 5, 10, 14, 21)
    for(rw in rsiWindows){
      df %<>%
        mutate(
          newRsi = TTR::RSI(adjusted, n = rw)
        )
      names(df)[ncol(df)] <- paste0('rsi_', rw)
    }
    
    # Calculate the number of days since the last divergence----
    minMaxDays <- c(40)
    for(mmd in minMaxDays){
      cat('Diverging strategy', mmd, '\n')
      tryCatch({
        tdf <- divergingStrategy(df %>% rename(time = date
                                               , currentPrice = adjusted)
                                 , periods = mmd) %>%
          select(ticker, time, pLessMin, pLessMax, rsiChangeMin, rsiChangeMax, divergence) %>%
          rename(date = time) %>%
          mutate(
            divergence = case_when(
              grepl('buy', x = divergence) ~ 1
              , grepl('sell', x = divergence) ~ -1
              , T ~ 0
            )
            , daysSinceBuy = case_when(
              divergence == 1 ~ date
            )
            , daysSinceBuy = zoo::na.locf(daysSinceBuy, na.rm = F)
            , daysSinceBuy = date - daysSinceBuy
            , daysSinceBuy = case_when(
              !is.na(daysSinceBuy) ~ daysSinceBuy
              , is.na(daysSinceBuy) ~ max(daysSinceBuy, na.rm = T)
            )
            , daysSinceSell = case_when(
              divergence == -1 ~ date
            )
            , daysSinceSell = zoo::na.locf(daysSinceSell, na.rm = F)
            , daysSinceSell = date - daysSinceSell
            , daysSinceSell = case_when(
              !is.na(daysSinceSell) ~ daysSinceSell
              , is.na(daysSinceSell) ~ max(daysSinceSell, na.rm = T)
            )
          ) %>%
          select(-divergence)
        names(tdf)[3:ncol(tdf)] <- paste(names(tdf)[3:ncol(tdf)], mmd, sep = '_')
        
        df %<>% left_join(tdf, by = c('ticker', 'date'))
      }, error = function(e){
        cat('Problem calculating the diverging strategy ', mmd, '\n')
      })
    }
    
    # Calculate the adjusted price relative to the min and max price----
    minMaxPeriods <- c(40, 255)
    for(mmp in minMaxPeriods){
      tryCatch({
        df <- df %>%
          mutate(
            adjRelMin = rollapply(adjusted
                                  , width = mmp
                                  , align = 'right'
                                  , FUN = min, na.rm = T
                                  , fill = NA
            )
            , adjRelMin = (adjusted - adjRelMin)/adjRelMin
            , adjRelMax = rollapply(adjusted
                                    , width = mmp
                                    , align = 'right'
                                    , FUN = max, na.rm = T
                                    , fill = NA
            )
            , adjRelMax = (adjusted - adjRelMax)/adjRelMax
          )
        names(df)[(ncol(df)-1):ncol(df)] <- paste(names(df)[(ncol(df)-1):ncol(df)], mmp, sep = '_')
      }, error = function(e){
        cat('Cannnot calculate the min and max for period', mmp, '\n')
      })
    }
    # Strategy indicators----
    df %<>%
      mutate(
        sma5Gtsma21 = ifelse(sma_5 > sma_21, 1, 0)
        , tb12 = ifelse(rsi_2 > 10 & adjusted < sma_10 & adjusted > ema_200, 1, 0)
      )
    # Add in FRED data----
    df %<>% left_join(marketStocksWide, by = 'date')
    # Information from prior periods----
    # names(df)
    daysPrior <- c(1,2,3,4,5,6,7,14,28,364)
    colsToLag <- names(df)[7:ncol(df)]
    
    for(dp in daysPrior){
      if(dp == 1){ # One trading day prior is different than multiples of seven because it could end up on a weekend
        df %<>%
          mutate(
            jc = dplyr::lag(date)
          )
      }else{
        df %<>%
          mutate(
            jc = date - dp
          )
      }
      jdf <- df[,c('date', colsToLag)]
      names(jdf)[2:ncol(jdf)] %<>% paste0('_lag', dp) 
      df %<>% left_join(jdf, by = c('jc' = 'date'))
      df %<>% select(-jc)
    }
    # names(df)
    # Part of the year----
    df %<>%
      mutate(
        wday = wday(date)
        , mday = mday(date)
        , month = month(date)
      )
    # Create the dvs----
    returnLevels <- quantile(df$return_1, seq(0,1,.25), na.rm = T)
    df %<>%
      mutate(
        # return_1_binaryDv = ifelse(return_1 > returnLevels[4], 1, 0) # Try to capture days in which the return will be large
        return_1_binaryDv = ifelse(return_1 > 0, 1, 0)
        # return_1_threeDv = case_when(
        #   return_1 < returnLevels[2] ~ 0
        #   , return_1 < returnLevels[3] ~ 1
        #   , return_1 <= returnLevels[4] ~ 2
        #   , return_1 <= returnLevels[5] ~ 3
        # )
      )
    
    # Deal with missing values----
    # Remove columns that have more than half of the observations that are missing
    colsToKeep <- colSums(is.na(df)) %>%
      .[which(. < round(nrow(df)/2))]
    df <- df[,which(names(df) %in% names(colsToKeep))]
    # Remove rows that have more than half of the observations missing
    df$missingVals <- rowSums(is.na(df))
    df %<>% 
      mutate(
        missingColThresh = round(ncol(df)/3)
      ) %>%
      filter(missingVals < missingColThresh) %>%
      select(-missingVals, -missingColThresh)
    # Fill in the remaining values with the last non-missing value
    df[,] %<>% lapply(na.locf, na.rm = F)
    # Remove the initial rows that still have missing values
    df %<>% na.omit()
    # Relocate the dv to the first column and select ivs----
    ivs <- names(df)[which(grepl('_lag', names(df)))]
    ivs <- c('wday', 'mday', 'month', ivs)
    dv <- names(df)[which(grepl('Dv', names(df)))]
    df2 <- df[,c(dv,ivs)]
    predictionData <- df2[nrow(df2),]
    df2 <- df2[1:(nrow(df2)-1),]
    # Split into testing and training----
    set.seed(2364)
    trainIndex <- createDataPartition(df2[,dv], times = 1, p = .8, list = F) # Make sure that the most recent observation is not in the training dataset
    train_data <- df2[trainIndex,-which(names(df2) %in% dv)]
    train_labels <- df2[trainIndex,which(names(df2) %in% dv)]
    test_data <- df2[-trainIndex,-which(names(df2) %in% dv)]
    test_labels <- df2[-trainIndex,which(names(df2) %in% dv)]
    prediction_data <- predictionData[,-which(names(df2) %in% dv)]
    # Convert to sparse matrix and create the model----
    dtrain <- xgb.DMatrix(data = data.matrix(train_data), label = train_labels)
    dtest <- xgb.DMatrix(data = data.matrix(test_data), label = test_labels)
    dpredict <- xgb.DMatrix(data = data.matrix(prediction_data))
    
    if(loadExistingModel == T){
      # Load an existing model----
      modelT <- xgb.load(paste0(existingModelPath, gsub('\\.rds', '', stm), '.model'))
    }else{
      # Create a new model using xgb.train approach----
      modelParams <- list(objective = 'binary:logistic')
      # modelParams <- list(objective = 'multi:softprob'
      #                     , num_class = 4)
      watchlist <- list(train = dtrain, test = dtest)
      modelT <- xgb.train(params = modelParams
                          , data = dtrain
                          , nrounds = 600
                          , watchlist = watchlist
                          , max.depth = 8
                          , eta = .01
                          , subsample = 1
                          # , min.child.weight = .8
                          , early_stopping_rounds = 5)
      # e <- data.frame(modelT$evaluation_log)
      # names(e)[2:3] <- c('train_loss', 'test_loss')
      # e %>%
      #   pivot_longer(cols = c(train_loss:test_loss), names_to = 'lossType', values_to = 'loss') %>%
      #   ggplot(aes(x = iter, y = loss, color = lossType)) +
      #   geom_line()
      
      # Save the model----
      modelFileName <- paste0(gsub('\\.rds', '', stm), '.model')
      xgb.save(modelT, paste0(existingModelPath, modelFileName))
      
    }
    # Evaluate model effectiveness----
    predT <- predict(modelT, dtest)
    testError <- round(mean(as.numeric(predT > .5) != test_labels), 2)
    paste0('test-error=', testError) # error
    # Positive predictive value (precision) is most meaningful to me
    # quantile(predT, seq(0,1,.2))
    confM <- confusionMatrix(factor(as.numeric(predT > quantile(predT, .8)), levels = c(1,0)) 
                             , reference = factor(test_labels, levels = c(1,0)))
    # # Variable importance----
    # importanceRaw <- xgb.importance(names(train_data),model = modelT)
    # xgb.plot.importance(importanceRaw[1:10,])
    # # Trees
    # xgb.plot.multi.trees(names(train_data),model = modelT)
    # xgb.plot.tree(model = modelT, trees = 99)
    # # Test whether results make sense
    # chisq.test(train_data$wday, train_labels)
    

    # Add data to the Google Sheet, especially the prediction----
    tryCatch({
      divRecent <- divergingStrategy(df[,c('ticker', 'date', 'adjusted')] %>% 
                                       rename(time = date, currentPrice = adjusted)
                                     , periods = 40) 
      lastDiv <- divRecent %>%
        filter(!is.na(divergence)) %>%
        .[nrow(.),] %>%
        select(time, divergence)
      daysSinceLastDiv <- difftime(Sys.Date(), lastDiv[1,'time'][[1]], units = 'days') %>% as.numeric()
      lastDivergence <- lastDiv[1,'divergence'][[1]]
      divergingToday <- divRecent %>%
        rename(date = time) %>%
        filter(date == max(date, na.rm = T)) %>%
        pull(divergence)
    }, error = function(e){
      cat('Problem with divergence', stm, '\n')
      daysSinceLastDiv <<- NA
      lastDiv <<- NA
    })
    newObservation <- data.frame(
      ticker = gsub('\\.rds', '', stm)
      , date = Sys.Date()
      , price = df[(nrow(df)-1),'adjusted']
      , return_1 = df[(nrow(df)-1),'return_1']
      , return_10 = df[(nrow(df)-1),'return_10']
      , divergence = divergingToday
      , daysSinceLastDiv = daysSinceLastDiv
      , lastDivergence = lastDivergence
      , predictionDate = df[nrow(df),'date']
      , testError = testError
      , posPredValue = confM$byClass[which(names(confM$byClass) == 'Pos Pred Value')][[1]] %>% round(2)
      , predictionProb = round(predict(modelT, dpredict), 3)
    ) %>%
      mutate(
        prediction = ifelse(predictionProb >=quantile(predT, .8), 'buy', NA)
      )
    dataForGoogleSheet %<>% bind_rows(newObservation)
  }, error = function(e){
    cat('Problem with ', stm, '---------\n')
  })
}
system('say done yo')
# dfgs <- dataForGoogleSheet
dataForGoogleSheet %<>% 
  arrange(daysSinceLastDiv, prediction, desc(posPredValue), desc(price)) %>%
  mutate(
    prediction = as.character(prediction)
  )

write_rds(dataForGoogleSheet, 'stocksToBuy.rds', compress = 'gz')
# Add the predictions to the Google Sheet----
# Refresh token so that I can get data from the Google Sheet
# https://cran.r-project.org/web/packages/googlesheets/README.html

# gToken <- gs4_auth() # Run this the first time to get the oAuth information
# saveRDS(gToken, "gToken.rds") # Save the oAuth information for non-interactive use
# suppressWarnings(readRDS("gToken.rds")) # Needs an interactive environment
suppressWarnings(gs4_auth(email = Sys.getenv('googleSheetsEmail')
                          , token = "gToken.rds")) # Auto refreshes stale oAuth token
# allSheets <- gs4_find(gToken) # Read in the metadata for all files in the account. Only need to run the first time to get the sheet key.

write_sheet(dataForGoogleSheet, ss = Sys.getenv('stockSheetsKey'), sheet = 'stocksToBuy')


# # Train the model----
# model <- xgboost(data = dtrain
#                  , max.depth = 3
#                  , eta = .1
#                  , nround = 10
#                  , objective = 'binary:logistic')
# pred <- predict(model, dtest)
# paste0('test-error=', mean(as.numeric(pred > .5) != test_labels)) # error
# # Positive predictive value (precision) is most meaningful to me
# confusionMatrix(factor(as.numeric(pred > .58), levels = c(1,0))
#                 , reference = factor(test_labels, levels = c(1,0)))
# # Variable importance
# importanceRaw <- xgb.importance(names(train_data),model = model)
# xgb.plot.importance(importanceRaw[1:10,])
# # Trees
# xgb.plot.multi.trees(names(train_data),model = model)
# xgb.plot.tree(model = model, trees = 9)
# # Test whether results make sense
# chisq.test(train_data$return_3_lag7, train_labels)
# # Save model
# xgb.save(model, 'aapl.model')
# # Load model
# mload <- xgb.load('aapl.model')
# predict(mload, dtest)
# # Multinomial predictions----
# predDf <- matrix(predT, nrow = 4) %>% 
#   t() %>% 
#   as.data.frame()
# names(predDf) <- c('bl', 'l', 'g', 'bg')
# predDf %<>%
#   rownames_to_column() %>%
#   pivot_longer(cols = bl:bg, names_to = 'pred_labels', values_to = 'prob') %>%
#   group_by(rowname) %>%
#   mutate(
#     pred = ifelse(prob == max(prob), 1, 0)
#     , rowname = as.numeric(rowname)
#   ) %>%
#   ungroup() %>%
#   filter(pred == 1) %>%
#   select(-pred) %>%
#   arrange(rowname) %>%
#   mutate(
#     test_labels = test_labels
#     , test_labels = case_when(
#       test_labels == 0 ~ 'bl'
#       , test_labels == 1 ~ 'l'
#       , test_labels == 2 ~ 'g'
#       , test_labels == 3 ~ 'bg'
#     )
#     , test_labels = factor(test_labels, levels = c('bl', 'l', 'g', 'bg'))
#     , pred_labels = factor(pred_labels, levels = c('bl', 'l', 'g', 'bg'))
#   )
# confusionMatrix(predDf$pred_labels,
#                 , reference = predDf$test_labels)