############ GET A LIST OF COMPANIES FROM VARIOUS INDEXES #############
library(tidyverse)
library(lubridate)
library(magrittr)
library(rvest)
library(here)

# Read in list of companies from slickcharts.com----
getNASDAQ <- function(){
  urlContents <- rvest::read_html('https://www.slickcharts.com/nasdaq100')
  t <- urlContents %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]]
  names(t) <- gsub('%', 'pct', names(t)) %>% gsub(' ', '_', .) %>% tolower()
  t %<>% 
    dplyr::rename(ticker = symbol) %>%
    mutate(
      pct_chg = gsub('[^0-9\\.\\-]', '', pct_chg) %>% as.numeric()
      , price = gsub(',', '', price) %>% as.numeric()
    ) %>%
    arrange(desc(pct_chg)) %>%
    select(-`#`)
  return(t)
}

getDJ <- function(){
  urlContents <- rvest::read_html('https://www.slickcharts.com/dowjones')
  t <- urlContents %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]]
  names(t) <- gsub('%', 'pct', names(t)) %>% gsub(' ', '_', .) %>% tolower()
  t %<>% 
    dplyr::rename(ticker = symbol) %>%
    mutate(
      pct_chg = gsub('[^0-9\\.\\-]', '', pct_chg) %>% as.numeric()
      , price = gsub(',', '', price) %>% as.numeric()
    ) %>%
    arrange(desc(pct_chg)) %>%
    select(-`#`)
  return(t)
}

getSP500 <- function(){
  urlContents <- rvest::read_html('https://www.slickcharts.com/sp500')
  t <- urlContents %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]]
  names(t) <- gsub('%', 'pct', names(t)) %>% gsub(' ', '_', .) %>% tolower()
  t %<>% 
    dplyr::rename(ticker = symbol) %>%
    mutate(
      pct_chg = gsub('[^0-9\\.\\-]', '', pct_chg) %>% as.numeric()
      , price = gsub(',', '', price) %>% as.numeric()
    ) %>%
    arrange(desc(pct_chg)) %>%
    select(-`#`)
  return(t)
}