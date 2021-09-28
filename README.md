# AlgorithmicTrading
R files for checking for buy sell signals, as well as sending me alerts when a signal comes up. Here are the process flows:

# Google Sheet
1. __Stocks__ worksheet contains stocks that I'm interested in. I read about these on Motley Fool. They could be ones that have a lot of potential, but are not necessarily on a major index.
2. __stocksToBuy__ contains information about stocks and whether I should buy them. The buy column is 1 if it is expected to have a closing price higher than the open price.
3. __gainersDaily__ contains summary information, like daily RSI and daily returns for all of the stocks for which I have daily data. I try to get those that are on the S&P 500, Dow Jones, and NASDAQ.
4. __gainers__ contains intraday information like RSI and returns. This is meant for intraday trading.
5. __Strategy__ contains ideas about strategies that I want to use.

# Intraday Data
1. Run the getIntradayData.R file every two minutes to collect intraday stock prices for up to 60 symbols.
2. Run the AlertCalculations.R file every two minutes during trading hours. Thhis updates data on the __gainers__ worksheet. This will read in data that is gathered from the getIntraDayData.R file and placed into two files:
  * The main file is the pricesIntraday.rds file that contains data for symbols that are on the stocks Google Sheet. This should have lots of intraday prices for consistent days.
  * The shortTermPrices.rds file contains intraday data that comes from symbols that are trending or gaining. This only has prices for symbols when they are trending or gaining, so it is inconsistent from day to day.
  
# Daily Data
1. Run the getDailyData.R file at least once a day until it gets daily stock data for the day before. These files are stored as .rds files in the firmSpecificData folder. It also gets data from FRED, such as unemployment, and crude oil prices that could be used in models. This file updates the data on the __gainersDaily__ worksheet.
2. Run the algorithmicModels.R file to create firm specific models. These models are stored in the firmSpecificModels folder so that they can be used in case the most recent data for modeling is not available.

# Want to do
1. Analyze the intraday data
2. Create forecasts for weekly returns
3. Identify stocks that are going to have a big jump and a big drop.
4. Incorporate earnings forecasts and surprises from finnhub.
5. Incorporate financial statement data.
6. Include indicators for whether the price is above/below the 52-week high/low, and support/resistance.
  
