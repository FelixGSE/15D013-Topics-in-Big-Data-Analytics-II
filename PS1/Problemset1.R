################################################################################
### Preamble
################################################################################

### Clear workspace
rm(list = ls())

### Set working directory
setwd("/Users/felix/Documents/GSE/Term 3/15D013 Topics in Big Data Analytics II/Problem sets/")

### Load Packages 
if (!require("quantmod")) install.packages("quantmod"); library(quantmod)
if (!require("tseries"))  install.packages("tseries");  library(tseries)
if (!require("forecast")) install.packages("forecast"); library(forecast)
if (!require("fGarch"))   install.packages("fGarch");   library(fGarch)

### Initialize auxilliary functions
source("Auxilliary_Functions_PS1.R")

################################################################################

################################################################################

# ------------------------------------------------------------------------------
# Exercise 1
# ------------------------------------------------------------------------------

# Get stock data from yahoo finance
symbols <- c('GOOG')
start   <- "2012-02-01"
end     <- "2013-02-01"
stock.series <- getSymbols( symbols , src = 'yahoo' , from = start , to = end , 
                            auto.assign = FALSE )

# Conver data to data frame 
df.series <- as.data.frame( stock.series )

# Extract closing price of series: GOOGLE 
google.close   <- google$GOOG.Close
google.returns <- log.returns( google.close , abs = TRUE )
google.acf     <- acf( google.returns )

# ------------------------------------------------------------------------------
# Exercise 2
# ------------------------------------------------------------------------------

# Get sentiment data and compute lenth
sentiment.index <- getSymbols( "UMCSENT" , src = "FRED" , auto.assign = FALSE )
N <- nrow(sentiment.index)

# Plot sentiment series
plot( sentiment.index )

# Compute ACF and PACF
sentiment.acf  <- acf( sentiment.index )
sentiment.pacf <- pacf( sentiment.index )

# for stationarity
adf.test( sentiment.index )

# Differencing the data to get stationary data 
sentintement.diff <- diff( sentiment.index )
adf.test( sentintement.diff[2:N] )

# Fit auto arima model to find best fit
m01 <- auto.arima( sentintement.diff )

# Square data and check for volatility clustering
sentiment.squared <- sentiment.index**2
sentiment.acfP2   <- acf( sentiment.squared )
sentiment.PacfP2  <- pacf( sentiment.squared )
Box.test( sentiment.squared )

# Fit ARMA-GARCH model
m02 <- garchFit( UMCSENT ~ garch(1, 1), data = sentiment.squared, trace = FALSE )

# ------------------------------------------------------------------------------
# Exercise 3
# ------------------------------------------------------------------------------

# Load data
dat <- read.csv( "SP500_shiller.csv" )

# Get price and dividend data
real.dividend <- dat$Real.Dividend 
real.price    <- dat$Real.Price
M             <- length( real.dividend )

# Compute ratio series
ratio          <- ( real.dividend / real.price )[1:(M-1)]
ratio.log      <- log.returns( ratio ) 
ratio.centered <- as.data.frame( scale( ratio.log , center = TRUE , scale = FALSE ) )

test <- lagged.data.set( 3 , ratio.centered )

################################################################################

################################################################################