################################################################################
### Preamble
################################################################################

### Clear workspace
rm(list = ls())

### Set working directory
setwd("/Users/felix/Documents/GSE/Term 3/15D013 Topics in Big Data Analytics II/Problem sets/15D013-Topics-in-Big-Data-Analytics-II/PS1")

### Load Packages 
if (!require("quantmod")) install.packages("quantmod"); library(quantmod)
if (!require("tseries"))  install.packages("tseries");  library(tseries)
if (!require("forecast")) install.packages("forecast"); library(forecast)
if (!require("fGarch"))   install.packages("fGarch");   library(fGarch)
if (!require("e1071"))    install.packages("e1071");    library(e1071)
if (!require("h2o"))      install.packages("h2o");      library(h2o)

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
google.close   <- df.series$GOOG.Close
N              <- length( google.close )
google.returns <- google.close[ 2:N ] / google.close[ 1:( N - 1) ]  

# Inspect Taylor effect
max.iteration  <- 5
max.lags       <- 5 
google.powers  <- matrix( NA , ncol = ( max.lags + 1 ), nrow = max.iteration ) 

# Compute ACF
for( i in 1:max.iteration ){
  temp.power.return <- google.returns**i
  temp.acf          <- acf(temp.power.return,plot = FALSE, lag.max = max.lags )$acf
  google.powers[i,] <- temp.acf
}

# Taylor Effect is true
sapply( 2:max.lags,function(i){ monotonically.decreasing(google.powers[,i]) } )

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
sp500         <- dat$SP500

# Compute ratio series
ratio          <- ( real.dividend / real.price )[1:(M-1)]
ratio.log      <- log.returns( ratio ) 
ratio.centered <- as.data.frame( scale( ratio.log , center = TRUE , scale = FALSE ) )

ratio.lagged   <- lagged.data.set( 3 , ratio.centered )
N.ratio.lagged <- nrow(ratio.lagged)

data <- as.data.frame( cbind( sp500[ (M - N.ratio.lagged+1):M ], ratio.lagged ) )
colnames(data)[1] <- "SP500"

split      <- dataSplit( data, 0.75 )
training   <- split$TrainingSet
test       <- split$TestSet
N.training <- nrow( training )
M.training <- ncol(training)
N.test     <- nrow( test )
M.training <- ncol(test)
  
y.test <- test$SP500
x.test <- test[,setdiff(colnames(test),c("SP500")) ]
  
# Support Vector Macines (SVM) - Grid Search over Parameters
gamma        <- 1:3
N.gamma      <- length( gamma )
cost         <- 1:3
N.cost       <- length( cost )
model.list   <- list()
name.counter <- 1

mse <- matrix( NA, nrow = N.gamma, ncol = N.cost )
rownames(mse) <- gamma
colnames(mse) <- cost

for( i in 1:N.gamma ){
  temp.gamma <- gamma[i]
  for( j in 1:N.cost ){
    temp.cost <- cost[j]
    tempsvm <- svm( SP500 ~.,data = training, scale = FALSE,
                    kernel = "radial", gamma = temp.gamma, cost = temp.cost )
    temp.prediction <- predict(tempsvm, newdata = x.test )
    temp.mse <- mean( ( y.test - temp.prediction )**2 )
    mse[i,j] <- temp.mse
    model.name   <- paste0( "svmGAM",temp.gamma,"COS",temp.cost )
    
    model.list[[name.counter]] <- tempsvm  
    names( model.list )[name.counter] <- model.name
    name.counter <- name.counter + 1
    cat("Itteration: i =", i,"/","j =",j,"\n" )
  }
  
if( i == N.gamma ){
  best.ind   <- which(mse == min(mse), arr.ind = TRUE)
  best.gamma <- best.ind[1]
  best.cost  <- best.ind[2]
  cat("\n",
      "------------------------------------","\n", 
      "GRID SEARCH FINISHED","\n",
      "------------------------------------","\n", 
      "Training Observation:", N.training  ,"\n",
      "Test Observations:", N.test, "\n",
      "BEST PARAMETER SPECIFICATION:","\n",
      "GAMMA:",best.gamma,"\n",
      "COST:", best.cost, "\n",
      "MSE:", mse[best.ind],"\n",
      "------------------------------------","\n", 
      "\n",
      "------------------------------------","\n"
      )
  }
  
}

# Neuronal Networks


## Start a local cluster with 2GB RAM
localH2O      <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
training.h20  <- as.h2o(training, destination_frame = "Training")
test.h20      <- as.h2o(test, destination_frame = "Test")


m01 <- h2o.deeplearning(x = 2:M.training,  
                        y = 1,   
                        training_frame = training.h20,
                        validation_frame = test.h20,
                        activation = "Tanh",
                        input_dropout_ratio = 0.2, 
                        hidden_dropout_ratios = c(0.5,0.5,0.5), 
                        hidden = c(50,50,50), 
                        epochs = 100
                        ) 

################################################################################

################################################################################