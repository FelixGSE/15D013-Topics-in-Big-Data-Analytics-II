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
if (!require("rgp"))      install.packages("rgp");      library(rgp)
if (!require("xts"))      install.packages("xts");      library(xts)

### Initialize auxilliary functions
source("Auxilliary_Functions_PS1.R")
install.packages("nomclust")

### Styling options
co <- 1/255 
pers.green      <- rgb( co *  14 ,  co * 105 , co *  90 )
pers.blue       <- rgb( co *  22 ,  co *  54 , co *  92 )
pers.red        <- rgb( co *  99 ,  co *  37 , co *  35 )
pers.gray       <- rgb( co * 150 ,  co * 150 , co * 150 )
pers.orange     <- rgb( co * 186 ,  co *  85 , co *  59 )
pers.beige      <- rgb( co * 196 ,  co * 189 , co * 151 )
full.color.set  <- list( pers.green , pers.blue, pers.red,
                         pers.gray, pers.orange, pers.beige 
                        ) 
################################################################################

################################################################################

# ------------------------------------------------------------------------------
# Exercise 1
# ------------------------------------------------------------------------------

# Get stock data from yahoo finance
symbols      <- c( 'GOOG' )
start        <- "2011-01-01"
end          <- "2015-12-31"
stock.series <- getSymbols( symbols , src = 'yahoo' , from = start , to = end ,auto.assign = FALSE )

# Conver data to data frame 
df.series <- as.data.frame( stock.series )

# Extract closing price of series: GOOGLE 
close          <- df.series[,6]
N              <- length( close )
#returns        <- returns( df[,column], abs = TRUE )




returns <- returns(close,log=FALSE)[-1]
returns <- df$rtrn
#$google.returns <- abs(diff(log(google.close),diff=1))[-1]
#google.returns <- diff(google.returns)
# Inspect Taylor effect
max.iteration  <- 4
max.lags       <- 5 
powers  <- matrix( NA , ncol = ( max.lags + 1 ), nrow = max.iteration ) 

# Compute ACF
for( i in 1:max.iteration ){
  temp.power.return <- returns**i
  #temp.power.abs    <- abs(temp.power.return)
  temp.acf          <- acf(temp.power.return,plot = FALSE, lag.max = max.lags )$acf
  powers[i,]        <- round(temp.acf,4)
}
acf(temp.power.return)
# Taylor Effect is true
sapply( 2:max.lags,function(i){ monotonically.decreasing(powers[,i]) } )
#acf(google.returns**2)

N <- nrow( powers )
M <- ncol( powers )
X <- 1:M
point.shape = 1:N
ymin <- min( powers ) - 0.5
ymax <- max( powers ) + 0.5

png("p1.png")
plot(X, powers[1,] , type = "b", pch = point.shape[1],
     xlab = "Lags", ylab = "Autocorrelation", col = full.color.set[[1]],
     ylim = c( -0.1 , ymax ), tck=-0.01 )

for( i in 2:max.iteration){
  lines(X, powers[i,] , type = "b", pch = point.shape[i],
  xlab = "Lags", ylab = "ACF", col = full.color.set[[i]] )
}

lab01 = expression(paste("|",R[t],"|",sep="") ^1)
lab02 = expression(paste("|",R[t],"|",sep="") ^2)
lab03 = expression(paste("|",R[t],"|",sep="") ^3)
lab04 = expression(paste("|",R[t],"|",sep="") ^4)
#lab05 = expression(paste("|",R[t],"|",sep="") ^5)

legend("topright",
       c(lab01,lab02,lab03,lab04), 
       lty = 1,
       col = c(pers.green,pers.blue,pers.red,pers.gray),
       pch = point.shape,
       cex = 1,
       bty = "n",
       ncol = 2,
       y.intersp = 2,
       )
dev.off()


dat <- getSymbols(sym, auto.assign = FALSE)
dat <- stock.series
df <- as.data.frame(dat)
df$rtrn <- 0

column <- paste(c("GOOG",".Adjusted"),collapse="")
print(column)

for(i in 2:nrow(df)){
  df[i,"rtrn"] <- abs(df[i,column]/df[(i-1),column]-1)
}



# ------------------------------------------------------------------------------
# Exercise 2
# ------------------------------------------------------------------------------

# Get sentiment data and compute lenth
sentiment.index <- getSymbols( "UMCSENT" , src = "FRED" , auto.assign = FALSE )

N <- nrow(sentiment.index)

# Plot sentiment series

png("p2.png")
plot( sentiment.index,main = "" )
dev.off()

# Compute ACF and PACF
sentiment.index <- returns( as.data.frame( sentiment.index)  )
sentiment.acf   <- acf( sentiment.index, plot =FALSE )

png("p3.png")
  plot(sentiment.acf,main="")
dev.off()

sentiment.pacf <- pacf( sentiment.index )

png("p4.png")
  plot(sentiment.pacf,main="")
dev.off()

# ARIMA(1,1,2)
m00 <- auto.arima( sentiment.index, ic = "aic" )

# for stationarity
adf.test( sentiment.index[,1] )
capture.output(adf.test,file="t0.txt")

# Differencing the data to get stationary data 
sentintement.diff <- diff( sentiment.index,diff=1 )[-1]

adf.test( sentintement.diff )
capture.output(adf.test,file="t02.txt")

# Fit auto arima model to find best fit  --> ARIMA(1,0,2)
m01 <- auto.arima( sentintement.diff,ic="aic",stationary=TRUE )
capture.output(m00,file="m01.txt")

# Square data and check for volatility clustering
sentiment.squared <- returns(as.data.frame(sentiment.index)$UMCSENT)**2
sentiment.squared <- sentiment.squared[-1]
sentiment.acfP2   <- acf( sentiment.squared )
sentiment.PacfP2  <- pacf( sentiment.squared )

capture.output(Box.test( sentiment.squared,type = "Ljung-Box" ),file="t03.txt")
am01 <- auto.arima( sentiment.squared,ic="aic",stationary=TRUE )
# Fit ARMA-GARCH model

m02 <- garchFit( UMCSENT ~ arma(1,2) + garch(1, 1), data = sentiment.index, trace = FALSE )
capture.output(m02,file="m02.txt")
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
pe10          <- dat$P.E10 


# Compute ratio series
ratio     <- ( real.dividend / real.price )
ratio.log <- as.data.frame(returns( ratio ,log = TRUE, center = TRUE ) )
splog     <- as.data.frame(returns(sp500,log=TRUE,center=FALSE) )
splog     <- splog[,1]
pe10log   <- as.data.frame(returns(pe10,log=TRUE,center=FALSE) ) 
len       <- length(splog)

# Create predictor data
predictor.data <- cbind(splog[1:(len-1)],ratio.log[2:len,])

# Find best SVM
svm.model <- smv.lag.grid.search(ratio.log, 
                                 splog, 
                                 response.name = "SP500",
                                 lag.array     = 1:3,
                                 include.self  = FALSE,   
                                 gamma.array   = c(seq(0.01,1,0.1),10), 
                                 cost.array    = c(seq(0.01,1,0.1),100), 
                                 data.split    = 0.80, 
                                 inner.trace   = FALSE, 
                                 outer.trace   = TRUE, 
                                 final.trace   = TRUE 
                                 )

# Neuronal Networks page 19 

## Start a local cluster with 2GB RAM
localH2O      <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

# Construct training and test data
lag.response  <- lagged.data.set(10,as.data.frame(splog) )
lag.predictor <- lagged.data.set(10,as.data.frame(ratio.log) )
N01           <- nrow(lag.response)
M01           <- ncol(lag.response)
N02           <- nrow(lag.predictor)
M02           <- ncol(lag.predictor)

resp          <- splog[(M  -N02+1):M  ]
full.data     <- cbind(resp,lag.response[,2:M01],lag.predictor)
split.data    <- dataSplit(full.data,0.80)

# Map to H20 data frame
training.h20  <- as.h2o(split.data$TrainingSet, destination_frame = "Training")
test.h20      <- as.h2o(split.data$TestSet, destination_frame = "Test")


#grid search 
list.of.layer <- list(c(200,200), c(100,300,100), c(500,500,500),c(100,100,100,100))
l1.optimal    <- c(1e-5,1e-7)
hyper_params  <- list( hidden = list.of.layer, l1 = l1.optimal)

model_grid    <- h2o.grid("deeplearning",
                          hyper_params = hyper_params,
                          x = 2:M01,
                          y = 1,
                          training_frame = training.h20,
                          validation_frame = test.h20
)

# print out the Test MSE for all of the models
for (model_id in model_grid@model_ids) {
  model <- h2o.getModel(model_id)
  mse <- h2o.mse(model, valid = TRUE)
  print(sprintf("Test set MSE: %f", mse))
}

# Construct deep neural network
m01 <- h2o.deeplearning(x = 2:M01,  
                        y = 1,   
                        training_frame   = training.h20,
                        validation_frame = test.h20,
                        activation       = "Tanh",
                        input_dropout_ratio   = 0.2, 
                        hidden_dropout_ratios = c(0.5,0.5,0.5,0.5), 
                        hidden = c(100,100,100,100), 
                        epochs = 300,
                        shuffle_training_data = FALSE
                        ) 


# ------------------------------------------------------------------------------
# Exercise 4
# ------------------------------------------------------------------------------

maeres <- function( residuals ){
  mean.absolute.error.of.residuals <- round(mean( abs( residuals ) ),5)
  return( mean.absolute.error.of.residuals )
} 
maeres <- function( residuals ){
  mean.absolute.error.of.residuals <- round(mean( ( residuals )**2 ),5)
  return( mean.absolute.error.of.residuals )
} 

series     <- returns(close,log=TRUE) [-1]
L          <- length(series) 
forecasts  <- 250 # number of 1-ahead forecasts 
outsamples <- series[(L-forecasts+1):L] # out-of-samples
insamples  <- series[1:(L-forecasts)] 
f1         <- rep( NA , forecasts )

garch.data <- as.data.frame(insamples)
colnames(garch.data) <- "series"
set.seed(12345) 
intial.value  <- 10
dim.insample  <- length(insamples)



garch    <- garchFit( series ~ arma(1,1) + garch(1, 1), data = garch.data, trace = FALSE )
pred     <- rep(NA,forecasts)
for(h in 1:forecasts){
  pred[h] <- predict(garch,n.ahead = 1,data = series[outsamples[1:h]])$meanForecast
  
}
  

e1    <- maeres(outsamples-pred)
e1
text1 <- paste("arima (MAE=",round(e1,digits=4),")",sep="")
ST    <- inputVariableSet( "X1" , "X2" ) 
FS    <- functionSet("+","*","-","/") 

gpts <- function( f, h = 0 ){ 
  if( h > 0 ){
    TS <- series
  } 
  else {
    TS <- series[ 1:dim.insample ]
  }

  dim.series <- length( TS )
  F <- rep( 0, dim.series ) 
  E <- rep( 0, dim.series ) 
  GARCH.chunk <- rep( 0, dim.series ) 
  MIXED.chunk <- rep( 0, dim.series ) 
  
  if( h > 0 ){
    I <- ( dim.series - h + 1 ):dim.series 
  } 
  else{
    I <- intial.value:dim.series
  } 
  for( i in I )
  { 
    F[i] <- f( TS[i-1] , TS[i-2] )
    
    # I ADDED SOME STUFF HERE
    GARCH.chunk[i] <- f( TS[i-1]**2 , TS[i-2]**2 )
    MIXED.chunk[i] <- f(  F[i] ,GARCH.chunk[i])
    
    #   F[i] <- f( TS[i-1], TS[i-2], TS[i-3] ) ##to work with 3 vars
    if( is.nan(F[i]) ){
      F[i] <- 0
    }
    E[i] <- TS[i] - F[i]
  }
  if( h > 0 ){
    return( F[I] )
  }  
  else{
    m.errors <- maeres( E[I] )
    return( m.errors )
  }  
}


CF1 <- constantFactorySet( function() rnorm( 1 ) )

mut <- function( func ){ 
  mutateSubtree( func, funcset = FS, inset = ST, conset = CF1,
                mutatesubtreeprob = 0.3, maxsubtreedepth = 4 )
  }


gp <- geneticProgramming(functionSet = FS,
                         inputVariables = ST,
                         constantSet = CF1,
                         populationSize = 500,
                         fitnessFunction = gpts,
                         stopCondition = makeStepsStopCondition(50),
                         mutationFunction = mut,
                         verbose = TRUE 
                        )

f2=gpts(gp$population[[which.min(gp$fitnessValues)]],h=forecasts)
e2 <- maeres( outsamples - f2 )
e2
text2=paste("gp (MAE=",round(e2,digits=4),")",sep="")
cat("best solution:\n")
print(gp$population[[which.min(gp$fitnessValues)]])
cat("gp fit MAE=",min(gp$fitnessValues),"\n")

# show quality of one-step ahead forecasts: 
ymin=min(c(outsamples,f1,f2))
ymax=max(c(outsamples,f1,f2))
#pdf("finsamples.pdf")
par(mar=c(4.0,4.0,0.1,0.1))
plot(outsamples,ylim=c(ymin,ymax),type="b",pch=1,
     xlab="time (months)",ylab="values",cex=0.8)
lines(f1,lty=2,type="b",pch=3,cex=0.5)
lines(f2,lty=3,type="b",pch=5,cex=0.5)
legend("topright",c("insamples",text1,text2),lty=1:3,pch=c(1,3,5))


################################################################################

################################################################################

