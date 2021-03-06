################################################################################
### Preamble
################################################################################

### Clear workspace
rm(list = ls())

### Set working directory
setwd("YOUR DIRECTORY")

### Load Packages 
if (!require("e1071")) install.packages("e1071"); library(e1071)
if (!require("h2o"))   install.packages("h2o");   library(h2o)

### Initialize auxilliary functions
source("Auxilliary_Functions_PS1.R")

### Load Data
dat <- read.csv( "SP500_shiller.csv" )

################################################################################

################################################################################

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


#-------------------------------------------------------------------------------
# Support Vector Machines
#--------------------------------------------------------------------------------


# Find best SVM
svm.model <- smv.lag.grid.search(ratio.log, 
                                 splog, 
                                 response.name = "SP500",
                                 lag.array     = 1:12,
                                 gamma.array   = c(seq(0.01,1,0.1),10), 
                                 cost.array    = c(seq(0.01,1,0.1),100), 
                                 data.split    = 0.80, 
                                 inner.trace   = FALSE, 
                                 outer.trace   = TRUE, 
                                 final.trace   = TRUE 
)


#-------------------------------------------------------------------------------
# Neuronal Networks
#--------------------------------------------------------------------------------

# The grid search is done applying the example in [Candel et.al., 2004] page 19

## Start a local cluster with 2GB RAM
localH2O      <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)

# Construct training and test data
lag.response  <- lagged.data.set(10,as.data.frame(splog) )
lag.predictor <- lagged.data.set(10,as.data.frame(ratio.log) )
N01           <- nrow(lag.response)
M01           <- ncol(lag.response)
N02           <- nrow(lag.predictor)
M02           <- ncol(lag.predictor)

# Create data
resp          <- splog[(M  -N02+1):M  ]
full.data     <- cbind(resp,lag.response[,2:M01],lag.predictor)
split.data    <- dataSplit(full.data,0.80)

# Map to H20 data frame
training.h20  <- as.h2o(split.data$TrainingSet, destination_frame = "Training")
test.h20      <- as.h2o(split.data$TestSet, destination_frame = "Test")

# Run grid search
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

################################################################################

################################################################################