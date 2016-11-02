# ------------------------------------------------------------------------------
# Compute returns from 
# ------------------------------------------------------------------------------

returns <- function( series, log = FALSE, abs = FALSE, center = FALSE, scale = FALSE ){
  # Indentify class of series
  series.class <- class( series )
  # Depending on object class define lagged and future series
  if( series.class == "data.frame" | series.class == "matrix" ){
    # Compute dimensions of input series
    N      <- nrow( series )
    # Compute ahead and lagged values
    future <- series[ 2:N, ]
    past   <- series[ 1:( N - 1), ]  
  } else {
    # Compute length of series in case of class numeric
    N      <- length( series )
    # Compute ahead and lagged values
    future <- series[ 2:N ]
    past   <- series[ 1:( N - 1) ] 
  }
  # Compute returns
  returns <- (future / past) - 1 
  # Run additional options
  if( log == TRUE ){
    returns <- returns + 1
    returns <- log( returns )
  }
  if( abs == TRUE ){
    returns <- abs( returns )
  }
  # Final adjustments
    returns <- as.data.frame( scale( returns, center = center, scale = scale ) )
  # return modified prices series
  return( returns )
}

# ------------------------------------------------------------------------------
# Compute a data set with lagged data
# ------------------------------------------------------------------------------

lagged.data.set <- function( nlag, data ){
  # Initialize dataframe and colnames
  data <- as.data.frame(data)
  lagged.data <- data
  colnames    <- colnames( data )
  # Run lagged columns according to arguments
  for ( lag in 1:nlag ){
    # Compute lagged values and append to data frame
    lagged.cols  <- data[ 1:(nrow(data) - lag ),colnames]
    all.colnames <- colnames(lagged.data)
    lagged.data  <- lagged.data[2:nrow(lagged.data),]
    lagged.data  <- cbind(lagged.data, lagged.cols)
    # Update column names
    colnames(lagged.data) <- append(all.colnames, sapply(colnames, paste0, paste0('.lag',lag)))
  }
  # Return data frame
  return(lagged.data)
}



# ------------------------------------------------------------------------------
# Data split function
# ------------------------------------------------------------------------------

dataSplit <- function(data,size){
  # Define dimensions of data split
  N 	<- nrow(data) 
  S   <- 1:N
  M 	<- ceiling( size * N )
  D 	<- N - M 
  # Sample the rows according to define dimensions
  dimTR <- 1:M
  dimTE <- (M+1):N
  # Define Training set and test set
  trainingSet <- data[dimTR,]
  testSet 	<- data[dimTE,]
  # Define output list and return it
  output <- list( TrainingSet=trainingSet, TestSet=testSet )
  # Return output
  return(output)
}

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

smv.lag.grid.search <- function( predictor, response, response.name = "SP500" ,lag.array = 1:10,
                                 include.self = TRUE,   
                                 gamma.array =  0.5:4, cost.array =  1:10, data.split = 0.99, 
                                 inner.trace = FALSE, outer.trace = TRUE, final.trace = TRUE ) {
  
  # Init storage objects
  global.mse.svm <- c()
  global.gam.svm <- c()
  global.cos.svm <- c()
  gamma <- gamma.array
  cost  <- cost.array
  
  for( h in lag.array ){
    
    ratio.lagged   <- lagged.data.set( h , predictor )
    N.ratio.lagged <- nrow(ratio.lagged)
    M <- length(response)
    index <-  (M - N.ratio.lagged + 1):M
    
    data <- as.data.frame( cbind( response[ index ], ratio.lagged ) )
    
    colnames(data)[1] <- response.name
    #if(include.self == TRUE){
    #  self <- as.data.frame(response)
    #  self.lagged <- lagged.data.set(h, self )
    #  N.self <- nrow(self.lagged)
    #  M.self <- ncol(self.lagged)
    #  self.lagged.adjusted <- as.data.frame(2:self.lagged[2:N.self,2:M.self])
    #  colnames(self.lagged.adjusted) <- sapply(1:(M.self-1),function(i){
    #    paste0("self",i)
    #  })
     # data <- cbind(data,self.lagged.adjusted)
    #}

    split      <- dataSplit( data, data.split )
    training   <- split$TrainingSet
    test       <- split$TestSet
    N.training <- nrow( training )
    M.training <- ncol( training )
    N.test     <- nrow( test )
    M.training <- ncol(test)
    
    y.test <- test$SP500
    x.test <- test[,setdiff(colnames(test),c(response.name)) ]

    # Support Vector Macines (SVM) - Grid Search over Parameters
    
    N.gamma      <- length( gamma )
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
                        kernel = "radial", gamma = temp.gamma, cost = temp.cost,
                        cross = 0)
        temp.prediction <- predict(tempsvm, newx = x.test )
        temp.mse <- mean( ( y.test - temp.prediction )**2 )
        mse[i,j] <- temp.mse
        model.name   <- paste0( "svmGAM",temp.gamma,"COS",temp.cost )
        
        model.list[[name.counter]] <- tempsvm  
        names( model.list )[name.counter] <- model.name
        name.counter <- name.counter + 1
        if( inner.trace == TRUE ){
          cat("Itteration: i =", i,"/","j =",j,"\n" ) 
        }
        
      }
      
      if( i == N.gamma ){
        best.ind   <- which(mse == min(mse), arr.ind = TRUE)
        best.gamma <- gamma[best.ind[1]]
        best.cost  <- cost[best.ind[2]]
        if(outer.trace == TRUE){
          cat("\n",
              ".","\n",
              ".","\n",
              ".","\n"
             )
          Sys.sleep(1)
          cat("\n",
              "------------------------------------","\n", 
              "SUB GRID SEARCH FINISHED","\n",
              "------------------------------------","\n", 
              "Number of Lags in Data:", h, "\n",
              "Training Observations:", N.training  ,"\n",
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
      
    }
    
    
    global.mse.svm <- c(global.mse.svm, min(mse))
    global.gam.svm <- c(global.gam.svm, best.gamma)
    global.cos.svm <- c(global.cos.svm, best.cost)
    
  }
  
  if( final.trace == TRUE){
    h.opt    <- lag.array[which.min( global.mse.svm )]
    gam.opt  <- min(global.gam.svm)
    cost.opt <- min(global.cos.svm)
    mse.opt  <- min(global.mse.svm)
    cat("\n",
        "************************************","\n",
        "------------------------------------","\n", 
        "FULL GRID SEARCH FINISHED","\n",
        "------------------------------------","\n", 
        "Number of Lags in Data:", h.opt, "\n",
        "Training Observations:", N.training  ,"\n",
        "Test Observations:", N.test, "\n",
        "BEST PARAMETER SPECIFICATION:","\n",
        "GAMMA:",gam.opt,"\n",
        "COST:", cost.opt, "\n",
        "MSE:", mse.opt,"\n",
        "------------------------------------","\n", 
        "\n",
        "------------------------------------","\n",
        "************************************","\n"
    )
    
  }
  final.output = list( mse = global.mse.svm, gamma = global.gam.svm, cost = global.cos.svm  )
  return( final.output )
}
