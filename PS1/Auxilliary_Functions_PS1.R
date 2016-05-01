# ------------------------------------------------------------------------------
# Compute Log returns
# ------------------------------------------------------------------------------

log.returns <- function( series, abs = FALSE ){
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
  # Compute log returns
  log.returns <- log( future / past )
  # Check if absolute returns should be computed
  if( abs == TRUE ){
    # Compute and return absolute log returns
    absolute.log.returns <- abs( log.returns )
    return( absolute.log.returns )
  } else {
    # Return log returns
    return( log.returns )
  }
}

# ------------------------------------------------------------------------------
# Compute a data set with lagged data
# ------------------------------------------------------------------------------

lagged.data.set <- function( nlag, data ){
  # Initialize dataframe and colnames
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
# Check if a series is monotonically decreasing
# ------------------------------------------------------------------------------

monotonically.decreasing <- function( series ){
  bool <- all(series == cummin(series))
  return(bool)
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
  dimTR <- sample(N,M,replace=FALSE)
  dimTE <- setdiff(S,dimTR)
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