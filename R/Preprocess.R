
#File description comment, including purpose of program, inputs
# and outputs

#' neighborMatrix
#' @description Compute constraint Matrix
#' @param NB data.frame neighbor list
#' @param conFactor integer contiguity constraint factor
#' @export
neighborMatrix <- function(NB, conFactor = 1){

  conMatrix <- Matrix::sparseMatrix(NB[,1], NB[,2], x = rep(1, nrow(NB)))

  if(conFactor >= 2){
    n <- nrow(conMatrix)
    nb <- Matrix::sparseMatrix(i = {}, j = {}, dims = c(nrow(conMatrix),
                                                        ncol(conMatrix)))
    #sparse identity matrix
    NBt <- Matrix::sparseMatrix(1:n, 1:n, x = rep(1,n))
    for(i in 1:conFactor){
      NBt <- NBt %*% conMatrix
      nb  <- nb + NBt
    }
    nb[nb != 0] <- 1
    diag(nb) <- 0
    conMatrix <- nb
  }

  conMatrix
}

outlierDetector <- function(data, outlier.Threshold = 0.2 ){
  # Compute the outlier of the data using Principal component
  #
  # Args:
  #     data: a numeric data frame or matrix
  #     outlier.Threshold: The Threshold which makes a data outlier.
  #
  # Returns:
  #    outId: A logical vecotor which specifies all the outliers.
  #
  # Error handeling

  #Principal Component
  pc <- stats::prcomp(data, scale = TRUE, center = TRUE)
  var <- pc$sdev^2
  cvar <- var / sum(var)
  n <- 1
  s <- cvar[n]
  while(s < 0.85){
    n <- n + 1
    s <- s + cvar[n]
  }
  dataNew <- pc$x[,1:n]
  # rm("pc","data")

  #Similarity calculation
  dist <- as.matrix(dist(dataNew))
  sigma <- stats::median(dist)
  dist <- exp(-dist^2 / (2*sigma^2))

  #Outliers detection
  diag(dist) <- 0
  i <- apply(dist, 1, max)
  outId <- i <= outlier.Threshold
  return(outId)
}

#' prinComp
#' @description Run the pricnipal componenet algroithm on the data
#' to reduce dimension
#' @param data a numeric data frame or matrix
#' @param outId A logical vecotor which specifies all the outliers.
#' @param showPC A logical value indicating whether principal compunent should be return or not.
#' @return dataNew: After Principal component data
#' @importFrom stats prcomp

prinComp <- function(data, outId, showPC = FALSE){

  outSize <-sum(outId)
  if(outSize != 0){
    colmean <- apply(data,2,mean)
    data[outId,] <- matrix(colmean, nrow = outSize, ncol= length(colmean),
                           byrow = TRUE)
  }
  # rm(colmean,outSize)
  pc <- stats::prcomp(data, scale = TRUE, center = TRUE)
  var <- pc$sdev^2
  cvar <- var / sum(var)
  n <- 1
  s <- cvar[n]
  while(s < 0.85){
    n <- n + 1
    s <- s + cvar[n]
  }
  dataNew <- pc$x[,1:n]
  if(showPC){
    return(pc)
  }
  return(dataNew)
}
