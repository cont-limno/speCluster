
#' Similarity
#' @importFrom stats median
#' @description Compute similarity matrix then multiply it to contiguity matrix
#' @param data n by p numeric matrix or data frame.
#' @param neighbors n by n numeric matrix which specifies contiguity matrix.
#' @return similarity: The similarity matrix

similarity <- function(data, neighbors){
  dist <- as.matrix(dist(data) )
  sigma <- stats::median(dist)
  dist <- exp(-dist^2/(2*sigma^2))

  similarity <- dist*neighbors
  return(similarity)
}

#' ProduceU
#' @description Given n by n similarity this function first calculate the Laplacian
# matrix L then generate n by ncol matrix U of top ncol eigenvectors of L.
#' @importFrom geigen geigen
#' @importFrom rARPACK eigs
#' @param similarity an n by n matrix
#' @param type The algrithm that should be choosen. options are 1, 2, and 3
#' @param ncol number of columns of the output matrix U
#' @param all.eig a logical value indicating whether all the eigenvector should be compute or not
#' @return n by ncol numeric matrix that contains the ncol tops
#'        eigenvectors of Laplacian matrix as column

produceU <- function(similarity, ncol, type = 2, all.eig = FALSE){

  if (!is.element(type,1:3)){
    stop("argument type must be on of 1,2,or 3")
  }
  if(type==2){
    warning("Type 2 algorithm might  need more than 4.0 G Ram")
  }

#calculate degree matrix
  diag <- apply(similarity , 1 , sum)
  l <- length(diag)
  D <- Matrix::sparseMatrix(1:l,1:l,x=diag)
  L <- D - similarity
# Compute Normalized Laplacian
if( type == 2){
  L <- as.matrix(L)
  D <- as.matrix(D)
  #start <- Sys.time()
  eig <- geigen::geigen(L,D, symmetric = TRUE)
  rm(L,D)
  #V <- eig$values
  U <- eig$vectors[,1:ncol]
  #Sys.time()-start
  return(U)
}
if(type ==3){
  #avoid dividing by zero
  diag[diag==0] <- .Machine$double.eps
  # calculate D^(-1/2)
  diag <- sqrt(diag)
  diag <- 1/diag
  D <- Matrix::sparseMatrix(1:l,1:l,x=diag)
  #Calcualte Normalized Laplacian
  L <- D%*%L%*%D
}
rm(diag, D)
#start<- Sys.time()
if(all.eig){
  eigen <-eigen(L)
  #Sys.time()-start
  U <- eigen$vectors[,1:ncol]
}else{
  eigen <- rARPACK::eigs(L,ncol,sigma = 0)
  U <- eigen$vectors
}

if(type==1){
  return(U)
}
# In case of the Jordan-Weiss algorithm,
# we need to normalize the eigenvectors row-wise
if(type==3){
  s <- sqrt(apply(U^2,1,sum))
  for(i in seq_len(nrow(U))){
    U[i,] <- U[i,]/s[i]
  }
  return(U)
}

}

#' kmeansU
#' @description Perform k-means clustering on the U matrix.
#' @param data numeric matrix U
#' @param cluster.number The number of clusters.
#' @param iter.max The maximum number of iterations allowed
#' @param repetition How many random sets should be chosen for
#'                  as the initial centers
#' @return cluster: A vector of integers(from 1:cluster.number) indicating the cluster to each point is allocated
#' @importFrom stats kmeans

kmeansU <- function(data, cluster.number,
                   repetition = 400, iter.max = 400){

  data <- data[,1:cluster.number]
  out <- kmeans(data, centers= cluster.number,
               nstart=repetition, iter.max = iter.max)
  return(out$cluster)
}

