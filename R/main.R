#' speCluster
#' @description Perform Spectral Clustering on a data matrix
#' @param data A numeric data frame or matrix where columns represent variables and rows represent locations
#' @param conMatrix Contiguity matrix.
#' @param cluster.number The number of clusters.
#' @param iter.max The maximum number of iterations allowed for kmeans step.
#' @param repetition How  many  random  sets  should  be  chosen for  as  the  initial centers in kmeans step.
#' @export
#' @return A list contains two parts:
#'         clusters: A vector of integers(from 1:cluster.number)
#'         indicating the cluster to which each point is allocated.
#'        SS: A list with two values SSW for Sum Squered Within and
#'                  SSB for SumSquered Between

speCluster <- function(data, conMatrix, cluster.number,
                       iter.max = 400, repetition = 400 ){

  # Preprocess
  outId <- outlierDetector(data)
  dataAfterPC <- prinComp(data = data, outId = outId)
  # rm(data)

  # Spectral clustering Algorithm
  S <- similarity(data = dataAfterPC, neighbors=conMatrix)
  # rm(outId, conMatrix)
  U <- produceU( similarity = S, ncol = cluster.number)
  # rm(S)
  clusters <- kmeansU(data=U, cluster.number = cluster.number,iter.max=500)

  # postprocess
  SS <- sumSquares(data=dataAfterPC, clusters= clusters)

  out <- list(clusters= clusters,SS= SS)
  return(out)
}

#' hspecCluster
#' @description Perform hierarchical Spectral Clustering on a data matrix
#' @inheritParams speCluster
#' @export
#' @examples \dontrun{
#' example("generateData", run.dontrun = TRUE)
#' results <- hspeCluster(data = input$data, conMatrix = input$conMatrix,
#'                cluster.number = 8)
#' mapping(lat = input$latLong[,1], long = input$latLong[,2],
#'     clusters = results$clusters[,7])
#' }
hspeCluster <- function(data, conMatrix, cluster.number,
                        iter.max = 400, repetition = 400 ){

  #Preprocess
  outId <-outlierDetector(data)
  dataAfterPC <- prinComp(data=data,outId=outId)
  rm(data, outId)

  # Spectral clustering Algorithm

  results <- matrix(nrow=nrow(dataAfterPC),ncol=(cluster.number-1))
  S <- similarity(data = dataAfterPC , neighbors=conMatrix)
  U <- produceU( similarity = S, ncol=2)
  results[,1] <- kmeansU(data=U, cluster.number = 2,iter.max=500)
  SS <- sumSquares(data=dataAfterPC, clusters= results[,1])

  for(i in 3:cluster.number){
    results[,i-1] = results[,i-2]
    print(cat("SSW: ", SS$SSWlist))
    splitCluster <- which.max(SS$SSWlist)
    message(sprintf("iter=%d, split cluster %d\n", i, splitCluster))
    j <- which(results[,i-1]==splitCluster)
    S <- similarity(data = dataAfterPC[j,], neighbors=conMatrix[j,j])
    U <- produceU( similarity = S, ncol=2)
    clusters <- kmeansU(data=U, cluster.number = 2,iter.max=500)
    k <- which(clusters==1)
    results[j[k],i-1] = splitCluster
    k <- which(clusters==2)
    results[j[k],i-1] = i
    SS <- sumSquares(data=dataAfterPC, clusters= results[,i-1])
  }

  out <- list(clusters= results,SS= SS)
  return(out)
}
