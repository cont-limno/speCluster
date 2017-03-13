# library("Matrix")
# library("geigen")
# library("rARPACK")
# library(maps)
# library(WDI)
# library(RColorBrewer)
# library("maptools")

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
#' @examples \dontrun{
#' speCluster()
#' }

speCluster <- function(data, conMatrix, cluster.number,
                       iter.max=400, repetition= 400 ){

  # Preprocess
  outId <- outlierDetector(data)
  dataAfterPC <- prinComp(data=data,outId=outId)
  rm(data)

  # Spectral clustering Algorithm
  S <- similarity(data = dataAfterPC , neighbors=conMatrix)
  rm(outId, conMatrix)
  U <- produceU( similarity = S, ncol=cluster.number)
  rm(S)
  clusters <- kmeansU(data=U, cluster.number = cluster.number,iter.max=500)

  # postprocess
  SS <- sumSquares(data=dataAfterPC, clusters= clusters)

  out <- list(clusters= clusters,SS= SS)
  return(out)
}
