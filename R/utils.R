stepOne <- function(data, conMatrix, ncol){
  # This function Computes the data after Principal component
  #
  #
  # Args:
  #     data: A numeric data frame or matrix.
  #     conMatrix: Contiguity matrix.
  #     ncol: number of columns of the output matrix U
  #
  #
  # Returns:
  #     A list contains two parts:
  #     dataAfterPC: After Principal component data
  #     U: n by ncol numeric matrix that contains the ncol tops
  #       eigenvectors of Laplacian matrix as column.
  #
  # Error handeling

  #Preprocess
  outId <-outlierDetector(data)
  dataAfterPC <- prinComp(data=data,outId=outId)
  rm(data)

  # Spectral clustering Algorithm
  S <- similarity(data = dataAfterPC, neighbors=conMatrix)
  rm(outId, conMatrix)
  U <- produceU( similarity = S, ncol=ncol)
  out <- list( dataAfterPC=dataAfterPC, U=U)
  return(out)
}

stepTwo <- function(data, U, cluster.number= cluster.number,
                    iter.max=400, repetition=400){
  # Perform Spectral Clustering on U matrix.
  #
  # Args:
  #     data: A numeric data frame or matrix.
  #     U: A numeric matrix
  #     cluster.number: The number of clusters.
  #     iter.max: The maximum number of iterations allowed for
  #               kmeans step.
  #     repetition: How  many  random  sets  should  be  chosen
  #                 for  as  the  initial centers in kmeans step.
  #
  # Returns:
  #     A list contains two parts:
  #        clusters: A vector of integers(from 1:cluster.number)
  #                  indicating the cluster to which each point is
  #                  allocated.
  #        SS: A list with two values SSW for Sum Squered Within and
  #                  SSB for SumSquered Between
  # Error handeling


  clusters <- kmeansU(data=U, cluster.number = cluster.number,
                      iter.max=iter.max, repetition=repetition)
  SS <- sumSquares(data=data, clusters= clusters)

  out <- list(clusters= clusters,SS= SS)
  return(out)
}


#' generateData
#' @description Generate the data for clustering
#' @param type three options "dataTerr", "dataFW", and "dataTerrFW"
#' @param islandsIn if TRUE the islands will be included
#' @param islands vector of islands?
#' @param latLong coordinates
#' @param NB18876 neighbor data.frame?
#' @param states a vector of states names that have to be included
#' @param conFactor contiguity constraint factor
#' @importFrom stats var
#' @export
#' @return a list with three elements: data, conMatrix, and latLong
#' @examples \dontrun{
#' dataTerr     <- read.csv("data-raw/terrData.csv", header = TRUE)
#' dataFW       <- read.csv("data-raw/freshData.csv", header = TRUE)
#' i <- which(colnames(dataFW) == "hu12_states")
#' dataTerrFW   <- merge(dataTerr, dataFW[-i], by.x = "zoneid", by.y = "zoneid")
#' islands      <- read.csv("data-raw/islandIdx.csv", header = TRUE)
#' latLong18876 <- read.csv("data-raw/latLong18876.csv", header = TRUE)
#' NB18876      <- read.csv("data-raw/NB_18876.csv", header = TRUE)
#'
#' input <- generateData(type = dataTerrFW, islands = islands,
#' latLong = latLong18876, NB18876, islandsIn = FALSE, states = c("MO"),
#' conFactor = 1)
#' }

generateData <- function(type, islands, latLong, NB18876, islandsIn = FALSE,
                         states = vector(), conFactor = 1){

  if(!is.logical(islandsIn)){
    stop("islandsIn must be logical variable.")
  }

  allStates <- levels(type$hu12_states)
  allStates <- allStates[nchar(allStates) == 2]
  if(!is.vector(states)){
    print(allStates)
    stop("The state variable must be a vector containing a subset of
         the above state list")
  }
  if(sum(states %in% allStates)!= length(states)){
    stop("The state variable must be a vector containing a subset of
         the above state list")
  }

  # finding the row index
  index <- rep(TRUE, nrow(type))

  # make index of islands False if islands are not included
  if(islandsIn == FALSE){
    index[c(islands)$x] <- FALSE
  }

  if(length(states) > 0){
    id <- rep(FALSE, nrow(type))
    outStates <- allStates[!allStates %in% states]
    for(i in seq_along(outStates)){
      state <- outStates[i]
      id <- grepl(state, type$hu12_states) | id
    }
    index <- index & !id
  }

  # generate the output data
  data <- type[index, -c(1, 2)]
  n    <- nrow(data)
  m    <- ncol(data)
  data <- as.matrix(data)
  data <- as.numeric(data)
  data <- matrix(data, nrow = n, ncol = m)
  data <- rm_constant_columns(data)

  latLong   <- latLong[index,]

  NB        <- NBindex(index, NB18876)
  browser()
  conMatrix <- neighborMatrix(NB, conFactor = conFactor)

  list(data = data, conMatrix = conMatrix, latLong = latLong)
}

NBindex <- function(index, NB18876){
  id <- which(index)
  NB <- data.frame()

  for(i in seq_len(nrow(NB18876))){
    if((NB18876[i,"row"] %in% id) &
        (NB18876[i,"neighbor"] %in% id)){
      NB <- rbind(NB, NB18876[i,c("row", "neighbor")])
    }
  }

  hash <- seq_along(id)
  names(hash) <- id
  for(i in seq_len(nrow(NB))){
    NB[i,1] <- hash[as.character(NB[i,1])]
    NB[i,2] <- hash[as.character(NB[i,2])]
  }

  return(NB)
}

#' rm_constant_columns
#' @export
rm_constant_columns <- function(data){
  colSum    <- apply(data, 2, stats::var)
  constants <- which(colSum == 0)
  if(length(constants) != 0){
    data <- data[,-constants]
  }
  data
}

#' nb_collapse
#' @export
#' @examples \dontrun{
#' nb_c <- nb_collapse(nb)
#' }
nb_collapse <- function(nb){
  nb <- data.frame(do.call("rbind", lapply(1:length(nb),
                                function(x) cbind(x, nb[[x]]))))
  names(nb) <- c("row", "neighbor")
  nb
}

# nb_e <- nb_expand(nb_c)
nb_expand <- function(nb){
  nb <- split(nb, f = nb$row)
  lapply(nb, function(x) x[,2])
}


plot_nb <- function(coords, index, xbuff = 0.1, ybuff = 0.1){
  pnts <- sf::st_multipoint(coords[,c(2,1)])
  plot(pnts,
       xlim = coords[index,][2] + c(-1 * xbuff, xbuff),
       ylim = coords[index,][1] + c(-1 * ybuff, ybuff),
       type = "n")
  text(coords[,c(2,1)])
}
