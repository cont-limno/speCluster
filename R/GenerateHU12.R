
#load spectral clustering functions
# source("main.R")

# Read data
dataTerr <- read.csv("data/terrData.csv", header=T)
dataFW <- read.csv("data/freshData.csv", header=T)
i <- which(colnames(dataFW)=="hu12_states")
dataTerrFW <- merge(dataTerr, dataFW[-i], by.x="zoneid", by.y="zoneid")
NB18876 <- read.csv("data/NB_18876.csv", header=T)
islands <- read.csv("data/islandIdx.csv", header=T)
latLong18876 <- read.csv("data/latLong18876.csv", header=T)

rm(i)

# function definition
NBindex <- function(index){
  id <- which(index)
  NB <- data.frame()
  for( i in 1:nrow(NB18876) ){
    if( (NB18876[i,"row"] %in% id) &
          (NB18876[i,"neighbor"] %in% id)){
     NB <- rbind(NB,NB18876[i,c("row","neighbor")])
    }
  }
  hash <- 1:length(id)
  names(hash) <- id
  for(i in 1:nrow(NB)){
    NB[i,1] <- hash[as.character(NB[i,1])]
    NB[i,2] <- hash[as.character(NB[i,2])]
  }
  return( NB)
}


# Second Load sepectralClustering file

save.image(file="HU12SpectralClustering.RData")
