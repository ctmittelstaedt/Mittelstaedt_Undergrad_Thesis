# Calculating MFCCs to reduce the dimentionality of pika calls in prep for
# Linear Discriminant Analysis

install.packages("tuneR")
install.packages("seewave")
install.packages("MASS")
install.packages("e1071")
install.packages("stringr")
install.packages("ggplot2")
install.packages("viridis")
install.packages("apcluster")
install.packages("tidyverse")

##### Load required libraries
library(tuneR)
library(seewave)
library(MASS)
library(e1071)
library(stringr)
library(ggplot2)
library(viridis)
library(apcluster)
library(tidyverse)

##### Save all .wav files and set working directory to access the files
setwd("C:\\PythonGitHub\\Mittelstaedt_Undergrad_Thesis\\Individuality\\Whole_dataset_potential_ind_clips\\Edited_PP_individual_clips")

input.dir <-
  "C:\\PythonGitHub\\Mittelstaedt_Undergrad_Thesis\\Individuality\\Whole_dataset_potential_ind_clips\\Edited_PP_individual_clips"

####List all .wav files in directory
L = list.files(input.dir, pattern = "*.wav", full.names = FALSE)
L

####Extract file names
filehandles <- str_split_fixed(L, pattern = ".wav", n = 2)[, 1]

####Create empty list to hold MFCC values
mfcc.vector.list = list()

####Loop to calculate MFCC for each .wav file in the directory
for (j in 1:length(filehandles)) {
  tryCatch({
    filehandle <-  L[j]
    filename <- paste(input.dir, "/", filehandle, sep = "")
    print(paste("processing", filehandle))
    
    # Read in wav file and filter
    w <- readWave(filename)
    
    # Find duration of .wav file and divide into 5 windows
    wav.dur <- length(w) / w@samp.rate
    win.time <- wav.dur / 9
    
    # Calculate MFCCs
    melfcc.output <- melfcc(
      w,
      minfreq = 700,
      hoptime = win.time,
      maxfreq = 8000,
      numcep = 12,
      wintime = win.time
    )
    
    # Calculate delta cepstral coefficients
    deltas.output <- deltas(melfcc.output)
    
    # Ensure only 4 time windows are used for MFCC and delta coefficients
    # Also append .wav duration
    mfcc.vector <-
      c(as.vector(t(melfcc.output[1:8, 2:12])), as.vector(t(deltas.output[1:4, 2:12])), wav.dur)
    
    # Add to list
    mfcc.vector.list[[j]] <- mfcc.vector
  }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
  })
}

####Convert the list to a vector
vec <- unlist(mfcc.vector.list)

####Split file names into site (female) id and call id
loc <- str_split_fixed(filehandles, pattern = "-", n = 2)[, 1]
id <- str_split_fixed(filehandles, pattern = "-", n = 2)[, 2]

####Create a matrix with MFCC and delta coefficient values
####The number of columns is a reflection of the number of time windows
data.matrix.all <-
  matrix(
    vec,
    nrow = length(filehandles),
    ncol = length(mfcc.vector.list[[1]]),
    byrow = T
  )

mfcc.data.frame <-
  cbind.data.frame(loc, filehandles, id, data.matrix.all)

seq.length <- (length(mfcc.vector.list[[1]]) - 1) / 2

col.names <-
  c(
    "loc",
    "filehandles",
    "id",
    paste(rep("mfcc", seq.length), seq(seq.length), sep = "_"),
    paste(rep("delta", seq.length), seq(seq.length), sep = "_"),
    "dur"
  )

colnames(mfcc.data.frame) <- col.names

mfcc.data.frame <- as.data.frame(mfcc.data.frame)

####Check data structure
str(mfcc.data.frame)

#remove non-MFCC columns
x <- mfcc.data.frame |> select(-c(loc, filehandles, id, dur))
x
x2 <- x |> select(1:136)
x2


## run affinity propagation
apres <- apcluster(negDistMat(r=2), x2, q=0.7, details=TRUE)

## plot information about clustering run
plot(apres)

## plot clustering result*
plot(apres, x2)

## perform agglomerative clustering of affinity propagation clusters
aggres1 <- aggExCluster(x=apres)

## show dendrograms
plot(aggres1)
plot(aggres1, showSamples=TRUE)


## show clustering result for 4 clusters*
plot(aggres1, x2, k=4)

## perform agglomerative clustering of whole data set
aggres2 <- aggExCluster(negDistMat(r=2), x2)

## show dendrogram
plot(aggres2)

## show heatmap along with dendrogram
heatmap(aggres2)

## show clustering result for 2 clusters*
plot(aggres2, x2, k=2)



