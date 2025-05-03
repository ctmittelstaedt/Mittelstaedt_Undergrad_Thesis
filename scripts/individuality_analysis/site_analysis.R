##################################################################################
# Extracting MFCCs from pika calls and conducting AP clustering on population calls
# Charlotte Mittelstaedt - The University of British Columbia
# Code adapted from Clink & Klinck (2021).https://doi.org/10.1111/2041-210X.13520
# Created 1 February 2025
##################################################################################

# Load required libraries
library(tuneR)
library(seewave)
library(MASS)
library(e1071)
library(stringr)
library(ggplot2)
library(viridis)
library(apcluster)
library(tidyverse)
library(dendextend)

# Define input directory
input.dir <- file.path("data","individuality", "site_analysis")

# List all .wav files in directory
L = list.files(input.dir, pattern = "*.wav", full.names = TRUE, recursive = TRUE)
L

# Extract file names
filehandles <- str_split_fixed(L, pattern = ".wav", n = 2)[, 1]

# Create empty list to hold MFCC values
mfcc.vector.list = list()

# Calculate MFCCs
mfcc.vector.list <- lapply(L, function(filehandle) {
  tryCatch({
    print(paste("processing", filehandle))
    
    # Read in wav file and filter
    w <- readWave(filehandle)
    
    # Find duration of .wav file and divide into 5 windows
    wav.dur <- length(w) / w@samp.rate  # Duration of the .wav file in seconds
    win.time <- wav.dur / 6  # Duration of each window, dividing the audio into 5 equal windows
    
    # Calculate MFCCs
    melfcc.output <- melfcc(
      w, 
      minfreq = 700, 
      hoptime = win.time, 
      maxfreq = 9000, 
      numcep = 12, 
      wintime = win.time
    )
    
    # Calculate delta cepstral coefficients
    deltas.output <- deltas(melfcc.output)
    
    # Ensure correct number of time windows are used for MFCC and delta coefficients
    # Also append .wav duration
    # Extract the first 5 windows of MFCCs and deltas
    mfcc.vector <- c(
      as.vector(t(melfcc.output[1:5, 2:12])),  # First 5 time windows of MFCCs
      as.vector(t(deltas.output[1:5, 2:12])), # First 5 time windows of delta MFCCs
      wav.dur                                  # Duration of the audio file
    )
    
    return(mfcc.vector)  # Return the processed vector
  }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
    return(NULL)  # Return NULL in case of an error, or handle as needed
  })
})


# Convert the list to a vector
vec <- unlist(mfcc.vector.list)

# Split file names into site id and call id
loc <- str_split_fixed(filehandles, pattern = "-", n = 2)[, 1]
id <- str_split_fixed(filehandles, pattern = "-", n = 2)[, 2]

# Create a matrix with MFCC and delta coefficient values
# The number of columns is a reflection of the number of time windows
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

# Check data structure
str(mfcc.data.frame)

# Remove non-MFCC/delta columns
x <- mfcc.data.frame |> dplyr::select(-c(loc, filehandles, id))
x
x2 <- x |> dplyr::select(1:111)
x2


# Run affinity propagation
apres <- apcluster(negDistMat(r=2), x2, q=0.7, details=TRUE, maxits = 100000,
                   convits = 10000,
                   nonoise = T)

# Plot information about clustering run
plot(apres)

# Perform agglomerative clustering of affinity propagation clusters
aggres1 <- aggExCluster(x=apres)

# Show dendrograms
plot(aggres1)
plot(aggres1, showSamples=TRUE)

# Convert clustering object to a dendrogram
tree <- as.dendrogram(aggres1)

# Extract site labels from the file paths
site_labels <- str_split_fixed(basename(dirname(L)), "/", n = 2)[, 1]

# Define a color mapping based on the site labels
colorCodes <- c("MB"="red", "MD"="blue", "PP"="green", "PC"="purple")

# Assign colors to the samples based on the site labels
site_colours <- colorCodes[site_labels]
names(site_colours) <- rownames(x2)  

# Set the label colors in the dendrogram
labels_colors(tree) <- site_colours[order.dendrogram(tree)]

# Plot the colored dendrogram
plot(tree, main="Colored Dendrogram with Site Labels")
legend("topright", legend = c("MB", "MD", "PP", "PC"), fill = c("red", "blue", "green", "purple"))


