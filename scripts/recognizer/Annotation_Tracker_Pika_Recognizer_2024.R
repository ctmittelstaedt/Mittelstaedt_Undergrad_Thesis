##################################################################################
# Annotation tracker
# Keeping track of how many files and vocalizations I have annotated for my pika
# recognizers
# Charlotte Mittelstaedt - The University of British Columbia
# Created 25 October 2024
##################################################################################

# install.packages
install.packages("vroom")
install.packages("dplyr")

# Set the working directory
setwd("D:\\Pika_Data_Analysis_2024\\Pika_recognizer\\Training_data")

# Count the number of annotation .txt files and number of hours annotated
# List all .txt files recursively
txt_files <- list.files(pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)

# Count the number of .txt files
num_txt_files <- length(txt_files)
print(num_txt_files)

# Calculate the number of hours annotated
num_hours <- (num_txt_files*10)/60
print(num_hours)


# Count the number of pika vocalizations
good.pika <- "PIKA"  # object for good quality vocalization annotations
maybe.pika <- "PIKA?"  # object for uncertain vocalization annotations
int.pika <- "PIKAI" # object for poor quality vocalization annotations (i.e. audio with interference)

# Initialize a count variable
total_count1 <- 0 # count variable for good quality vocalizations
total_count2 <- 0 # count variable for uncertain vocalizations
total_count3 <- 0 # count variable for vocalizations with interference

# Loop through each file and count occurrences of each of the three objects
for (file in txt_files) {

  # Read the data from the file
  data <- read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)  
  
  if ("Annotation" %in% colnames(data)) {
    # Count occurrences of "PIKA" in annotation column
    count1 <- sum(data$Annotation == good.pika, na.rm = TRUE)
    total_count1 <- total_count1 + count1
    

    # Count occurrences of "PIKA?" in annotation column
    count2 <- sum(data$Annotation == maybe.pika, na.rm = TRUE)
    total_count2 <- total_count2 + count2
    
    # Count occurrences of "PIKAI" in annotation column
    count3 <- sum(data$Annotation == int.pika, na.rm = TRUE)
    total_count3 <- total_count3 + count3
  }
}
 
print(total_count1) # Displays the number of good quality pika calls annotated
print(total_count2) # Displays the number of uncertain pika calls annotated
print(total_count3) # Displays the number of poor quality pika calls annotated

