##################################################################################
# Ohun pika recognizer
# Using the ohun package (an extension of warbleR, created by Marcelo Araya-Salas)
# for the signal detection of collared pika calls via template-matching
# Charlotte Mittelstaedt - The University of British Columbia
# Created 20 October 2024
##################################################################################

# installing ohun package
install.packages("ohun")

# install package to use latest developmental version from GitHub
remotes::install_github("maRce10/ohun")

#load packages
library(ohun)
library(tuneR)
library(warbleR)