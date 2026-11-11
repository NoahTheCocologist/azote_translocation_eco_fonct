# Load Packages ----
source('loadpackages.R')

# Read csv files ----
files <- c("carbone", "azote")
source('readdatafiles.R')
readdatafiles("data/", files,"_df", ".csv")

#

