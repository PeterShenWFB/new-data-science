# Assignment 1 part 1
# Author: Peter Shen, Date: 11/9/2015
# Needs to install packages data.table and cron
#

library(data.table)
pollutantmean <- function(directory, pollutant, id = 1:332) {

# check inputs
if (!file.exists(directory))
	stop("Error: directory path not found " + directory)

if (!identical(pollutant, "sulfate") && !identical(pollutant, "nitrate"))
	stop("Don't understand, which pollutant?")

if (id[1] < 1 || id[length(id)] > 332)
	stop("Error: id range is between 1 to 332")

# file list
ids <- formatC(id, width=3, format="d", flag="0")
temp <- paste(directory, ids, sep="/")
files <- paste(temp, "csv", sep=".")

# read and combind data
inputdata <- rbindlist(lapply(files, read.csv))

# get the right column and remove NA
# missingtarget <- is.na(target)

## important note:  have to use get(var) to reference the column name
## in data.table, which is different than referecing data.frame column
## by variable name

target <- inputdata[, get(pollutant)]

mean(target, na.rm = TRUE)

}

#  Test results:
#> source("./Assignment1/pollutantmean.R")
#> pollutantmean("./Assignment1/specdata", "sulfate", 1:10)
#[1] 4.064128
#> pollutantmean("./Assignment1/specdata", "nitrate", 70:72)
#[1] 1.706047
#> pollutantmean("./Assignment1/specdata", "nitrate", 23)
#[1] 1.280833
 
