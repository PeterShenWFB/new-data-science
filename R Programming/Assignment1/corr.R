# Assignment 1 part 3, corr.R
# Author: Peter Shen, Date: 11/13/2015
#
corr <- function(directory, threshold = 0){
        
        if (!file.exists(directory))
                stop("Error: directory path not found " + directory)
        
        if (!is.numeric(threshold) && length(threshold) > 1)
                stop("ERROR: threshold needs to be a single length numberic vector")
        
        # read all files
        rc <- vector(mode = "integer")
        files <- dir(directory, pattern = ".csv", full.names = TRUE)
        
        for (i in 1:length(files)) {
                dt <- read.csv(files[i])
                ct <- complete.cases(dt)
                if (length(ct[ct==TRUE]) > threshold) {
                        rc <- append(rc, cor(dt$sulfate, dt$nitrate, use="pairwise.complete.obs"))          
                }
        }
        
        rc
}