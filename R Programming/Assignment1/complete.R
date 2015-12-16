# Assignment 1 part 2, complete.R
# Author: Peter Shen, Date: 11/12/2015
#
complete <- function(directory, id = 1:332) {
        
        if (!file.exists(directory))
                stop("Error: directory path not found " + directory)
        
        if (id[1] < 1 || id[length(id)] > 332)
                stop("Error: id range is between 1 to 332")
        
        # file list
        ids <- formatC(id, width=3, format="d", flag="0")
        temp <- paste(directory, ids, sep="/")
        files <- paste(temp, "csv", sep=".")

        x <- NULL
        y <- NULL
        for (i in 1:length(id)) {
            x <- append(x, id[i])
            ct = complete.cases(read.csv(files[i]))
            y <- append(y, length(ct[ct==TRUE]))
        }
        
        data.frame(id = x, nobs = y)
}
