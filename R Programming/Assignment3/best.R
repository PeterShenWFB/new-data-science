##  best.R
##  created: 11/24/2015
##  author:  Peter Shen
##  
##  The R programm provides the best(state, outcome) function that search the 
##  outcome-of-care-measures.csv file and return the hospital name(s) that having
##  the lowest 30-day mortality rate of the outcome within the given state.
##

best <- function(stateCode, outcome) {
        # read outcome data, skip "Not Available" data
        out <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)

        # validate input arguments
        if (!(stateCode %in% out[, "State"]))
                stop("invalid state")
        
        o <- c("heart attack", "heart failure", "pneumonia")
        if (!tolower(outcome) %in% o)
                stop("invalid outcome")

        # select only needed columns, and shorten the column headers
        # we could check input args after this step, but save some energy if we have invalid arg(s)
        out <- out[, c(2,7,11,17,23)]
        names(out) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        # select only needed state
        st <- out[out$state == stateCode, ]
        
        # convert target outcome to a numeric matrix
        targetoutcome <- sapply(st[outcome], as.numeric)
        
        # return hospital name in that state with the lowest 30-day death rate
        # find the index of lowest, could be more than 1
        idx <- which(targetoutcome == min(targetoutcome, na.rm = TRUE))
        
        # check if there are more than 1 lowest, sort by alphabetical order and return the 1st one
        if (length(idx) > 1) {
                hospitals <- st[idx, 1]
                hospitals <- sort(hospitals)
                hospitals[1]
        }
        else {
                st[idx, 1]
        }
}