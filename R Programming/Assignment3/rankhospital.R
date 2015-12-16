##  rankhospital.R
##  created: 11/25/2015
##  author:  Peter Shen
##  
##  The R programm provides the rankhospital(state, outcome, num = "best") function that search the 
##  outcome-of-care-measures.csv file and return the hospital name that has the rank of the
##  30-day mortality rate of the outcome within the given state.
##

rankhospital <- function(state, outcome, num = "best") {
        # read outcome data, skip "Not Available" data
        out <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        
        # validate input arguments
        if (!(state %in% out[, "State"]))
                stop("invalid state")
        
        o <- c("heart attack", "heart failure", "pneumonia")
        if (!tolower(outcome) %in% o)
                stop("invalid outcome")
        
        # select only needed columns, and shorten the column headers
        # we could check input args after this step, but save some energy if we have invalid arg(s)
        out <- out[, c(2,7,11,17,23)]
        names(out) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        # select only needed state, then drop the NAs for outcome
        st <- out[out$state == state, ]
        st <- subset(st, !is.na(st[outcome]))

        # Order st based on ascending order of outcome and hospital name
        idx <- order(st[outcome], st$hospital)
        st <- st[idx, ]
        
        # return by num, only the first occurance if there are more than one hospital in the rank
        if (num < 1 || num > nrow(st)) {
                rc <- NA
        }
        if (num == "best") {
                rc <- st[1, 1]
        }
        else if (num == "worst") {
                rc <- st[nrow(st), 1]
        }
        else {
                rc <- st[num, 1]
        }
        rc
}