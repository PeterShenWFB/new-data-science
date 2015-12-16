##  rankall.R
##  created: 11/25/2015
##  author:  Peter Shen
##  
##  The R programm provides the rankall(outcome, num = "best") function that search the 
##  outcome-of-care-measures.csv file and return the hospital name that has the rank of the
##  30-day mortality rate of the outcome of all states.
##

rankall <- function(outcome, num = "best") {
        # check outcome argument
        o <- c("heart attack", "heart failure", "pneumonia")
        if (!tolower(outcome) %in% o)
                stop("invalid outcome")
        
        # read outcome data, skip "Not Available" data
        out <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        # out <- read.csv("outcome-of-care-measures.csv")
        
        # select only needed columns, and shorten the column headers
        # we could check input args after this step, but save some energy if we have invalid arg(s)
        out <- out[, c(2,7,11,17,23)]
        names(out) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")

        # drop NA from the outcome
        st <- subset(out, !is.na(out[outcome]))
        
        # Order st based on ascending order of states, outcome, and hospital name
        idx <- order(st$state, st[outcome], st$hospital)
        st <- st[idx, ]

        # split st into groups by state, there are 54 groups, 4 additional special groups? 
        sp <- split(st, st$state)
        
        # For the purpose of Assignment 3, we don't need to unsplit or lapply/combine back to the
        # original data.frame.  We can use the list[[n]] and convert it back to a
        # DF.
        # add ranking value
        # setrank <- function(ranklist, rk_num) {
        #        df1 <- ranklist # convert list to data.frame
        #         df1$rank <- rk_num
        # }
        # lapply(sp, setrank(x))  #????
        
        # return by num for each state, if num is larger than the ranking within that state,
        # return NA for that state
        # we can initialize the data frame by pre-allocate 54 rows.
        # there are other ways to grow data frame, since we new 54 is our length.
        
        mx <- length(sp)
        rc <- as.data.frame(matrix(ncol=2, nrow=mx))
        names(rc) <- c("hospital", "state")
        
        for(i in 1:mx) {
                df <- sp[[i]]                                   # for each state, df is all ranks in that state
                # rk <- split(df, df[outcome])                    # split to ranking groups by outcome
                if (num == "best") {
                        rc[i, ] <- c(df[1, 1], df[1, 2])
                }
                else if (num == "worst") {
                        rc[i, ] <- c(df[nrow(df), 1], df[1, 2])
                }
                else if (num > nrow(df)) {                           # no such ranking for the state
                        rc[i, ] <- c(NA, df[1, 2])
                }
                else {
                        rc[i, ] <- c(df[num, 1], df[1, 2])
                }
        }
        
        rc
}