## Author:   E. Crestaz
## Location: 61032 Fano (PU), Italy
## Date:     2015-08-26
## Contact:  ezio.crestaz@syndial.it
##
## Description:
## Read outcome data, check that state and outcome are valid, return hospital name
## in that state with the given rank 30-day death rate
##
## Notes: 
## xx
library(dplyr)
library(stringi)  # To remove white spaces in function arguments

## Check if number is whole (that's without decimals), also if it store as numeric
## Arguments:
##   - a, number
## Return:
##   - boolean stating if argument is whole
## Function credits:
##   https://stat.ethz.ch/pipermail/r-help/2003-April/032471.html
is.whole <- function(a) { 
  (is.numeric(a) && floor(a)==a) ||
    (is.complex(a) && floor(Re(a)) == Re(a) && floor(Im(a)) == Im(a))
}

## Read in data 
## Arguments:
##   - state, 2 letters ISO symbol for US state
##   - outcome, among current options 'heart attack', 'heart failure' or 'pneumonia'
##   - num, "best", "worst" or a ranking number we want to ask for
## Return:
##   - hospital name in that state with lowest 30-day death rate, or, in case of ties,
##     the name of the first hospital sorted in alphabetic order
rankhospital <- function(state, outcome, num = "best") {

  # Set working directory and read outcome data
  setwd("C:/Syndial/Programming/R/JohnHopkinsUn/Rprogramming/ProgrammingAssignment3")

  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check that state and outcome are valid
  if (!state %in% state.abb) {
    stop("invalid state")
  }
  
  if (!outcome %in% c('heart attack','heart failure','pneumonia')) {
    stop("invalid outcome")
  }

  ## Check if num (ranking order we want to ask for) is a whole (whether integer or numeric)
  ## or contains accepted key words "best" or "worst". Otherwise an error is issued!
  ## Still to check that num, when a number, is positive gt 0
  if (!is.whole(num) && !num %in% c("best","worst")) {
    #stop("invalid num (ranking order asked for); it must be a whole positive number, 'best' or 'worst' strings")
    stop("invalid num (ranking order asked for); it must be a whole number, 'best' or 'worst' strings")
  }
  
  # Substitute spaces with dots in outcome, as sorting data frame columns using dplyr
  # library would result in an error if their names contain white spaces
  outcome <- stri_replace_all_fixed(outcome, " ", ".")
  
  df <- select(df,c(2,7,11,17,23))
  colnames(df) <- c('Hospital.Name','State','heart.attack','heart.failure','pneumonia')
  
  # Convert 30-days mortality rates from char to numeric
  df$heart.attack = as.numeric(df$heart.attack)
  df$heart.failure = as.numeric(df$heart.failure)
  df$pneumonia = as.numeric(df$pneumonia)
  
  ## Return hospital name in selected state with lowest 30-day death rate for selected
  ## disease. Note that SE (Standard Evaluator) version of the dplyr is used (_ at the end)
  ## instead of NSE, which is commonly used in dplyr.
  ## Search for "Non-standard evaluation" document for your own reference
  df <- filter(df, State==state & !is.na(outcome))
  
  # Redefine num according for bounding conditions (best and worst) or return NA if beyond
  # number of rows in (filtered) data frame
  n <- nrow(df)
  
  if (num == "best") {num <- 1} 
  else if (num == "worst") {num <- n}
  else if (num > n) {return(NA)}  
  
  # Return hospital name for requested ranking
  arrange_(df, outcome)[num,]$Hospital.Name
}

