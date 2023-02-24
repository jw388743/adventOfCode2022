## Load data = setup -----------------------------------------------------------

library(purrr)
library(magrittr)


## Day 1 Excercise -------------------------------------------------------------
setwd("C:/Users/jawilliams/Documents/adventOfCode2022")

dataDay4 <- read.delim("dataDay4.txt")

extractedVec <- dataDay4[[1]]

## Remove - for : to further downstream coerce to actual ranges of values

extractedVec <- gsub("-", ":", extractedVec)

ranges1 <- gsub(",.+", "", extractedVec)
ranges2 <- gsub(".+,", "", extractedVec)

## We won't be able to sucess fully coerce these vectors to numerics so further
## Data clean up is needed 

toNumericsFromChrs <- function(chrEl) {
  
  val1 <- gsub(":.+", "", chrEl)
  
  val2 <- gsub(".+:", "", chrEl)
  
  return(val1:val2)
}

rangesNumerics1 <- map(ranges1, ~ toNumericsFromChrs(.x))
rangesNumerics2 <- map(ranges2, ~ toNumericsFromChrs(.x))


## Now determine how many of the pairs are totally overlapping
doTheyOverLap <- map2_lgl(
  rangesNumerics1,
  rangesNumerics2,
  function(rng1, rng2) {
    all(rng1 %in% rng2) || all(rng2 %in% rng1)
  }
)

which(doTheyOverLap == TRUE) %>% length()

## Part 2 ----------------------------------------------------------------------
doTheyOverLap2 <- map2_lgl(
  rangesNumerics1,
  rangesNumerics2,
  function(rng1, rng2) {
    any(rng1 %in% rng2) || any(rng2 %in% rng1)
  }
)

which(doTheyOverLap2 == TRUE) %>% length()
  
