## Load data = setup -----------------------------------------------------------

library(purrr)
library(magrittr)


## Day 1 Excercise -------------------------------------------------------------
setwd("C:/Users/jawilliams/Documents/adventOfCode2022")

dataDay2 <- read.delim("dataDay3.txt")

extractedVec <- c(names(dataDay2), dataDay2$gvNbShZZgQfWdQhdPQmggLTFLwmwjFqjVVgM)

## Find number of characters in each string

ncharItems <- map_dbl(extractedVec, ~ nchar(.x))

## Dived ncharExtractedVec by 2 to get where to spit strings
whereToSplit <- ncharItems/2


splitStrings <- pmap(
  list(
    extractedVec,
    whereToSplit,
    ncharItems
  ),
  function(strng, break1, break2) {
    
    list(
      "String 1" = substr(strng, start = 1, stop = break1),
      "String 2" = substr(strng, start = break1 + 1, stop = break2)
    ) 
  }
)


## let's make sure all of our strings are of the length that we intended 

map2_lgl(whereToSplit,
         splitStrings,
         function(correctLength, strngList) {
           all(nchar(strngList[[1]]) == correctLength,
               nchar(strngList[[2]]) == correctLength)
         }) %>% 
  all()


## That worked - now let's find the common elements between the strings---------

## We'll use strsplt to split the strings into a vector that's elements are all 
## the chrs of the strings, then intersect to find the common element
commonEls <- map_chr(
  splitStrings,
  function(strngList) {
    spltStrng1 <- strsplit(strngList[[1]], "")[[1]]
    
    spltStrng2 <- strsplit(strngList[[2]], "")[[1]]
    
    intersect(spltStrng1, spltStrng2)
  }
)

## Write a function that takes a vector el and converts it to the numeric value
## assinged to each letter in the excercise a:z = 1:26, A:Z == 27:52

convertLetterToNumValue <- function(letterEl) {
  if (is.element(letterEl, letters)) {
    return(which(letters == letterEl))
  }
  
  ## For CAPS adjust for the 27:52 index
  if (is.element(letterEl, LETTERS)) {
    return(which(LETTERS == letterEl) + 26)
  }
  
}

priorityVals <- map_dbl(commonEls, ~ convertLetterToNumValue(.x))

## Answer 
sum(priorityVals)


## Part 2 ----------------------------------------------------------------------

## I basically need to repeat the process except group the strings by 3s and
## find the common els 

length(extractedVec)/3 ## there's 100 unq groups 

groupsOf3 <- split(extractedVec, ceiling(seq_along(extractedVec) / 3))


commonEls2 <- map_chr(
  groupsOf3,
  function(strngVec) {
    spltStrng1 <- strsplit(strngVec[1], "")[[1]]
    
    spltStrng2 <- strsplit(strngVec[2], "")[[1]]
    
    spltStrng3 <- strsplit(strngVec[3], "")[[1]]
    
    intersect(intersect(spltStrng1, spltStrng2), spltStrng3)
  }
)

priorityVals2 <- map_dbl(commonEls2, ~ convertLetterToNumValue(.x))

sum(priorityVals2)
