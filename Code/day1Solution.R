library(purrr)
library(magrittr)


## Day 1 Excercise -------------------------------------------------------------
setwd("C:/Users/jawilliams/Documents/adventOfCode2022")

dataDay1 <- read.delim("dataDay1.txt", blank.lines.skip = FALSE)

## Extract data as numeric vector
extractedVec <- dataDay1[[1]] %>% as.numeric()

## How many elfs are there? Add 1 to account for first el not NA
which(extractedVec == 0) %>% length() + 1

## Flip all NAs to 0 for splitting vector
extractedVec[is.na(extractedVec)] <- 0

split.vec <- function(vec, sep = 0) {
  ## Find where all the breaks are
  is.sep <- vec == sep
  
  ## Split all of the non elements of the vector that are not breaks by
  ## where the break points occur
  split(vec[!is.sep], cumsum(is.sep)[!is.sep])
}

## List of all the itemized calorie potential.
itemizedCalories <- split.vec(extractedVec)

## Clean up names of list to be reflective & descriptive of excercise 
names(itemizedCalories) <- paste("Elf", 1:length(itemizedCalories))

totalCaloriesPerElf <- map_dbl(itemizedCalories, ~ sum(.x))

## Total calories of elf with most calories: solution
max(totalCaloriesPerElf)

## For fun, which elf is it - Elf 107
which(totalCaloriesPerElf == max(totalCaloriesPerElf))


## Sort totalCalories in decreasing order
sortedTotalCalories <- sort(totalCaloriesPerElf, decreasing = TRUE)

top3 <- sortedTotalCalories[1:3]

## Sum of top 3 elves calories
sum(top3)

## Which elves are the top 3
names(top3)
