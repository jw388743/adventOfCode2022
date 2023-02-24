## Load data = setup -----------------------------------------------------------

library(purrr)
library(magrittr)


## Day 1 Excercise -------------------------------------------------------------
setwd("C:/Users/jawilliams/Documents/adventOfCode2022")

dataDay2 <- read.delim("dataDay2.txt", col.names = "V1")

## I'ts skipping the first row I'm just manually adding because i don't feel like
## figuring it out
extractedVec <- c("B Z", dataDay2[["V1"]])

## Create a list with my moves and opps moves
oppMoves <- gsub("\\s.+", "", extractedVec)

myMoves <- gsub(".+\\s", "", extractedVec)

## Make sure opp moves only contains ABC
unique(oppMoves)

## Make sure my moves only contains XYZ
unique(myMoves)

## Part 1 ----------------------------------------------------------------------
## Write a function with some simple boolean logic to use downstream
toWinLossDraw <- function(myMove, oppMove) {
  ## MY MOVE ROCK
  if (myMove == "X") {
    if (oppMove == "A")
      return("Draw")
    
    if (oppMove == "B")
      return("Loss")
    
    if (oppMove == "C")
      return("Win")
  }
  
  ## MY MOVE PAPER 
  if (myMove == "Y") {
    if (oppMove == "A")
      return("Win")
    
    if (oppMove == "B")
      return("Draw")
    
    if (oppMove == "C")
      return("Loss")
  }
  
  ## MY MOVE SCISSOR 
  if (myMove == "Z") {
    if (oppMove == "A")
      return("Loss")
    
    if (oppMove == "B")
      return("Win")
    
    if (oppMove == "C")
      return("Draw")
  }
}

## What are the round results? 
roundResults <- map2_chr(myMoves, oppMoves, ~ toWinLossDraw(.x, .y))

## Convert myMoves to Points

myMovePoints <- dplyr::case_when(
  myMoves == "X" ~ 1,
  myMoves == "Y" ~ 2,
  myMoves == "Z" ~ 3
)

roundResultPoints <- dplyr::case_when(
  roundResults == "Win" ~ 6,
  roundResults == "Draw" ~ 3,
  roundResults == "Loss" ~ 0
)

totalPointsPerRound <- map2_dbl(myMovePoints, roundResultPoints, ~ .x + .y)

sum(totalPointsPerRound)

## Part 2 ----------------------------------------------------------------------

## My Moves
# X = rock
# Y = paper
# Z = scissors 

## Opp Moves
# A  = rock
# B = paper
# C = scissors 

## Outcomes
# X = lose
# Y = draw
# z = win

toMyMoves <- function(oppMove, outcome) {
  ## OPP MOVE ROCK
  if (oppMove == "A") {
    if (outcome == "X")
      return("Z")
    
    if (outcome == "Y")
      return("X")
    
    if (outcome == "Z")
      return("Y")
  }
  
  ## OPP MOVE PAPER 
  if (oppMove == "B") {
    if (outcome == "X")
      return("X")
    
    if (outcome == "Y")
      return("Y")
    
    if (outcome == "Z")
      return("Z")
  }
  
  ## OPP MOVE SCISSOR 
  if (oppMove == "C") {
    if (outcome == "X")
      return("Y")
    
    if (outcome == "Y")
      return("Z")
    
    if (outcome == "Z")
      return("X")
  }
}

desiredOutcome <- myMoves

myMoves2 <- map2_chr(oppMoves, desiredOutcome, ~ toMyMoves(.x, .y))

myMovePoints2 <- dplyr::case_when(
  myMoves2 == "X" ~ 1,
  myMoves2 == "Y" ~ 2,
  myMoves2 == "Z" ~ 3
)

roundResultPoints2 <- dplyr::case_when(
  desiredOutcome == "Z" ~ 6,
  desiredOutcome == "Y" ~ 3,
  desiredOutcome == "X" ~ 0
)

totalPointsPerRound2 <- map2_dbl(myMovePoints2, roundResultPoints2, ~ .x + .y)

sum(totalPointsPerRound2)
