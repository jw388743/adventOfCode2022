## Load data = setup -----------------------------------------------------------

library(purrr)
library(magrittr)


## Day 1 Excercise -------------------------------------------------------------
setwd("C:/Users/jawilliams/Documents/adventOfCode2022")

dataDay5 <- read.delim("dataDay5.txt")

extractedVec <- dataDay5[[1]]

digitsOnly <- map(
  extractedVec,
  function(strng) {
    as.numeric(unlist(stringr::str_extract_all(strng, "\\d+")))
  }
)

## make sure all digitsOnly elements are length == 3
map_lgl(digitsOnly, ~ length(.x) == 3) %>% all()


## I am now going to take the initial stacks and create an object.

initialStacks <- list(
  c("W", "L", "S"),
  c("Q", "N", "T", "J"),
  c("J", "F", "H", "C", "S"),
  c("B", "G", "N", "W", "M", "R", "T"),
  c("B", "Q", "H", "D", "S", "L", "R", "T"),
  c("L", "R", "H", "F", "V", "B", "J", "M"),
  c("M", "J", "N", "R", "W", "D"),
  c("J", "D", "N", "H", "F", "T", "Z", "B"),
  c("T", "F", "B", "N", "Q", "L", "H")
)


## Now I'll write a fn that will be reduced over 
toMovementsInStacks <- function(stackList, move, from, to) {
  ## Extract stacks needed from "from" el
  stacksToMove <- stackList[[from]][move:1]
  
  ## Add extracted stacks to new stack
  stackList[[to]] <- c(stacksToMove, stackList[[to]])
  
  ## Remove stacks from "from" el
  stackList[[from]] <- stackList[[from]][(move + 1):length(stackList[[from]])]
  
  stackList
}

toMovementsInStacks(initialStacks, digitsOnly[[1]][1],
                    digitsOnly[[1]][2],
                    digitsOnly[[1]][3])


finalStacks <- reduce(
  digitsOnly,
  function(.acc, nextMoves) {
    toMovementsInStacks(
      stackList = .acc,
      move = nextMoves[1],
      from = nextMoves[2],
      to = nextMoves[3]
    )
  },
  .init = initialStacks
)

## Part2 -----------------------------------------------------------------------
toMovementsInStacks2 <- function(stackList, move, from, to) {
  ## Extract stacks needed from "from" el
  stacksToMove <- stackList[[from]][1:move]
  
  ## Add extracted stacks to new stack
  stackList[[to]] <- c(stacksToMove, stackList[[to]])
  
  ## Remove stacks from "from" el
  stackList[[from]] <- stackList[[from]][(move + 1):length(stackList[[from]])]
  
  stackList
}

finalStacks2 <- reduce(
  digitsOnly,
  function(.acc, nextMoves) {
    toMovementsInStacks2(
      stackList = .acc,
      move = nextMoves[1],
      from = nextMoves[2],
      to = nextMoves[3]
    )
  },
  .init = initialStacks
)
