listPeeps <- function() {
  allClimbers <- readr::read_csv("data/people", col_names = TRUE)
  nameVec <- allClimbers$Climber
  return(nameVec)
}

listAllInfo <- function() {
  allInfo <- readr::read_csv("data/people", col_names = TRUE)
  return(allInfo)
}

listProblems <- function() {
  allProblems <- readr::read_csv("data/problems", col_names = TRUE)
  probVec <- allProblems$Problem
  return(probVec)
}

listAllProblems <- function() {
  allProblems <- readr::read_csv("data/problems", col_names = TRUE)
  return(allProblems)
}

probsByWeek <- function(week) {
  week <- stringr::str_sub(week, 6, 6)
  allProbs <- listAllProblems()
  probsVec <- allProbs[allProbs$Week == week, 1]
  return(probsVec)
}

compScore <- function(handicap, grade, attempts) {
  x <- 0
  if (is.na(attempts)) {x <- x + 0}
  else if (attempts == 0) {x <- x + 0}
  else if (handicap == "C1" & grade <= -1) {x <- x + 5}
  else if (handicap == "C1" & grade == 0) {x <- x + 10}
  else if (handicap == "C1" & grade == 1) {x <- x + 15}
  else if (handicap == "C1" & grade == 2) {x <- x + 20}
  else if (handicap == "C1" & grade == 3) {x <- x + 25}
  else if (handicap == "C2" & grade >= 0) {x <- x + 5}
  else if (handicap == "C2" & grade <= 1) {x <- x + 10}
  else if (handicap == "C2" & grade == 2) {x <- x + 15}
  else if (handicap == "C2" & grade == 3) {x <- x + 20}
  else if (handicap == "C2" & grade >= 4) {x <- x + 25}
  else if (handicap == "C3" & grade <= 1) {x <- x + 5}
  else if (handicap == "C3" & grade == 2) {x <- x + 10}
  else if (handicap == "C3" & grade == 3) {x <- x + 15}
  else if (handicap == "C3" & grade == 4) {x <- x + 20}
  else if (handicap == "C3" & grade >= 5) {x <- x + 25}
  else if (handicap == "C4" & grade <= 2) {x <- x + 5}
  else if (handicap == "C4" & grade == 3) {x <- x + 10}
  else if (handicap == "C4" & grade == 4) {x <- x + 15}
  else if (handicap == "C4" & grade == 5) {x <- x + 20}
  else if (handicap == "C4" & grade >= 6) {x <- x + 25}
  else if (handicap == "C5" & grade <= 3) {x <- x + 5}
  else if (handicap == "C5" & grade == 4) {x <- x + 10}
  else if (handicap == "C5" & grade == 5) {x <- x + 15}
  else if (handicap == "C5" & grade == 6) {x <- x + 20}
  else if (handicap == "C5" & grade >= 7) {x <- x + 25}
  else if (handicap == "C6" & grade <= 4) {x <- x + 5}
  else if (handicap == "C6" & grade == 5) {x <- x + 10}
  else if (handicap == "C6" & grade == 6) {x <- x + 15}
  else if (handicap == "C6" & grade == 7) {x <- x + 20}
  else if (handicap == "C6" & grade >= 8) {x <- x + 25}
  else if (handicap == "C7" & grade <= 5) {x <- x + 5}
  else if (handicap == "C7" & grade == 6) {x <- x + 10}
  else if (handicap == "C7" & grade == 7) {x <- x + 15}
  else if (handicap == "C7" & grade == 8) {x <- x + 20}
  else if (handicap == "C7" & grade >= 9) {x <- x + 25}
  return(x)
}
