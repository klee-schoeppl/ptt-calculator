install.packages(c("readxl", "magrittr", "tidyverse", "dplyr"))
install.packages("xtable")
install.packages("rapportools")

library(readxl) #import data from .xls files
library(tidyverse) #Data analysis
library(dplyr) #Gives us the mutate function
library(xtable) #outputs tables as LaTeX code
library(rapportools)

analyzeDiceTask <- function(vecA, vecC) {
  # checking that the input constitutes a diceTask
  
  if (!is.vector(vecA) |!is.vector(vecC)){
    stop('Invalid input: Expected 2 vectors.')
  }
  if (length(vecA)!= length(vecC)){
    stop('Invalid input: Unequal vector length.')
  }
  if (!all(is.logical(vecA))) {
    stop('Invalid input: Vector A contains non-boolean value.')
  }
  if (!all(is.logical(vecC))){
    stop('Invalid input: Vector C contains non-boolean value.')
  }
  if (length(vecA)>6){
    stop('Invalid input: Vectors contain too many values.')
  }
  
  #-----------------------------------------------------------------------------
  #Vectors shorter than 6 values are taken to be uncertainty tasks with an 
  #appropriate amount of blank sides.
  aBlanks = list(c(0), c(0), c(0), c(0), c(0), c(0))
  cBlanks = list(c(0), c(0), c(0), c(0), c(0), c(0))
  constituents <<- NULL
  numberOfConceiledSides <- (6 - length(vecA))
  
  if (numberOfConceiledSides > 0){ 
    for (i in 1 :numberOfConceiledSides ){ #preparation to expand the grid
      additionalVariable <- list(c(0,1),c(0,1))
      constituents <- c(constituents, additionalVariable)
    }
    constituents <- rev(expand.grid(constituents)) #grid expansion
    for (i in 1 : numberOfConceiledSides){ #grid to vectors
      aBlanks[[i]]<-constituents[,i]
      cBlanks[[i]]<-constituents[,(i+numberOfConceiledSides)]
    }
  }
  #-----------------------------------------------------------------------------
  #Reads out the inputs and (if applicable) the arrays containing the variables
  #for blank sides. 
  
  A <- (sum(vecA) + aBlanks[[1]] + aBlanks[[2]] + aBlanks[[3]] + aBlanks[[4]] 
        + aBlanks[[5]] + aBlanks[[6]]) #irrelevant values equal 0
  C <- (sum(vecC) + cBlanks[[1]] + cBlanks[[2]] + cBlanks[[3]] + cBlanks[[4]] 
        + cBlanks[[5]] + cBlanks[[6]]) #irrelevant values equal 0
  
  nA<- 6 - A # count of ¬A sides
  nC<- 6 - C # count of ¬C sides
  tempA <- sum(vecA) #ignoring the blank sides
  tempC <- sum(vecC) #ignoring the blank sides

  tempAC <- 0 #ignoring the blank sides
  if (length(vecA)>0){
  for (i in 1 : length(vecA)){
    if (vecA[i] & vecC[i]){
      tempAC <- tempAC + 1
    }
   }
  }

  #AC is the count of sides with both A and C
  AC<- (tempAC + aBlanks[[1]]*cBlanks[[1]] + aBlanks[[2]]*cBlanks[[2]]
        + aBlanks[[3]]*cBlanks[[3]] + aBlanks[[4]]*cBlanks[[4]] 
        + aBlanks[[5]]*cBlanks[[5]]  + aBlanks[[6]]*cBlanks[[6]])
  
  tempnAnC <- 0 #ignoring the blank sides
  if(length(vecA)>0){
    for (i in 1 : length(vecA)){
      if (!vecA[i] & !vecC[i]){
        tempnAnC <- tempnAnC + 1
      }
    }
  }

  # nAnC is the count of sides with both ¬A and ¬C
  nAnC <- tempnAnC 
  if (numberOfConceiledSides > 0){
  for (i in 1 : numberOfConceiledSides){
    nAnC <- nAnC + ((1-aBlanks[[i]])*(1-cBlanks[[i]]))/2 
  }}
    
  # CnA is the count of sides with C but ¬A
  CnA <- C - AC
  # AnC is the count of sides with A but ¬C
  AnC <- A - AC
  
  # pCgA is the probability of C given A
  pCgA<-AC/A
  for (i in 1 : length(pCgA)){
    if (is.na(pCgA[i])){
      pCgA[i]<-c(0)
    }
  }
  
  # pCgnA is the probability of C given ¬A
  pCgnA<-CnA/nA
  for (i in 1 : length(pCgnA)){
    if (is.na(pCgnA[i])){
      pCgnA[i]<-c(0)
    }
  }
  

  #-----------------------------------------------------------------------------
  # Calculates the uncertainty intervals for a wide range of interpretations of
  # the natural language conditionals used in the experiments. 
  # Substitution of 0 for undefined values resulting from division by 0.
 
  # -> interpretation
  materialConditional <- ((AC + CnA + nAnC)/6) 
  
  
  
  # <--> interpretation
  equivalent <- ((AC / 6 + nAnC / 6)) 
 
  # & interpretation
  conjunction <- AC/6
  
  # | interpretation
  conditionalP <- pCgA
  
  # || interpretation
  biconditionalP <- AC / (6 - nAnC) 
  for (i in 1 : length(biconditionalP)){
    if (is.na(biconditionalP[i])){
      biconditionalP[i]<-c(0)
    }
  }
  
  # 'halfway' | interpretation
  fullignoreConditionalP <- tempAC / tempA # without blanks
  for (i in 1 : length( fullignoreConditionalP)){
    if (is.na( fullignoreConditionalP[i])){
      fullignoreConditionalP[i]<-c(0)
    }
  }
  
  #-----------------------------------------------------------------------------
  #Calculates various interpretations of argument strength, among them delta P.
  
  
  deltaP<-pCgA-pCgnA # Nozick 1981
  
  #-----------------------------------------------------------------------------
  #prints the results
  
  interpretationTable = data.frame(
    interpretation = c("-->",
                       "<->",
                       "&",
                       "|",
                       "||",
                       "|u",
                       "|l",
                       "|ul"),
    min = c(min(materialConditional),
            min(equivalent),
            min(conjunction),
            min(conditionalP),
            min(biconditionalP),
            min(conditionalP),
            min(fullignoreConditionalP),
            min(fullignoreConditionalP)),
    max = c(max(materialConditional),
            max(equivalent),
            max(conjunction),
            max(conditionalP),
            max(biconditionalP),
            max(fullignoreConditionalP),
            max(conditionalP),
            max(fullignoreConditionalP)),
    stringsAsFactors = FALSE
  )
  
  print(interpretationTable)
  
  print(summary(cbind(deltaP)))
  
}
#-------------------------------------------------------------------------------
# TESTING THE SUBROUTINE
# Expects as input 2 equal length vectors containing 6 or fewer boolean values 
# each. VecA should contain whether the side in question makes the 
# antecedent of the conditional true, VecC the conditional. 

#vectorA <- logical() #for empty boolean vectors
#vectorC <- logical() #for emtpy boolean vectors
vectorA <- c(T,T,T,F)
vectorC <- c(T,T,F,T)
analyzeDiceTask(vectorA, vectorC)


