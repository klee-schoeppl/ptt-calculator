install.packages(c("readxl", "magrittr", "tidyverse", "dplyr"))
install.packages("xtable")
install.packages("rapportools")

library(readxl) #import data from .xls files
library(tidyverse) #Data analysis
library(magrittr) # gives us the %>% function
library(dplyr) #Gives us the mutate function
library(ggplot2)
library(xtable) #outputs tables as LaTeX code
library(rapportools)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# TRYING MY HANDS AT A SUBROUTINE FOR THE INTERPRETATIONS

analyzeDiceTask <- function(vecA, vecC) {
  #Expects as input 2 equal length vectors containing 6 or fewer boolean values 
  #each. VecA should contain whether the side in question makes the 
  #antecedent of the conditional true, VecC the conditional. 
  
  if (!is.vector(vecA) |!is.vector(vecC)){
    stop('Invalid input: Expected 2 vectors')
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
  cBlanks = list(c(0), c(0), c(0), c(0), c(0), c(0))#eventuell NULL setzen
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
  nA<- 6 - A
  nC<- 6 - C
  tempAC <- 0
  if (length(vecA)>0){
  for (i in 1 : length(vecA)){
    if (vecA[i] & vecC[i]){
      tempAC <- tempAC + 1
    }
  }
  }

  
  AC<- (tempAC + aBlanks[[1]]*cBlanks[[1]] + aBlanks[[2]]*cBlanks[[2]]
        + aBlanks[[3]]*cBlanks[[3]] + aBlanks[[4]]*cBlanks[[4]] 
        + aBlanks[[5]]*cBlanks[[5]]  + aBlanks[[6]]*cBlanks[[6]])
  
  tempnAnC <- 0
  if(length(vecA)>0){
    for (i in 1 : length(vecA)){
      if (!vecA[i] & !vecC[i]){
        tempnAnC <- tempnAnC + 1
      }
    }
  }

  nAnC <- tempnAnC 
           
  
  for (i in 1 : numberOfConceiledSides){
    nAnC <- nAnC + ((1-aBlanks[[i]])*(1-cBlanks[[i]]))/2 
    
  }
           
          # + ((1-aBlanks[[2]])*(1-cBlanks[[2]]))/2
           #+ ((1-aBlanks[[3]])*(1-cBlanks[[3]]))/2 
           #+ ((1-aBlanks[[4]])*(1-cBlanks[[4]]))/2 
           #+ ((1-aBlanks[[5]])*(1-cBlanks[[5]]))/2 
           #+ ((1-aBlanks[[6]])*(1-cBlanks[[6]]))/2 
           #)
  
  CnA <- C - AC
  AnC <- A - AC
  print(nAnC)
  #-----------------------------------------------------------------------------
  #Calculates delta P, substituting 0 for undefined values resulting from 
  #division by 0
  materialConditional <- ((AC + CnA + nAnC)/6) #continue later
 
  
  
  equivalent <- ((AC / 6 + nAnC / 6)) #continue later
  
  
  
  conjunction <- AC/6
 
  
  
 
  
  pCgA<-AC/A
  for (i in 1 : length(pCgA)){
    if (is.na(pCgA[i])){
      pCgA[i]<-c(0)
    }
  }
  pCgnA<-CnA/nA
  for (i in 1 : length(pCgnA)){
    if (is.na(pCgnA[i])){
      pCgnA[i]<-c(0)
    }
  }
  deltaP<-pCgA-pCgnA
  
  #-----------------------------------------------------------------------------
  #prints the results
  print(paste('The --> interpretation has as its minimum:', min(materialConditional),
        'and as its maximum:', max(materialConditional), '.'))
  
  print(paste('The <--> interpretation has as its minimum:', min(equivalent),
      'and as its maximum:', max(equivalent),'.'))
  
  print(paste('The conjunction interpretation has as its minimum:', min(conjunction),
      'and as its maximum:', max(conjunction),'.'))
  
  print(summary(cbind(deltaP)))
  
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# TESTING THE SUBROUTINES

#vectorA <- logical() #for empty boolean vectors
#vectorC <- logical() #for emtpy boolean vectors
vectorA <- c(T,F,F)
vectorC <- c(F,F,F)
analyzeDiceTask(vectorA, vectorC)


