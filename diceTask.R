# Leon Schöppl (2021) <https://github.com/leon-schoeppl/ptt-calculator/>
# DTSv1.0

analyzeDiceTask <- function(vecA, vecC) {
  library(tidyverse)
  library(dplyr)
  library(rapportools)
  #-----------------------------------------------------------------------------
  # checking that the input constitutes a Dice Task
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
  # Vectors shorter than 6 values are taken to be uncertainty tasks with an 
  # appropriate amount of blank sides.
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
  # function substituting 0 for undefined values resulting from division by 0.
  replaceNA <- function(column){
    for (i in 1 : length(column)){
      if (is.na(column[i]) | is.nan(column[i])){
        column[i]<-c(0)
      }
    }
    return(column)
  }
  #-----------------------------------------------------------------------------
  # Reads out the inputs and (if applicable) the arrays containing the variables
  # for blank sides. 
  A <- (sum(vecA) + aBlanks[[1]] + aBlanks[[2]] + aBlanks[[3]] + aBlanks[[4]] 
        + aBlanks[[5]] + aBlanks[[6]]) #irrelevant values equal 0
  C <- (sum(vecC) + cBlanks[[1]] + cBlanks[[2]] + cBlanks[[3]] + cBlanks[[4]] 
        + cBlanks[[5]] + cBlanks[[6]]) #irrelevant values equal 0
  
  nA<- 6 - A # count of ¬A sides
  nC<- 6 - C # count of ¬C sides
  
  tempA <- sum(vecA) # ignoring the blank sides
  tempC <- sum(vecC) # ignoring the blank sides
  
  tempAC <- 0 # ignoring the blank sides
  if (length(vecA)>0){
    for (i in 1 : length(vecA)){
      if (vecA[i] & vecC[i]){
        tempAC <- tempAC + 1
      }
    }
  }
  
  # AC is the count of sides with both A and C
  AC<- (tempAC + aBlanks[[1]]*cBlanks[[1]] + aBlanks[[2]]*cBlanks[[2]]
        + aBlanks[[3]]*cBlanks[[3]] + aBlanks[[4]]*cBlanks[[4]] 
        + aBlanks[[5]]*cBlanks[[5]]  + aBlanks[[6]]*cBlanks[[6]])
  
  tempnAnC <- 0 # ignoring the blank sides
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
      nAnC <- nAnC + ((1-aBlanks[[i]])*(1-cBlanks[[i]])) 
    }}
  
  # CnA is the count of sides with C but ¬A
  CnA <- C - AC
  tempCnA <- tempC - tempAC # ignores blanks
  # AnC is the count of sides with A but ¬C
  AnC <- A - AC
  tempAnC <- tempA - tempAC # ignores blanks
  
  # pCgA is the probability of C given A
  pCgA<-AC/A
  pCgA <-replaceNA(pCgA)
  
  # pCgnA is the probability of C given ¬A
  pCgnA<-CnA/nA
  pCgnA <- replaceNA(pCgnA)
  
  # pAgC is the probability of A given C
  pAgC <- AC/C
  pAgC <- replaceNA(pAgC)
  
  # pAgnC is the probability of A given ¬C
  pAgnC <- AnC/nC
  pAgnC <- replaceNA(pAgnC)
  
  # pnCgA is the probability of ¬C given A
  pnCgA <- AnC/A
  pnCgA <- replaceNA(pnCgA)
  
  #-----------------------------------------------------------------------------
  # Calculates the uncertainty intervals for a wide range of interpretations of
  # the natural language conditionals used in the experiments. 
  
  # --> interpretation
  materialConditional <- ((AC + CnA + nAnC)/6) 
  
  # 'halfway' --> interpretation
  fullignoreMaterialConditional <- ((tempAC + tempCnA + tempnAnC)/ 6)
  
  # <-> interpretation
  equivalent <- (AC / 6 + nAnC / 6)
  
  # 'halfway' <-> interpretation
  fullignoreEquivalent <- tempAC / 6 + tempnAnC / 6
  
  # & interpretation
  conjunction <- AC / 6
  
  # 'halfway'& interpretation
  fullignoreConjunction <- tempAC / 6
  
  # | interpretation
  conditionalP <- pCgA
  
  # 'halfway' | interpretation
  fullignoreConditionalP <- tempAC / tempA # without blanks
  fullignoreConditionalP<-replaceNA(fullignoreConditionalP)
  
  # || interpretation
  biconditionalP <- AC / (6 - nAnC) 
  biconditionalP<-replaceNA(biconditionalP)
  
  # 'halfway' || interpretation
  fullignoreBiconditionalP <- tempAC / (6 - tempnAnC)
  fullignoreBiconditionalP<-replaceNA(fullignoreBiconditionalP)
  
  # special &l interpretation
  specialLowignoredConjunction <- tempAC / (6 - numberOfConceiledSides)
  specialLowignoredConjunction <- replaceNA(specialLowignoredConjunction)
  
  #-----------------------------------------------------------------------------
  # Calculates various measures of confirmation, among them delta P.
  
  # Nozick, 1981 
  nozick<-pAgC -pAgnC
  
  # Christensen, 1999 (also 'delta P')
  deltaP<-pCgA-pCgnA 
  
  # Kemeny & Oppenheim, 1952
  kemeny<-(pAgC-pAgnC)/(pAgC+pAgnC) 
  kemeny<-replaceNA(kemeny)
  
  # Finch, 1960
  finch<- (pCgA/(C/6))
  finch<-replaceNA(finch)
  finch <- finch - 1
  
  # Rips, 2001
  rips<-pnCgA/(nC/6)
  rips<-replaceNA(rips)
  rips<- 1-rips
  
  # difference (Carnap, 1962; Eells, 1982; Jeffrey, 1992)
  difference<- pCgA - C/6
  
  # Carnap, 1962 
  carnap<-(AC/6)-(A/6 * C/6)
  
  # Mortimer, 1988
  mortimer<- pAgC - A/6
  
  #-----------------------------------------------------------------------------
  # prints the results
  
  interpretationTable = data.frame(
    interpretation = c("Material Implication (-->)",
                       "--> upper-ignored",
                       "--> lower-ignored",
                       "--> fully-ignored",
                       "Equivalent (<->) ",
                       "<-> upper-ignored",
                       "<-> lower-ignored",
                       "<-> fully-ignored",
                       "Conjunction (&)",
                       "& upper-ignored",
                       "& lower-ignored",
                       "& fully-ignored",
                       "Conditional Probability (|)",
                       "| upper-ignored",
                       "| lower-ignored",
                       "| fully-ignored",
                       "Biconditional (||) ",
                       "|| upper-ignored",
                       "|| lower-ignored",
                       "|| fully-ignored",
                       "special & lower-ignored"),
    min = c(min(materialConditional),
            min(materialConditional),
            min(fullignoreMaterialConditional),
            min(fullignoreMaterialConditional),
            min(equivalent),
            min(equivalent),
            min(fullignoreEquivalent),
            min(fullignoreEquivalent),
            min(conjunction),
            min(conjunction),
            min(fullignoreConjunction),
            min(fullignoreConjunction),
            min(conditionalP),
            min(conditionalP),
            min(fullignoreConditionalP),
            min(fullignoreConditionalP),
            min(biconditionalP),
            min(biconditionalP),
            min(fullignoreBiconditionalP),
            min(fullignoreBiconditionalP),
            min(specialLowignoredConjunction)),
    max = c(max(materialConditional),
            max(fullignoreMaterialConditional),
            max(materialConditional),
            max(fullignoreMaterialConditional),
            max(equivalent),
            max(fullignoreEquivalent),
            max(equivalent),
            max(fullignoreEquivalent),
            max(conjunction),
            max(fullignoreConjunction),
            max(conjunction),
            max(fullignoreConjunction),
            max(conditionalP),
            max(fullignoreConditionalP),
            max(conditionalP),
            max(fullignoreConditionalP),
            max(biconditionalP),
            max(fullignoreBiconditionalP),
            max(biconditionalP),
            max(fullignoreBiconditionalP),
            max(conjunction)),
    stringsAsFactors = FALSE
  )
  print(interpretationTable)
  
  if(length(vecA)==6){
    print(
      paste0("Because this task doesn't feature uncertainty, ",
             "interpretations predict point values instead of intervals ",
             "and halfway interpretations trivially overlap with their ",
             "main versions.")
    )
  }
  
  consequenceNotionTable = data.frame(
    inferentialStrengthNotion = c("deltaP/ Christensen",
                          "Kemeny & Oppenheim",
                          "Difference",
                          "Carnap",
                          "Nozick",
                          "Mortimer",
                          "Finch",
                          "Rips"),
    min = c(min(deltaP),
            min(kemeny),
            min(difference),
            min(carnap),
            min(nozick),
            min(mortimer),
            min(finch),
            min(rips)),
    max = c(max(deltaP),
            max(kemeny),
            max(difference),
            max(carnap),
            max(nozick),
            max(mortimer),
            max(finch),
            max(rips)),
    mean = c(mean(deltaP),
             mean(kemeny),
             mean(difference),
             mean(carnap),
             mean(nozick),
             mean(mortimer),
             mean(finch),
             mean(rips)),
    median = c(median(deltaP),
               median(kemeny),
               median(difference),
               median(carnap),
               median(nozick),
               median(mortimer),
               median(finch),
               median(rips)),
    stringsAsFactors = FALSE
  )
  print(consequenceNotionTable)
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# USAGE:
# Expects as input 2 equal length vectors containing 6 or fewer boolean values 
# each. VecA should contain whether the side in question makes the 
# antecedent of the conditional true, VecC the conditional. 
# for empty boolean vectors use <- logical()

vectorA <- c(F,T,T,T)
vectorC <- c(F,F,F,T)
analyzeDiceTask(vectorA, vectorC)
