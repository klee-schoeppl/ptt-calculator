library(shiny)
library(tidyverse)
library(dplyr)
library(rapportools)

server <- function(input, output) {
  observeEvent(input$show, {
      output$text <- renderText("Incomplete interpretations are calculated by
                                ignoring the [?] sides when determining the 
                                lower bound, upper bound or both.")
      formulaeTable = data.frame(
        result = c("Material Implication (-->)",
                   "Equivalent (<->) ",
                   "Conjunction (&)",
                   "Conditional Probability (|)",
                   "Biconditional (||) ",
                   "special & lower-ignored",
                   "deltaP/ Christensen",
                   "Kemeny & Oppenheim",
                   "Difference",
                   "Carnap",
                   "Nozick",
                   "Mortimer",
                   "Finch",
                   "Rips"),
        formula = c("P(A ∧ C) +  P(¬A ∧ C) + P(¬A ∧ ¬C)",
                    "P(A ∧ C) + P(¬A ∧ ¬C)",
                    "P(A ∧ C)",
                    "P(A ∧ C) / (P(A ∧ C) + P(A ∧ ¬C))",
                    "P(A ∧ C) / (P(A ∧ C) + P(A ∧ ¬C) + P(¬A ∧ C))",
                    "Here, P(A ∧ C) is calculated by dividing by visible sides only.",
                    "P(C | A) - P(C | ¬A)",
                    "(P(A | C) - P(A | ¬C)) / (P(A | C) + P(A | ¬C))",
                    "P(C | A) - P(C)",
                    "P(A ∧ C) - P(A) * P(C)",
                    "P(A | C) - P(A | ¬C)",
                    "P(A | C) - P(A)",
                    "(P(C | A) / P(C)) - 1",
                    "1 - (P(¬C | A) / P(¬C))"),
        
       
        stringsAsFactors = FALSE
      )
      output$formulae <- renderTable({formulaeTable})
      output$interpretations <- NULL
      output$notionsOfArgumentStrength <- NULL
    
    
  })
  observeEvent(input$do, {
    output$formulae <- NULL
    #---------------------------------------------------------------------------------
    #Checks the input;
   
    if ((input$blank + input$AC + input$AnC + input$nAC + input$nAnC) != input$caseCount){
      output$text <- renderText("Please make sure the cases add up to your chosen total.")
      output$interpretations <- NULL
      output$notionsOfArgumentStrength <- NULL
    } else {
      if (input$blank == 0){
        output$text <- renderText("Because this task doesn't feature uncertainty, 
                                  interpretations predict point values instead of intervals
                                  and halfway interpretations trivially overlap with their main 
                                  versions.")
      } else {
        output$text <- renderText("")
      }
      #--------------------------------------------------------------------------------
      # Saves the inputs to two Boolean vectors;
      vecA <- logical()
      vecC <- logical()
   
      i <- input$AC
      while (i > 0) {
        vecA <- c(vecA, TRUE)
        vecC <- c(vecC, TRUE)
        i <- i - 1
      }
      i <- input$AnC
      while (i > 0) {
        vecA <- c(vecA, TRUE)
        vecC <- c(vecC, FALSE)
        i <- i - 1
      }
      i <- input$nAC
      while (i > 0) {
        vecA <- c(vecA, FALSE)
        vecC <- c(vecC, TRUE)
        i <- i - 1
      }
      i <- input$nAnC
      while (i > 0) {
        vecA <- c(vecA, FALSE)
        vecC <- c(vecC, FALSE)
        i <- i - 1
      }
      #------------------------------------------------------------------------
      # Creating the table.
      aBlanks = list()
      for (i in 1 : input$caseCount){
        aBlanks <- append(aBlanks, c(0))
       
      }
      cBlanks <- aBlanks
      constituents <<- NULL
      
      
      if (input$blank > 0){ 
        for (i in 1 :input$blank){ #preparation to expand grid
          additionalVariable <- list(c(0,1),c(0,1))
          constituents <- c(constituents, additionalVariable)
        }
        constituents <- rev(expand.grid(constituents)) #grid expansion
        for (i in 1 : input$blank){ #grid to vectors
          aBlanks[[i]]<-constituents[,i]
          cBlanks[[i]]<-constituents[,(i+input$blank)]
        }
      }
      #-----------------------------------------------------------------------
      # function substituting 0 for NA values resulting from division by 0.
      replaceNA <- function(column){
        for (i in 1 : length(column)){
          if (is.na(column[i]) | is.nan(column[i])){
            column[i]<-c(0)
          }
        }
        return(column)
      }
      #------------------------------------------------------------------------
      # Create vectors containing the count of relevant cases
      # nX contains the count of ¬X cases
      # XY contains the count of X ∧ Y cases
      # tempX contains the count of X if one ignores all [?] cases
      
      A <- sum(vecA) + aBlanks[[1]] 
      C <- sum(vecC) + cBlanks[[1]] 
      for (i in 2 : input$caseCount) {
        A <- A + aBlanks[[i]]
        C <- C + cBlanks[[i]]
      }
      
      nA<- input$caseCount - A 
      nC<- input$caseCount - C 
      tempA <- sum(vecA) 
      tempC <- sum(vecC) 
      
      tempAC <- 0 
      if (length(vecA)>0){
        for (i in 1 : length(vecA)){
          if (vecA[i] & vecC[i]){
            tempAC <- tempAC + 1
          }
        }
      }
      
      AC <- tempAC 
      if (input$blank > 0){
        for (i in 1 : input$blank){
          AC <- AC + ((aBlanks[[i]])*(cBlanks[[i]])) 
        }}
      
      tempnAnC <- 0 
      if(length(vecA)>0){
        for (i in 1 : length(vecA)){
          if (!vecA[i] & !vecC[i]){
            tempnAnC <- tempnAnC + 1
          }
        }
      }
      
      nAnC <- tempnAnC 
      if (input$blank > 0){
        for (i in 1 : input$blank){
          nAnC <- nAnC + ((1-aBlanks[[i]])*(1-cBlanks[[i]]))
        }}
      
      CnA <- C - AC
      tempCnA <- tempC - tempAC 
      AnC <- A - AC
      tempAnC <- tempA - tempAC 
      
      
      #pXgY is the probability of X given Y
      pCgA<-AC/A
      pCgA <-replaceNA(pCgA)
      pCgnA<-CnA/nA
      pCgnA <- replaceNA(pCgnA)
      pAgC <- AC/C
      pAgC <- replaceNA(pAgC)
      pAgnC <- AnC/nC
      pAgnC <- replaceNA(pAgnC)
      pnCgA <- AnC/A
      pnCgA <- replaceNA(pnCgA)
      
      # pX is the probability of X
      pC <- C / input$caseCount
      pA <- A / input$caseCount
      pnC <- nC / input$caseCount
      pnA <- nA / input$caseCount
      pAC <- AC / input$caseCount
      pnAnC <- nAnC / input$caseCount
      
      #-------------------------------------------------------------------------
      print(aBlanks)
      print(cBlanks)
      print(A)
      print(C)
      print(AC)
      print(nAnC)
      
      #-----------------------------------------------------------------------
      # Calculates the uncertainty intervals for a wide range of 
      # interpretations of the natural language conditionals used. 
      
      # --> interpretation
      materialConditional <- ((AC + CnA + nAnC)/ input$caseCount) 
      
      # 'halfway' --> interpretation
      fullignoreMaterialConditional <- ((tempAC + tempCnA + tempnAnC) / input$caseCount)
      
      # <-> interpretation
      equivalent <- (pAC + pnAnC)
      
      # 'halfway' <-> interpretation
      fullignoreEquivalent <- tempAC / input$caseCount + tempnAnC / input$caseCount
      
      # & interpretation
      conjunction <- pAC
      
      # 'halfway'& interpretation
      fullignoreConjunction <- tempAC / input$caseCount
      
      # | interpretation
      conditionalP <- pCgA
      
      # 'halfway' | interpretation
      fullignoreConditionalP <- tempAC / tempA 
      fullignoreConditionalP<-replaceNA(fullignoreConditionalP)
      
      # || interpretation
      biconditionalP <- AC / (input$caseCount - nAnC) 
      biconditionalP<-replaceNA(biconditionalP)
      
      # 'halfway' || interpretation
      fullignoreBiconditionalP <- tempAC / (input$caseCount - tempnAnC)
      fullignoreBiconditionalP<-replaceNA(fullignoreBiconditionalP)
      
      # special &l interpretation
      specialLowignoredConjunction <- tempAC / (input$caseCount - input$blank)
      specialLowignoredConjunction <- replaceNA(specialLowignoredConjunction)
      
      #-----------------------------------------------------------------------
      # Calculates various measures of confirmation, among them delta P.
      
      # Nozick, 1981 
      nozick<-pAgC -pAgnC
      
      # Christensen, 1999 (also 'delta P')
      deltaP<-pCgA-pCgnA 
      
      # Kemeny & Oppenheim, 1952
      kemeny<-(pAgC-pAgnC)/(pAgC+pAgnC) 
      kemeny<-replaceNA(kemeny)
      
      # Finch, 1960
      finch<- (pCgA/pC)
      finch<-replaceNA(finch)
      finch <- finch - 1
      
      # Rips, 2001
      rips<-pnCgA/(pnC)
      rips<-replaceNA(rips)
      rips<- 1-rips
      
      # difference (Carnap, 1962; Eells, 1982; Jeffrey, 1992)
      difference<- pCgA - pC
      
      # Carnap, 1962 
      carnap<-(pAC)-(pA * pC)
      
      # Mortimer, 1988
      mortimer<- pAgC - pA
      
      #-----------------------------------------------------------------------
      # prints the results
      
      interpretationsTable = data.frame(
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
      
      output$interpretations <- renderTable({interpretationsTable})
      output$notionsOfArgumentStrength <- renderTable({consequenceNotionTable})
     
      
    }
    })
}

