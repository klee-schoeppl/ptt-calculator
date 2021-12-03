library(shiny)
library(tidyverse)
library(dplyr)
library(rapportools)
library(xtable)
library(latex2exp)



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
                 "Pierce (NOR)",
                 "Sheffer (¬&)",
                 "Contravalence (</>)",
                 "Postsection (-/>)",
                 "Replication (<--)",
                 "Postnonpendence (¬C)",
                 "Prenonpendence (¬A)",
                 "Prependence (A)",
                 "Prependence (C)",
                 "Tautology (T)",
                 "Contradiction (⊥)",
                 "Disjunction (OR)",
                 "Presection (</-)",
                 "special & lower-ignored",
                 "deltaP/ Christensen",
                 "Kemeny & Oppenheim",
                 "Difference",
                 "Carnap",
                 "Nozick",
                 "Mortimer",
                 "Finch",
                 "Rips"),
      #"P(A \land C) + P(\neg A \land C) + P(\neg A \land \neg C)"
      #"P(A ∧ C) +  P(¬A ∧ C) + P(¬A ∧ ¬C)",
      formula = c("P(A ∧ C) +  P(¬A ∧ C) + P(¬A ∧ ¬C)",
                  "P(A ∧ C) + P(¬A ∧ ¬C)",
                  "P(A ∧ C)",
                  "P(A ∧ C) / (P(A ∧ C) + P(A ∧ ¬C))",
                  "P(A ∧ C) / (P(A ∧ C) + P(A ∧ ¬C) + P(¬A ∧ C))",
                  "P(¬A ∧ ¬C)",
                  "P(¬A ∨ ¬C)",
                  "P(¬(A <-> C))",
                  "P(A ∧ ¬C)",
                  "P(A ∨ ¬C)",
                  "P(¬C)",
                  "P(¬A)",
                  "P(A)",
                  "P(C)",
                  "P(A ∨ ¬A)",
                  "P(A ∧ ¬A)",
                  "P(A ∨ C)",
                  "P(¬A ∧ C)",
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
    
    formulaexTable = data.frame(
      result = c("Material Implication ($\\rightarrow$)",
                 "Equivalent ($\\leftrightarrow$) ",
                 "Conjunction ($\\land$)",
                 "Conditional Probability ($|$)",
                 "Biconditional ($||$) ",
                 "Pierce (NOR)",
                 "Sheffer ($\\neg \\land$)",
                 "Contravalence ($\\not \\leftrightarrow$)",
                 "Postsection ($\\not \\rightarrow$)",
                 "Replication ($\\leftarrow$)",
                 "Postnonpendence ($\\neg C$)",
                 "Prenonpendence ($\\neg A$)",
                 "Prependence (A)",
                 "Prependence (C)",
                 "Tautology ($\\top$)",
                 "Contradiction ($\\bot$)",
                 "Disjunction (OR)",
                 "Presection ($\\not \\leftarrow$)",
                 "special $\\land$ lower-ignored",
                 "deltaP/ Christensen",
                 "Kemeny and Oppenheim",
                 "Difference",
                 "Carnap",
                 "Nozick",
                 "Mortimer",
                 "Finch",
                 "Rips"),
      
      formula = c("$P(A \\land C) + P(\\neg A \\land C) + P(\\neg A \\land \\neg C)$",
                  "$P(A \\land C) + P(\\neg A \\land \\neg C)$",
                  "$P(A \\land C)$",
                  "$\\frac{P(A \\land C)}{P(A \\land C) + P(A \\land \\neg C)}$",
                  "$\\frac{P(A \\land C)} {P(A \\land C) + P(A \\land \\neg C) + P(\\neg A \\land C)}$",
                  "$P(\\neg A \\land \\neg C)$",
                  "$P(\\neg A \\lor \\neg C)$",
                  "$P(\\neg (A \\leftrightarrow C))$",
                  "$P(A \\land \\neg C)$",
                  "$P(A \\lor \\neg C)$",
                  "$P(\\neg C)$",
                  "$P(\\neg A)$",
                  "$P(A)$",
                  "$P(C)$",
                  "$P(A \\lor \\neg A)$",
                  "$P(A \\land \\neg A)$",
                  "$P(A \\lor C)$",
                  "$P(\\neg A \\land C)$",
                  "Here, $P(A \\land C)$ is calculated by dividing by visible sides only.",
                  "$P(C | A) - P(C | \\neg A)$",
                  "$\\frac{P(A | C) - P(A | \\neg C)}{(P(A | C) + P(A | \\neg C)}$",
                  "$P(C | A) - P(C)$",
                  "$P(A \\land C) - P(A) \\times P(C)$",
                  "$P(A | C) - P(A | \\neg C)$",
                  "$P(A | C) - P(A)$",
                  "$\\frac{P(C | A)}{P(C)} - 1$",
                  "$1 - \\frac{P(¬C | A)}{P(¬C)}$"),
      
      stringsAsFactors = FALSE
    )
    
    if(input$outputL){
      
      
      output$LaTeX1 <- renderPrint({print(xtable(formulaexTable), sanitize.text.function=function(x){x})})
      
      
      
    } else {
      output$LaTeX1 <- NULL
      
    }
    output$LaTeX2 <- NULL
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
      
      #-----------------------------------------------------------------------
      # functions creating halfway-interpretation tables and interpretation rows
      createTable <- function(symbol, fullignore, interpretation){
        table = data.frame(
          interpretation = c(paste (symbol, "upper-ignored"),
                             paste (symbol,"lower-ignored"),
                             paste (symbol, "fully-ignored")),
          min = c(min(interpretation),
                  min(fullignore),
                  min(fullignore)),
          
          max = c(max(fullignore),
                  max(interpretation),
                  max(fullignore)),
          
          stringsAsFactors = FALSE
        )
        return(table)
      }
      
      createRow <- function(name, interpretation){
        row = data.frame(
          interpretation = c(name),
          min = c(min(interpretation)),
          
          max = c(max(interpretation)),
          
          stringsAsFactors = FALSE
        )
        return(row)
      }
      #------------------------------------------------------------------------
      # Calculates the uncertainty intervals for a wide range of 
      # interpretations of the natural language conditionals used. 
      
      # T interpretation
      tautology <- 1
      
      # ⊥ interpretation
      contradiction <- 0 
      
      # A interpretation
      prependence <- pA
      
      # C interpretation
      postpendence <- pC
      
      # ¬A interpretation
      prenonpendence <- 1 - pA
      
      # ¬C interpretation
      postnonpendence <- 1 - pC
      
      # ¬& interpretation (Sheffer stroke)
      negatedConjunction <- 1 - pAC
      
      # halfway ¬& interpretation
      halfwayNegatedConjunction <- 1 - tempAC / input$caseCount
      
      # <-- interpretation
      replication <- ((AC + AnC + nAnC) / input$caseCount)
      
      # --> interpretation
      materialConditional <- ((AC + CnA + nAnC)/ input$caseCount)
      
      # -/> interpretation
      postsection <- 1 - materialConditional
      
      # </- interpretation
      presection <- CnA / input$caseCount
      
      # 'halfway' --> interpretation
      fullignoreMaterialConditional <- ((tempAC + tempCnA + tempnAnC) / input$caseCount)
      
      # <-> interpretation
      equivalent <- (pAC + pnAnC)
      
      # </> interpretation
      contravalence <- 1 - equivalent
      
      # NOR interpretation
      pierce <- 1 - pnAnC
      
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
      specialIgnoredConjunction <- tempAC / (input$caseCount - input$blank)
      specialIgnoredConjunction <- replaceNA(specialIgnoredConjunction)
      
      # OR - interpretation
      disjunction <- 1 - pnAnC
      
      
      
      
      
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
      # prints the results as requested in the UI
      
      interpretationsTable = data.frame(
        interpretation = c(),
        min = c(),
        max = c(),
        stringsAsFactors = FALSE
      )
      
      interpretationsxTable = data.frame(
        interpretation = c(),
        min = c(),
        max = c(),
        stringsAsFactors = FALSE
      )
      
      # Natural language "and"
      if(input$connectiveType == '[A] and [C].'){
        interpretationsTable <- rbind(interpretationsTable, createRow("Conjunction (&)", conjunction))
        
        interpretationsxTable <- interpretationsTable
        interpretationsxTable[["interpretation"]] <- c("conjunction ($\\land$)")
      
        
        
        if (input$includeH){
          #shiny table
          interpretationsTable <- rbind(interpretationsTable, createTable("&", fullignoreConjunction, conjunction))
          interpretationsTable <- rbind(interpretationsTable, createTable("special-&", specialIgnoredConjunction, conjunction))
          
          #LaTeX table
          interpretationsxTable <- rbind(interpretationsxTable, createTable("\\land", fullignoreConjunction, conjunction))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("special-$\\land$", specialIgnoredConjunction, conjunction))
          
        }
      }
      
      # Natural language "or"
      if(input$connectiveType == '[A] or [C]'){
        interpretationsTable <- rbind(interpretationsTable, createRow("Disjunction (OR)", disjunction))
        
        interpretationsxTable <- interpretationsTable
        interpretationsxTable[["interpretation"]] <- c("disjunction ($\\lor$)")
        
      }
      
      # Natural language "if, then"
      if(input$connectiveType == 'If [A], then [C].'){
        interpretationsTable <- rbind(interpretationsTable, createRow("Conditional Probability (|)", conditionalP))
        interpretationsTable <- rbind(interpretationsTable, createRow("Biconditional Probability (||)", biconditionalP))
        interpretationsTable <- rbind(interpretationsTable, createRow("Material Conditional (-->)", materialConditional))
        interpretationsTable <- rbind(interpretationsTable, createRow("Equivalent (<->)", equivalent))
        interpretationsTable <- rbind(interpretationsTable, createRow("Conjunction (&)", conjunction))
        
        interpretationsxTable <- interpretationsTable
        interpretationsxTable[["interpretation"]] <- c("Conditional Probability ($|$)",
                                                       "Biconditional Probability ($||$)",
                                                       "Material Conditional ($\\rightarrow$)",
                                                       "Equivalent ($\\leftrightarrow$)",
                                                       "conjunction ($\\land$)")
        
        if (input$includeH){
          interpretationsTable <- rbind(interpretationsTable, createTable("|", fullignoreConditionalP, conditionalP))
          interpretationsTable <- rbind(interpretationsTable, createTable("||", fullignoreBiconditionalP, biconditionalP))
          interpretationsTable <- rbind(interpretationsTable, createTable("-->", fullignoreMaterialConditional, materialConditional))
          interpretationsTable <- rbind(interpretationsTable, createTable("<->", fullignoreEquivalent, equivalent))
          interpretationsTable <- rbind(interpretationsTable, createTable("&", fullignoreConjunction, conjunction))
          interpretationsTable <- rbind(interpretationsTable, createTable("special-&", specialIgnoredConjunction, conjunction))
          
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$|$", fullignoreConditionalP, conditionalP))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$||$", fullignoreBiconditionalP, biconditionalP))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$\\rightarrow$", fullignoreMaterialConditional, materialConditional))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$\\leftrightarrow$", fullignoreEquivalent, equivalent))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$\\land$", fullignoreConjunction, conjunction))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("special-$\\land$", specialIgnoredConjunction, conjunction))
          
        
        }
      }
      
      # ANY
      if(input$connectiveType == '[A] [any connective] [C].'){
        interpretationsTable <- rbind(interpretationsTable, createRow("Conditional Probability (|)", conditionalP))
        interpretationsTable <- rbind(interpretationsTable, createRow("Biconditional Probability (||)", biconditionalP))
        interpretationsTable <- rbind(interpretationsTable, createRow("Material Conditional (-->)", materialConditional))
        interpretationsTable <- rbind(interpretationsTable, createRow("Equivalent (<->)", equivalent))
        interpretationsTable <- rbind(interpretationsTable, createRow("Conjunction (&)", conjunction))
        interpretationsTable <- rbind(interpretationsTable, createRow("Pierce (NOR)", pierce))
        interpretationsTable <- rbind(interpretationsTable, createRow("Sheffer (¬&)", negatedConjunction))
        interpretationsTable <- rbind(interpretationsTable, createRow("Contravalence (</>)", contravalence))
        interpretationsTable <- rbind(interpretationsTable, createRow("Postsection (-/>)", postsection))
        interpretationsTable <- rbind(interpretationsTable, createRow("Replication (<--)", replication))
        interpretationsTable <- rbind(interpretationsTable, createRow("Postnonpendence (¬C)", postnonpendence))
        interpretationsTable <- rbind(interpretationsTable, createRow("Prenonpendence (¬A)", prenonpendence))
        interpretationsTable <- rbind(interpretationsTable, createRow("Prependence (A)", prependence))
        interpretationsTable <- rbind(interpretationsTable, createRow("Postpendence (C)", postpendence))
        interpretationsTable <- rbind(interpretationsTable, createRow("Tautology (T)", tautology))
        interpretationsTable <- rbind(interpretationsTable, createRow("Contradiction (⊥)", contradiction))
        interpretationsTable <- rbind(interpretationsTable, createRow("Disjunction (OR)", disjunction))
        interpretationsTable <- rbind(interpretationsTable, createRow("Presection (</-)", presection))
        
        interpretationsxTable <- interpretationsTable
        interpretationsxTable[["interpretation"]] <- c("Conditional Probability ($|$)",
                                                       "Biconditional Probability ($||$)",
                                                       "Material Conditional ($\\rightarrow$)",
                                                       "Equivalent ($\\leftrightarrow$)",
                                                       "conjunction ($\\land$)",
                                                       "Pierce (NOR)",
                                                       "Sheffer ($\\neg \\land$)",
                                                       "Contravalence ($\\not \\leftrightarrow$)",
                                                       "Postsection ($\\not \\rightarrow$)",
                                                       "Replication ($\\leftarrow$)",
                                                       "Postnonpendence ($\\neg C$)",
                                                       "Prenonpendence ($\\neg A$)",
                                                       "Prependence (A)",
                                                       "Postpendence (C)",
                                                       "Tautology ($\\top$)",
                                                       "Contradiction ($\\bot$)",
                                                       "Disjunction (OR)",
                                                       "Presection ($\\not \\leftarrow$)")
        
        
        
        if (input$includeH){
          interpretationsTable <- rbind(interpretationsTable, createTable("|", fullignoreConditionalP, conditionalP))
          interpretationsTable <- rbind(interpretationsTable, createTable("||", fullignoreBiconditionalP, biconditionalP))
          interpretationsTable <- rbind(interpretationsTable, createTable("-->", fullignoreMaterialConditional, materialConditional))
          interpretationsTable <- rbind(interpretationsTable, createTable("<->", fullignoreEquivalent, equivalent))
          interpretationsTable <- rbind(interpretationsTable, createTable("&", fullignoreConjunction, conjunction))
          interpretationsTable <- rbind(interpretationsTable, createTable("special-&", specialIgnoredConjunction, conjunction))
          interpretationsTable <- rbind(interpretationsTable, createTable("¬&", halfwayNegatedConjunction, negatedConjunction))
          
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$|$", fullignoreConditionalP, conditionalP))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$||$", fullignoreBiconditionalP, biconditionalP))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$\\rightarrow$", fullignoreMaterialConditional, materialConditional))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$\\leftrightarrow$", fullignoreEquivalent, equivalent))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$\\land$", fullignoreConjunction, conjunction))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("special-$\\land$", specialIgnoredConjunction, conjunction))
          interpretationsxTable <- rbind(interpretationsxTable, createTable("$\\neg \\land$", halfwayNegatedConjunction, negatedConjunction))
          
          
        }
        
        
      }
      
      
      
      
      #-------------------------------------------------------------------------
      # Combine the Notions of inferential strength into a table.
      
      consequenceNotionTable = data.frame(
        inferentialStrengthNotion = c("deltaP/ Christensen",
                                      "Kemeny and Oppenheim",
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
      #-------------------------------------------------------------------------
      # Decide what to output based on the UI settings
      
      if(input$outputL){ #include LaTeX interpretations-table?
        output$LaTeX1 <- renderPrint({print(xtable(interpretationsxTable), sanitize.text.function=function(x){x})})
      } else {
        output$LaTeX1 <- NULL
      }
      
      if (input$includeN){ #include notions-table?
        output$notionsOfArgumentStrength <- renderTable({consequenceNotionTable})
        
        if(input$outputL){ #include LaTeX notions-table?
          
          output$LaTeX2 <- renderPrint({print(xtable(consequenceNotionTable), sanitize.text.function=function(x){x})})
          
          
        } else {
          output$LaTeX2 <- NULL
        }
        
        
      } else {
        output$notionsOfArgumentStrength <- NULL
      }
      output$interpretations <- renderTable({interpretationsTable})
      
      
    }
  })
}

