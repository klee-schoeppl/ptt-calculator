library(shiny)
library(tidyverse)
library(dplyr)
library(rapportools)

fluidPage(

    titlePanel("Dice Task Calculator"),

    
    sidebarLayout(
        
        sidebarPanel(
            h5("Select the setup for this task, the conditional being:"),
            h5("If the side facing up shows A, then the side shows C."),
            selectInput("one", "Side 1", choices = c('Hidden (?)', 
                                                        'Both A and C', 
                                                        'A, but not C',
                                                        'Not A, but C',
                                                        'Neither A nor C')),
            selectInput("two", "Side 2", choices = c('Hidden (?)', 
                                                        'Both A and C', 
                                                        'A, but not C',
                                                        'Not A, but C',
                                                        'Neither A nor C')),
            selectInput("three", "Side 3", choices = c('Hidden (?)', 
                                                        'Both A and C', 
                                                        'A, but not C',
                                                        'Not A, but C',
                                                        'Neither A nor C')),
            selectInput("four", "Side 4", choices = c('Hidden (?)', 
                                                        'Both A and C', 
                                                        'A, but not C',
                                                        'Not A, but C',
                                                        'Neither A nor C')),
            selectInput("five", "Side 5", choices = c('Hidden (?)', 
                                                        'Both A and C', 
                                                        'A, but not C',
                                                        'Not A, but C',
                                                        'Neither A nor C')),
            selectInput("six", "Side 6", choices = c('Hidden (?)', 
                                                        'Both A and C', 
                                                        'A, but not C',
                                                        'Not A, but C',
                                                        'Neither A nor C')),
            
            actionButton("do","Calculate")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("interpretations"),
            tableOutput("notionsOfaArgumentStrength"),
            textOutput("text")
        )
    )
)



