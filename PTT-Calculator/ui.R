library(shiny)
library(tidyverse)
library(dplyr)
library(rapportools)
fluidPage(

    titlePanel("PTT-Calculator"),

    
    sidebarLayout(
        
        sidebarPanel(
            h5("Choose the setup for this task. Keep in mind that for high 
               # of [?], the calculation might take a while (Θ(2^2n))."),
            selectInput("connectiveType", "Select a connective type.", 
                        choices = c('If [A], then [C].', '[A] and [C].')),
            sliderInput("caseCount", "Select the total number of cases.",
                        min = 2, max = 10,
                        value = 6),
            
            numericInput("AC", "# of [A ∧ C] cases.", value = 0, min = 0, 
                         max = 10),
            numericInput("AnC", "# of [A ∧ ¬C] cases.", value = 0, min = 0, 
                         max = 10),
            numericInput("nAC", "# of [¬A ∧ C] cases.", value = 0, min = 0, 
                         max = 10),
            numericInput("nAnC", "# of [¬A ∧ ¬C] cases.", value = 0, min = 0, 
                         max = 10),
            numericInput("blank", "# of [?] cases.", value = 0, min = 0, 
                         max = 10),
            checkboxInput("outputL", "Output in LaTeX format", value = FALSE, width = NULL),
            
            actionButton("show", "Display formulae"),
            actionButton("do","Calculate")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("text"),
            tableOutput("formulae"),
            tableOutput("interpretations"),
            tableOutput("notionsOfArgumentStrength")
        )
    )
)
