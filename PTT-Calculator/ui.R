library(shiny)
library(tidyverse)
library(dplyr)
library(rapportools)
fluidPage(
    
    titlePanel("PTT-Calculator"),
    
    
    sidebarLayout(
        
        sidebarPanel(
            h4("Choose the setup for this task."),
            
            selectInput("connectiveType", "Select a connective type.", 
                        choices = c('If [A], then [C].', 
                                    '[A] and [C].', 
                                    '[A] or [C]',
                                    '[A] [any connective] [C].')),
            
            h5("As the total number of possible interpretation is quite large, 
               we have tried to group them according to the natural language 
               connectives that they seem most appropriate for. If you wish 
               to see every interpretation, choose '[A] [any connective] [C]'."),
            
            sliderInput("caseCount", "Select the total number of cases.",
                        min = 2, max = 10,
                        value = 6),
            
            h5("Keep in mind that for a high number (n) of [?] (especially above 6), the 
               calculation might take a while (Θ(2^2n))."),
            
            
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
            
            checkboxInput("outputL", "Include output in LaTeX format", 
                          value = FALSE, width = NULL),
            checkboxInput("includeH", "Include halfway interpretations",
                          value = FALSE, width = NULL),
            checkboxInput("includeN", "Include Notions of Argument Strength",
                          value = FALSE, width = NULL),
            
            actionButton("show", "Display formulae"),
            actionButton("do","Calculate Task")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            textOutput("text"),
            tableOutput("formulae"),
            tableOutput("interpretations"),
            tableOutput("notionsOfArgumentStrength"),
            textOutput("LaTeX1"),
            textOutput("LaTeX2")
        )
    )
)

