
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("2016 Presidential Campaign Contributions"),
  
  fixedRow(
    column(9,
           selectInput("candidate", 
                       label="Choose a candidate",
                       choices=list("All",
                                    "Bush, Jeb",
                                    "Carson, Benjamin S.",
                                    "Clinton, Hillary Rodham",
                                    "Cruz, Rafael Edward 'Ted'",
                                    "Fiorina, Carly",
                                    "Graham, Lindsey O.",
                                    "Huckabee, Mike",
                                    "Jindal, Bobby",
                                    "O'Malley, Martin Joseph",
                                    "Pataki, George E.",
                                    "Paul, Rand",
                                    "Perry, James R. (Rick)",
                                    "Rubio, Marco",
                                    "Sanders, Bernard",
                                    "Santorum, Richard J."))       
    )
  ),  
  
  fixedRow(column(9,
    tabsetPanel(
      tabPanel("Summary", uiOutput("summary")),
      tabPanel("Occupations", plotOutput("occupationsPlot")),
      tabPanel("Employers", plotOutput("employersPlot")),
      tabPanel("States", plotOutput("statesPlot")),
      tabPanel("Raw", dataTableOutput('table'))
    )
  )
  )
))
