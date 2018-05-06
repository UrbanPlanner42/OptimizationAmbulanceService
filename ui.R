library(shiny)

#Layout
fluidPage(
  
  # Application title
  titlePanel("Probability of Non Available Ambulance"),
  
  #NUA_Schedule Plot
  mainPanel(
    tableOutput("probabilty_table")
    ),
  
  fluidRow(
    
      column(4, offset = 1,
      numericInput("ambulances", "Enter the number of Ambulances:", 5),
      selectInput("month","Select the month",choices = month,selected = 11),
      selectInput("weekday","Select the day",choices = weekdays,selected = "Friday")
      ),
      
      column(4,
      numericInput("shift_begin", "Beginning of Shift", 0),
      numericInput("shift_end", "End of shift", 12))
  )
)
             