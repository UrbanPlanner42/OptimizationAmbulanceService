
function(input, output) {
  
  
  schedule_data<-reactive({
    
    nua_schedule <- nua_schedule(incident,
                                 input$ambulances,
                                 input$month,
                                 input$weekday,
                                 input$shift_begin,
                                 input$shift_end)
    
    return(nua_schedule)
  })
  
  
  
  
  output$probabilty_table <- renderTable({
    
    probablity_data <- schedule_data()
    
    probablity_data
  })
}
