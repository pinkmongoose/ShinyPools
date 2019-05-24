#server.r
#Darren Green
#21/05/2019

library(shiny)

shinyServer(
  
  function(input,output) {
    D <- reactiveValues()
    D$active <- F
    D$err <- F
    D$warn <- ""
    source("pools.r",local=T)
    
    observeEvent(input$stop, {
      showModal(modalDialog(title="Error","Stop"))
    })
    
    observeEvent(input$submit, {
      D$err <- F
      if (!D$err) withProgress(
        message="Calculating",
        detail="...",
        value=0, min=0, max=1,
        {RunModel()}
      )
      if (!D$err) D$active <- T
    })
    
    
    output$plot <- renderPlot({
      if (D$active) DrawGraph()
    })
    
    output$table <- renderTable({
      if (!D$active) return(NULL)
      cis <- D$cis
      colnames(cis)<-c("Number of positive pools","Low confidence interval","Point estimate","High confidence interval")
      return(cis)
    },digits=3)
  }
  
)
