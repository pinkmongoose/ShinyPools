#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme="styles.css",
    titlePanel(
        "Prevalence in pooled testing"),
    sidebarLayout(
        sidebarPanel(
            helpText("Select values for the model run and then click 'GO!'."),
            h4("Sample sizes"),
            sliderInput("m","size of each pool",min=1,max=250,step=1,value=10),
            sliderInput("n","number of pools",min=1,max=250,step=1,value=10),
            h4("Test values"),
            sliderInput("sens","test sensitivity",min=0.5,max=1,step=0.01,value=1),
            sliderInput("spec","test specificity",min=0.5,max=1,step=0.01,value=1),
            h4("Other parameters"),
            sliderInput("ci","confidence interval",min=0.1,max=0.998,step=0.01,value=0.95),
            actionButton("submit","GO!")
        ),
        mainPanel(
            h4("Sample prevalence versus number of positive pools"),
            plotOutput("plot",height="500px"),
            tableOutput("table")
        )
    ),
    img(src='ioa_logo.png',style="width: 256px; align: left; margin-right: 2em"),
    "Darren Green (2021), with inspiration from www.ausvet.com.au",
    img(src='parasite_2.png',style="width: 64px; align: right; margin-left: 2em")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
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

# Run the application 
shinyApp(ui = ui, server = server)
