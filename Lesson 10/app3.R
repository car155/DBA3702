library("shiny")
library("ggplot2")
library("rstudioapi")

# read data
initwd = getwd()
setwd(dirname(getActiveDocumentContext()$path))
custdata = read.csv("./Data/Customers.csv")
custdata = custdata[complete.cases(custdata$is.member), ]
setwd(initwd)

ui = pageWithSidebar(
  # on the top
  headerPanel("How customer's choices on buying health insurance are affected?"), 
  # on the side
  sidebarPanel(selectInput(inputId="attribute",
                           label="Customer Attribute",
                           choices=c("housing.type", "gender", "is.employed",
                                     "marital.status")),
               radioButtons(inputId="position",
                            label="Bar Positioning",
                            choices=c("stack", "dodge", "fill"))), 
  # the main display panel
  mainPanel(plotOutput("custPlot")) 
)

server = function(input, output) {
  output$custPlot = renderPlot({
    ggplot(custdata) + 
      # note that we must use aes_string since input$attribute is a string, not a column
      geom_bar(aes_string(x=input$attribute, fill="is.member"), 
               position=input$position)
  })
}

app = shinyApp(ui=ui, server=server)
runApp(app)