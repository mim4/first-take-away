library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("spacelab"),
                h1="hola")

# Define server logic required to draw a histogram
server <- function(input, output) { }

# Run the application 
shinyApp(ui = ui, server = server)