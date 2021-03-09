library(shiny)
library(shinythemes)
require(shinyjs)
# Define UI for application that draws a histogram
ui <- navbarPage("First Take Away",
                 theme = shinytheme("spacelab"),
                 tabPanel("Data"),
                 tabPanel("Machine Learning"),
                 useShinyjs()
)

# Define server logic required to draw a histogram
server <- function(input, output) { }

# Run the application 
shinyApp(ui = ui, server = server)