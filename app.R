###########################################################################

library(devtools)
library(usethis)

library(kaggler)
kaggler::kgl_auth(username="martailundain",key="55489399e4f868af9b80e535ef1ab841")

library(shiny)
library(shinythemes)
require(shinyjs)
require(summarytools)

winedata <- kgl_datasets_download(owner_dataset = "rajyellow46/wine-quality", 
                              fileName = "winequalityN.csv")


#We set up the data given a certain fraction of data to train and a randomseed that we can choose.
setupData <- function(trainpercent, randomseed)
{
  # To make computations reproducible.
  set.seed(randomseed)
  
  #Preprocess of the data
  
  wine = na.omit(winedata)
  wine$type = as.factor(wine$type)
  wine[,7] = log(wine[,7])
  colnames(wine)[7] <- "log_free.sulfur.dioxide"
  wine[,8] = log(wine[,8])
  colnames(wine)[8] <- "log_total.sulfur.dioxide"

  # Partition intro training and testing.
  spl = caret::createDataPartition(wine$type, p = trainpercent, list = FALSE)  # 80% for training
  
  wineTrain = wine[spl,]
  wineTest = wine[-spl,]
  invisible( list(training=wineTrain, testing=wineTest) )
}

###########################################################################
ui <- navbarPage("First Take Away",
                 theme = shinytheme("spacelab"),
                 tabPanel("Data",
                    fluidPage(
                       titlePanel("Main characteristics of the variables"),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                       p("To show a summary of the data, we have used the function dfSummary,
                                  which shows  univariate statistics and/or frequency distributions, 
                                  bar charts or histograms, as well as missing data counts and proportions"),
              
                     ),
                     mainPanel(
                       verbatimTextOutput("summary")
                     )
                   )
                 )
                 ),
                 tabPanel("Machine Learning",
                          fluidPage(
                            
                            # Application title
                            titlePanel("Wine Classification using Caret"),
                            
                            # Sidebar with controls
                            sidebarLayout(
                              sidebarPanel(
                                submitButton("Click to carry out a certain method"),
                                br(),
                                sliderInput("trainpercent",
                                            "Fraction of data that goes to training",
                                            value = 0.75, step = 0.05, min = 0.50, max = 0.80),
                                br(),
                                numericInput("randomseed", "Random Seed", 1, min=1, max=1000000), #In the report we have set.seed(1)
                                HTML("<hr>"),
                                selectInput("method", h4("Caret Model"),
                                            choices = c("Linear Discriminant Analysis "= "lda",
                                                        "Quadratic Discriminant Analysis " = "qda",
                                                        " Decision Tree" = "rpart"),
                                            selected = NULL,
                                            multiple = FALSE),
                              ),
                              
                              # Tab panels:
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Fit",              verbatimTextOutput("fit")),
                                            tabPanel("ConfusionMatrix",  verbatimTextOutput("confusion")))
                              )
                            )
                          )
                          
                          
                          
                          
                          ),
                 tabPanel("Plots of the data",
                          fluidPage(
                            titlePanel("Interactive plots of the data"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                p("To show a summary of the data, we have used the function dfSummary,
                                  which shows  univariate statistics and/or frequency distributions, 
                                  bar charts or histograms, as well as missing data counts and proportions"),
                                
                              ),
                              mainPanel(
                                
                              )
                            )
                          )
                 ),
                 useShinyjs()
)


# Train our selected model.
generic.fit <- function(method, training, testing)
{
  
  fit <- train(type ~ ., data = training, method=method)
  OutOfSample  <- predict(fit, newdata=testing)
  confusion <- confusionMatrix(testing$type, OutOfSample, positive='white')
  
  invisible( list(fit=fit, confusion=confusion) )
}

# Setup data when the partition or the seed change and use a specified method.
responseRoutine <- function(method, trainpercent, randomseed)
{
  d <- setupData(trainpercent, randomseed)
  fit <- generic.fit(method, d$training, d$testing)
  invisible(fit)
}

###########################################################################
server <- function(input, output) {
  
  
  # Reactive expression called whenever inputs change with setseed or partition.
  data <- reactive({
    responseRoutine(input$method, input$trainpercent, input$randomseed)
  })
  
  # Show the fit and the confusion matrix of each model.
  
  output$fit <- renderPrint({
    data()$fit
  })
  
  output$confusion <- renderPrint({
    data()$confusion
  })
  
  output$summary <- renderPrint({
    dfSummary(wine)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)