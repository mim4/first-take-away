###########################################################################

library(devtools)
library(usethis)
library(ggplot2)

library(kaggler)
kaggler::kgl_auth(username="martailundain",key="55489399e4f868af9b80e535ef1ab841")

library(shiny)
library(shinythemes)
require(shinyjs)
require(summarytools)

winedata <- kgl_datasets_download(owner_dataset = "rajyellow46/wine-quality", 
                              fileName = "winequalityN.csv")

#Preprocess of the data

wine = na.omit(winedata)
wine$type = as.factor(wine$type)
wine[,7] = log(wine[,7])
colnames(wine)[7] <- "log_free.sulfur.dioxide"
wine[,8] = log(wine[,8])
colnames(wine)[8] <- "log_total.sulfur.dioxide"



#We set up the data given a certain fraction of data to train and a randomseed that we can choose.
setupData <- function(trainpercent, randomseed)
{
  # To make computations reproducible.
  set.seed(randomseed)

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
                            radioButtons("view", "",
                                         choices =
                                           list("About this dataset" = "info",
                                                "Summary" = "sum",
                                                "Raw data" = "raw")),
                            
                            submitButton("Update selected"),
                            
                            ),
                     mainPanel(
                       conditionalPanel(
                         condition = "input.view == 'info'",
                         textOutput("datainfo")
                       ),
                       conditionalPanel(
                         condition = "input.view == 'sum'",
                         verbatimTextOutput("summary")
                       ),
                       conditionalPanel(
                         condition = "input.view == 'raw'",
                         tableOutput("rawdata")
                       )
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
                            "In this tab we can see the main characteristics of the dataset using some plots. These
                            plots are static, we can select between a Histogram (separated by variable type),
                            a boxplot (also with groups separated by type of wine), and a scatterplot.
                            ",
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                submitButton("Click to update selection and show plot"),
                                radioButtons("view2", "",
                                             choices =
                                               list("Histograms" = "histogram",
                                                    "boxplots" = "Boxplots",
                                                    "Scatterplots" = "scatterplot")),
                                
                                conditionalPanel(
                                  condition = "input.view2 == 'histogram'",
                                  
                                  selectInput("numvariables", "Variables",
                                              choices = c("fixed.acidity",
                                                          "volatile.acidity",
                                                          "citric.acid",
                                                          "residual.sugar",
                                                          "chlorides",
                                                          "log_free.sulfur.dioxide",
                                                          "log_total.sulfur.dioxide",
                                                          "density",
                                                          "pH",
                                                          "sulphates",
                                                          "alcohol",
                                                          "quality"),
                                              selected = "fixed.acidity",
                                              multiple = FALSE),
                                  sliderInput("n_bins", label = NULL, min = 10, max = 50, value = 20)
                                ),
                                conditionalPanel(
                                  condition = "input.view2 == 'Boxplots'",
                                  
                                  selectInput("facvariables", "Variables",
                                              choices = c("fixed.acidity",
                                                          "volatile.acidity",
                                                          "citric.acid",
                                                          "residual.sugar",
                                                          "chlorides",
                                                          "log_free.sulfur.dioxide",
                                                          "log_total.sulfur.dioxide",
                                                          "density",
                                                          "pH",
                                                          "sulphates",
                                                          "alcohol",
                                                          "quality"),
                                              selected = "fixed.acidity",
                                              multiple = FALSE)
                                ),
                                conditionalPanel(
                                  condition = "input.view2 == 'scatterplot'",
                                  
                                  selectInput("allvariables1", "Variable 1 (x)",
                                              choices = c("type","fixed.acidity",
                                                                      "volatile.acidity",
                                                                      "citric.acid",
                                                                      "residual.sugar",
                                                                      "chlorides",
                                                                      "log_free.sulfur.dioxide",
                                                                      "log_total.sulfur.dioxide",
                                                                      "density",
                                                                      "pH",
                                                                      "sulphates",
                                                                      "alcohol",
                                                                      "quality"),
                                                          selected = "fixed.acidity",
                                              multiple = FALSE),
                                  selectInput("allvariables2", "Variable 2 (y)",
                                              choices = c("type","fixed.acidity",
                                                          "volatile.acidity",
                                                          "citric.acid",
                                                          "residual.sugar",
                                                          "chlorides",
                                                          "log_free.sulfur.dioxide",
                                                          "log_total.sulfur.dioxide",
                                                          "density",
                                                          "pH",
                                                          "sulphates",
                                                          "alcohol",
                                                          "quality"),
                                              selected = "fixed.acidity",
                                              multiple = FALSE),
                                  h6(paste("Note: We can click on the graph and there will appear in a table the 
                                           observations (up to 10) which are more close to where we click. In order
                                           to see this in the table, click on the button Click to update selection
                                           and show plot. Doing this, the table will be uploaded.")),
                                  
                            
                                
                              )),
                              mainPanel(
                                conditionalPanel(
                                  condition = "input.view2 == 'Boxplots'",
                                  plotOutput("boxplot")
                                ),
                                conditionalPanel(
                                  condition = "input.view2 == 'histogram'",
                                  plotOutput("histplot")
                                ),
                                conditionalPanel(
                                  condition = "input.view2 == 'scatterplot'",
                                  plotOutput("scatplot", click = "plot_click"),
                                  tableOutput("plotinfo")
                                )
                                
                              )
                            )
                          )
                 ),
                 tabPanel("Interactive plots",
                          fluidPage(
                            titlePanel("Interactive plots of the data"),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                p("in this panel")
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Pieplot", plotlyOutput("pie")),
                                            tabPanel("3D plot",   plotlyOutput("threedp")),
                                            tabPanel("Bubble chart",   plotlyOutput("bubble")))

                          )
                 )
                 )),
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
  
  output$rawdata <- renderTable({
    wine
  })
  
  # Show the fit and the confusion matrix of each model.
  
  output$fit <- renderPrint({
    data()$fit
  })
  
  output$confusion <- renderPrint({
    data()$confusion
  })
  
  output$summary <- renderText({
    dfSummary(wine)
  })
  
  output$datainfo <- renderPrint({
    h4("This shiny shows the main characteristics of a wine dataset. This wine dataset
    is a mix of two datasets, one for red wine and another one for white one. The two datasets 
    are related to red and white variants of the Portuguese Vinho Verde wine. The reference
    [Cortez et al., 2009]. Due to privacy and logistic issues, only physicochemical (inputs) 
    and sensory (the output) variables are available (e.g. there is no data about grape types, 
    wine brand, wine selling price, etc.).
       
       
       This datasets has as variables: Input variables (based on physicochemical tests):
fixed acidity, volatile acidity, citric acid, residual sugar, chlorides, free sulfur dioxide, total sulfur dioxide,  density
pH, sulphates, alcohol and an output variable (based on sensory data) which is quality (score between 0 and 10).
    
  Acknowledgements:
         
         P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis.
       Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.")



  })
  
  output$boxplot <- renderPlot({
    ggplot(data=wine, aes_string( wine$type, input$facvariables )) + 
      geom_boxplot(aes_string(fill = wine$type))+ ylab("Total Price") +
      theme_bw() + ggtitle("Boxplot in relationship to Total Price") 
  })
  
  output$histplot = renderPlot({
    ggplot(data = wine, aes_string(x = input$numvariables, color=wine$type)) +
      geom_histogram(bins = input$n_bins, fill = "white", alpha=0.5, position="identity") +
      theme_bw() + ggtitle("Histogram of numerical variables") 
  })
  
  output$scatplot = renderPlot({
    ggplot(data = wine, aes_string(x= input$allvariables1, y= input$allvariables2)) +
      geom_point(shape=18, color="chartreuse3", size = 3) + theme_bw() +
      geom_smooth(method=lm, linetype="dashed",
                  color="black", fill="blue") + ggtitle("Scatterplot of selected variables")
  })
  
  output$plotinfo <- renderTable({
    
      nearPoints(wine, 
                 input$plot_click, threshold = 10, maxpoints = 10,
                 addDist = TRUE)
    
  })
  
  output$pie <- renderPlotly({
    
    plot_ly(wine, labels = ~type,  type = 'pie')  %>% layout(title = 'Pieplot of variable type')
    })
  
  output$threedp = renderPlotly({
    plot_ly(wine, x = ~log_free.sulfur.dioxide, y = ~log_total.sulfur.dioxide, z = ~alcohol) %>%
      add_markers(color = ~type)
  })
  
  output$bubble = renderPlotly({
    
    fig <- plot_ly(wine, x = ~log_total.sulfur.dioxide, y = ~alcohol, text = ~type, type = 'scatter', mode = 'markers', color = ~quality, colors = 'Reds',
                   marker = list(size = ~quality, opacity = 0.5))
    fig <- fig %>% layout(title = 'Sulfur dioxide and alcohol given type and quality',
                          xaxis = list(showgrid = FALSE),
                          yaxis = list(showgrid = FALSE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)