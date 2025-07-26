## Problem 1:Please select any data set from below links or any other suitable dataset
crabData = read.csv('CrabAgePrediction.csv',header=T,sep=",")

##Import libraries:
library(sqldf)
library(urca)
library(dplyr)
library(stringr)
library(ggplot2)
library(car)
library(shiny)
library(bslib)
library(rsconnect)

## 2. Select an algorithm suitable for the above data set (Classification/Regression/Clustering/Other)

#Using a linear regression to create a model
crabModel <- lm(Age ~ Length + Diameter + Weight + Shell.Weight + Shucked.Weight, data = crabData)
lengthModel <- lm(Age ~ Length, data = crabData)
diameterModel <-lm(Age ~ Diameter, data = crabData)
weightModel <-lm(Age ~ Weight, data = crabData)
shellWeightModel <-lm(Age ~ Shell.Weight, data = crabData)
shuckedWeightModel <- lm(Age ~ Shucked.Weight, data = crabData)
summary(crabModel)
crPlots(crabModel)
## 3. Explain the mathematical/statistical details of the algorithm

## 4. Create a Shiny application giving end user options to change parameters and to see how do they effect in results. Please remember data science life cycle lecture and follow the suggestions. Explain your data set and machine learning modeling methodology in the decsriptive on your shiny application. 
ui <- page_sidebar(
  # App title ----
  title = "Crab Ages",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "Length",
      label = "Length of Crab (ft):",
      min = 0.19,
      max = 2.04,
      value = 1.36
    ), 
    sliderInput(
      inputId = "Diameter",
      label = "Diameter of Crab (ft):",
      min = 0.14,
      max = 1.63,
      value = 1.06
    ),
    sliderInput(
      inputId = "Weight",
      label = "Weight of Crab (oz):",
      min = 0.06,
      max = 80.1,
      value = 22.8
    ),
    sliderInput(
      inputId = "ShellWeight",
      label = "Shell Weight (cm):",
      min = 0.04,
      max = 28.5,
      value = 6.66
    ),
    sliderInput(
      inputId = "ShuckedWeight",
      label = "Shucked Weight (oz):",
      min = 0.03,
      max = 42.2,
      value = 9.54
    )
  ),
  # Output: Text ----
  textOutput("prediction"),
  textOutput("Lprediction"), 
  textOutput("Dprediction"),
  textOutput("Wprediction"),
  textOutput("SWprediction"),
  textOutput("UWprediction"),
  fluidRow(id ='Plot', plotOutput("Plot")), 
  h3("Data Explanation"), 
  p("For this assignment, I found a dataset on Kaggle that featured different characteristics of crabs. This data set included each crab’s gender, length, diameter, height, weight, shucked weight, viscera weight, shell weight, and age. Shucked weight is the weight of the crab without the shell and viscera weight is weight that wraps around your abdominal organs deep inside the body. Throughout the assignment, subsets and the whole data set were used to perform various analyses."), 
  h3("Model Explanation"), 
  p("	The algorithm chosen is a multiple linear regression which is a statistical method of modeling the relationship between one or more independent variables and one dependent variable. Since the intention for this project is to investigate if the dimensions (weight, length, diameter, etc) of a crab can be used to predict its age, the linear regression model is suitable. Linear regression is a good model for datasets that are continuous, which this one is. The multiple linear regression is expressed as the formula y = b0 + b1x1 + b2x2+....+e, where y is the dependent variable, b0 is the intercept, x1,x2,....xn are independent variables and b1,b2, ….bn are their corresponding coefficients, and e is an error term. This algorithm assumes that the relationship between the variables in linear and that observations are independent of each other. The coefficients in the model are generally estimated using the method of least squares which works by minimizing the sum of the squared differences between observed and predicted values.")
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$Plot = renderPlot({
    values = c(summary(lengthModel)$r.squared, summary(diameterModel)$r.squared, summary(weightModel)$r.squared, summary(shellWeightModel)$r.squared, summary(shuckedWeightModel)$r.squared, summary(crabModel)$r.squared)
    variables= c("length", "diameter", "weight", "shell weight", "shucked weight", "overall")
    #ggplot(plotValues, aes(x=variables, y=values)) + geom_bar() + labs(x="Variables", y ="R^2 Value", title="R^2 Values of each Variable")
    barplot(values, names= variables, main="R^2 Value for each Variable", xlab="Variables", ylab="R^2 Values")
  })
  output$prediction = renderText(
    {
      inputValues = data.frame(Length = input$Length,
                               Diameter = input$Diameter ,
                               Weight = input$Weight,
                               Shell.Weight = input$ShellWeight,
                               Shucked.Weight = input$ShuckedWeight)
      paste("Crab Age Estimation (overall): ",predict(crabModel, inputValues))
    }
  )
  
  output$Lprediction = renderText({
    inputLValues = data.frame(Length = input$Length)
    paste("Crab Age Estimation (length): ", predict(lengthModel, inputLValues))
  })
  
  output$Dprediction = renderText({
    inputDValues = data.frame(Diameter = input$Diameter)
    paste("Crab Age Estimation (diameter): ", predict(diameterModel, inputDValues))
  })
  
  output$Wprediction = renderText({
    inputWValues = data.frame(Weight = input$Weight)
    paste("Crab Age Estimation (weight): ", predict(weightModel, inputWValues))
  })
  
  output$SWprediction = renderText({
    inputSWValues = data.frame(Shell.Weight = input$ShellWeight)
    paste("Crab Age Estimation (shell weight): ", predict(shellWeightModel, inputSWValues))
  })
  
  output$UWprediction = renderText({
    inputUWValues = data.frame(Shucked.Weight = input$ShuckedWeight)
    paste("Crab Age Estimation (shucked weight): ", predict(shuckedWeightModel, inputUWValues))
  })
}

shinyApp(ui = ui, server = server)

## 5. Deploy your application to https://www.shinyapps.io

