### Web Tool for EV Price Prediction ###

# getwd()
# setwd("C:/Users/adrian182/OneDrive/Documents/Master's thesis/App/EV_Price_Prediction")

## Loading final data ##
library(readxl)
library(openxlsx)
df_final <- read_excel("df_final.xlsx")
# str(df_final)

# Converting all char columns to factor
df_final <- as.data.frame(unclass(df_final), stringsAsFactors = TRUE)
str(df_final)


# # 1. Stratified Splitting:
library(caret)
set.seed(222)  # Setting seed for reproducibility
# Stratified sampling based on quantiles of 'price'
trainIndexStratified <- createDataPartition(df_final$price, p=0.7, list=FALSE)

trainDataStratified <- df_final[trainIndexStratified, ]
testDataStratified <- df_final[-trainIndexStratified, ]


# # 2. Random Splitting:
# set.seed(222)
# trainIndexRandom <- sample(seq_len(nrow(df_final)), size = floor(0.7 * nrow(df_final)))
# 
# trainDataRandom <- df_final[trainIndexRandom, ]
# testDataRandom <- df_final[-trainIndexRandom, ]



#Installing needeed packages
#Install.packages(c("shiny", "xgboost", "Matrix"))
library(xgboost)
library(shiny)
library(Matrix)

#Loading the saved model from disk
model.xgboost.app <- readRDS(file = "model.xgboost.app.rds")
#Important to know that model was train on  on 10 top  filters used on motors.co.uk, namely
#make + model + registration_year + mileage + body_style + colour + x0_to_62_mph_secs + top_speed_mph + nedc_maximum_ev_range_miles + battery_capacity_in_k_wh,
#it highlight the fact that what featers for prce predicting model find improtant are not always selectable in the filter sidebar on the website,
#therefore it is sometimes hard to deploy working up based on theorital results from pure ML models. As seen from the results
#R-squared decreased a bit, and MAE increased compare to the best xgb model from modeling section.

# Defining the UI of the application
ui <- fluidPage(
  titlePanel("EV Price Predictor - Master's Thesis"),
  
  tags$style(HTML("
     .mainPanel  {
      width: 70% !important;  # Adjust the main panel width so that both widths sum up to 100%
     }
    
    #predictedPrice {
        font-weight: bold;
        font-size: 30px;
        color: black;
    }
 
  ")),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
               # Sliders for Continuous Variables
               selectInput("make", "Make:", 
                           choices = unique(trainDataStratified$make)
               )
        ),
        column(6,
               selectInput("model", "Model:", 
                           choices = unique(trainDataStratified$model)
               )
        ),
        column(6,        
               selectInput("body_style", "Body Style:", 
                           choices = unique(trainDataStratified$body_style)
               )
        ),
        column(6,        
               selectInput("colour", "Colour:", 
                           choices = sort(unique(trainDataStratified$colour))
               )
        ),
        column(6,

               selectInput("registration_year", "Registration Year:", 
                           choices = sort(unique(trainDataStratified$registration_year))
               )
        ),
        column(6,        
               sliderInput("mileage", "Mileage:", 
                           min = min(trainDataStratified$mileage), 
                           max = max(trainDataStratified$mileage), 
                           value = min(trainDataStratified$mileage),
                           step = 100)
        ),
        column(6,        
               sliderInput("x0_to_62_mph_secs", "0 to 62 mph secs:", 
                           min = 0, 
                           max = max(trainDataStratified$x0_to_62_mph_secs), 
                           value = min(trainDataStratified$x0_to_62_mph_secs),
                           step = 1)
        ),
        column(6,        
               sliderInput("top_speed_mph", "Top Speed mph:", 
                           min = min(trainDataStratified$top_speed_mph),  
                           max = max(trainDataStratified$top_speed_mph), 
                           value = min(trainDataStratified$top_speed_mph),
                           step = 10)
        ),
        column(6,        
               sliderInput("nedc_maximum_ev_range_miles", "Range miles:", 
                           min = min(trainDataStratified$nedc_maximum_ev_range_miles),  
                           max = max(trainDataStratified$nedc_maximum_ev_range_miles), 
                           value = min(trainDataStratified$nedc_maximum_ev_range_miles),
                           step = 10)
               
        ),
        column(6,      
               sliderInput("battery_capacity_in_k_wh", "Battery Capacity:", 
                           min = min(trainDataStratified$battery_capacity_in_k_wh),  
                           max = max(trainDataStratified$battery_capacity_in_k_wh), 
                           value = min(trainDataStratified$battery_capacity_in_k_wh),
                           step = 10)
        )
      ),   
      
      # Predict Button
      actionButton("predict", "Predict")
    ),
    
    mainPanel(
      textOutput("predictedPrice")
    )
  )
)


#Server checkpoint
server <- function(input, output, session) { 
  
  # Starting app
  observe({
    # Subset your data for models of the selected make
    available_models <- unique(trainDataStratified$model[trainDataStratified$make == input$make])
    
    # Update the model dropdown
    updateSelectInput(session, "model", choices = available_models)
  })
  
  # This code observes changes in both 'make' and 'model' and updates the 'body_style' dropdown
  observe({
    make <- input$make
    model <- input$model
    
    # Subset your data for body styles based on the selected make and model
    available_body_styles <- unique(trainDataStratified$body_style[trainDataStratified$make == make & trainDataStratified$model == model])
    
    # Update the body_style dropdown
    updateSelectInput(session, "body_style", choices = available_body_styles)
  })
  
  observeEvent(input$predict, {
    
    # Createing  data frame based on the user input
    testData <- data.frame(
      make = as.factor(input$make),
      model = as.factor(input$model),
      body_style = as.factor(input$body_style),
      colour = as.factor(input$colour),
      registration_year = as.factor(input$registration_year),
      mileage = input$mileage,
      x0_to_62_mph_secs = input$x0_to_62_mph_secs,
      top_speed_mph = input$top_speed_mph,
      nedc_maximum_ev_range_miles = input$nedc_maximum_ev_range_miles,
      battery_capacity_in_k_wh = input$battery_capacity_in_k_wh
    )
    
    # Predicting the price using the XGBoost model
    predicted_value <- predict(model.xgboost.app, newdata = testData)
    #predicted_value <- predict.xgb.Booster(model.xgboost.app, newdata = testData)
    
    
    # Displaying the predicted value in a formatted manner
    output$predictedPrice <- renderText({
      paste("Predicted Price:", formatC(round(predicted_value), format="f", big.mark=",", digits=0), "GBP")
    })
  })
  
  # Handle session ending to stop the app
  onSessionEnded(function() {
    stopApp()
  })
}


#Running the app locally
shinyApp(ui, server)

#Publish the app to the server under the link:  https://adrian182.shinyapps.io/EV_Price_Prediction/

