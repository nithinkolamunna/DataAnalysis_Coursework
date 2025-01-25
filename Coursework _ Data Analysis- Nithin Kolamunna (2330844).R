
# Set the working directory
  setwd("/Users/nithinkolamunna/Documents/Documents/MSc Big Data Analytics/Lectures/Semester 2/03. CMM703 - Data Analysis/Course Work")
  getwd()

###############################
### Task 01 ###################
###############################

# Load necessary libraries
  library(ggplot2)

# Load source data and filter dataset
  weatherData = subset(read.csv("02. SriLanka_Weather_Dataset.csv"), time >= "2010-01-01" & time <= "2023-01-01")
  str(weatherData)

# check the filtered data
  min(weatherData$time);max(weatherData$time)

# convert time column to date type column
  weatherData$time = as.Date(weatherData$time)
#print(weatherData$time)

# --------------------------------------------

## Plot 01
# temperature variation by time in cites
  plotTemperature = ggplot(weatherData, aes(x = time)) +
  geom_line(aes(y = temperature_2m_mean, color = city), alpha = 0.3) +
  labs(title = "Temperature by Time",x = "Date",y = "Temperature",color = "City") +
  theme_grey()

  print(plotTemperature)
# --------------------------------------------

## Plot 02
# Scatter Plot of Temperature vs Rain
  plotTempvsRain = ggplot(weatherData, aes(x = temperature_2m_mean, y = rain_sum, color = city)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of Temperature vs Rain",x = "Temperature",y = "Rain",color = "City") + 
  geom_point() +
  theme_grey()

  print(plotTempvsRain)
# --------------------------------------------


## Plots Improve

# Use different colors for each cities, (given colors are much more similar)
# Adding a trend line to show any correlation between temperature and rainfall.

  
  ###############################
  ### Task 02 ###################
  ###############################
  
  # external libraries
  #install.packages("DescTools")
  #install.packages("caret")
  #install.packages("ResourceSelection")
  library(DescTools)
  library(caret)
  library(ResourceSelection)
  
  ## load CSV file
  dt = read.csv("02. lepto_data.csv")
  dataFrame = dt
  
  # (Task 2.1)  
  hist(dataFrame$Age, main = "Distribution of Age")
  boxplot(Age ~ Sex, data = dataFrame, main = "Age Distribution by Sex")
  
  # (Task 2.2)    
  # Checking missing values
  sum(is.na(dataFrame))
  #print(colSums(is.na(dataFrame))) #count per column 
  
  # Replace all the 99 values from NA
  dataFrame[dataFrame =='99'] = NA
  
  # if more than 30% NA values, remove columns
  percentageNA = colMeans(is.na(dataFrame))
  columnsRemoveNA = names(percentageNA[percentageNA > 0.3])
  dataFrame = dataFrame[, !names(dataFrame) %in% columnsRemoveNA]
  #44
  
  
  # Impute data
  for (col in names(dataFrame)){
    #Numerical
    if(is.numeric(dataFrame[, col])){
      dataFrame[is.na(dataFrame[, col]), col] = Mode((dataFrame[, col]), na.rm = TRUE)
    }
    #categorical
    else{
      dataFrame[is.na(dataFrame[, col]), i] = names(sort(table(dataFrame[, i]), decreasing = TRUE))[1]
    }
  }  
  
  # After analysis we can remove serial and year columns,no impact for final result
  dataFrame$Serial = NULL
  dataFrame$Year = NULL
  #42
  
  # checking NA values after column deletion
  unique(colSums(is.na(dataFrame)) > 0) 
  
  # define categorical value columns (Age,Final)
  for (col in names(dataFrame)) {
    if (!(col %in% c("Age", "Final"))) {
      dataFrame[[col]] = as.factor(dataFrame[[col]])
    }
  }
  
  # change 1 to 0 and 2 to 1  (values must be 0 <= y <= 1)
  dataFrame$Final = ifelse(dataFrame$Final == 1, 0,
                           ifelse(dataFrame$Final == 2, 1, dataFrame$Final))  
  
  
  set.seed(146)
  trainID  = sample(nrow(dataFrame),0.8*nrow(dataFrame))
  trainSet = dataFrame[trainID,]
  testSet  = dataFrame[-trainID,]
  #str(trainSet)
  
  testData = testSet[,!names(testSet) %in% c("Final")]
  testDataActl = testSet[,names(testSet) %in% c("Final")]
  
  levelOfAllFactors = lapply(trainSet[, sapply(trainSet, is.factor)], levels)
  
  
  # (Task 2.3)  
  # fit the full model
  fullModel = glm(trainSet$Final ~., data=trainSet,family= binomial(link = "logit"))
  summary(fullModel)
  
  modelOutput = predict(fullModel, newdata = testData, type = "response")
  testOutputBinaryPredict = ifelse(modelOutput >= 0.5, 1, 0)
  
  # accuracy check
  modelAccuracy = sum(testDataActl == testOutputBinaryPredict)/length(testDataActl)
  modelAccuracy
  
  
  # confusion matrix
  testDataActlFactor = as.factor(testDataActl)
  testOutputBinaryPredictFactor = as.factor(testOutputBinaryPredict)
  confusionMatrix(testDataActlFactor,testOutputBinaryPredictFactor)
  
  hoslem.test(trainSet$Final, fitted(fullModel), g = 10)
  
  
  # (Task 2.4)
  nonClinical = glm(Final~Month + Hospital + Sample + ICU + OPD + Sex + Age + Ethnicity + Education +
                      TertiaryEducation  + Prophylactics + Pasttreatments + Chronicillness + 
                      Possibleexposure + WPqPCRDiagnosis + Isolate,
                    data = trainSet,
                    family = binomial(link = "logit"))
  summary(nonClinical)
  
  modelOutputNonClinical = predict(nonClinical, newdata = testData, type = "response")
  testOutputBinaryPredictNonClinical = ifelse(modelOutputNonClinical >= 0.5, 1, 0)
  
  # accuracy check  
  ModelNonClinicalAccuracy = sum(testDataActl == testOutputBinaryPredictNonClinical)/length(testDataActl)
  ModelNonClinicalAccuracy
  
  # confusion matrix
  testDataActlFactor = as.factor(testDataActl)
  testOutputBinaryPredictNonClinicalFactor = as.factor(testOutputBinaryPredictNonClinical)
  confusionMatrix(testDataActlFactor, testOutputBinaryPredictNonClinicalFactor)
  
  hoslem.test(trainSet$Final, fitted(nonClinical), g = 10)
  
  # if p-value (<0.05) suggest that the model not fit the date well,
  # if p-value (>0.05) suggest that model provied a good fot to the data.
  
  
###############################
### Task 03 ###################
###############################

# external libraries
#install.packages("shiny")
#install.packages("DT")  
library(DT)  
library(shiny)
  
  task03 = function(dt,response) {
    
    # qualitative and quantitative (Task3.1)
    
    quantitativeVariables = names(dt)[sapply(dt, is.numeric)]
    qualitativeVariables = names(dt)[sapply(dt, is.character)]
    combinedDF = rbind(data.frame(Variable = qualitativeVariables, Type = "Qualitative"), 
                       data.frame(Variable = quantitativeVariables, Type = "Quantitative"))
    
    
    # Count the missing values and Impute (Task 3.2)
    missingValueDF = t(as.data.frame(colSums(is.na(data))))
    
    for (col in names(dt)) {
      # mean,numeric
      if (col %in% quantitativeVariables && anyNA(dt[[col]])) {
        dt[[col]][is.na(dt[[col]])] = mean(dt[[col]], na.rm = TRUE)
      }
      # mode,categorical  
      else if (col %in% qualitativeVariables && anyNA(dt[[col]])) {
        dt[[col]][is.na(dt[[col]])] = names(sort(table(dt[[col]]), decreasing = TRUE)[1])
      }
    } 
    
    # outliers (Task3.3)
    
    outliers = lapply(dt, function(col) {
      if (is.numeric(col)) {
        q1 = quantile(col, 0.25, na.rm = TRUE)
        q3 = quantile(col, 0.75, na.rm = TRUE)
        iqr = q3 - q1
        min = q1 - 1.5 * iqr
        max = q3 + 1.5 * iqr
        outliers = col[col < min | col > max]
        return(outliers)
      }
    })
    
    # Summarize variables (Task3.4)
    
    plots = list()
    for(col in names(dt)){
      # Histogram for numeric
      if (is.numeric(dt[[col]])) {
        tmp = ggplot(dt, aes(x = dt[[col]])) + 
          geom_histogram(binwidth = 10, fill = "blue", color = "black") +
          labs(title = paste("Histogram of", col), x = col, y = "Frequency")
        
        plots[[paste0("Histogram :", col)]] = tmp
      } 
      # BarPlot for categorical  
      else if (is.factor(dt[[col]]) | is.character(dt[[col]])) {
        tmp = ggplot(dt, aes(x = factor(dt[[col]]))) +
          geom_bar(fill = "lightgreen", color = "black") +
          labs(title = paste("Bar Plot of", col), x = col, y = "Frequency")
        
        plots[[paste0("BarPlot :", col)]] = tmp
      }
      # Not numeric or categorical    
      else {
        print(paste("Variable", col, "is not numeric or categorical."))
      }
    }
    
    # boxplot for outliers
    # layout for the plots
    nos = sum(sapply(dt, is.numeric))
    par(mfrow = c(ceiling(nos/2), 2))
    
    # Create boxplots for each numeric variable
    for (col in names(dt)) {
      if (is.numeric(dt[[col]])) {
        plots[[paste0("BarPlot :", col)]] = boxplot(dt[[col]], main = paste("Boxplot of", col))
      }
    }
    dev.off # reset the boxplot sizes
    
    
    # Model fitting and evaluation (Task3.5)
    
    response = "numeric_var1"
    if(!is.null(response)){
      model= NA 
      modelFit = NA
      isPlot = NA
      
      #linear regression, continuous
      if(all(is.numeric(dt[[response]]) ,length(unique(dt[[response]])) > 2)){
        predi = names(dt)[!names(dt) %in% response]
        formula = paste(response,paste(predi, collapse = "+"),sep = "~")
        fit = lm(formula = formula, data = dt)
        isPlot = 1
        
        #logistic regression,binary  
      }else if(all(is.numeric(dt[[response]]), unique(dt[[response]]) %in% c(0, 1))){
        predi = names(dt)[!names(dt) %in% response]
        formula = paste(response, paste(predi, collapse = "+"), sep = "~")
        fit = glm(formula =formula , data = dt, family = binomial)  
        isPlot = 1
        
      }else{
        print("response variable is not a continuous or binary")
      }
      if(isPlot == 1){
        #plot
        par(mflow = c(2,2))
        plot(fit)
        par(mflow = c(1,1))  
      }
    }else{
      print("response variable is invalid")
    }
    
    return(list(combinedDF = combinedDF, missingValueDF = missingValueDF, outliers = outliers,plots = plots))
  }
  
  # R Shiny Dashboard (Task3.6)
  
  # Design UI
  design = fluidPage(
    titlePanel("Data Analysis - Coursework Dashboard"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV File"),
        actionButton("start", "Start Processing")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Qualitative and Quantitative", tableOutput("combined_table")),
          tabPanel("Missing Values", tableOutput("missing_table")),
          tabPanel("Outliers", DTOutput("outliers_table")), 
          tabPanel("Variable Summaries", uiOutput("variableSum"))  
        )
      )
    )
  )
  
  # Declare server
  shinyServer = function(input, output) {
    
    # Reactive values to store data
    data = reactiveValues(file = NULL,
                          missingValueDF = NULL,
                          combinedDF = NULL,
                          outliers = NULL
    )
    
    # process data
    process_data = function() {
      req(data$file) # file is uploaded
      processed_data = task03(data$file,response)
      data$missingValueDF = processed_data$missingValueDF
      data$combinedDF = processed_data$combinedDF
      data$outliers = processed_data$outliers
    }
    
    # Upload file
    observeEvent(input$file, {data$file = read.csv(input$file$datapath)})
    
    # processing
    observeEvent(input$start, {process_data()})
    
    # Update tables
    output$missing_table = renderTable({data$missingValueDF})
    output$combined_table = renderTable({data$combinedDF})
    
    output$outliers_table = renderDataTable({
      outliers = data$outliers
      outliers_df = data.frame(Column = names(outliers),
                               Outliers = sapply(outliers, function(x) paste(x, collapse = ", ")))
      outliers_df
    })
    
    output$variable_summaries = renderUI({
      if (!is.null(data$plots)) {
        plotT = lapply(names(data$plots), function(plot_name) {plotOutput(plot_name)})
        do.call(tabsetPanel, plotT)
      }
    })
    
    # Render
    observe({
      if (!is.null(data$plots)) {
        for (plotName in names(data$plots)) {
          output[[plotName]] = renderPlot({
            data$plots[[plotName]]
          })
        }
      }
    })
  }
  
  # load Application
  shinyApp(ui = design, server = shinyServer)


