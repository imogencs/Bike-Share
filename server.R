library(rsconnect)
library(ggplot2)
library(shiny)
library(cowplot)
library(magick)
library(gganimate)
library(animation)

function(input, output, session) {
  
  fullDataset = read.csv("day.csv")
  library(caret)
  splitVector = caret::createDataPartition(fullDataset[,1], p=.8, list=F, times=1)
  train = fullDataset[splitVector,] # the training data set
  test = fullDataset[!row.names(fullDataset) %in% row.names(train),] # the test data set
  
  # normalize
  train$normDays = (train$instant-1)/730
  test$normDays = (test$instant-1)/730
  
  train$goodWeather = ifelse(train$weathersit == 1, 1, 0)
  train$mediumWeather = ifelse(train$weathersit == 2, 1, 0)
  train$badWeather = ifelse(train$weathersit == 3, 1, 0)
  
  test$goodWeather = ifelse(test$weathersit == 1, 1, 0)
  test$mediumWeather = ifelse(test$weathersit == 2, 1, 0)
  test$badWeather = ifelse(test$weathersit == 3, 1, 0)
  
  train$jan = ifelse(train$mnth == 1, 1, 0)
  train$feb = ifelse(train$mnth == 2, 1, 0)
  train$mar = ifelse(train$mnth == 3, 1, 0)
  train$apr = ifelse(train$mnth == 4, 1, 0)
  train$may = ifelse(train$mnth == 5, 1, 0)
  train$jun = ifelse(train$mnth == 6, 1, 0)
  train$jul = ifelse(train$mnth == 7, 1, 0)
  train$aug = ifelse(train$mnth == 8, 1, 0)
  train$sep = ifelse(train$mnth == 9, 1, 0)
  train$oct = ifelse(train$mnth == 10, 1, 0)
  train$nov = ifelse(train$mnth == 11, 1, 0)
  
  test$jan = ifelse(test$mnth == 1, 1, 0)
  test$feb = ifelse(test$mnth == 2, 1, 0)
  test$mar = ifelse(test$mnth == 3, 1, 0)
  test$apr = ifelse(test$mnth == 4, 1, 0)
  test$may = ifelse(test$mnth == 5, 1, 0)
  test$jun = ifelse(test$mnth == 6, 1, 0)
  test$jul = ifelse(test$mnth == 7, 1, 0)
  test$aug = ifelse(test$mnth == 8, 1, 0)
  test$sep = ifelse(test$mnth == 9, 1, 0)
  test$oct = ifelse(test$mnth == 10, 1, 0)
  test$nov = ifelse(test$mnth == 11, 1, 0)
  
  # define the coefficients and constant
  coeff = c(1:19)
  
  # the hypothesis function
  hypothesis <- function(coeffi, i, df) {
    return (coeffi[1] +
              coeffi[2] * df$windspeed[i] +
              coeffi[3] * df$atemp[i] +
              coeffi[4] * df$normDays[i] +
              coeffi[5] * df$hum[i] +
              coeffi[6] * df$goodWeather[i] +
              coeffi[7] * df$mediumWeather[i] + 
              coeffi[8] * df$workingday[i] +
              coeffi[9] * df$jan[i] +
              coeffi[10] * df$feb[i]+
              coeffi[11] * df$mar[i]+
              coeffi[12] * df$apr[i]+
              coeffi[13] * df$may[i]+
              coeffi[14] * df$jun[i]+
              coeffi[15] * df$jul[i]+
              coeffi[16] * df$aug[i]+
              coeffi[17] * df$sep[i]+
              coeffi[18] * df$oct[i]+
              coeffi[19] * df$nov[i])
  }
  
  # find the coefficients and record them
  model = lm(cnt ~ windspeed + atemp + normDays + hum + goodWeather + mediumWeather 
             + workingday + jan + feb + mar + apr + may + jun + jul + aug + sep 
             + oct + nov, data=train)
  for (i in 1:length(model$coefficients)){
    coeff[i] = model$coefficients[i]
  }
  
  # animate 
  # the animations do not display properly in the pdf report
  library(gganimate)
  library(gifski)
  anim = data.frame(cnt=c(train$cnt,fitted(model)), 
                    instant=c(train$instant,train$instant), 
                    indicator = c(rep("Actual Counts (purple)", 587), 
                                  rep("Predicted Counts (green)", 587)))
  colors = c(rep("mediumorchid", 587), rep("chartreuse3", 587))
  
  # functions for evaluating model
  
  # the PRESS function 
  press = function(coeffi, df) {
    sumOfResidualsSquared = 0
    for (i in 1:length(df$instant)) {
      residual = df$cnt[i] - hypothesis(coeffi, i, df)
      sumOfResidualsSquared = sumOfResidualsSquared + residual ^ 2
    }
    return (sumOfResidualsSquared)
  }
  
  # check the R^2 value 
  coeffOfDetermination = function(coeffi, df) {
    numerator = 0
    denominator = 0
    for (i in 1:length(df$instant)){
      numerator = numerator + ( hypothesis(coeffi, i, df) - df$cnt[i] )^2
      denominator = denominator + ( df$cnt[i] - mean(df$cnt) )^2
    }
    return (1 - (numerator / denominator))
  }
  
  
  # Root mean squares error of prediction
  rmsep = function(actual, predicted) {
    return (sqrt( (1/length(actual)) * sum((actual - predicted)^2)))
  }
  
  
  # stuff for the interactive parts of the app
  modifiedCoeff = coeff;
  df = train
  values = data.frame(cnt=c(df$cnt,hypothesis(modifiedCoeff, 1:length(df$instant), df)), 
                      instant=c(df$instant,df$instant), 
                      indicator = c(rep("Actual Counts (purple)", length(df$instant)), 
                                    rep("Predicted Counts (green)", length(df$instant))),
                      colors = c(rep("mediumorchid", length(df$instant)), 
                                 rep("chartreuse3", length(df$instant))))
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$plot1 <- renderPlot({
    df = train 
    if (input$dataset == "test") {
      df = test
    }
    modifiedCoeff[1] = input$intercept
    modifiedCoeff[2] = input$windspeed
    modifiedCoeff[3] = input$atemp
    modifiedCoeff[4] = input$normDays
    modifiedCoeff[5] = input$hum
    modifiedCoeff[6] = input$goodWeather
    modifiedCoeff[7] = input$mediumWeather
    modifiedCoeff[8] = input$workingday
    if (input$month == "jan"){
      modifiedCoeff[9] = input$monthCoef
    }
    else if (input$month == "feb"){
      modifiedCoeff[10] = input$monthCoef
    }
    
    else if (input$month == "mar"){
      modifiedCoeff[11] = input$monthCoef
    }
    else if (input$month == "apr"){
      modifiedCoeff[12] = input$monthCoef
    }
    else if (input$month == "may"){
      modifiedCoeff[13] = input$monthCoef
    }
    else if (input$month == "jun"){
      modifiedCoeff[14] = input$monthCoef
    }
    else if (input$month == "jul"){
      modifiedCoeff[15] = input$monthCoef
    }
    else if (input$month == "aug"){
      modifiedCoeff[16] = input$monthCoef
    }
    else if (input$month == "sep"){
      modifiedCoeff[17] = input$monthCoef
    }
    else if (input$month == "oct"){
      modifiedCoeff[18] = input$monthCoef
    }
    else if (input$month == "nov"){
      modifiedCoeff[19] = input$monthCoef
    }
    values = data.frame(cnt=c(df$cnt,hypothesis(modifiedCoeff, 1:length(df$instant), df)), 
                        instant=c(df$instant,df$instant), 
                        indicator = c(rep("Actual Counts (purple)", length(df$instant)), 
                                      rep("Predicted Counts (green)", length(df$instant))),
                        colors = c(rep("mediumorchid", length(df$instant)), 
                                   rep("chartreuse3", length(df$instant))))
    
    ggplot(values, aes(x=instant,y=cnt, fill=values$color)) +
      geom_point(color=values$colors, size=2, pch=4) + 
      labs(title = "Count of Users vs Date") +
      xlab("Day (from 1 Jan 2011 to 31 Dec 2012)") +
      ylab("Count of Bike Share Users") +
      scale_fill_discrete(name = "Legend", labels = c("Actual Values (green)", "Predicted Values (purple)"))
  })
  
  output$plot2 <- renderImage({
    df = train 
    if (input$dataset == "test") {
      df = test
    }
    modifiedCoeff[1] = input$intercept
    modifiedCoeff[2] = input$windspeed
    modifiedCoeff[3] = input$atemp
    modifiedCoeff[4] = input$normDays
    modifiedCoeff[5] = input$hum
    modifiedCoeff[6] = input$goodWeather
    modifiedCoeff[7] = input$mediumWeather
    modifiedCoeff[8] = input$workingday
    if (input$month == "jan"){
      modifiedCoeff[9] = input$monthCoef
    }
    else if (input$month == "feb"){
      modifiedCoeff[10] = input$monthCoef
    }
    
    else if (input$month == "mar"){
      modifiedCoeff[11] = input$monthCoef
    }
    else if (input$month == "apr"){
      modifiedCoeff[12] = input$monthCoef
    }
    else if (input$month == "may"){
      modifiedCoeff[13] = input$monthCoef
    }
    else if (input$month == "jun"){
      modifiedCoeff[14] = input$monthCoef
    }
    else if (input$month == "jul"){
      modifiedCoeff[15] = input$monthCoef
    }
    else if (input$month == "aug"){
      modifiedCoeff[16] = input$monthCoef
    }
    else if (input$month == "sep"){
      modifiedCoeff[17] = input$monthCoef
    }
    else if (input$month == "oct"){
      modifiedCoeff[18] = input$monthCoef
    }
    else if (input$month == "nov"){
      modifiedCoeff[19] = input$monthCoef
    }
    values = data.frame(cnt=c(df$cnt,hypothesis(modifiedCoeff, 1:length(df$instant), df)), 
                        instant=c(df$instant,df$instant), 
                        indicator = c(rep("Actual Counts (purple)", length(df$instant)), 
                                      rep("Predicted Counts (green)", length(df$instant))),
                        colors = c(rep("mediumorchid", length(df$instant)), 
                                   rep("chartreuse3", length(df$instant))))
    
    outfile <- tempfile(fileext='.gif')
    
    p = ggplot(values, aes(x=instant,y=cnt)) +
      geom_point(color=values$colors, size=2, pch=4) +
      transition_states(indicator)+ 
      ease_aes('cubic-in-out') +
      labs(title = "Count of Users vs Date: Animated",
           subtitle = 'Now Showing {closest_state}') +
      xlab("Day (from 1 Jan 2011 to 31 Dec 2012)") +
      ylab("Count of Bike Share Users")
    
    anim_save("outfile.gif", animate(p)) # New
    
    list(src = "outfile.gif", contentType = 'image/gif')
  }, deleteFile = TRUE)
  
  output$residPlot <- renderPlot({
    df = train 
    if (input$dataset == "test") {
      df = test
    }
    modifiedCoeff[1] = input$intercept
    modifiedCoeff[2] = input$windspeed
    modifiedCoeff[3] = input$atemp
    modifiedCoeff[4] = input$normDays
    modifiedCoeff[5] = input$hum
    modifiedCoeff[6] = input$goodWeather
    modifiedCoeff[7] = input$mediumWeather
    modifiedCoeff[8] = input$workingday
    if (input$month == "jan"){
      modifiedCoeff[9] = input$monthCoef
    }
    else if (input$month == "feb"){
      modifiedCoeff[10] = input$monthCoef
    }
    
    else if (input$month == "mar"){
      modifiedCoeff[11] = input$monthCoef
    }
    else if (input$month == "apr"){
      modifiedCoeff[12] = input$monthCoef
    }
    else if (input$month == "may"){
      modifiedCoeff[13] = input$monthCoef
    }
    else if (input$month == "jun"){
      modifiedCoeff[14] = input$monthCoef
    }
    else if (input$month == "jul"){
      modifiedCoeff[15] = input$monthCoef
    }
    else if (input$month == "aug"){
      modifiedCoeff[16] = input$monthCoef
    }
    else if (input$month == "sep"){
      modifiedCoeff[17] = input$monthCoef
    }
    else if (input$month == "oct"){
      modifiedCoeff[18] = input$monthCoef
    }
    else if (input$month == "nov"){
      modifiedCoeff[19] = input$monthCoef
    }
    
    values = data.frame(cnt=c(df$cnt,hypothesis(modifiedCoeff, 1:length(df$instant), df)), 
                        instant=c(df$instant,df$instant), 
                        indicator = c(rep("Actual Counts (purple)", length(df$instant)), 
                                      rep("Predicted Counts (green)", length(df$instant))),
                        colors = c(rep("mediumorchid", length(df$instant)), 
                                   rep("chartreuse3", length(df$instant))))
    
    hist(values$cnt - hypothesis(modifiedCoeff, 1:length(values$instant), df), 
         main = "Histogram of the Residuals", 
         xlab = "Residual", 
         ylab = "Frequency", 
         col = "mediumorchid")
  })
  
  output$r2 = renderText({
    df = train 
    if (input$dataset == "test") {
      df = test
    }
    modifiedCoeff[1] = input$intercept
    modifiedCoeff[2] = input$windspeed
    modifiedCoeff[3] = input$atemp
    modifiedCoeff[4] = input$normDays
    modifiedCoeff[5] = input$hum
    modifiedCoeff[6] = input$goodWeather
    modifiedCoeff[7] = input$mediumWeather
    modifiedCoeff[8] = input$workingday
    if (input$month == "jan"){
      modifiedCoeff[9] = input$monthCoef
    }
    else if (input$month == "feb"){
      modifiedCoeff[10] = input$monthCoef
    }
    
    else if (input$month == "mar"){
      modifiedCoeff[11] = input$monthCoef
    }
    else if (input$month == "apr"){
      modifiedCoeff[12] = input$monthCoef
    }
    else if (input$month == "may"){
      modifiedCoeff[13] = input$monthCoef
    }
    else if (input$month == "jun"){
      modifiedCoeff[14] = input$monthCoef
    }
    else if (input$month == "jul"){
      modifiedCoeff[15] = input$monthCoef
    }
    else if (input$month == "aug"){
      modifiedCoeff[16] = input$monthCoef
    }
    else if (input$month == "sep"){
      modifiedCoeff[17] = input$monthCoef
    }
    else if (input$month == "oct"){
      modifiedCoeff[18] = input$monthCoef
    }
    else if (input$month == "nov"){
      modifiedCoeff[19] = input$monthCoef
    }
    coeffOfDetermination(modifiedCoeff, df)
  })
  
  
  output$rmsep = renderText({
    df = train 
    if (input$dataset == "test") {
      df = test
    }
    modifiedCoeff[1] = input$intercept
    modifiedCoeff[2] = input$windspeed
    modifiedCoeff[3] = input$atemp
    modifiedCoeff[4] = input$normDays
    modifiedCoeff[5] = input$hum
    modifiedCoeff[6] = input$goodWeather
    modifiedCoeff[7] = input$mediumWeather
    modifiedCoeff[8] = input$workingday
    if (input$month == "jan"){
      modifiedCoeff[9] = input$monthCoef
    }
    else if (input$month == "feb"){
      modifiedCoeff[10] = input$monthCoef
    }
    
    else if (input$month == "mar"){
      modifiedCoeff[11] = input$monthCoef
    }
    else if (input$month == "apr"){
      modifiedCoeff[12] = input$monthCoef
    }
    else if (input$month == "may"){
      modifiedCoeff[13] = input$monthCoef
    }
    else if (input$month == "jun"){
      modifiedCoeff[14] = input$monthCoef
    }
    else if (input$month == "jul"){
      modifiedCoeff[15] = input$monthCoef
    }
    else if (input$month == "aug"){
      modifiedCoeff[16] = input$monthCoef
    }
    else if (input$month == "sep"){
      modifiedCoeff[17] = input$monthCoef
    }
    else if (input$month == "oct"){
      modifiedCoeff[18] = input$monthCoef
    }
    else if (input$month == "nov"){
      modifiedCoeff[19] = input$monthCoef
    }
    rmsep(df$cnt, hypothesis(modifiedCoeff, 1:length(df$cnt), df))
  })
  
  
  output$press = renderText({
    df = train 
    if (input$dataset == "test") {
      df = test
    }
    modifiedCoeff[1] = input$intercept
    modifiedCoeff[2] = input$windspeed
    modifiedCoeff[3] = input$atemp
    modifiedCoeff[4] = input$normDays
    modifiedCoeff[5] = input$hum
    modifiedCoeff[6] = input$goodWeather
    modifiedCoeff[7] = input$mediumWeather
    modifiedCoeff[8] = input$workingday
    if (input$month == "jan"){
      modifiedCoeff[9] = input$monthCoef
    }
    else if (input$month == "feb"){
      modifiedCoeff[10] = input$monthCoef
    }
    
    else if (input$month == "mar"){
      modifiedCoeff[11] = input$monthCoef
    }
    else if (input$month == "apr"){
      modifiedCoeff[12] = input$monthCoef
    }
    else if (input$month == "may"){
      modifiedCoeff[13] = input$monthCoef
    }
    else if (input$month == "jun"){
      modifiedCoeff[14] = input$monthCoef
    }
    else if (input$month == "jul"){
      modifiedCoeff[15] = input$monthCoef
    }
    else if (input$month == "aug"){
      modifiedCoeff[16] = input$monthCoef
    }
    else if (input$month == "sep"){
      modifiedCoeff[17] = input$monthCoef
    }
    else if (input$month == "oct"){
      modifiedCoeff[18] = input$monthCoef
    }
    else if (input$month == "nov"){
      modifiedCoeff[19] = input$monthCoef
    }
    press(modifiedCoeff, df)
  })  
  
  # when the reset button is pressed, reset all the coefficients to the best fit model
  observeEvent(input$reset, {
    updateSliderInput(session, inputId="intercept", value=coeff[1])
    updateSliderInput(session, inputId="windspeed", value=coeff[2])
    updateSliderInput(session, inputId="atemp", value=coeff[3])
    updateSliderInput(session, inputId="normDays", value=coeff[4])
    updateSliderInput(session, inputId="hum", value=coeff[5])
    updateSliderInput(session, inputId="goodWeather", value=coeff[6])
    updateSliderInput(session, inputId="mediumWeather", value=coeff[7])
    updateSliderInput(session, inputId="workingday", value=coeff[8])
    updateSliderInput(session, inputId = "monthCoef", value=coeff[9])
    updateSelectInput(session, inputId = "month", selected = "January")
  })
}
