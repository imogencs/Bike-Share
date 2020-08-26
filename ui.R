library(ggplot2)
library(shiny)
library(cowplot)
library(magick)
library(gganimate)
library(animation)
library(rsconnect)




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


fluidPage(
  tags$head(tags$style('h1 {color:purple;font-family:serif;font-style:bold;}')),
  tags$head(tags$style('h3 {color:green;font-family:serif;}')),
  tags$head(tags$style('p {font-family:serif;}')),
  
  headerPanel(title="Multivariate Linear Regression Model for Bike Share User Count",
              windowTitle = "Bike Share Model"),
  
  sidebarPanel(
    selectInput(
      inputId = "dataset",
      label = "Which portion of the data set would you like to use?",
      choices = c("Training Data" = "train", 
                  "Testing Data" = "test"), 
      selected = "Training Data"
    ),
    
    sliderInput(
      inputId = "intercept",
      label = "Choose the intercept value: ",
      min = -5000,
      max = 5000,
      value = coeff[1]
    ),
    
    sliderInput(
      inputId = "windspeed",
      label = "Choose the coefficient for wind speed: ",
      min = -5000,
      max = 5000,
      value = coeff[2]
    ),
    
    sliderInput(
      inputId = "atemp",
      label = "Coefficient for percieved temperature: ",
      min = -7000,
      max = 7000,
      value = coeff[3]
    ),
    
    sliderInput(
      inputId = "normDays",
      label = "Coefficient for time (days): ",
      min = -5000,
      max = 5000,
      value = coeff[4]
    ),
    
    sliderInput(
      inputId = "hum",
      label = "Coefficient for humidity: ",
      min = -5000,
      max = 5000,
      value = coeff[5]
    ),
    
    sliderInput(
      inputId = "goodWeather",
      label = "Coefficient for sunny weather (binary value): ",
      min = -5000,
      max = 5000,
      value = coeff[6]
    ),
    
    sliderInput(
      inputId = "mediumWeather",
      label = "Coefficient for cloudy weather (binary value): ",
      min = -5000,
      max = 5000,
      value = coeff[7]
    ),
    
    sliderInput(
      inputId = "workingday",
      label = "Coefficient for working day (binary value): ",
      min = -5000,
      max = 5000,
      value = coeff[8]
    ),
    
    selectInput(inputId = "month", 
                "Select which month to change the coefficient for:",
                choices = c(
                  "January" = "jan", 
                  "February" = "feb", 
                  "March" = "mar", 
                  "April" = "apr", 
                  "May" = "may", 
                  "June" = "jun", 
                  "July" = "jul", 
                  "August" = "aug", 
                  "September" = "sep", 
                  "October" = "oct", 
                  "November" = "nov" ),
                selected = "January"),
    
    sliderInput(
      inputId = "monthCoef",
      label = "Coefficient for selected month: ",
      min = -5000,
      max = 5000,
      value = coeff[9]
    ),
    
    actionButton(
      inputId = "reset",
      label = "Reset to Best Fitted Model"
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    p("By Imogen Cleaver-Stigum"),
    p("Note: The animated plot takes about 15 seconds to render."),
    
    h1("Actual vs Predicted Values Scatterplots"),
    tabsetPanel(type = "tabs",
                tabPanel("Non-Animated Plot", plotOutput("plot1")),
                tabPanel("Animated Plot",    imageOutput("plot2"))
    ),
    
    br(), br(), br(), br(),
    h1("Evaluating How Well this Model Fits"),
    br(), br(), br(), 
    plotOutput('residPlot'),
    br(), br(), br(), 
    p("Coefficient of Determination (R^2): "),
    textOutput("r2"),
    br(), br(), 
    p("RMSEP: "),
    textOutput("rmsep"),
    br(), br(), 
    p("PRESS: "),
    textOutput("press")
  ),
  
  tabsetPanel(type='tabs',
              tabPanel("The Data", 
                       h1("The Data"),
                       
                       p("For this case study, I selected a Bike Sharing Data Set from UCI’s Machine Learning Repository. The topic 
                         of this data is important to me because bike sharing programs are a valuable asset to a community, as well 
                         as an environmentally friendly alternative to other vehicles. In addition, as the Chair of the Gompei’s 
                         Gears Bike Share Program on my campus, I am interested to find out what factors might influence the frequency
                         with which people use the bikes. This information could affect my decisions - for example, I could choose 
                         to maintain and repair the bikes on days when the bikes are less likely to be checked out, to avoid disrupting 
                         any users and to make sure the majority of the bikes will be available for me to fix."),
                       h3("Technical Considerations for the Data"),
                       p("I selected this data set because of the topic as well as some technical considerations. One technical 
                         consideration was that this data set has about 17389 instances total and about 16 attributes (but 
                         the instances are broken up between two data sets), which makes it a good size to use for basic 
                         machine learning techniques. The data is also very clean and has no missing values. It is in a 
                         .csv file, which makes it easy to use in R as a data frame. "),
                       p("This data set has two different files - one which contains the daily data, and one with the hourly data. The other than the “hour” attribute, these files have the same attributes; however the “hour” file has data for only that hour whereas the “day” file has data for the entire day. For this case study, I am only using the file with data about the entire day for simplicity, and because there are significantly fewer instances in the “day” file since both files deal with the same two-year period, making it easier to work with. There is exactly one entry per day.  "),
                       p("The data has attributes including: the season, month, date, and year; the day of the week and whether or not that day is a holiday or a workday; the weather, temperature, humidity, and wind speed; and the count of bike share users on that day. It has data for every day from 1 Jan 2011 to 31 Dec 2012."),
                       p("All of the attributes are normalized, so they are on a range of [0,1] rather than their original ranges. This means that the units are not the original units. 
                        For example, if the coefficient for temperature is 4500, then the highest temperature day will be predicted to have 4500 more bike share users than the lowest 
                        temperature day in the data set. These values are normalized for the whole data set, not only the training set."),
                       p("Several of the attributes are also categorical variables rather than numeric, so they are either 0 or 1. These are indicated with (binary value) on the sliders.
                        If there are n possible values for the categorical variable, then there are n-1 binary predictor variables (or n-1 coefficients) needed in the model. 
                        This is why there are only 2 coefficients for weather (sunny and cloudy), when there are 3 categories (sunny, cloudy, and precipitation). This
                        is also why there are only 11 months you can change the coefficients for. While there is not a coefficient for the final category, you could change that value
                        by changing the coefficients of the other categories all by the same amount, and then modifying the intercept value.")
                       
              ),
              tabPanel("The Data Science Life Cycle", 
                       h1("The Data Science Life Cycle"),
                       
                       h3("1. Discovery"),
                       
                       p("First for the discovery phase, I selected the problem that was meaningful to investigate and considered the 
                         context that surrounds the problem of the bike share user numbers. In this case, there was not much context 
                         I had to learn about since I already understand bike share systems. I did have to consider why it is important 
                         to predict how many users would use a bike share on a given day: this could inform the people who maintain the 
                         bike share about which days they will have access to most of the bikes, as opposed to all of the bikes being 
                         in use, so they can maintain the bikes. Then, I found a data set that addressed this problem. "),
                       p("I also formed a hypothesis to test: if a multivariate linear regression model is used to predict the 
                        count of bike share users on a given day, the user count will depend on the wind speed, perceived temperature, 
                        humidity, date, whether the day is a working day, the weather, and the month. I defined that this model 
                         would be “good enough” if the R^2 value is above .80. I also planned out how I would progress through the rest of this project. "),
                       p("However, some aspects of the discovery phase, such as assigning roles within the data science team, were 
                         not relevant because this was an independent project and not connected to any company. Considering who the stakeholders are in this project was also 
                         not relevant because the project was not intended for use by any company, although it might inform what 
                         I do on the campus bike share, which would affect the other members of the bike share maintenance crew. "),
                       
                       h3("2. Data Preparation"),
                       
                       p("For the data preparation phase, I first split the data into testing and training data sets. Then I 
                         explored the data. I investigated the relationship between each individual attribute available in the 
                         data set, and the total count of users on that day, making a visualization wherever relevant. This 
                         revealed that some potential predictor variables, such as the temperature, had moderate-to-strong 
                         correlations while others, such as humidity and wind speed, had very weak correlations to the user 
                         count when considered individually. Also in the data preparation phase, I modified some of the potential 
                         predictor variables to be easier to analyze with linear regression. For example, I normalized the dates 
                         as “days since 1 Jan 2011.” I also made the categorical variable of weather into a series of three binary 
                         variables, and did the same for the categorical variable of month. "),
                       
                       h3("3. Model Planning "),
                       
                       p("For the model planning phase, I decided on which predictors I might use in the final model. I planned these based on the exploration of each individual variable, as well as the fact that some of the variables are repetitive. For example, it would not make sense to include both perceived temperature and actual temperature in the model because they are very similar. I planned to use a multivariate linear regression model because it allows me to include all the most relevant predictors, but because it is a linear mode, it is relatively simple and easy to calculate, evaluate, and modify. "),
                       
                       h3("4. Model Building"),
                       
                       p("For the model building phase, I wrote the code that implements the multivariate linear regression 
                        algorithm, mainly using R’s lm() function. I also evaluated the model based on how well it described 
                        both the training and testing data sets, and tried out some tweaks such as changing which predictor 
                        variables I used and seeing if the model became more or less accurate. This step revealed that the 
                        predictors that I had planned to use from the hypothesis did give the best model, while adding others 
                        either did not make the model fit the data any better, or made it over-fit the data (meaning there was 
                        a large difference between how well the model fit the training data versus the test data). I evaluated
                        this using the R^2 and RMSEP values for the testing and training data sets. "),
                       
                       h3("5. Communicate Results "),
                       
                       p("To communicate the results, I put the relevant visualizations into the Shiny application in an interactive 
                         manner. I also made the r markdown file very readable, including visualizations wherever they are useful. 
                         Finally, I explained the results in this report. However, some aspects of this phase, such as quantifying 
                         the business value, were not relevant because there is no value of this project to any business. "),
                       
                       h3("6. Operationalize"),
                       
                       p("To operationalize this project, I deployed it as a Shiny app and put it in Github. "),
              ),
              
              tabPanel("Exploration", 
                       h1("Findings from the Exploration Phase"),
                       
                       p("While exploring the data in preparation for building the model, I looked into each potential predictor to see how it related to the user count. 
                        This influenced which variables I used as predictors in the final model. "),
                       p("The date or number of days since 1 Jan 2011 had a positive relationship with the user count, 
                        with a correlation of about .65, indicating a moderate correlation. "),
                       p("The season and month also showed that there were more users during summer and fall, or during 
                        the months in the middle of the year. The month variable (1 to 12) did not have a linear relationship 
                        with the user count, therefore it should be treated as a categorical variable with 12 categories. 
                        I chose to use month rather than season because the month is more specific, whereas the season is more ambiguous. "),
                       p("Note: There are no sliders for the month variables because there would have to be 12 of them, which would "),
                       p("The weather had a significant effect on the user count: days with sunny weather had almost three 
                        times as many users as days with precipitation. "),
                       p("There was not much difference between the relationship of the perceived temperature vs the actual 
                        temperature to the user count. I chose to use the perceived temperature because it had a slightly 
                        higher correlation, but both correlations were around .64, indicating a moderate correlation. "),
                       p("Holidays had slightly fewer users than non-holidays, and there was a slight variation based on the
                        day of the week. I chose to use the variable of whether or not the date is a “working day” (neither
                        weekend nor holiday) because this had the biggest effect on the user count, and including both the 
                        working day variable and the day of the week or holiday variables would be repetitive."), 
                       p("Humidity and wind speed both had relatively weak correlations with the user counts."),
                       p("I can provide the visualizations and more specific calculations to accompany each of these explorations 
                        in the R markdown report. "),
              ), 
              
              tabPanel("The Math", 
                       
                       h1("Mathematical Explanation of the Multivariate Linear Regression "),
                       
                       p("I chose to create a multivariate linear regression model for this data set, even though the count of users using bikes on any given day (the target variable) is actually a discrete variable instead of continuous. "),
                       
                       h3("Selecting Predictor Variables"),
                       
                       p("The target variable, Y, is the total number of bikes rented on a given day. The predictor variables are the wind speed, perceived temperature, month, humidity, date, weather, and whether that day is a workday or not. I selected these variables manually (rather than having some machine learning algorithm figure out which potential predictors the target variable was most dependent on). I selected them based on the findings from the exploration phase, in which I looked at the relationship between each individual variable and the count of bike share users. "),
                       
                       h3("The Math"),
                       
                       p("This method uses a linear combination of all the predictors to find the target 
                         variable for given values of the predictors. For a single predictor, this would be a 
                         basic linear equation with two unknowns: the slope and the intercept. However, since 
                         this model will take into account multiple variables, it has 19 unknowns, which makes 
                         it a good task for the computer to execute instead of a human. The model seeks to minimize 
                         the residuals of all the data points, which are the distances between the actual Y 
                         value and the corresponding prediction. These are minimized using the least squares
                         method. This method, called PRESS (predicted residual sum of squares), is 
                         executed by minimizing the sum of the squares of residuals of each data point: "),
                       p("sum(yi - ŷi)^2"),
                       p("So, for multivariate linear regression, the goal is to find the coefficients for each term in the multivariate linear equation such that this sum is minimized. "),
                       
                       h3("The Algorithm"),
                       
                       p("1. Normalize all the predictor variables so that they are on a range of [0,1]."),
                       p("       - For each value of a particular predictor, subtract the mean of all the values of that predictor, and divide the difference by the range."),
                       p("       - Wind speed, perceived and actual temperature, and humidity are already normalized in the data set. "),
                       p("2. Define the hypothesis function as a linear combination of all the predictors. "),
                       p("       - h(x) = c0 + c1 * x1 + c2 * x2 + … + cn * xn"),
                       p("       - Where h(x) is the hypothesis function, xi are the n predictor variables, and ci are the unknown constants and coefficients. "),
                       p("3. Define the cost function using the least squares method. "),
                       p("       - cost = sum(yi - ŷi)^2 "),
                       p("4. Select the hypothesis function’s unknowns such that the cost function is minimized."),
                       p("       - In R, the function lm() does this. "),
                       
                       h3("The Math Behind the lm() Function"),
                       
                       p("As explained by Matthew Drury and Purdue University Department of Statistics"),
                       p("The implementation of the lm() function in R is rather complex because it has to do error handling, jumping between programming
                        languages (R, C, and Fortran), etc. But
                        the math it uses to fit a linear regression model the same as the usual way of calculating linear regression coefficients.  
                        There are four matrices: the design matrix (X), the vector of parameters (B), the 
                        vector of error terms (E), and the response vector (Y)."),
                       p("The design matrix (587x19) is a matrix with one row for each instance in the data, 
                        so in this case it would have 587 rows because the training data set has 80% of the total, which is 731 instances. The design matrix also has
                        one column for each predictor variable. In this case, it would have 19 columns because there are 19 unknowns, including the y-intercept and the 18 coefficients. 
                        The first column consists of all 1's, and the following columns are variables. "),
                       p("This design matrix is then multiplied by a vector of parameters. This is a matrix with one row, and a column for each predictor 
                        coefficient and intercept (1x19). These values are still unknown. The product of the design matrix and the vector of parameters will give a vector (587x1) 
                        containing the predicted values for the target variable."),
                       p("To this product, we add a 587x1 matrix, called the vector of error terms, containing the error terms for each y-value. These are like residuals. "),
                       p("This sum must be equal to the response vector: a matrix with one row, and a column for each actual y value (the values we are trying to predict) (587x1).  "),
                       p("Y = X * B + E"),
                       p("With these defined, the goal is to minimize the sum of squares of the residuals. The sum of the squared residuals is equal 
                        to E times its transpose: "),
                       p("E * E'= (Y - X * B) * (Y - X * B)'  "),
                       p("To minimize this function, you take the derivative with respect to B and find the minimum (the point where the derivative crosses through zero from negative to positive, 
                        and where the original function is at a minimum compares to the other critical points.) This is the point where the coefficients collectively
                        minimize the sum of squared residuals, therefore it is the best fit model. Ultimately, for the lm() function, this minimization is done in Fortran, not R.
                        This process is difficult to write out in this Shiny app 
                        because there are not as many special/mathematical characters or usual formatting options, but I have written a properly formatted version of this, which I 
                        can provide if needed. "),

                       h3("Evaluating the Model"),
                       
                       p("There are several measures of how well the multivariate linear regression model fits the data. One is by the residuals, explained above. Another is by using the coefficient of determination, R^2. The coefficient of determination describes how much of the variability in the original data is accounted for in the model, so a higher coefficient of determination means the model fits the data better. It is calculated by the following formula: "),
                       p("R^2 = sum(yi - Ӯ)^2 / sum(yi - ŷi)^2"),
                       p("Where Ӯ is the mean, yi is the actual value, and ŷi is the predicted value, summed over the whole dataset. This gives the variation that is explained by the model divided by the total variation, which is the proportion of the total variation that is explained by the model. It should be a value from 0 to 1. "),
                       p("Another way to evaluate the model is by the root	 mean squared error of prediction (RMSEP). It measures the error in the predictions compared to the actual values. This is given by the following formula: "),
                       p("RMSEP = sqrt( 1/n * sum(yi - ŷi)^2 ) "),
                       p("Unlike R^2, this does not give a value between 0 and 1. A lower RMSEP (closer to 0) means the model fits the data better. "),
                       p("I included the aniated plot because it makes it much clearer how the predicted and actual values correspond to each other."),
                       p("Note: All the sums are from i=0 to n where n is the number of values of y. I can provide my written report with formatted equations if needed. "),
                       
              ), 
              tabPanel("Findings", 
                       h1("Findings from the Regression Model"),
                       
                       p("Using the methods described above, I made a linear regression model for the data. The final model used predictors of date, month, wind speed, humidity, perceived temperature, weather, and whether the day was a working day. The table shows the measures of how good the model is: "),
                       strong("Training Data  "),
                       p("R^2   =     .82    "),
                       p("PRESS  = 402564278   "),
                       p("RMSEP   =   830  "),
                       strong("Testing Data"),
                       p("R^2   =   .80"),
                       p("PRESS  =           94202380"),
                       p("RMSEP   =         810"),
                       p("These values are all approximate, like the coefficients of correlation from earlier, because the train and test data sets are separated randomly, so they are slightly different each time the program runs. These values indicate that the model is good enough, as defined earlier. The model only fits the training data a very small amount better than it fits the testing data, which indicates that the model is not over-trained. "),
              ),
              
              tabPanel("Conclusions",
                       h1("Conclusions"),
                       
                       p("In conclusion, this model met the requirements for a good predictive model for this data. It proved the 
                        hypothesis correct, because the model was best when it
                        used the predictor variables outlined in the hypothesis. In addition, the model performed similarly on 
                        the test data and the training data, indicating that 
                        it is not over-trained, so the findings are applicable. This information is useful because it shows that 
                        the bike share has most users on days that are:"),
                       p(" - Hot"),
                       p(" - Working days"),
                       p(" - In the summer and fall months"),
                       p(" - Less windy"),
                       p(" - Less humid"),
                       p(" - Sunny rather than cloudy, or cloudy rather than with precipitation"),
                       p("and that the bike share popularity increases over time. Therefore, days that do not meet these criteria 
                        will likely be the best days for bike maintenance. On
                        days that do meet these criteria, the bike share crew sohuld be prepared for the fact that there will likely be more users. "),
              ), 
              
              tabPanel("Works Cited", 
                       h1("Works Cited"),
                       
                       p('"Bike Sharing Data Set". UCI Machine Learning Repository, 2013, https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset Accessed 10 Jul 2020. '),
                       p('Drury, Matthew. "A Deep Dive Into How R Fits a Linear Model." Scatterplot Smoothers, 2020, 
                        http://madrury.github.io/jekyll/update/statistics/2016/07/20/lm-in-R.html Accessed 20 Jul 2020. '),
                       p('"Linear Regression in Matrix Form." Purdue University, Department of Statistics, 2008. https://www.stat.purdue.edu/~boli/stat512/lectures/topic3.pdf 
                        Accessed 20 Jul 2020.'))
  ),
  
  h6("The code behind this project can be found on Github: https://github.com/imogencs/Bike-Share")
  
)