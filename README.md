# DS210 Final Project
## Esti Stern
### December 22 2024

#### Part 1

https://public.tableau.com/app/profile/esti.stern/viz/FinalProject-EstiStern/Auto-Data

[Final Project Part 1 - Esti Stern.pdf](https://github.com/user-attachments/files/18220748/Final.Project.Part.1.-.Esti.Stern.pdf)

[Uploading Fi# Loading the data file
file_path <- "C:\\Users\\besti\\OneDrive\\Desktop\\Sarah Schenirer\\Intro Data Science\\auto-mpg.csv"
auto_data = read.csv(file_path)

# Check the structure of the data
str(auto_data)

# Change horsepower from chr to num
auto_data$horsepower <- as.numeric(as.character(auto_data$horsepower))

# Split the data into train/test
train <- auto_data[1:300, ]

test <- auto_data[301:398, ]

# Rearrange the sequence of the test data to start from 1 instead of 301
rownames(test) <- seq(length=nrow(test))

# Simple Linear Regression

# using train data 
# weight as independent variable
simple_model <- lm(train$mpg ~ train$weight, data=train)
summary(simple_model)

b0_1 = simple_model$coefficients[1]
b1_1 = simple_model$coefficients[2]

# Multiple R-squared: 0.7741
# Adjusted R-squared: 0.7733
# Linear Regression Equation: y = 40.3879027 + -0.0062524 * weight

# horsepower as independent variable
simple_model2 <- lm(train$mpg ~ train$horsepower, data=train)
summary(simple_model2)

b0_2 = simple_model2$coefficients[1]
b1_2 = simple_model2$coefficients[2]

# Multiple R-squared: 0.6408
# Adjusted R-squared: 0.6396
# Linear Regression Equation: y = 34.794687 + -0.125105 * horsepower


# Multiple Linear Regression

# using train data
# weight, horsepower, displacement as independent variables
multiple_model <- lm(train$mpg ~ train$weight + train$horsepower + train$displacement, data=train)
summary(multiple_model)

b0 <- multiple_model$coefficients[1]
b1 <- multiple_model$coefficients[2]
b2 <- multiple_model$coefficients[3]
b3 <- multiple_model$coefficients[4]

# Multiple R-squared: 0.783
# Adjusted R-squared: 0.7808
# Linear Regression Equation: y = 39.3739544 + -0.0047898 * weight + -0.0205727 * horsepower + -0.0058457 * displacement

# weight, horsepower as independent variables
# removed displacement because not statistically significant
multiple_model2 <- lm(train$mpg ~ train$weight + train$horsepower, data=train)
summary(multiple_model2)

B0 <- multiple_model2$coefficients[1]
B1 <- multiple_model2$coefficients[2]
B2 <- multiple_model2$coefficients[3]

# Multiple R-squared: 0.782 
# Adjusted R-squared: 0.7805
# Linear Regression Equation: y = 40.1577216 + -0.0052317 * weight + -0.0264219 * horsepower

# Using multiple_model2 on test data to predict mpg
y_pred <- B0 + B1*test$weight + B2*test$horsepower
# comparing to actual mpg
y_actual <- test[, 1]
error <- y_actual - y_pred
# Residual Plot
plot(error, xlab="Error", ylab="Residual")
abline(0,0 ,col='red')
# Histogram
hist(error, prob=T, breaks=20, xlab="Error Residual", ylab="Density")
nal Project Part 1.R…]()

#### Part 2

[Upload# Loading the data
file_path <- "C:\\Users\\besti\\OneDrive\\Desktop\\Sarah Schenirer\\Intro Data Science\\Call_Center.csv"
call_data <- read.csv(file_path)

# Checking the structure of the data
str(call_data)

# Viewing the first 6 rows of data
head(call_data)


# Find factors that impact Sentiment

# Does Call duration have an impact on sentiment?

# Factoring and changing Sentiment to numeric:
# Very Negative - -2
# Negative - -1
# Neutral - 0
# Positive - 1
# Very Positive - 2

call_data$Sentiment <- as.numeric(factor(call_data$Sentiment, 
                                         levels = c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive"))) - 3 

# Simple Linear Regression
# Independent variable - call duration
# Dependent variable - sentiment

model <- lm(Sentiment ~ Call.Duration.In.Minutes, data = call_data)
summary(model)

# The R=squared shows that this is not a good model
# Call duration does not have a significant impact on sentiment


# Does response time impact sentiment?

# Factoring and changing Response Time to numeric:
# Below SLA - -1
# Within SLA - 0
# Above SLA - 1

call_data$Response.Time <- as.numeric(factor(call_data$Response.Time,
                                             levels = c("Below SLA", "Within SLA", "Above SLA"))) -2

# Simple Linear Regression
# Independent variable - response time
# Dependent variable - sentiment

model2 <- lm(Sentiment ~ Response.Time, data = call_data)
summary(model2)

# The R-squared shows that this is not a good model
# Response time does not have a significant impact on sentiment

# Does channel impact sentiment?

# Simple Linear Regression
# Independent variable - channel
# Dependent variable - sentiment

model3 <- lm(Sentiment ~ Channel, data = call_data)
summary(model3)

# The R-squared shows that this is not a good model
# Channel does not have a significant impact on sentiment

# Does it change when the independent variables are put together in multiple linear regression?

# Multiple Linear Regression
# Independent variables - call duration, response time, channel
# Dependent variable - sentiment

mmodel <- lm(Sentiment ~ Call.Duration.In.Minutes + Response.Time + Channel, data = call_data)
summary(mmodel)

# Again the R-squared shows that this is not a good model
# None of the independent variables have a significant impact on sentiment

# Conclusion
# None of the variables tested have a significant impact on sentiment
# It is still unknown what variables do impact sentiment

# Compare call duration, sentiment and csat score by Channel

# Factoring Channel
call_data$Channel <- factor(call_data$Channel)

# Summary statistics for call duration, sentiment and csat score by channel

summary_call_duration <- aggregate(Call.Duration.In.Minutes ~ Channel, data = call_data, summary)
summary_call_duration

summary_sentiment <- aggregate(Sentiment ~ Channel, data = call_data, summary)
summary_sentiment

summary_csat_score <- aggregate(Csat.Score ~ Channel, data = call_data, summary)
summary_csat_score

# Boxplots

library(ggplot2)

# Call duration by Channel
ggplot(call_data, aes(x = Channel, y = Call.Duration.In.Minutes)) + geom_boxplot() + labs(title = "Call Duration by Channel", x = "Channel", y = "Call Duration in Minutes")
# Call duration is the same across the different channels

# Sentiment by Channel
ggplot(call_data, aes(x = Channel, y = Sentiment)) + geom_boxplot() + labs(title = "Sentiment by Channel", x = "Channel", y = "Sentiment")
# Sentiment is the same across the different channels

# Csat score by Channel
ggplot(call_data, aes(x = Channel, y = Csat.Score)) + geom_boxplot() + labs(title = "CSat Score by Channel", x = "Channel", y = "CSat Score")
# Call-Center and Web have the same median but call-center has a wider spread which means that there is mor variability in call-center
# Csat score for chatbot and email are the same
# the Csat score column has a lot of null values which were removed in the boxplot

# Conclusion 
# Call duration and sentiment are the same across the different channels
# Csat scores are different for different channels but many rows of data have null values for Csat

# Compare call duration, sentiment and csat score by state

# Factoring State
call_data$State <- factor(call_data$State)

# Mean call duration by state
call_duration_by_state <- aggregate(Call.Duration.In.Minutes ~ State, data = call_data, mean)

# Mean sentiment by state
sentiment_by_state <- aggregate(Sentiment ~ State, data = call_data, mean)

# Mean csat score by state
csat_by_state <- aggregate(Csat.Score ~ State, data = call_data, mean, na.rm=TRUE)
# removed null values

# Bar plots

# Call duration by state
ggplot(call_duration_by_state, aes(x = reorder(State, Call.Duration.In.Minutes), y = Call.Duration.In.Minutes)) + geom_bar(stat = "identity") + labs(title = "Average Call Duration by State", x = "State", y = "Average Call Duration") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sentiment by State
ggplot(sentiment_by_state, aes(x = reorder(State, Sentiment), y = Sentiment)) + geom_bar(stat = "identity") + labs(title = "Average Sentiment by State", x = "State", y = "Average Sentiment") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Csat by state
ggplot(csat_by_state, aes(x = reorder(State, Csat.Score), y = Csat.Score)) + geom_bar(stat = "identity") + labs(title = "Average Csat Score by State", x = "State", y = "Average Csat Score") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Conclusion
# Average sentiment changes by state with Wyoming having the lowest average sentiment by far. 
# Average call duration is pretty much the same across all the states with slight variation
# Average csat scores are also pretty much the same across the states with the exception of Maine with a very low average csat scoreing Final Project Part 2.R…]()


[Final Project Part 2 - Esti Stern.pdf](https://github.com/user-attachments/files/18220751/Final.Project.Part.2.-.Esti.Stern.pdf)
