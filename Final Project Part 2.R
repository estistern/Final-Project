# Loading the data
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
# Average csat scores are also pretty much the same across the states with the exception of Maine with a very low average csat score