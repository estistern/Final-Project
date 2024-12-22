# DS210 Final Project
## Esti Stern
### December 22 2024

#### Part 1

[Final Project Tableau Workbook - Esti Stern.pdf](https://github.com/user-attachments/files/18222289/Final.Project.Tableau.Workbook.-.Esti.Stern.pdf)

```[Uploading Final Project Par# Loading the data file
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
t 1.R…]()```

[Final Project Part 1 - Esti Stern.pdf](https://github.com/user-attachments/files/18222293/Final.Project.Part.1.-.Esti.Stern.pdf)
