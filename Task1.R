#library(readxl)
#library(xlsx)
#library(tidyr)
# <- read_excel("Annalect_case_data_coding_and_simple_regression_2023.xlsx", skip = 12)
#my_data = my_data[,6:dim(my_data)[2]]
#load(my_data)
#write.csv(my_data, "data.csv", row.names=FALSE)
data = read.csv('data.csv')
#my_data %>% drop_na()
Pizza = data$Pizza
Year2017 = data[1:52,]
Year2018 = data[53:104,]
Year2019 = data[105:157,]
Year2020 = data[158:209,]
Year2021 = data[210:dim(data)[1],]


years = c("2017", "2018", "2019", "2020", "2021")
colors = c("blue", "green", "red", "purple", "orange")
dev.new()
par(mfrow = c(1, 2))

# Create the first bar plot
barplot(c(sum(Year2017$Pizza), sum(Year2018$Pizza), sum(Year2019$Pizza), sum(Year2020$Pizza), sum(Year2021$Pizza)), 
        names.arg = years, 
        xlab = "Year", ylab = "Number of Sold Pizza",col=colors)

# Create the second bar plot
barplot(c(mean(Year2017$Temperature), mean(Year2018$Temperature), mean(Year2019$Temperature), mean(Year2020$Temperature), mean(Year2021$Temperature)), 
        names.arg = years, 
        xlab = "Year", ylab = "Temperature mean",col=colors)

#####################################################################################
# Create the first bar plot
dev.new()
par(mfrow = c(1, 2))
barplot(c(sum(Year2017$Pizza), sum(Year2018$Pizza), sum(Year2019$Pizza), sum(Year2020$Pizza), sum(Year2021$Pizza)), 
        names.arg = years, 
        xlab = "Year", ylab = "Number of Sold Pizza",col=colors)

# Create the second bar plot
barplot(c(mean(Year2017$Sun), mean(Year2018$Sun), mean(Year2019$Sun), mean(Year2020$Sun), mean(Year2021$Sun)), 
        names.arg = years, 
        xlab = "Year", ylab = "Sun mean",col=colors)
########################################################################################
# Create the first bar plot
dev.new()
par(mfrow = c(1, 2))
# Create the first bar plot
barplot(c(sum(Year2017$Pizza), sum(Year2018$Pizza), sum(Year2019$Pizza), sum(Year2020$Pizza), sum(Year2021$Pizza)), 
        names.arg = years, 
        xlab = "Year", ylab = "Number of Sold Pizza",col=colors)

# Create the second bar plot
barplot(c(mean(Year2017$Precipitation), mean(Year2018$Precipitation), mean(Year2019$Precipitation), mean(Year2020$Precipitation), mean(Year2021$Precipitation)), 
        names.arg = years, 
        xlab = "Year", ylab = "Precipitation mean",col=colors)

####################################################################################3
dev.new()
# Set the layout to 4 rows and 1 column
par(mfrow = c(2, 3))

# Create a histogram of the data for Year 2017 with frequency on the y-axis
hist(Year2017$Pizza,
     main = "Histogram of Year 2017", xlab = "PIZZA", ylab = "Frequency",
     col = "blue")

# Create a histogram of the data for Year 2018 with frequency on the y-axis
hist(Year2018$Pizza, 
     main = "Histogram of Year 2018", xlab = "PIZZA", ylab = "Frequency",
     col = "green", border = "green")

# Create a histogram of the data for Year 2019 with frequency on the y-axis
hist(Year2019$Pizza,
     main = "Histogram of Year 2019", xlab = "PIZZA", ylab = "Frequency",
     col = "red", border = "red")

# Create a histogram of the data for Year 2020 with frequency on the y-axis
hist(Year2020$Pizza,  
     main = "Histogram of Year 2020", xlab = "PIZZA", ylab = "Frequency",
     col = "purple", border = "purple")

# Create a histogram of the data for Year 2021 with frequency on the y-axis
hist(Year2021$Pizza, 
     main = "Histogram of Year 2021", xlab = "PIZZA", ylab = "Frequency",
     col = "orange", border = "orange")

##############################################################################################
dev.new()
# Set the layout to 4 rows and 1 column
par(mfrow = c(2, 3))

# Create a histogram of the data for Year 2017 with frequency on the y-axis
hist(Year2017$Temperature,
     main = "Histogram of Year 2017", xlab = "Temperature", ylab = "Frequency",
     col = "blue",breaks = 50)

# Create a histogram of the data for Year 2018 with frequency on the y-axis
hist(Year2018$Temperature, 
     main = "Histogram of Year 2018", xlab = "Temperature", ylab = "Frequency",
     col = "green", border = "green",breaks = 50)

# Create a histogram of the data for Year 2019 with frequency on the y-axis
hist(Year2019$Temperature,
     main = "Histogram of Year 2019", xlab = "Temperature", ylab = "Frequency",
     col = "red", border = "red",breaks = 50)

# Create a histogram of the data for Year 2020 with frequency on the y-axis
hist(Year2020$Temperature,  
     main = "Histogram of Year 2020", xlab = "Temperature", ylab = "Frequency",
     col = "purple", border = "purple",breaks = 50)

# Create a histogram of the data for Year 2021 with frequency on the y-axis
hist(Year2021$Temperature, 
     main = "Histogram of Year 2021", xlab = "Temperature", ylab = "Frequency",
     col = "orange", border = "orange",breaks = 50)

###########################################################################################


#) 


data = read.csv('data.csv')


df2=data[, -c(1)]

temp = c("Campaign.14", "Campaign.15", "Campaign.16","Campaign17","Campaign.18" )
df2 = df2[ ,! names(df2) %in% temp]
df3=as.data.frame(scale(df2))

data=df3
for(i in 1:ncol(data)) {
  data[is.na(data[,i]), i] <- min(data[,i], na.rm = TRUE)
}
# Replace missing values with the median of the column
missing_rows <- sapply(data, function(x) any(is.na(x)))
missing_rows_index <- which(missing_rows)


n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.7))



train=data[id,] 
test=data[-id,] 
trainX=as.matrix(train[,-1])
testX=as.matrix(test[,-1])



m1=lm(Pizza~., data=train)
summary(m1)
mean((train$Pizza-predict(m1,train))^2)  
mean((test$Pizza-predict(m1,test))^2)  
qqnorm(residuals(m1))
qqline(residuals(m1))
#model2
model <- lm(Pizza ~ Price.offer +Temperature +Precipitation + Sun + Christmas + New.year + X17th.of.May...National.day + Easter + 
              Kr..Himmelfart..Ascension.day. + Pinse..pentecost. + Summer.vacation + Campaign.1 + 
              Campaign.2 + Campaign.3 + Campaign.4 + Campaign.5 + Campaign.6 + Campaign.7 + 
              Campaign.8 + Campaign.9 + Campaign.10 + Campaign.11 + Campaign.12 + Campaign.13 , data = train)
summary(model)
mean((train$Pizza-predict(model,train))^2,na.rm=TRUE)  
mean((test$Pizza-predict(model,test))^2,na.rm=TRUE) 
dev.new()
qqnorm(residuals(model))
qqline(residuals(model))
##########################################################

dev.new()
library(glmnet)
model0=glmnet(trainX, train$Pizza, alpha=1,family="gaussian")
plot(model0, xvar="lambda", label=TRUE)

model1=glmnet(trainX, train$Pizza, alpha=0,family="gaussian")
plot(model1, xvar="lambda", label=TRUE)



set.seed(12345)
model=cv.glmnet(trainX, train$Pizza, alpha=1,family="gaussian")
model$lambda.min
plot(model)


mB=glmnet(trainX, train$Pizza, alpha=1,family="gaussian", lambda = model$lambda.min)
plot(predict(mB, testX),test$Pizza)

coefs <- coef(mB, s = "lambda.min")
important_feature_indices <- order(coefs, decreasing = TRUE)[1:3]
Feature_name = names(train)
important_features <- Feature_name[important_feature_indices]
print(important_features)
#  "Price.offer" "Campaign.5"  "Campaign.2"
###############################################################

lm2 = lm(Pizza ~ Price.offer+Temperature+Precipitation + Sun,data = train)
summary(lm2)
mean((train$Pizza-predict(lm2))^2)  
mean((test$Pizza-predict(lm2,test))^2)  
qqnorm(residuals(lm2))
qqline(residuals(lm2))
#################################################################

