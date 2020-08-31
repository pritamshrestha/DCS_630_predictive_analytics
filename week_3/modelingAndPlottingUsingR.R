# data importing 

dogers_data<-read.csv("C:\\Users\\pritam\\Desktop\\Data Science Courses\\Predictive Analytics(DSC_630)\\Week_3\\formated_data.csv")
View(dogers_data)
# summary stastistics of the data
summary(dogers_data)

# checking the data structure
head(dogers_data)

# loading library
library(ggplot2)

# scatter plot day vs attend
ggplot(dogers_data,aes(day,attend))+geom_point()

# scatter plot day_of_week vs attend
ggplot(dogers_data,aes(day_of_week,attend))+geom_point()

# scatter plot day_of_week vs attend
ggplot(dogers_data,aes(day_of_week,attend))+geom_point()

# scatter plot day_of_week vs attend
ggplot(dogers_data,aes(month,attend))+geom_point()

# boxplot month vs attend
ggplot(dogers_data,aes(month,attend))+geom_boxplot()

# boxplot day_of_week vs attend
ggplot(dogers_data,aes(day_of_week,attend))+geom_boxplot()


# now performing linear regression model
cor(dogers_data$day,dogers_data$attend)
# now using liner regression model to see the relationship between them.

plot(dogers_data$day,dogers_data$attend,main="scatter plot")
# evaluating linear regression 
mod<-lm(dogers_data$day~dogers_data$attend)
summary(mod)
plot(mod)

# liner regression between two variables
mod1<-lm(dogers_data$temp~dogers_data$attend)
summary(mod1)
plot(mod1)

# checking data types
is.numeric(dogers_data$month)

# converting into numeric format
dogers_data$month<-as.numeric(dogers_data$month)

# checking data types
is.numeric(dogers_data$month)

# converting day_of_week into numeric format.
dogers_data$day_of_week<-as.numeric(dogers_data$day_of_week)

# checking data types
is.numeric(dogers_data$day_of_week)

# performing multiple regression
my_model<-lm(dogers_data$attend~dogers_data$month+dogers_data$day+dogers_data$day_of_week)

summary(my_model)
# plotting
plot(my_model)

# Now i am going to find which time is better for the marketing 
# plotting day_night vs attend
ggplot(dogers_data,aes(day_night,attend))+geom_point()

# based on the plot it is proved that more people
# come in the night time to watch game.So night time is
# Much better for the marketing.


# checking data types
is.numeric(dogers_data$day_night)

# converting into numerical data
dogers_data$day_night<-as.numeric(dogers_data$day_night)


# checking data types of the day_night
mod2<-lm(dogers_data$attend~dogers_data$day+dogers_data$month+dogers_data$day_of_week)

# now i am spliting the main datasets into two sets(80% and 20%)
main_sample<-sample(2,nrow(dogers_data),replace=TRUE,prob=c(0.8,0.2))

# training_data and testing data
training_data<-dogers_data[main_sample==1,]
testing_data<-dogers_data[main_sample==2,]

# checking the data sets
head(training_data)
head(testing_data)

# fitting my_model for training data
my_model.fitting_training_data<-lm(my_model,data=training_data)
summary(my_model.fitting_training_data)

my_model.fitting_testing_data<-lm(my_model,data=testing_data)
summary(my_model.fitting_testing_data)




# Predict from Training Set
training_data_predict<- predict(my_model.fitting_training_data)
summary(training_data_predict)

# predict from testing set
testing_data_predict<-predict(my_model.fitting_testing_data)
summary(testing_data_predict)


# now fitting the model for the main data.
my_model.fitting<-lm(my_model, data=dogers_data)
summary(my_model.fitting)

coefficients(my_model)
coefficients(my_model.fitting)

# predictine model for main data
predict_my_model<-predict(my_model,dogers_data)
summary(predict_my_model)

# predicting model for the training data
predict_training_data<-predict(my_model,training_data)
summary(predict_training_data)

# predicting model for the testing data
predict_testing_data<-predict(my_model,testing_data)
summary(predict_testing_data)

# Now we need to find the day which is very good for marketing purpose.
ggplot(dogers_data,aes(day,attend))+geom_point()

# creating new dataframe to answer the question
dayandattend<-data.frame(dogers_data$day,dogers_data$attend,dogers_data$month,dogers_data$day_of_week)
View(dayandattend)

# checking maximum value of the attend
which.max(dayandattend$dogers_data.attend)

# it displays aarray [1], it means heighest value is in first index

# max value=56000
# based on the available value we can conclude that
# day=10,month=1(january) and day_of_week is 6
# hence, the actual data of maximum attendet to watch 
# game is firday january 10.
# if we do marketing promotion on that day we might get more attende in the futuer.

#######################################
######################################
dodger.model <- {attend ~ month + day_of_week + bobblehead}
set.seed(1234)
traintest_key <- c(rep(1, trunc((2/3)*nrow(dodgers_raw))), rep(2, trunc((1/3)*nrow(dodgers_raw))))


