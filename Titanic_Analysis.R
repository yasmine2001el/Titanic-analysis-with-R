data.frame <- read.csv("./train.csv", na.strings = "")

View(data.frame)


summary(data.frame)

dim(data.frame)


str(data.frame)


table(data.frame$Survived)

prop.table(table(data.frame$Survived))

table(data.frame$Sex, data.frame$Survived)

prop.table(table(data.frame$Sex, data.frame$Survived),margin = 1)

colSums(is.na(data.frame))

install.packages('psych')
install.packages('GGally')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('Amelia')

#Clean & Prepare
library(Amelia)
missmap(data.frame, col=c("black", "grey"))

library(dplyr)
data.frame = select(data.frame, Survived, Pclass, Age, Sex, SibSp, Parch)
data.frame2 = select(data.frame, Survived, Pclass, Age, Sex, SibSp, Parch)

View(data.frame)
missmap(data.frame, col=c("black", "grey"))
data.frame = na.omit(data.frame)
missmap(data.frame, col=c("black", "grey"))

#Check structure
str(data.frame)

#Visualize Data
library(GGally)
ggcorr(data.frame,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")


data.frame$Survived = factor(data.frame$Survived)
data.frame$Pclass = factor(data.frame$Pclass, order=TRUE, levels = c(3, 2, 1))
str(data.frame)


#Create train/test set
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample = 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

View(data.frame)
train <- create_train_test(data.frame, 0.8, train = TRUE)
test <- create_train_test(data.frame, 0.8, train = FALSE)
View(train)
dim(train)
dim(test)

#Survived count
library(ggplot2)
ggplot(train, aes(x = Survived)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

#Survived count by Gender
ggplot(train, aes(x = Survived, fill=Sex)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

#Survived count by PClass
ggplot(train, aes(x = Survived, fill=Pclass)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()

#Age Density
ggplot(train, aes(x = Age)) + geom_density(fill='coral')


# Discretize age to plot survival
train$Discretized.age = cut(train$Age, c(0,10,20,30,40,50,60,70,80,100))
train$Discretized.age
View(train)
# Plot discretized age
ggplot(train, aes(x = Discretized.age, fill=Survived)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()


train$Discretized.age = NULL


#DT
library(rpart)
my_dt <- rpart(Survived ~ ., 
                data = train, 
                method = "class")

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(my_dt)

round(prop.table(table(train$Survived)),2)
round(prop.table(table(train$Sex, train$Survived),margin = 1),2)

predicted = predict(my_dt, test, type = 'class')
table_mat = table(test$Survived, predicted)
dt_accuracy = sum(diag(table_mat)) / sum(table_mat)
paste("The accuracy is : ", dt_accuracy)
print(table_mat)

performance_tune <- function(fit) {
  predict <- predict(fit, test, type = 'class')
  table_mat <- table(test$Survived, predict)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  print(accuracy_Test)
}

control <- rpart.control(minsplit = 5,
                         minbucket = round(7 / 3),
                         maxdepth = 4,
                         cp = 0)
tune_dt <- rpart(Survived~., data = train, method = 'class', control = control)
performance_tune(tune_dt)

new_test <- read.csv("../Data/titanic/test.csv", na.strings = "")
new_test = na.omit(new_test)
missmap(new_test, col=c("black", "grey"))

new_test$Pclass = factor(new_test$Pclass, order=TRUE, levels = c(3, 2, 1))
str(new_test)

new_prediction <- predict(tune_dt, new_test, type = "class")
submit <- data.frame(PassengerId = new_test$PassengerId, Survived = new_prediction)
write.csv(submit, file = "newTestResult.csv", row.names = FALSE)

View(submit)
