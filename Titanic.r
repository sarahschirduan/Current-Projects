#R Titanic Kaggle

#Read files
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Training data set has column of Survived - yes or no, test data set does not have 
#that column, creates new data frame, repeats "None" for number of rows in test dataset
#it applies it to entire test data frame
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#Creates large data frame with both sets of data, has same number of columns
data.combined <- rbind(train, test.survived)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

str(data.combined)

#Look at distribution for survived variable
table(data.combined$Survived)

#Look at male to female ratio for survived variable
table(train$Sex, train$Survived)

library(ggplot2)

#Explore hypthosesis that rich people had a better change at survival
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
geom_bar(width = 0.5) +
  xlab("Pclass") + ylab("Total Count") + labs(fill = "Survived")

#Examine first few names
head(train$Name)
head(as.character(train$Name))

#Determine number of instances of names (should be 1309)
length(unique(as.character(data.combined$Name)))

#Highlight duplicate names
data.combined$Name[duplicated(data.combined$Name)]

dup.names <- as.character(data.combined[which(duplicated(as.character
                                      (data.combined$Name))), "Name"])

data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]


males <- data.combined[which(data.combined$Sex == "male"),]
males[1:5,]

#Create new variable - title
extractTitle <- function(Name) {
  name <- as.character(Name)
    if(length(grep("Miss.", Name))>0) {
      return ("Miss.")
    }  else if (length(grep("Master.", Name))>0) {
    return ("Master.")
    }  else if (length(grep("Mrs.", Name))>0) {
    return ("Mrs.")
    }  else if (length(grep("Mr.", Name))>0) {
      return ("Mr.")
    } else {
      return ("Other")  
  }
}

titles <- NULL

for (i in 1:nrow(data.combined)) {
 titles <- c(titles, extractTitle(data.combined[i, "Name"])) 
}

data.combined$title <- as.factor(titles)

install.packages('ggplot2')
install.packages('colorspace')
library(ggplot2)

#This looks at Survival rate given gender and class
ggplot(data = data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(width = 0.8) + facet_wrap(~Pclass) + ggtitle("Pclass") +
  xlab("Sex") + ylab("Total Count") + labs(fill = "Survived")


#This looks at Survival rate given title
ggplot(data = data.combined[1:891,], aes(x = title, fill = Survived)) +
  geom_bar(width = 0.8) + facet_wrap(~Pclass) + ggtitle("Pclass") +
  xlab("Title") + ylab("Total Count") + labs(fill = "Survived")

#Look at age distribution of all passengers
summary(data.combined$Age)

#Look at age distribution for all passengers with the title Master by
#creating new data frame of just 'Masters.'
boys <- data.combined[which(data.combined$title == 'Master.'),]
summary(boys$Age)

#Look at ticket variable
str(data.combined$Ticket)

#Change it from factor to character
data.combined$Ticket <- as.character(data.combined$Ticket)
head(data.combined$Ticket)

#Check for NAs in Ticket variable (we find none)
sum(is.na(data.combined$Ticket))

#Look at Fare variable
summary(data.combined$Fare)
length(unique(data.combined$Fare))
str(data.combined$Fare)

#Look at Cabin variable
summary(data.combined$Cabin)
length(unique(data.combined$Cabin))
str(data.combined$Cabin)

#Transform Cabin from factor to string
data.combined$Cabin <- as.character(data.combined$Cabin)
head(data.combined$Cabin)

#Look at Embarked / departure location variable
summary(data.combined$Embarked)
length(unique(data.combined$Embarked))
str(data.combined$Embarked)

library(randomForest)

#Exploratory modeling - using random forest with pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived)

#Set seed & create plot
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
print(rf.1)
varImpPlot(rf.1)

#Exploratory modeling - using random forest with pclass & title & Sibling variable
rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]
rf.label <- as.factor(train$Survived)

#Set seed & create plot
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
print(rf.2)
varImpPlot(rf.2)

#Modeling - random forest with pclass & title & SibSp & Parch
rf.train.3 <- data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]
rf.label <- as.factor(train$Survived)

#Set seed & create plot
set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
print(rf.3)
varImpPlot(rf.3)

#Subset our test records & features
test.submit.df <- data.combined[892:1309, c("Pclass", "title", "SibSp", "Parch")]

#Make predictions
rf.3.preds <- predict(rf.3, test.submit.df)
table(rf.3.preds)

#Output a csv file for Kaggle

submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.3.preds)

write.csv(submit.df, file = "RF_SUB_20190622_1.csv", row.names = FALSE)

library(ggplot2)
library(caret)
library(doSNOW)

print(rf.label)

set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

#Set up trainControl
ctrl.1 <- trainControl(method="repeatedcv", number = 3, repeats = 10, 
                       index = cv.3.folds)

#Set up a cluster, do multicore training
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)

install.packages('e1071', dependencies=TRUE)

#Set seed for reproducibility & train
set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.3, y = rf.label, method = "rf", 
                   tuneLength = 3, ntree = 100, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

#Check out results
rf.5.cv.3