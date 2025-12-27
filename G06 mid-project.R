install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

train <- read.csv("https://drive.google.com/uc?export=download&id=1nzqojT2375EItrlpgipfnV15uWFxfFdS")
str(train)
head(train)


colSums(is.na(train) | train == "")
train <- train %>% select(-PassengerId, -Name, -Pclass, -Cabin)
train$Age[is.na(train$Age)] <- median(train$Age, na.rm = TRUE)
mode_embark <- names(sort(table(train$Embarked), decreasing = TRUE))[1]
mode_embark
train$Embarked[train$Embarked == ""] <- mode_embark
train <- distinct(train)
train <- train %>% mutate(FamilySize = SibSp + Parch + 1)


summary(train %>% select(Age, Fare, FamilySize))
ggplot(train, aes(x = Age)) + geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Train: Age Distribution")
ggplot(train, aes(x = Fare)) + geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "Train: Fare Distribution")
ggplot(train, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") + labs(title = "Train: Survival by Sex", fill = "Survived")




test <- read.csv("https://drive.google.com/uc?export=download&id=1WCWRSV-P7p3SK55dAsr72u1MM_ZGgKD6")
str(test)
head(test)


colSums(is.na(test) | test == "")
test <- test %>% select(-PassengerId, -Name, -Pclass, -Cabin)
test$Age[is.na(test$Age)] <- median(test$Age, na.rm = TRUE)
test$Fare[is.na(test$Fare)] <- median(test$Fare, na.rm = TRUE)
test <- distinct(test)
test <- test %>% mutate(FamilySize = SibSp + Parch + 1)


summary(test %>% select(Age, Fare, FamilySize))
ggplot(test, aes(x = Age)) + geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Test: Age Distribution")
ggplot(test, aes(x = Fare)) + geom_histogram(binwidth = 10, fill = "purple", color = "black") +
  labs(title = "Test: Fare Distribution")




gender_submission <- read.csv("https://drive.google.com/uc?export=download&id=1kNqNK8RmjUS1nYe-VA5MEH-Xe1uzFN8P")
str(gender_submission)
head(gender_submission)


gender_submission <- distinct(gender_submission)

table(gender_submission$Survived)
ggplot(gender_submission, aes(x = factor(Survived))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Gender Submission: Predicted Survival", x = "Survived", y = "Count")
