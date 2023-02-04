##call libraries
library(dplyr)
library(tidyverse)
library(rpart.plot)
library(rattle)
library(rpart)


##read data
cardio <- read.csv("processed.cleveland.data", header = FALSE, na.strings = '?')            
head(cardio)

##adding names
names(cardio) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "status")

str(cardio)

##change to factor
cardio[c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "status")] <- lapply(cardio[c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "status")], factor)

cardio$status <- fct_collapse(cardio$status, "1" = c("1","2", "3", "4"))
levels(cardio$status) <- c("normal", "abnormal")
levels(cardio$sex) <- c("female", "male")

summary(cardio)


cardio <- na.omit(cardio)
sum(is.na(cardio))

cardio %>% 
  gather(-sex, -cp, -fbs, -restecg, -exang, -slope, -ca, -thal, -status, key = "var", value = "value") %>%
  ggplot(aes(x=value, y = ..count.., colour = status)) +
  scale_color_manual(values = c("#000000", "#FF0000"))+
  geom_density()+
  facet_wrap(~var, scales = "free", nrow = 2) +
  theme_bw()


##Creating train and test data

set.seed(20)
train.index <- sample(1:nrow(cardio), round(0.75*nrow(cardio),0))
cardio.train <- cardio[train.index, ]
cardio.test <- cardio[-train.index, ]

#model development
cardio.train.fit <- rpart(status ~ . , data = cardio.train,method = "class", cp = 0.05)
summary(cardio.train.fit)

fancyRpartPlot(cardio.train.fit, palettes = c("Blues", "Greens"), sub = "")


cardio.train.fit.2 <- rpart(status ~ . , data = cardio.train, method = "class", cp = 0)
fancyRpartPlot(cardio.train.fit.2, palettes = c("Blues", "Greens"), sub = "")


printcp(cardio.train.fit.2)
plotcp(cardio.train.fit.2, lty = 3, col = 2, upper = "splits")


optimalcp <- cardio.train.fit.2$cptable[which.min(cardio.train.fit.2$cptable[, "xerror"]), "CP"]
optimalcp


cardio.predict <- predict(cardio.train.fit.2, cardio.test, type = "class")


conf.matrix <- table(cardio.test$status, cardio.predict)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Predicted", colnames(conf.matrix), sep = ":")
conf.matrix


##Defining a loss function
cardio.train.fit.3 <- rpart(status ~ . , data = cardio.train, method = "class", cp = 0,parms = list(loss = matrix(c(0,175,15,0))))  
cardio.predict.2 <- predict(cardio.train.fit.3, cardio.test, type = "class")
conf.matrix.2 <- table(cardio.test$status, cardio.predict.2)
rownames(conf.matrix.2) <- paste("Actual", rownames(conf.matrix.2), sep = ":")
colnames(conf.matrix.2) <- paste("Predicted", colnames(conf.matrix.2), sep = ":")
conf.matrix.2


##Random forest
library(randomForest)
rf1 <- randomForest(status ~ ., data = cardio.train, ntree = 100,parms = list(loss = matrix(c(0,175,15,0))))
summary(rf1)
varImpPlot(rf1)

predict.rf <- predict(rf1, cardio.test, type = "class")
conf.matrix.3 <- table(cardio.test$status, predict.rf)
