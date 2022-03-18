library(tidyverse)
library(caret)
library(titanic)
library(mlr)

drop_vars <- c("PassengerId", "Ticket", "Cabin", "Name")
data <- titanic_train %>% 
	select(-all_of(drop_vars)) %>% 
	mutate_if(is.character,factor) %>%
	drop_na() %>%
	as_tibble() %>%
	mutate(Survived = factor(Survived))

ind <- createDataPartition(data$Survived, p=.75,list=FALSE)
train <- data[ind,]
test <- data[-ind,]

train <- caret::downSample(train,train$Survived,list=F) %>%
	as_tibble() %>%
	select(-Class)

#test <- caret::upSample(test,test$Survived,list=F)

task = makeClassifTask(data = train, target = "Survived")
clf = makeLearner("classif.randomForest", fix.factors.prediction = FALSE)

#getParamSet("classif.randomForest")
clf = setHyperPars(clf, ntree = 200, mtry=8)#,maxnodes=1)

# Train the learner
mod = train(clf, task)

pred = predict(mod, newdata = test)
head(as.data.frame(pred))

meas = list(acc, ber)
rdesc = makeResampleDesc("CV", iters = 5)
benchmark(clf, task, rdesc, measures = meas)

performance(pred, measures = acc)

calculateROCMeasures(pred)
calculateROCMeasures(pred)$confusion.matrix
round(calculateROCMeasures(pred)$measures$acc,5)*100


