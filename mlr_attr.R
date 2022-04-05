library(tidyverse)
library(caret)
library(mlr)
library(randomForest)

train <- read.csv("attr_train.csv") %>%
	mutate_if(is.character, factor) %>%
	select(Alder, Attrition)
test  <- read.csv("attr_test.csv") %>%
	mutate_if(is.character, factor) %>%
	select(Alder, Attrition)

task = makeClassifTask(data = train, target = "Attrition")
clf = makeLearner("classif.randomForest", fix.factors.prediction = FALSE)

#getParamSet("classif.randomForest")
clf = setHyperPars(clf, ntree = 200, mtry=8)#,maxnodes=1)

# Train the learner
mod = mlr::train(clf, task)

pred = predict(mod, newdata = test)
head(as.data.frame(pred))

meas = list(acc, ber)
rdesc = makeResampleDesc("CV", iters = 5)
benchmark(clf, task, rdesc, measures = meas)

performance(pred, measures = acc)

calculateROCMeasures(pred)
calculateROCMeasures(pred)$confusion.matrix
round(calculateROCMeasures(pred)$measures$acc,5)*100


