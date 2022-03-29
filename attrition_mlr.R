library(tidyverse)
library(mlr)
library(caret)

df <- read_csv("attrition.csv") %>%
	select(Age, Attrition, BusinessTravel, Department, DistanceFromHome,
				 EducationField, Gender, JobRole, PerformanceRating, YearsAtCompany,
				 YearsInCurrentRole,MonthlyIncome) %>%
	mutate_if(is.character,factor)

ind <- createDataPartition(df$Attrition, p=.75, list=FALSE)
train <- df[ind,]
test <- df[-ind,]

train <- caret::upSample(train,train$Attrition,list=F) %>%
	as_tibble() %>%
	select(-Class)

test <- caret::upSample(test,test$Attrition,list=F) %>%
	as_tibble() %>%
	select(-Class)

write_csv(train,"attr_train.csv")
write_csv(test,"attr_test.csv")

task = makeClassifTask(data = train, target = "Attrition")
clf = makeLearner("classif.randomForest", 
									fix.factors.prediction = FALSE,
									predict.type = "response")

#getParamSet("classif.randomForest")
clf = setHyperPars(clf, ntree = 1000, mtry=2, maxnodes=3)

# Train the learner
mod = mlr::train(clf, task)

pred = predict(mod, newdata = test)
#head(as.data.frame(pred))
performance(pred, measures = acc)
calculateROCMeasures(pred)$confusion.matrix

#meas = list(acc)
#rdesc = makeResampleDesc("CV", iters = 5)
#benchmark(clf, task, rdesc, measures = meas)
#calculateROCMeasures(pred)
#round(calculateROCMeasures(pred)$measures$acc,5)*100
