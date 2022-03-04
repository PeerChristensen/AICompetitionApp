library(mlr3)


task = as_task_classif(Sonar, target = "Class", positive = "R")