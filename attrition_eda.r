

library(tidyverse)

df <- read_csv("attrition.csv") %>%
	select(Age, Attrition, BusinessTravel, Department, DistanceFromHome,
				 EducationField, Gender, JobRole, PerformanceRating, YearsAtCompany,
				 YearsInCurrentRole,MonthlyIncome)

write_csv(df,"attrition_subset.csv")

df %>%
	count(Gender,Attrition) %>%
	group_by(Gender) %>%
	mutate(tot=sum(n)) %>%
	mutate(prop = n/tot) %>%
	ggplot(aes(Gender,prop,fill=Attrition)) +
	geom_col()


df %>%
	count(BusinessTravel,Attrition) %>%
	group_by(BusinessTravel) %>%
	mutate(tot=sum(n)) %>%
	mutate(prop = n/tot) %>%
	ggplot(aes(BusinessTravel,prop,fill=Attrition)) +
	geom_col()

df %>%
	ggplot(aes(Attrition,YearsAtCompany,fill=Attrition)) +
	geom_boxplot()

df %>%
	ggplot(aes(Attrition,DistanceFromHome,fill=Attrition)) +
	geom_boxplot()

df %>%
	ggplot(aes(Age,colour=Attrition)) +
	geom_density()

