#  Institution: Stevens Institute of Technology
#  Project    : Final Project
#  Purpose    : Predicting Salaries of NYC Govertnment Jobs
#  Team Members	: Manali Satghar (10419945), Vitaan Thapar (10416650), Sachin Ranganathan (10415967), Charmi Bhikadiya (10412409)
#  Date       : 12/08/2016

library('C50')
# Cleaning the objects
rm(list=ls())
JobsData <- read.csv("~/Downloads/JobsData42(RF).csv")
set.seed(9850)
g <-runif(nrow(JobsData))
JobsData <- JobsData[order(g),]
JobsData <- JobsData[sample(200),]

Years.Experience = as.numeric(JobsData$Years.Experience)
WorkHours.PerDay = as.numeric(JobsData$WorkHours.PerDay)
Min.GPA= as.numeric(JobsData$Min.GPA)
Days.PerWeek= as.numeric(JobsData$Days.PerWeek)
Salary = as.factor(JobsData$Salary)
Masters.Degree=as.character(JobsData$Masters.Degree)
Bachelors.Degree=as.character(JobsData$Bachelors.Degree)
Communication.Skills=as.character(JobsData$Communication.Skills)
Microsoft.Office=as.character(JobsData$Microsoft.Office)
Civil.Service.Title = as.character(JobsData$Civil.Service.Title)

JobsData1 <- data.frame(Salary, Years.Experience,WorkHours.PerDay, Masters.Degree,Bachelors.Degree,Communication.Skills,Microsoft.Office,Min.GPA,Days.PerWeek,Civil.Service.Title)

set.seed(9850)
g <-runif(nrow(JobsData1))
JobsData1 <- JobsData1[order(g),]
JobsData1 <- JobsData1[sample(1:nrow(JobsData1)),]

library(randomForest)
# Split the data into 70% training and 30% testing 
idx <- sample(nrow(JobsData1), as.integer(.70 * nrow(JobsData1)))
test<-JobsData1[-idx,]
training<-JobsData1[idx,]
trainset<-training

# covert salary variabels into factor in order to be used in RF
trainset$Salary<-as.factor(trainset$Salary)

#------------------------------------- RF---------------------------------------------
tree.data<-randomForest(Salary~., data=trainset, keep.forest=TRUE,importance=TRUE, proximity=TRUE,ntree=125,nPerm=10,mtry=3)
getTree(tree.data)
importance(tree.data) # To understand important variables
print(tree.data)
summary(tree.data)

#------------------- Using the resulted RF to predict the testing data----------
resultRF<- predict(tree.data, test, type="prob")
#--------------- Getting the accuarcy------------
rFTable<-table(predict(tree.data),trainset$Salary)
accuracy=(sum(diag(rFTable))/sum(rFTable))*100
accuracy 

# Plotting Importance Variable Graph
imVar<-round(importance(tree.data),2)
varImpPlot(tree.data, main="Class= Feature Importance ")


