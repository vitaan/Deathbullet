library('C50')
# Cleaning the objects
rm(list=ls())
JobsData <- read.csv("~/Downloads/JobsData(72)C50(600).csv")
set.seed(9850)
g <-runif(nrow(JobsData))
JobsData <- JobsData[order(g),]
JobsData <- JobsData[sample(1:600),-1]

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
str(JobsData1)
JobsData1<- distinct(JobsData1)

# Normalizing the data

normalize <- function(x,min,max)
{
  z<- ( (x- min(x)) / (max(x) - min(x)))
  return(z)
}

Years.Experience <- normalize(JobsData1[,2],min(JobsData1[,2]),max(JobsData1[,2]))
WorkHours.PerDay <- normalize(JobsData1[,3],min(JobsData1[,3]),max(JobsData1[,3]))
Min.GPA <- normalize(JobsData1[,8],min(JobsData1[,8]),max(JobsData1[,8]))
Days.PerWeek <- normalize(JobsData1[,9],min(JobsData1[,9]),max(JobsData1[,9]))
Salary=JobsData1$Salary
Masters.Degree=JobsData1$Masters.Degree
Bachelors.Degree=JobsData1$Bachelors.Degree
Communication.Skills=JobsData1$Communication.Skills
Microsoft.Office=JobsData1$Microsoft.Office
Civil.Service.Title=JobsData1$Civil.Service.Title

JobsData1<-data.frame(Salary,Years.Experience,WorkHours.PerDay,Masters.Degree,Bachelors.Degree,Communication.Skills,Microsoft.Office,Min.GPA,Days.PerWeek,Civil.Service.Title)

set.seed(9850)

# Segregating the data into test and training data for peforming C5.0 Classification
idx <- sample(nrow(JobsData1), as.integer(.70 * nrow(JobsData1)))
test<-JobsData1[-idx,]
training<-JobsData1[idx,]

# Applying C5.0 Classification
treeModel<-C5.0(x= training[,-1], y=as.factor(training$Salary), rules=TRUE)

# Computing Attribute Usage
C5imp(treeModel)

# Computing percentage of split associated with each attributes
C5imp(treeModel, metric = "splits")

# Visualizing the results
summary(treeModel)

# Computing the accuracy
result<- predict(treeModel, training, type="class")
rTable<-table(Predict=result, Test=training[,1])
error=(1-sum(diag(rTable))/nrow(training))*100
accuracy= 100 - error
accuracy






