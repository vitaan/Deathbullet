#  Institution: Stevens Institute of Technology
#  Project    : Final Project
#  Purpose    : Predicting Salaries of NYC Govertnment Jobs
#  Team Members	: Manali Satghar (10419945), Vitaan Thapar (10416650), Sachin Ranganathan (10415967), Charmi Bhikadiya (10412409)
#  Date       : 12/08/2016

library(stringr)
library(dplyr)
# Cleaning the objects
rm(list=ls())

# Reading the NYC Jobs data contained in the downloads folder
data <- read.csv("~/Downloads/NYC_Jobs.csv")
data<-data[which(data[,'Salary.Frequency'] == "Annual"), ]

# Creating a subset of only the attribute needed for performing KNN Classfication
JobsSubset<-subset(data,select=c(Salary.Range.From, Salary.Range.To, Minimum.Qual.Requirements, Preferred.Skills))

# Removing Duplicates
JobsData <- distinct(JobsSubset)

set.seed(9850)
g <-runif(nrow(JobsData))
JobsData <- JobsData[order(g),]
JobsData <- JobsData[sample(nrows(JobsData)),-1]

# Filtering the data by the masters degree required
JobsDataMastersSubset<-JobsData %>% filter(str_detect(Minimum.Qual.Requirements,"master's"))
JobsDataNoMastersSubset<-setdiff(JobsData, JobsDataMastersSubset)

# Adding binary value 1 if masters degree is found else 0 is assigned
JobsDataMastersSubset$Masters.Degree<-1
JobsDataNoMastersSubset$Masters.Degree<-0

# Combining both the data frames
JobsData<-rbind(JobsDataMastersSubset,JobsDataNoMastersSubset)

# Filtering the data by the bachelors degree required
JobsDataBachelorsSubset<-JobsData %>% filter(str_detect(Minimum.Qual.Requirements,"baccalaureate"))
JobsDataNoBachelorsSubset<-setdiff(JobsData, JobsDataBachelorsSubset)

# Adding binary value 1 if bachelors degree is found else 0 is assigned
JobsDataBachelorsSubset$Bachelors.Degree<-1
JobsDataNoBachelorsSubset$Bachelors.Degree<-0

# Combining both the data frames
JobsData<-rbind(JobsDataBachelorsSubset,JobsDataNoBachelorsSubset)

# Filtering the data by the MS Office required
office <- c("MS Office", "Microsoft", "Word", "Excel")
sapply(JobsData$Preferred.Skills, function(x) any(sapply(office, str_detect, string = x)))
JobsDataOfficeSubset<-JobsData %>% filter(str_detect(Preferred.Skills, paste(office, collapse = '|')))
JobsDataNoOfficeSubset<-setdiff(JobsData, JobsDataOfficeSubset)

# Adding binary value 1 if MS Office is found else 0 is assigned
JobsDataOfficeSubset$Microsoft.Office<-1
JobsDataNoOfficeSubset$Microsoft.Office<-0

# Combining both the data frames
JobsData<-rbind(JobsDataOfficeSubset,JobsDataNoOfficeSubset)

# Filtering the data by the Communication Skills required
JobsDataCommunicationSubset<-JobsData %>% filter(str_detect(Preferred.Skills,"communication"))
JobsDataNoCommunicationSubset<-setdiff(JobsData, JobsDataCommunicationSubset)

# Adding binary value 1 if Communication Skills is found else 0 is assigned
JobsDataCommunicationSubset$Communication.Skills<-1
JobsDataNoCommunicationSubset$Communication.Skills<-0

# Combining both the data frames
JobsData<-rbind(JobsDataCommunicationSubset,JobsDataNoCommunicationSubset)

# Removing blank entities
JobsData<- JobsData[!(JobsData$Preferred.Skills == " "),]
JobsData<- JobsData[!(JobsData$Preferred.Skills == "#NAME?"),]
JobsData<- JobsData[!(JobsData$Minimum.Qual.Requirements == ""),]

# Removing the Min Req and Prefered Skills and replacing them with their binary equivalent for KNN Classfication
JobsData$Minimum.Qual.Requirements<-NULL
JobsData$Preferred.Skills<-NULL

# Taking Average of the Salary
JobsData$Average.Salary<- (JobsData$Salary.Range.From + JobsData$Salary.Range.To)/2
JobsData$Salary.Range.From<-NULL
JobsData$Salary.Range.To<-NULL

JobsData$DUI<-(sample(0:2,nrow(JobsData),replace=T))

Salary<-ifelse(JobsData$DUI==0,"Low",ifelse(JobsData$DUI==1,"Moderate","High" )  ) 

JobsData$Civil.Service.Title<-as.numeric(sample(1:25,nrow(JobsData),replace=T))

JobsData$Salary.Level<-NULL
JobsData$Average.Salary<-NULL
JobsData$DUI<-NULL

JobsData$Min.GPA<-round(runif(nrow(JobsData), min=2, max = 4),digits = 2)
JobsData$Days.PerWeek<-as.numeric(sample(1:7,nrow(JobsData),replace=T))
JobsData$WorkHours.PerDay<-as.numeric(sample(5:10,nrow(JobsData),replace=T))
JobsData$Years.Experience<-as.numeric(sample(0:12,nrow(JobsData),replace=T))
JobsData$Salary<-as.factor(Salary)


# Normalizing the data

normalize <- function(x,min,max)
{
  z<- ( (x- min(x)) / (max(x) - min(x)))
  return(z)
}

Years.Experience <- normalize(JobsData[,9],min(JobsData[,9]),max(JobsData[,9]))
WorkHours.PerDay <- normalize(JobsData[,8],min(JobsData[,8]),max(JobsData[,8]))
Min.GPA <- normalize(JobsData[,6],min(JobsData[,6]),max(JobsData[,6]))
Days.PerWeek <- normalize(JobsData[,7],min(JobsData[,7]),max(JobsData[,7]))
Salary=JobsData$Salary
Masters.Degree=JobsData$Masters.Degree
Bachelors.Degree=JobsData$Bachelors.Degree
Communication.Skills=JobsData$Communication.Skills
Microsoft.Office=JobsData$Microsoft.Office
Civil.Service.Title<-normalize(JobsData[,5],min(JobsData[,5]),max(JobsData[,5]))

JobsData<-data.frame(Salary,Years.Experience,WorkHours.PerDay,Masters.Degree,Bachelors.Degree,Communication.Skills,Microsoft.Office,Min.GPA,Days.PerWeek,Civil.Service.Title)

set.seed(9850)
g <-runif(nrow(JobsData))
JobsData <- JobsData[order(g),]
JobsData <- JobsData[sample(1:nrow(JobsData)),]

# Loading the libraries required 
library(caret)
library(ISLR)
library(class)

# Segregating the data into test and training set
idx <- sample(nrow(JobsData), as.integer(.60 * nrow(JobsData)))
test<-JobsData[-idx,]
training<-JobsData[idx,]

# Apply KNN algorithm on the segregated data
knn.result <-knn(training[,-10],test[,-10],training[,10],k=5)

# Calculating the confusion matrix for accuracy calculation
confusionMatrix<-table(Predict=knn.result,Actual=test[,10])
error<-(1-sum(diag(cm))/sum(cm))*100
Accuracy = 100-error
Accuracy

# Ploting the knn graph 
plot(knn.result )

# Pre processing the data
trainX <- training[,names(training) != "Salary"]
preProcValues <- preProcess(x = training,method = c("center", "scale"))
preProcValues

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(Salary ~ ., data = training, method = "knn",trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
confusionMatrix(knnFit)
#Output of kNN fit
knnFit
plot(knnFit)


# ########## Correlation Matrix
library(corrplot)
library(RColorBrewer)
M<-cor(JobsData)
corrplot(M, method="number", col=brewer.pal(n=8, name="Spectral"),tl.col="steelblue4", tl.srt=60)

