#  Institution: Stevens Institute of Technology
#  Project    : Final Year Project
#  Team Members	: Manali Satghar, Vitaan Thapar, Sachin Ranganathan, Charmi Bhikadiya
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
JobsData <- JobsData[sample(1:200),-1]

# Filtering the data by the masters degree required
JobsDataMastersSubset<-JobsData %>% filter(str_detect(Minimum.Qual.Requirements,"master's"))
JobsDataNoMastersSubset<-setdiff(JobsData, JobsDataMastersSubset)

# Adding binary value 1 if masters degree is found else 0 is assigned
JobsDataMastersSubset$Masters.Degree<-"Master's Degree required"
JobsDataNoMastersSubset$Masters.Degree<-"Master's Degree not required"

# Combining both the data frames
JobsData<-rbind(JobsDataMastersSubset,JobsDataNoMastersSubset)

# Filtering the data by the bachelors degree required
JobsDataBachelorsSubset<-JobsData %>% filter(str_detect(Minimum.Qual.Requirements,"baccalaureate"))
JobsDataNoBachelorsSubset<-setdiff(JobsData, JobsDataBachelorsSubset)

# Adding binary value 1 if bachelors degree is found else 0 is assigned
JobsDataBachelorsSubset$Bachelors.Degree<-"Bachelor's Degree required"
JobsDataNoBachelorsSubset$Bachelors.Degree<-"Bachelor's Degree not required"

# Combining both the data frames
JobsData<-rbind(JobsDataBachelorsSubset,JobsDataNoBachelorsSubset)

# Filtering the data by the MS Office required
office <- c("MS Office", "Microsoft", "Word", "Excel")
sapply(JobsData$Preferred.Skills, function(x) any(sapply(office, str_detect, string = x)))
JobsDataOfficeSubset<-JobsData %>% filter(str_detect(Preferred.Skills, paste(office, collapse = '|')))
JobsDataNoOfficeSubset<-setdiff(JobsData, JobsDataOfficeSubset)

# Adding binary value 1 if MS Office is found else 0 is assigned
JobsDataOfficeSubset$Microsoft.Office<-"Know MS Office"
JobsDataNoOfficeSubset$Microsoft.Office<-"Don't know MS Office"

# Combining both the data frames
JobsData<-rbind(JobsDataOfficeSubset,JobsDataNoOfficeSubset)

# Filtering the data by the Communication Skills required
JobsDataCommunicationSubset<-JobsData %>% filter(str_detect(Preferred.Skills,"communication"))
JobsDataNoCommunicationSubset<-setdiff(JobsData, JobsDataCommunicationSubset)

# Adding binary value 1 if Communication Skills is found else 0 is assigned
JobsDataCommunicationSubset$Communication.Skills<-"Strong Communication Skill"
JobsDataNoCommunicationSubset$Communication.Skills<-"Weak Communication Skill"

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

min(JobsData$Average.Salary)
max(JobsData$Average.Salary)

JobsData$DUI<-as.numeric(sample(0:2,nrow(JobsData),replace=T))
length(JobsData$DUI)

Salary<-ifelse(JobsData$DUI==0,1,ifelse(JobsData$DUI==1,2,3 )  ) 

JobsData$civil<-as.numeric(sample(1:25,nrow(JobsData),replace=T))

Civil.Service.Title<-ifelse(JobsData$civil==1,'ACCOUNTANT',ifelse(JobsData$civil==2,'ADMINISTRATIVE PROJECT MANAGER',ifelse(JobsData$civil==3,'AGENCY ATTORNEY',ifelse(JobsData$civil==4,'AGENCY CHIEF CONTRACTING OFFICER',ifelse(JobsData$civil==5,'AGENCY MEDICAL DIRECTOR',ifelse(JobsData$civil==6,'AIR POLLUTION INSPECTOR',ifelse(JobsData$civil==7,'ARCHITECT',ifelse(JobsData$civil==8,'ASSISTANT ARCHITECT',ifelse(JobsData$civil==9,'ASSISTANT CITY ASSESSOR',ifelse(JobsData$civil==10,'ASSOCIATE CHEMIST',ifelse(JobsData$civil==11,'ASSOCIATE FRAUD INVESTIGATOR',ifelse(JobsData$civil==12,'ASSOCIATE STAFF ANALYST',ifelse(JobsData$civil==13,'AUTOMOTIVE SERVICE WORKER',ifelse(JobsData$civil==14,'BUSINESS PROMOTION COORDINATOR',ifelse(JobsData$civil==15,'EXAMINER OF ACCOUNTS',ifelse(JobsData$civil==16,'CERT IT ADMINISTRATOR (DB)',ifelse(JobsData$civil==17,'CITY CUSTODIAL ASSISTANT',ifelse(JobsData$civil==18,'CITY MEDICAL DIRECTOR',ifelse(JobsData$civil==19,'CITY PARK WORKER',ifelse(JobsData$civil==20,'CITY RESEARCH SCIENTIST',ifelse(JobsData$civil==21,'COMMUNITY COORDINATOR',ifelse(JobsData$civil==22,'COMPUTER SPECIALIST',ifelse(JobsData$civil==23,'CONFIDENTIAL INVESTIGATOR',ifelse(JobsData$civil==24,'CRIMINALIST', 'ELECTRICAL ENGINEER') ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ))) 

JobsData$Salary.Level<-NULL
JobsData$Average.Salary<-NULL
JobsData$DUI<-NULL
JobsData$civil<-NULL

JobsData$Salary<-as.factor(Salary)
JobsData$Civil.Service.Title<-as.factor(Civil.Service.Title)
JobsData$Years.Experience<-as.numeric(sample(0:12,nrow(JobsData),replace=T))
JobsData$WorkHours.PerDay<-as.numeric(sample(5:10,nrow(JobsData),replace=T))
JobsData$Min.GPA<-round(runif(nrow(JobsData), min=2, max = 4),digits = 2)
JobsData$Days.PerWeek<-as.numeric(sample(1:7,nrow(JobsData),replace=T))

# Normalizing the data

normalize <- function(x,min,max)
{
  z<- ( (x- min(x)) / (max(x) - min(x)))
  return(z)
}

Years.Experience <- normalize(JobsData[,7],min(JobsData[,7]),max(JobsData[,7]))
WorkHours.PerDay <- normalize(JobsData[,8],min(JobsData[,8]),max(JobsData[,8]))
Min.GPA <- normalize(JobsData[,9],min(JobsData[,9]),max(JobsData[,9]))
Days.PerWeek <- normalize(JobsData[,10],min(JobsData[,10]),max(JobsData[,10]))
Salary=JobsData$Salary
Masters.Degree=JobsData$Masters.Degree
Bachelors.Degree=JobsData$Bachelors.Degree
Communication.Skills=JobsData$Communication.Skills
Microsoft.Office=JobsData$Microsoft.Office
Civil.Service.Title=JobsData$Civil.Service.Title

JobsData<-data.frame(Salary,Years.Experience,WorkHours.PerDay,Masters.Degree,Bachelors.Degree,Communication.Skills,Microsoft.Office,Min.GPA,Days.PerWeek,Civil.Service.Title)

set.seed(9850)
g <-runif(nrow(JobsData))
JobsData <- JobsData[order(g),]
JobsData <- JobsData[sample(1:nrow(JobsData)),]

# Loading the nnet libray for multinomial classfication
library(nnet)

# Considering Salary: High as the reference class
JobsData$out<-relevel(JobsData$Salary, ref = "1")

# Apply multinomial logistic regression
mymodel<-multinom(out~Masters.Degree+Bachelors.Degree+Microsoft.Office+Communication.Skills+Civil.Service.Title+Years.Experience+WorkHours.PerDay+Min.GPA+Days.PerWeek, data = JobsData)

# Visualising the results
summary(mymodel)
predict(mymodel,JobsData, type="prob")

#Calculating the accuracy
cm<-table(predict(mymodel),JobsData$Salary)
cm
error<-(1-sum(diag(cm))/sum(cm))*100
Accuracy = 100-error
Accuracy

# Deviance is the error left over in the model, coefficients is used while calculating probabilities

write.csv(JobsData, "~/Downloads/JobsData.csv")

