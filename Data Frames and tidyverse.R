#-----Reading Files-------
#-----Getting Working Directory-----
getwd()
#------Setting Working Directory-----
setwd("D:/R Programs")

getwd()
library(tidyverse)

#Reading csv files
titanic<-read.csv("titanic_data.csv",header=T)

#Structure of the file
str(titanic)

#first six records
head(titanic)

#last six records
tail(titanic)

# Summary of the file
summary(titanic)

#Selecting certain records
titanic$Survived
titanic$Gender

#Selecting multiple columns
titanic[,c("Name","Gender","Fare")] #selecting multiple columns 

titanic[,5:8] #Selecting columns using index

#--------Filter Operation---------
#Filter and select passengers where age>35
titanic[c(titanic$Age>35),c("PassengerId","Gender","Age")]

titanic[c(titanic$Age>35),c("Gender","Fare","Age")]

#Displaing first six records based on the condition
head(titanic[c(titanic$Age>35),c("Gender","Fare","Age")])

#selecting using SELECT -select specific colums
sel_set_1<-titanic %>% select(Pclass,Age,Fare,Survived)

#Females Only
female_Passengers<-titanic %>% filter(Gender=="female")%>%select(Pclass,Age,Fare,Survived)

#Adding Column
titanic$Survival_Status<-ifelse(titanic$Survived>0,"Survived","Dead")

#Creating a Familycount column using mutate() to create new columns 
#merging two columns and adding it as another column
titanic<-titanic%>%mutate(FamilyMembers=titanic$SibSp+titanic$Parch)
titanic<-titanic%>%mutate(AgeGroup=ifelse(titanic$Age>18,"Adult","Kid"))

#--------Sorting-----------
#Ascending Order
fares_asc<-titanic %>% arrange(Fare)

#Descending order
fares_Des<-titanic%>%arrange(desc(Fare))

#Update the age of the passengerId to 23
#Retrieving the current age of the Passenger
titanic[titanic$PassengerId==1,"Age"]
titanic$Age[titanic$PassengerId==1]

#Updating it to 23
titanic$Age[titanic$PassengerId==1]<-23
titanic[titanic$PassengerId==1,"Age"]

#----Group By------
#Find the Numbers of male and Females
titanic%>%group_by(Gender)%>%summarise(count=n())
#titanic%>%group_by(Gender)%>%summarise(count=n())
#Find the mean
titanic%>%group_by(Gender)%>%summarise(AvgAge=mean(Age))
titanic%>%group_by(Gender)%>%summarise(AvgAge=mean(Fare))
#Count the survivors by gender
titanic%>%group_by(Gender)%>%summarise(SurvivalCount=sum(Survived))
#Count Survivors by class
titanic%>%group_by(Pclass)%>%summarise(count=n())
#Count Survivors by class
titanic%>%group_by(Pclass)%>%summarise(Count=sum(Survived))%>%arrange(count)

#Writing a data into a file
write.csv(titanic,"Modified_Titanic.csv")

#Reading a Txt File
text_data<-readLines("R Programming.txt")
text_data  
#Reading a Txt file into a table
Text_data<-read.table("Student_Marks.txt",header=T)
Text_data                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    

#Writing to a Txt File
Student_data<-data.frame(Name=c("Adarsh","Tarun","Sameer","Dhruva"),
                      Age=c(20,18,18,19),
                      Marks=c(79,84,73,94))
write.table(Student_data,"Student_Marks.txt",sep="\t",row.names=FALSE)


















                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             














































































































                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
                          
