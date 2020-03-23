#Objective: Prediction task is to determine whether a person makes over 50K a year.  

#set up the data: read the data into a table, set the variable names and assign them to the data
install.packages("gmodels")
install.packages("class")
library(class)     

adults= read.csv("data/adult.data", header=F)
# Done this way to remove comma

adultnames <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","income")
length(adultnames)
adultnames

names(adults) <- adultnames
names(adults)
adults

#Question 2
adultsCopy <- adults[-c(2,4,6,14)]
adultsCopy
#Remove education because that correlates to edu num (Masters == 14, Bachelors = 13, some college = 10, 
#hs grad=9, 9th grade=5, 5th-6th = 3 etc)
#Remove marital status cause that correlates to relationship (only thing to consider not-in-family connects to divorce
#or never married, relationship also mentions if never-married or divorce people have kids)
#Remove country of origin because that often translates into race and race is a greater indictor of income than native country
#Remove workClass because that doesn't really indicate a person's income for the first 20 data set I looked at
str(adultsCopy)

# translating alphanumeric val to num val
adultsCopy$age<-as.numeric(adultsCopy$age)
adultsCopy$fnlwgt<-as.numeric(adultsCopy$fnlwgt)
adultsCopy$`education-num`<-as.numeric(adultsCopy$`education-num`)
adultsCopy$age<-as.numeric(adultsCopy$age)
adultsCopy$`capital-gain`<-as.numeric(adultsCopy$`capital-gain`)
adultsCopy$`capital-loss`<-as.numeric(adultsCopy$`capital-loss`)
adultsCopy$`hours-per-week`<-as.numeric(adultsCopy$`hours-per-week`)

str(adultsCopy)

replace(adultsCopy,adultsCopy == " ?",NA)
#I tried to replace data values with ? to NA so we can remove them later for the analysis but it doesn't work

levels(adultsCopy$occupation)
levels(adultsCopy$relationship)
levels(adultsCopy$race)
levels(adultsCopy$sex)
#Create a mapping for each field of values to integers. Make sure you put these mappings into your report.
#Levels explains the mapping 

adultsCopy$occupation<-as.numeric(adultsCopy$occupation)
adultsCopy$relationship<-as.numeric(adultsCopy$relationship)
adultsCopy$race<-as.numeric(adultsCopy$race)
adultsCopy$sex<-as.numeric(adultsCopy$sex)
#now that we have the mapping assign numeric values to each one


#Question 3
#Eliminate <=50K and >50K column from the data set.
adultsCopy <- adultsCopy[-c(11)]
str(adultsCopy)


adultsCopy<-na.omit(adultsCopy)
#omit the NA








