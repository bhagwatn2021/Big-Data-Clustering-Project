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

#Create a mapping for each field of values to integers. Make sure you put these mappings into your report.
#Look into factors and levels and all of that


#Question 3
#Eliminate <=50K and >50K column from the data set.
adultsCopy <- adultsCopy[-c(11)]
str(adultsCopy)









