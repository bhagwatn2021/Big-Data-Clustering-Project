install.packages("gmodels")
install.packages("factoextra")
install.packages("class")
install.packages("psych")
install.packages("dplyr")
install.packages("rgl")
install.packages("cluster")
library(class)
library(psych)
library(dplyr)
library(scatterplot3d)
library(rgl)
library(cluster)
library(class)
#Question 1
#Load the data
adults= read.csv("data/adult.data", header=FALSE,stringsAsFactors = FALSE)

#Assign names to the columns
adultnames <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","income")
length(adultnames)
adultnames

#Change names of adults to the names in the new columns
names(adults) <- adultnames
names(adults)
str(adults)
adults

#make a copy of adults for plotting purposes
plotAdults <- adults

#investigate statistics of the data

#Translating alphanumeric values to numeric values for age, fnlwgt, education-num, 
#capital-gain, capital loss, hours-per-week to ensure that we are working with 
#integers when pairwise plotting
plotAdults$age<-as.numeric(plotAdults$age)
plotAdults$fnlwgt<-as.numeric(plotAdults$fnlwgt)
plotAdults$`education-num`<-as.numeric(plotAdults$`education-num`)
plotAdults$`capital-gain`<-as.numeric(plotAdults$`capital-gain`)
plotAdults$`capital-loss`<-as.numeric(plotAdults$`capital-loss`)
plotAdults$`hours-per-week`<-as.numeric(plotAdults$`hours-per-week`)
str(adults)
#We still need to be able to plot workclass, education, martial status, occupation, relationship, 
#race, sex, income since they are characters.


#make workclass numeric: 

#check the unique values of workclass to replace with integers
unique(plotAdults$workclass)

#1 for State-gov
#2 for Self-emp-not-inc
#3 for Private
#4 for Federal-gov
#5 for Local-gov
#6 for Self-emp-inc
#NA for unknown values 

#loop through each value of workclass and assign the appropriate integer
for(row in 1:nrow(plotAdults)){
  if(plotAdults[row,"workclass"]==" State-gov"){ 
    plotAdults[row,"workclass"] <- 1
  } 
  else if(plotAdults[row,"workclass"]==" Self-emp-not-inc"){ 
    plotAdults[row,"workclass"] <- 2
  } 
  else if(plotAdults[row,"workclass"]==" Private"){ 
    plotAdults[row,"workclass"] <- 3
  } 
  else if(plotAdults[row,"workclass"]==" Federal-gov"){ 
    plotAdults[row,"workclass"] <- 4
  } 
  else if(plotAdults[row,"workclass"]==" Local-gov"){ 
    plotAdults[row,"workclass"] <- 5
  } 
  else if(plotAdults[row,"workclass"]==" Self-emp-inc"){ 
    plotAdults[row,"workclass"] <- 6
  }
  else if(plotAdults[row,"workclass"]==" ?"){ 
    plotAdults[row,"workclass"] <- na_if( plotAdults[row,"workclass"], " ?")
  } 
}
#convert every value of workclass to ensure that we are plotting numeric values
plotAdults$workclass <- as.numeric(as.character(plotAdults$workclass))
#check whether all values were converted
unique(plotAdults$workclass)
str(plotAdults)

#since education-num correlates to education, we can omit the education column for plotting purposes.

#check the mapping between education and education-num for our reference
checkEducationMapping <- plotAdults[-c(1,2,3,6,7,8,9,10,11,12,13,14,15)]
checkEducationMapping

#education-num mapping for level of education:
#1 for Preschool
#2 for 1st-4th
#3 for 5th-6th
#4 for 7th-8th
#5 for 9th
#6 for 10th
#7 for 11th
#8 for 12th
#9 for HS-grad
#10 for Some-college
#11 for Assoc-voc 
#12 for Assoc-admin
#13 for Bachelors 
#14 for Masters
#15 for Prof-school
#16 for Doctorate

#omit education
plotAdults <- plotAdults[-c(4)]
str(plotAdults)

#make martial-status numeric: 

#check the unique values of marital-status to replace with integers
unique(plotAdults$`marital-status`)

#1 for Never-married
#2 for Married-civ-spouse
#3 for Divorced
#4 for Married-spouse-absent
#5 for Separated
#6 for Married-AF-spouse
#7 for Widowed
#NA for unknown values 

#loop through each value and assign the appropriate integer
for(row in 1:nrow(plotAdults)){
  if(plotAdults[row,"marital-status"]==" Never-married"){ 
    plotAdults[row,"marital-status"] <- 1
  } 
  else if(plotAdults[row,"marital-status"]==" Married-civ-spouse"){ 
    plotAdults[row,"marital-status"] <- 2
  } 
  else if(plotAdults[row,"marital-status"]==" Divorced"){ 
    plotAdults[row,"marital-status"] <- 3
  } 
  else if(plotAdults[row,"marital-status"]==" Married-spouse-absent"){ 
    plotAdults[row,"marital-status"] <- 4
  } 
  else if(plotAdults[row,"marital-status"]==" Separated"){ 
    plotAdults[row,"marital-status"] <- 5
  } 
  else if(plotAdults[row,"marital-status"]==" Married-AF-spouse"){ 
    plotAdults[row,"marital-status"] <- 6
  }
  else if(plotAdults[row,"marital-status"]==" Widowed"){ 
    plotAdults[row,"marital-status"] <- 7
  }
  else if(plotAdults[row,"marital-status"]==" ?"){ 
    plotAdults[row,"marital-status"] <- na_if( plotAdults[row,"marital-status"], " ?")
  } 
}
#convert every value of martial-status to ensure that we are plotting numeric values
plotAdults$`marital-status` <- as.numeric(as.character(plotAdults$`marital-status`))
#check whether all values were converted
unique(plotAdults$`marital-status`)
str(plotAdults)


#make occupation numeric: 

#check the unique values of occupation to replace with integers
unique(plotAdults$occupation)

#1 for Adm-clerical
#2 for Exec-managerial
#3 for Handlers-cleaners
#4 for Prof-specialty
#5 for Other-service
#6 for Sales
#7 for Craft-repair
#8 for Transport-moving
#9 for Farming-fishing
#10 for Machine-op-inspct
#11 for Tech-support
#12 for Protective-serv
#13 for Armed-Forces
#14 for Priv-house-serv
#NA for unknown values 

#loop through each value and assign the appropriate integer
for(row in 1:nrow(plotAdults)){
  if(plotAdults[row,"occupation"]==" Adm-clerical"){ 
    plotAdults[row,"occupation"] <- 1
  } 
  else if(plotAdults[row,"occupation"]==" Exec-managerial"){ 
    plotAdults[row,"occupation"] <- 2
  } 
  else if(plotAdults[row,"occupation"]==" Handlers-cleaners"){ 
    plotAdults[row,"occupation"] <- 3
  } 
  else if(plotAdults[row,"occupation"]==" Prof-specialty"){ 
    plotAdults[row,"occupation"] <- 4
  } 
  else if(plotAdults[row,"occupation"]==" Other-service"){ 
    plotAdults[row,"occupation"] <- 5
  } 
  else if(plotAdults[row,"occupation"]==" Sales"){ 
    plotAdults[row,"occupation"] <- 6
  }
  else if(plotAdults[row,"occupation"]==" Craft-repair"){ 
    plotAdults[row,"occupation"] <- 7
  }
  if(plotAdults[row,"occupation"]==" Transport-moving"){ 
    plotAdults[row,"occupation"] <- 8
  } 
  else if(plotAdults[row,"occupation"]==" Farming-fishing"){ 
    plotAdults[row,"occupation"] <- 9
  } 
  else if(plotAdults[row,"occupation"]==" Machine-op-inspct"){ 
    plotAdults[row,"occupation"] <- 10
  } 
  else if(plotAdults[row,"occupation"]==" Tech-support"){ 
    plotAdults[row,"occupation"] <- 11
  } 
  else if(plotAdults[row,"occupation"]==" Protective-serv"){ 
    plotAdults[row,"occupation"] <- 12
  } 
  else if(plotAdults[row,"occupation"]==" Armed-Forces"){ 
    plotAdults[row,"occupation"] <- 13
  }
  else if(plotAdults[row,"occupation"]==" Priv-house-serv"){ 
    plotAdults[row,"occupation"] <- 14
  }
  else if(plotAdults[row,"occupation"]==" ?"){ 
    plotAdults[row,"occupation"] <- na_if( plotAdults[row,"occupation"], " ?")
  } 
}
#convert every value of workclass to ensure that we are plotting numeric values
plotAdults$occupation <- as.numeric(as.character(plotAdults$occupation))
#check whether all values were converted
unique(plotAdults$occupation)
str(plotAdults)


#make relationship numeric: 

#check the unique values of relationship to replace with integers
unique(plotAdults$relationship)

#1 for Not-in-family
#2 for Husband
#3 for Wife
#4 for Own-child
#5 for Unmarried
#6 for Other-relative
#NA for unknown values 

#loop through each value and assign the appropriate integer
for(row in 1:nrow(plotAdults)){
  if(plotAdults[row,"relationship"]==" Not-in-family"){ 
    plotAdults[row,"relationship"] <- 1
  } 
  else if(plotAdults[row,"relationship"]==" Husband"){ 
    plotAdults[row,"relationship"] <- 2
  } 
  else if(plotAdults[row,"relationship"]==" Wife"){ 
    plotAdults[row,"relationship"] <- 3
  } 
  else if(plotAdults[row,"relationship"]==" Own-child"){ 
    plotAdults[row,"relationship"] <- 4
  } 
  else if(plotAdults[row,"relationship"]==" Unmarried"){ 
    plotAdults[row,"relationship"] <- 5
  } 
  else if(plotAdults[row,"relationship"]==" Other-relative"){ 
    plotAdults[row,"relationship"] <- 6
  }
  else if(plotAdults[row,"relationship"]==" ?"){ 
    plotAdults[row,"relationship"] <- na_if( plotAdults[row,"relationship"], " ?")
  } 
}
#convert every value of relationship to ensure that we are plotting numeric values
plotAdults$relationship <- as.numeric(as.character(plotAdults$relationship))
#check whether all values were converted
unique(plotAdults$relationship)
str(plotAdults)

#make race numeric: 

#check the unique values of race to replace with integers
unique(plotAdults$race)

#1 for White
#2 for Black
#3 for Asian-Pac-Islander
#4 for Amer-Indian-Eskimo
#5 for Other
#NA for unknown values 

#loop through each value and assign the appropriate integer
for(row in 1:nrow(plotAdults)){
  if(plotAdults[row,"race"]==" White"){ 
    plotAdults[row,"race"] <- 1
  } 
  else if(plotAdults[row,"race"]==" Black"){ 
    plotAdults[row,"race"] <- 2
  } 
  else if(plotAdults[row,"race"]==" Asian-Pac-Islander"){ 
    plotAdults[row,"race"] <- 3
  } 
  else if(plotAdults[row,"race"]==" Amer-Indian-Eskimo"){ 
    plotAdults[row,"race"] <- 4
  } 
  else if(plotAdults[row,"race"]==" Other"){ 
    plotAdults[row,"race"] <- 5
  } 
  else if(plotAdults[row,"race"]==" ?"){ 
    plotAdults[row,"race"] <- na_if(plotAdults[row,"race"], " ?")
  } 
}
#convert every value to ensure that we are plotting numeric values
plotAdults$race <- as.numeric(as.character(plotAdults$race))
#check whether all values were converted
unique(plotAdults$race)
str(plotAdults)


#make sex numeric: 

#check the unique values of sex to replace with integers
unique(plotAdults$sex)

#1 for Male
#2 for Female
#NA for unknown values 

#loop through each value and assign the appropriate integer
for(row in 1:nrow(plotAdults)){
  if(plotAdults[row,"sex"]==" Male"){ 
    plotAdults[row,"sex"] <- 1
  } 
  else if(plotAdults[row,"sex"]==" Female"){ 
    plotAdults[row,"sex"] <- 2
  } 
  else if(plotAdults[row,"sex"]==" ?"){ 
    plotAdults[row,"sex"] <- na_if(plotAdults[row,"sex"], " ?")
  } 
}
#convert every value to ensure that we are plotting numeric values
plotAdults$sex <- as.numeric(as.character(plotAdults$sex))
#check whether all values were converted
unique(plotAdults$sex)
str(plotAdults)

#make native-country numeric: 

#check the unique values of native-country to replace with integers
unique(plotAdults$`native-country`)

#1 for United-States
#2 for Cuba
#3 for Jamaica
#4 for India
#5 for Mexico
#6 for South
#7 for Puerto-Rico
#8 for Honduras
#9 for England
#10 for Canada
#11 for Germany
#12 for Iran
#13 for Philippines
#14 for Italy
#15 for Poland
#16 for Columbia
#17 for Cambodia
#18 for Thailand
#19 for Ecuador
#20 for Laos
#21 for Taiwan
#22 for Haiti
#23 for Portugal
#24 for Dominican-Republic
#25 for El-Salvador
#26 for France
#27 for Guatemala
#28 for China
#NA for unknown values 

#loop through each value and assign the appropriate integer
for(row in 1:nrow(plotAdults)){
  if(plotAdults[row,"native-country"]==" United-States"){ 
    plotAdults[row,"native-country"] <- 1
  } 
  else if(plotAdults[row,"native-country"]==" Cuba"){ 
    plotAdults[row,"native-country"] <- 2
  } 
  else if(plotAdults[row,"native-country"]==" Jamaica"){ 
    plotAdults[row,"native-country"] <- 3
  } 
  else if(plotAdults[row,"native-country"]==" India"){ 
    plotAdults[row,"native-country"] <- 4
  } 
  else if(plotAdults[row,"native-country"]==" Mexico"){ 
    plotAdults[row,"native-country"] <- 5
  } 
  else if(plotAdults[row,"native-country"]==" South"){ 
    plotAdults[row,"native-country"] <- 6
  }
  else if(plotAdults[row,"native-country"]==" Puerto-Rico"){ 
    plotAdults[row,"native-country"] <- 7
  }
  if(plotAdults[row,"native-country"]==" Honduras"){ 
    plotAdults[row,"native-country"] <- 8
  } 
  else if(plotAdults[row,"native-country"]==" England"){ 
    plotAdults[row,"native-country"] <- 9
  } 
  else if(plotAdults[row,"native-country"]==" Canada"){ 
    plotAdults[row,"native-country"] <- 10
  } 
  else if(plotAdults[row,"native-country"]==" Germany"){ 
    plotAdults[row,"native-country"] <- 11
  } 
  else if(plotAdults[row,"native-country"]==" Iran"){ 
    plotAdults[row,"native-country"] <- 12
  } 
  else if(plotAdults[row,"native-country"]==" Philippines"){ 
    plotAdults[row,"native-country"] <- 13
  }
  else if(plotAdults[row,"native-country"]==" Italy"){ 
    plotAdults[row,"native-country"] <- 14
  }
  if(plotAdults[row,"native-country"]==" Poland"){ 
    plotAdults[row,"native-country"] <- 15
  } 
  else if(plotAdults[row,"native-country"]==" Columbia"){ 
    plotAdults[row,"native-country"] <- 16
  } 
  else if(plotAdults[row,"native-country"]==" Cambodia"){ 
    plotAdults[row,"native-country"] <- 17
  } 
  else if(plotAdults[row,"native-country"]==" Thailand"){ 
    plotAdults[row,"native-country"] <- 18
  } 
  else if(plotAdults[row,"native-country"]==" Ecuador"){ 
    plotAdults[row,"native-country"] <- 19
  } 
  else if(plotAdults[row,"native-country"]==" Laos"){ 
    plotAdults[row,"native-country"] <- 20
  }
  else if(plotAdults[row,"native-country"]==" Taiwan"){ 
    plotAdults[row,"native-country"] <- 21
  }
  else if(plotAdults[row,"native-country"]==" Haiti"){ 
    plotAdults[row,"native-country"] <- 22
  } 
  else if(plotAdults[row,"native-country"]==" Portugal"){ 
    plotAdults[row,"native-country"] <- 23
  } 
  else if(plotAdults[row,"native-country"]==" Dominican-Republic"){ 
    plotAdults[row,"native-country"] <- 24
  } 
  else if(plotAdults[row,"native-country"]==" El-Salvador"){ 
    plotAdults[row,"native-country"] <- 25
  } 
  else if(plotAdults[row,"native-country"]==" France"){ 
    plotAdults[row,"native-country"] <- 26
  } 
  else if(plotAdults[row,"native-country"]==" Guatemala"){ 
    plotAdults[row,"native-country"] <- 27
  }
  else if(plotAdults[row,"native-country"]==" China"){ 
    plotAdults[row,"native-country"] <- 28
  }
  else if(plotAdults[row,"native-country"]==" ?"){ 
    plotAdults[row,"native-country"] <- na_if( plotAdults[row,"native-country"], " ?")
  } 
}
#convert every value of workclass to ensure that we are plotting numeric values
plotAdults$`native-country` <- as.numeric(as.character(plotAdults$`native-country`))
#check whether all values were converted
unique(plotAdults$`native-country`)
str(plotAdults)


#make income numeric: 

#check the unique values of numeric to replace with integers
unique(plotAdults$income)

#1 for <=50K
#2 for >50K
#NA for unknown values 

#loop through each value and assign the appropriate integer
for(row in 1:nrow(plotAdults)){
  if(plotAdults[row,"income"]==" <=50K"){ 
    plotAdults[row,"income"] <- 1
  } 
  else if(plotAdults[row,"income"]==" >50K"){ 
    plotAdults[row,"income"] <- 2
  } 
  else if(plotAdults[row,"income"]==" ?"){ 
    plotAdults[row,"income"] <- na_if(plotAdults[row,"income"], " ?")
  } 
}
#convert every value to ensure that we are plotting numeric values
plotAdults$income <- as.numeric(as.character(plotAdults$income))
#check whether all values were converted
unique(plotAdults$income)
str(plotAdults)

#omit any NA values and take a subset of 1000 records of the data
plotAdults<- na.omit(plotAdults)
plotAdults <- plotAdults[1:1000,]
#plot data
plot(plotAdults)

#3d plots with diff varibales 
attach(plotAdults)
scatterplot3d(plotAdults$age,plotAdults$income,plotAdults$`education-num`, main="3D Scatterplot 1")
scatterplot3d(plotAdults$income,plotAdults$occupation,plotAdults$relationship, main="3D Scatterplot 2")
scatterplot3d(plotAdults$age,plotAdults$income,plotAdults$occupation, main="3D Scatterplot 3")


#Question 3
#Eliminate <=50K and >50K column from the data set.

plotAdults <- plotAdults[-c(14)]
str(plotAdults)

describe(plotAdults)
#plot(plotAdults)
adultsTest<- plotAdults[1:398,]
adultsTest
str(adultsTest)
#plot(adultsTest)
#Suppose to keep the linear or near linear relationships but I can't tell with the plot
#delete attributes that are not linear 

#normalization Process
normalize<- function(x) {((x-min(x))/(max(x)-min(x)))}
normalize
normalize(c(1,2,3,4,5,6,7,8,9, 10))

plotAdults.norm<- as.data.frame(lapply(plotAdults,normalize))
plotAdults.norm

#Kmeans 2 to 10
#It doens't look like the profs example at all
plotAdults.k2<-kmeans(plotAdults.norm, centers = 2, nstart = 25)
plotAdults.k2
factoextra::fviz_cluster(plotAdults.k2, plotAdults)

plotAdults.k3<-kmeans(plotAdults.norm, centers = 3, nstart = 25)
plotAdults.k3
factoextra::fviz_cluster(plotAdults.k3, plotAdults)

plotAdults.k4<-kmeans(plotAdults.norm, centers = 4, nstart = 25)
str(plotAdults.k4)
factoextra::fviz_cluster(plotAdults.k4, plotAdults)

plotAdults.k5<-kmeans(plotAdults.norm, centers = 5, nstart = 25)
plotAdults.k5
factoextra::fviz_cluster(plotAdults.k5, plotAdults)

plotAdults.k6<-kmeans(plotAdults.norm, centers = 6, nstart = 25)
plotAdults.k6
factoextra::fviz_cluster(plotAdults.k6, plotAdults)

plotAdults.k7<-kmeans(plotAdults.norm, centers = 7, nstart = 25)
plotAdults.k7
factoextra::fviz_cluster(plotAdults.k7, plotAdults)

plotAdults.k8<-kmeans(plotAdults.norm, centers = 8, nstart = 25)
plotAdults.k8
factoextra::fviz_cluster(plotAdults.k8, plotAdults)

plotAdults.k9<-kmeans(plotAdults.norm, centers = 9, nstart = 25)
plotAdults.k9
factoextra::fviz_cluster(plotAdults.k9, plotAdults)

plotAdults.k10<-kmeans(plotAdults.norm, centers = 10, nstart = 25)
plotAdults.k10
factoextra::fviz_cluster(plotAdults.k10, plotAdults)

#shows the optimal number of clusters
factoextra::fviz_nbclust(plotAdults, FUNcluster=kmeans,print.summary=TRUE)


# KNN

#Prep for knn, 70-30 training ratio

plotAdults.norm.nrows<-nrow(plotAdults.norm)
plotAdults.norm.sample<-0.7
plotAdults.norm.train.index<-sample(plotAdults.norm.nrows, plotAdults.norm.sample*plotAdults.norm.nrows)
length(plotAdults.norm.train.index)
plotAdults.norm.train<-plotAdults.norm[plotAdults.norm.train.index,]
plotAdults.norm.train[1:20,]
plotAdults.norm.test<-plotAdults.norm[-plotAdults.norm.train.index,]

#knn=2
plotAdults.norm.train.k2<-kmeans(plotAdults.norm.train, centers=2)
plotAdults.norm.train.k2$cluster
plotAdults.norm.test.k2<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k2$cluster, k=2)
plotAdults.norm.test.k2
plotAdults.norm.test.k2<-kmeans(plotAdults.norm.test, centers = 2)
plotAdults.norm.test.k2$cluster

#knn=3
plotAdults.norm.train.k3<-kmeans(plotAdults.norm.train, centers=3)
plotAdults.norm.train.k3
plotAdults.norm.test.k3<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k3$cluster, k=3)
plotAdults.norm.test.k3
plotAdults.norm.test.k3<-kmeans(plotAdults.norm.test, centers = 3)
plotAdults.norm.test.k3$cluster

#knn=4
plotAdults.norm.train.k4<-kmeans(plotAdults.norm.train, centers=4)
plotAdults.norm.train.k4
plotAdults.norm.test.k4<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k4$cluster, k=4)
plotAdults.norm.test.k4
plotAdults.norm.test.k4<-kmeans(plotAdults.norm.test, centers = 4)
plotAdults.norm.test.k4$cluster

#knn=5
plotAdults.norm.train.k5<-kmeans(plotAdults.norm.train, centers=5)
plotAdults.norm.train.k5
plotAdults.norm.test.k5<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k5$cluster, k=5)
plotAdults.norm.test.k5
plotAdults.norm.test.k5<-kmeans(plotAdults.norm.test, centers = 5)
plotAdults.norm.test.k5$cluster

#knn=6
plotAdults.norm.train.k6<-kmeans(plotAdults.norm.train, centers=6)
plotAdults.norm.train.k6
plotAdults.norm.test.k6<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k6$cluster, k=6)
plotAdults.norm.test.k6
plotAdults.norm.test.k6<-kmeans(plotAdults.norm.test, centers = 6)
plotAdults.norm.test.k6$cluster

#knn=7
plotAdults.norm.train.k7<-kmeans(plotAdults.norm.train, centers=7)
plotAdults.norm.train.k7
plotAdults.norm.test.k7<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k7$cluster, k=7)
plotAdults.norm.test.k7
plotAdults.norm.test.k7<-kmeans(plotAdults.norm.test, centers = 7)
plotAdults.norm.test.k7$cluster

#knn=8
plotAdults.norm.train.k8<-kmeans(plotAdults.norm.train, centers=8)
plotAdults.norm.train.k8
plotAdults.norm.test.k8<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k8$cluster, k=8)
plotAdults.norm.test.k8
plotAdults.norm.test.k8<-kmeans(plotAdults.norm.test, centers = 8)
plotAdults.norm.test.k8$cluster

#knn=9
plotAdults.norm.train.k9<-kmeans(plotAdults.norm.train, centers=9)
plotAdults.norm.train.k9
plotAdults.norm.test.k9<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k9$cluster, k=9)
plotAdults.norm.test.k9
plotAdults.norm.test.k9<-kmeans(plotAdults.norm.test, centers = 9)
plotAdults.norm.test.k9$cluster

#knn=10
plotAdults.norm.train.k10<-kmeans(plotAdults.norm.train, centers=10)
plotAdults.norm.train.k10
plotAdults.norm.test.k10<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k10$cluster, k=10)
plotAdults.norm.test.k10
plotAdults.norm.test.k10<-kmeans(plotAdults.norm.test, centers = 10)
plotAdults.norm.test.k10$cluster


# lm/glm/pred:  70 - 30

# 1st Attribute Combo: Age ~ race + sex + capital.gain
# lm - age over everything else 
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$age~plotAdults.norm.train$race+
                                 plotAdults.norm.train$sex+plotAdults.norm.train$capital.gain,
                               data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$age~plotAdults.norm.train$race+
                                   plotAdults.norm.train$sex+plotAdults.norm.train$capital.gain, 
                                 family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$race)
summary(plotAdults.norm.test$sex)
summary(plotAdults.norm.test$capital.gain)

# 2nd Attribute Combo: Education ~ workclass + race
# lm -  
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$workclass+
                                 plotAdults.norm.train$race, data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$workclass+
                                   plotAdults.norm.train$race, family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$education.num)
summary(plotAdults.norm.test$workclass)
summary(plotAdults.norm.test$race)


# 3rd Attribute Combo: Education ~ capitalgain + race
# lm -  
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$capital.gain+
                                 plotAdults.norm.train$race, data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$capital.gain+
                                   plotAdults.norm.train$race, family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$education.num)
summary(plotAdults.norm.test$capital.gain)
summary(plotAdults.norm.test$race)



# Prep for knn, 60-40 training ratio

plotAdults.norm.nrows<-nrow(plotAdults.norm)
plotAdults.norm.sample<-0.6
plotAdults.norm.train.index<-sample(plotAdults.norm.nrows, plotAdults.norm.sample*plotAdults.norm.nrows)
length(plotAdults.norm.train.index)
plotAdults.norm.train<-plotAdults.norm[plotAdults.norm.train.index,]
plotAdults.norm.train[1:20,]
plotAdults.norm.test<-plotAdults.norm[-plotAdults.norm.train.index,]
plotAdults.norm.test[1:20,]

#knn=2
plotAdults.norm.train.k2<-kmeans(plotAdults.norm.train, centers=2)
plotAdults.norm.train.k2$cluster
plotAdults.norm.test.k2<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k2$cluster, k=2)
plotAdults.norm.test.k2
plotAdults.norm.test.k2<-kmeans(plotAdults.norm.test, centers = 2)
plotAdults.norm.test.k2$cluster

#knn=3
plotAdults.norm.train.k3<-kmeans(plotAdults.norm.train, centers=3)
plotAdults.norm.train.k3
plotAdults.norm.test.k3<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k3$cluster, k=3)
plotAdults.norm.test.k3
plotAdults.norm.test.k3<-kmeans(plotAdults.norm.test, centers = 3)
plotAdults.norm.test.k3$cluster

#knn=4
plotAdults.norm.train.k4<-kmeans(plotAdults.norm.train, centers=4)
plotAdults.norm.train.k4
plotAdults.norm.test.k4<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k4$cluster, k=4)
plotAdults.norm.test.k4
plotAdults.norm.test.k4<-kmeans(plotAdults.norm.test, centers = 4)
plotAdults.norm.test.k4$cluster

#knn=5
plotAdults.norm.train.k5<-kmeans(plotAdults.norm.train, centers=5)
plotAdults.norm.train.k5
plotAdults.norm.test.k5<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k5$cluster, k=5)
plotAdults.norm.test.k5
plotAdults.norm.test.k5<-kmeans(plotAdults.norm.test, centers = 5)
plotAdults.norm.test.k5$cluster

#knn=6
plotAdults.norm.train.k6<-kmeans(plotAdults.norm.train, centers=6)
plotAdults.norm.train.k6
plotAdults.norm.test.k6<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k6$cluster, k=6)
plotAdults.norm.test.k6
plotAdults.norm.test.k6<-kmeans(plotAdults.norm.test, centers = 6)
plotAdults.norm.test.k6$cluster

#knn=7
plotAdults.norm.train.k7<-kmeans(plotAdults.norm.train, centers=7)
plotAdults.norm.train.k7
plotAdults.norm.test.k7<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k7$cluster, k=7)
plotAdults.norm.test.k7
plotAdults.norm.test.k7<-kmeans(plotAdults.norm.test, centers = 7)
plotAdults.norm.test.k7$cluster

#knn=8
plotAdults.norm.train.k8<-kmeans(plotAdults.norm.train, centers=8)
plotAdults.norm.train.k8
plotAdults.norm.test.k8<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k8$cluster, k=8)
plotAdults.norm.test.k8
plotAdults.norm.test.k8<-kmeans(plotAdults.norm.test, centers = 8)
plotAdults.norm.test.k8$cluster

#knn=9
plotAdults.norm.train.k9<-kmeans(plotAdults.norm.train, centers=9)
plotAdults.norm.train.k9
plotAdults.norm.test.k9<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k9$cluster, k=9)
plotAdults.norm.test.k9
plotAdults.norm.test.k9<-kmeans(plotAdults.norm.test, centers = 9)
plotAdults.norm.test.k9$cluster

#knn=10
plotAdults.norm.train.k10<-kmeans(plotAdults.norm.train, centers=10)
plotAdults.norm.train.k10
plotAdults.norm.test.k10<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k10$cluster, k=10)
plotAdults.norm.test.k10
plotAdults.norm.test.k10<-kmeans(plotAdults.norm.test, centers = 10)
plotAdults.norm.test.k10$cluster


# lm/glm/pred:  60 - 40

# 1st Attribute Combo: Age ~ race + sex + capital.gain
# lm - age over everything else 
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$age~plotAdults.norm.train$race+
                                 plotAdults.norm.train$sex+plotAdults.norm.train$capital.gain,
                               data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$age~plotAdults.norm.train$race+
                                   plotAdults.norm.train$sex+plotAdults.norm.train$capital.gain, 
                                 family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$race)
summary(plotAdults.norm.test$sex)
summary(plotAdults.norm.test$capital.gain)

# 2nd Attribute Combo: Education ~ workclass + race
# lm -  
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$workclass+
                                 plotAdults.norm.train$race, data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$workclass+
                                   plotAdults.norm.train$race, family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$education.num)
summary(plotAdults.norm.test$workclass)
summary(plotAdults.norm.test$race)


# 3rd Attribute Combo: Education ~ capitalgain + race
# lm -  
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$capital.gain+
                                 plotAdults.norm.train$race, data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$capital.gain+
                                   plotAdults.norm.train$race, family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$education.num)
summary(plotAdults.norm.test$capital.gain)
summary(plotAdults.norm.test$race)



# Prep for knn, 50-50 training ratio

plotAdults.norm.nrows<-nrow(plotAdults.norm)
plotAdults.norm.sample<-0.5
plotAdults.norm.train.index<-sample(plotAdults.norm.nrows, plotAdults.norm.sample*plotAdults.norm.nrows)
length(plotAdults.norm.train.index)
plotAdults.norm.train<-plotAdults.norm[plotAdults.norm.train.index,]
plotAdults.norm.train[1:20,]
plotAdults.norm.test<-plotAdults.norm[-plotAdults.norm.train.index,]
plotAdults.norm.test[1:20,]

#knn=2
plotAdults.norm.train.k2<-kmeans(plotAdults.norm.train, centers=2)
plotAdults.norm.train.k2$cluster
plotAdults.norm.test.k2<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k2$cluster, k=2)
plotAdults.norm.test.k2
plotAdults.norm.test.k2<-kmeans(plotAdults.norm.test, centers = 2)
plotAdults.norm.test.k2$cluster

#knn=3
plotAdults.norm.train.k3<-kmeans(plotAdults.norm.train, centers=3)
plotAdults.norm.train.k3
plotAdults.norm.test.k3<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k3$cluster, k=3)
plotAdults.norm.test.k3
plotAdults.norm.test.k3<-kmeans(plotAdults.norm.test, centers = 3)
plotAdults.norm.test.k3$cluster

#knn=4
plotAdults.norm.train.k4<-kmeans(plotAdults.norm.train, centers=4)
plotAdults.norm.train.k4
plotAdults.norm.test.k4<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k4$cluster, k=4)
plotAdults.norm.test.k4
plotAdults.norm.test.k4<-kmeans(plotAdults.norm.test, centers = 4)
plotAdults.norm.test.k4$cluster

#knn=5
plotAdults.norm.train.k5<-kmeans(plotAdults.norm.train, centers=5)
plotAdults.norm.train.k5
plotAdults.norm.test.k5<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k5$cluster, k=5)
plotAdults.norm.test.k5
plotAdults.norm.test.k5<-kmeans(plotAdults.norm.test, centers = 5)
plotAdults.norm.test.k5$cluster

#knn=6
plotAdults.norm.train.k6<-kmeans(plotAdults.norm.train, centers=6)
plotAdults.norm.train.k6
plotAdults.norm.test.k6<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k6$cluster, k=6)
plotAdults.norm.test.k6
plotAdults.norm.test.k6<-kmeans(plotAdults.norm.test, centers = 6)
plotAdults.norm.test.k6$cluster

#knn=7
plotAdults.norm.train.k7<-kmeans(plotAdults.norm.train, centers=7)
plotAdults.norm.train.k7
plotAdults.norm.test.k7<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k7$cluster, k=7)
plotAdults.norm.test.k7
plotAdults.norm.test.k7<-kmeans(plotAdults.norm.test, centers = 7)
plotAdults.norm.test.k7$cluster

#knn=8
plotAdults.norm.train.k8<-kmeans(plotAdults.norm.train, centers=8)
plotAdults.norm.train.k8
plotAdults.norm.test.k8<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k8$cluster, k=8)
plotAdults.norm.test.k8
plotAdults.norm.test.k8<-kmeans(plotAdults.norm.test, centers = 8)
plotAdults.norm.test.k8$cluster

#knn=9
plotAdults.norm.train.k9<-kmeans(plotAdults.norm.train, centers=9)
plotAdults.norm.train.k9
plotAdults.norm.test.k9<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k9$cluster, k=9)
plotAdults.norm.test.k9
plotAdults.norm.test.k9<-kmeans(plotAdults.norm.test, centers = 9)
plotAdults.norm.test.k9$cluster

#knn=10
plotAdults.norm.train.k10<-kmeans(plotAdults.norm.train, centers=10)
plotAdults.norm.train.k10
plotAdults.norm.test.k10<-knn(plotAdults.norm.train, plotAdults.norm.test, plotAdults.norm.train.k10$cluster, k=10)
plotAdults.norm.test.k10
plotAdults.norm.test.k10<-kmeans(plotAdults.norm.test, centers = 10)
plotAdults.norm.test.k10$cluster

# lm/glm/pred:  50 - 50

# 1st Attribute Combo: Age ~ race + sex + capital.gain
# lm - age over everything else 
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$age~plotAdults.norm.train$race+
                                 plotAdults.norm.train$sex+plotAdults.norm.train$capital.gain,
                               data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$age~plotAdults.norm.train$race+
                                   plotAdults.norm.train$sex+plotAdults.norm.train$capital.gain, 
                                 family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$race)
summary(plotAdults.norm.test$sex)
summary(plotAdults.norm.test$capital.gain)

# 2nd Attribute Combo: Education ~ workclass + race
# lm -  
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$workclass+
                                 plotAdults.norm.train$race, data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$workclass+
                                   plotAdults.norm.train$race, family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$education.num)
summary(plotAdults.norm.test$workclass)
summary(plotAdults.norm.test$race)


# 3rd Attribute Combo: Education ~ capitalgain + race
# lm -  
plotAdults.norm.train.lm <- lm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$capital.gain+
                                 plotAdults.norm.train$race, data = plotAdults.norm.train)
summary.lm(plotAdults.norm.train.lm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.lm)

# glm
plotAdults.norm.train.glm <- glm(formula = plotAdults.norm.train$education.num~plotAdults.norm.train$capital.gain+
                                   plotAdults.norm.train$race, family = gaussian, data = plotAdults.norm.train)
summary.glm(plotAdults.norm.train.glm)
par(mfrow=c(2,2))
plot(plotAdults.norm.train.glm)

# pred
plotAdults.norm.train.pred <- predict(plotAdults.norm.train.lm, newdata = plotAdults.norm.test)
summary(plotAdults.norm.train.pred)
summary(plotAdults.norm.test$education.num)
summary(plotAdults.norm.test$capital.gain)
summary(plotAdults.norm.test$race)



# iclust:

iclust(plotAdults, nclusters = 2)
iclust(plotAdults, nclusters = 3)
iclust(plotAdults, nclusters = 4) 
#Got a warning when nclusters=4
iclust(plotAdults, nclusters = 5)
iclust(plotAdults, nclusters = 6)
#nclust=6 more warnings
#THere are more issues below
iclust(plotAdults, nclusters = 7)
iclust(plotAdults, nclusters = 8)
iclust(plotAdults, nclusters = 9)
iclust(plotAdults, nclusters = 10)

