#Objective: Prediction task is to determine whether a person makes over 50K a year.  

#set up the data: read the data into a table, set the variable names and assign them to the data
install.packages("gmodels")
install.packages("factoextra")
install.packages("class")
install.packages("psych")
library(class)
library(psych)

adults= read.csv("data/adult.data", header=F)
# Done this way to remove commas

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
#omit the NA except does nothing cause can't change ? --> NA

describe(adultsCopy)
#plot(adultsCopy)
adultsTest<- adultsCopy[1:398,]
adultsTest
str(adultsTest)
#plot(adultsTest)
#Suppose to keep the linear or near linear relationships but I can't tell with the plot
#delete attributes that are not linear 

#normalization Process
normalize<- function(x) {((x-min(x))/(max(x)-min(x)))}
normalize
normalize(c(1,2,3,4,5,6,7,8,9, 10))
adultsCopy.norm<- as.data.frame(lapply(adultsCopy[,1:10],normalize))
adultsCopy.norm[1:10,]

#Kmeans 2 to 10
#It doens't look like the profs example at all
adultsCopy.k2<-kmeans(adultsCopy.norm, centers = 2, nstart = 25)
adultsCopy.k2
factoextra::fviz_cluster(adultsCopy.k2, adultsCopy)

adultsCopy.k3<-kmeans(adultsCopy.norm, centers = 3, nstart = 25)
adultsCopy.k3
factoextra::fviz_cluster(adultsCopy.k3, adultsCopy)

adultsCopy.k4<-kmeans(adultsCopy.norm, centers = 4, nstart = 25)
adultsCopy.k4
factoextra::fviz_cluster(adultsCopy.k4, adultsCopy)

adultsCopy.k5<-kmeans(adultsCopy.norm, centers = 5, nstart = 25)
adultsCopy.k5
factoextra::fviz_cluster(adultsCopy.k5, adultsCopy)

adultsCopy.k6<-kmeans(adultsCopy.norm, centers = 6, nstart = 25)
adultsCopy.k6
factoextra::fviz_cluster(adultsCopy.k6, adultsCopy)

adultsCopy.k7<-kmeans(adultsCopy.norm, centers = 7, nstart = 25)
adultsCopy.k7
factoextra::fviz_cluster(adultsCopy.k7, adultsCopy)

adultsCopy.k8<-kmeans(adultsCopy.norm, centers = 8, nstart = 25)
adultsCopy.k8
factoextra::fviz_cluster(adultsCopy.k8, adultsCopy)

adultsCopy.k9<-kmeans(adultsCopy.norm, centers = 9, nstart = 25)
adultsCopy.k9
factoextra::fviz_cluster(adultsCopy.k9, adultsCopy)

adultsCopy.k10<-kmeans(adultsCopy.norm, centers = 10, nstart = 25)
adultsCopy.k10
factoextra::fviz_cluster(adultsCopy.k10, adultsCopy)

#shows the optimal number of clusters
#takes too long on my computer so I couldn't see the results
# Also we don;t have to do this 
factoextra::fviz_nbclust(adultsCopy, FUNcluster=kmeans,print.summary=TRUE)

#Prep for knn
adultsCopy.norm.nrows<-nrow(adultsCopy.norm)
adultsCopy.norm.sample<-0.7
adultsCopy.norm.train.index<-sample(adultsCopy.norm.nrows, adultsCopy.norm.sample*adultsCopy.norm.nrows)
length(adultsCopy.norm.train.index)
adultsCopy.norm.train<-adultsCopy.norm[adultsCopy.norm.train.index,]
adultsCopy.norm.train[1:20,]
adultsCopy.norm.test<-adultsCopy.norm[-adultsCopy.norm.train.index,]
adultsCopy.norm.test[1:20,]

#knn=2
adultsCopy.norm.train.k2<-kmeans(adultsCopy.norm.train, centers=2)
adultsCopy.norm.train.k2
adultsCopy.norm.test.k2<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k2$cluster, k=2)
adultsCopy.norm.test.k2
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k2$cluster
adultsCopy.norm.train.labels

#knn=3
adultsCopy.norm.train.k3<-kmeans(adultsCopy.norm.train, centers=3)
adultsCopy.norm.train.k3
adultsCopy.norm.test.k3<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k3$cluster, k=3)
adultsCopy.norm.test.k3
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k3$cluster
adultsCopy.norm.train.labels

#knn=4
adultsCopy.norm.train.k4<-kmeans(adultsCopy.norm.train, centers=4)
adultsCopy.norm.train.k4
adultsCopy.norm.test.k4<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k4$cluster, k=4)
adultsCopy.norm.test.k4
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k4$cluster
adultsCopy.norm.train.labels

#knn=5
adultsCopy.norm.train.k5<-kmeans(adultsCopy.norm.train, centers=5)
adultsCopy.norm.train.k5
adultsCopy.norm.test.k5<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k5$cluster, k=5)
adultsCopy.norm.test.k5
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k5$cluster
adultsCopy.norm.train.labels

#knn=6
adultsCopy.norm.train.k6<-kmeans(adultsCopy.norm.train, centers=6)
adultsCopy.norm.train.k6
adultsCopy.norm.test.k6<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k6$cluster, k=6)
adultsCopy.norm.test.k6
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k6$cluster
adultsCopy.norm.train.labels

#knn=7
adultsCopy.norm.train.k7<-kmeans(adultsCopy.norm.train, centers=7)
adultsCopy.norm.train.k7
adultsCopy.norm.test.k7<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k7$cluster, k=7)
adultsCopy.norm.test.k7
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k7$cluster
adultsCopy.norm.train.labels

#knn=8
adultsCopy.norm.train.k8<-kmeans(adultsCopy.norm.train, centers=8)
adultsCopy.norm.train.k8
adultsCopy.norm.test.k8<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k8$cluster, k=8)
adultsCopy.norm.test.k8
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k8$cluster
adultsCopy.norm.train.labels

#knn=9
adultsCopy.norm.train.k9<-kmeans(adultsCopy.norm.train, centers=9)
adultsCopy.norm.train.k9
adultsCopy.norm.test.k9<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k9$cluster, k=9)
adultsCopy.norm.test.k9
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k9$cluster
adultsCopy.norm.train.labels

#knn=10
adultsCopy.norm.train.k10<-kmeans(adultsCopy.norm.train, centers=10)
adultsCopy.norm.train.k10
adultsCopy.norm.test.k10<-knn(adultsCopy.norm.train, adultsCopy.norm.test, adultsCopy.norm.train.k10$cluster, k=10)
adultsCopy.norm.test.k10
adultsCopy.norm.train.labels<-adultsCopy.norm.train.k10$cluster
adultsCopy.norm.train.labels

#iclust: IDK how to do this and there wasn't any notes on this
ic.out<-ICLUST(adultsCopy)
out.file <- file.choose(new=TRUE) 
ic.out <- ICLUST(adultsCopy,nclusters =10)  #use all defaults and if possible stop at 10 clusters
ICLUST.graph(ic.out,out.file,title = "IClUST of the data with 10 variable")


#Question 4

