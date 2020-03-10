#set up the data: read the data into a table, set the variable names and assign them to the data
adults <- read.table("data/adult.data",stringsAsFactors = FALSE)
adultnames <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", "occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","income")
length(adultnames)
adultnames

names(adults) <- adultnames
names(adults)
adults

