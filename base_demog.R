# Author - Anupama Rajaram



# -------------------- Section - 1 ------------------- #
# ---------------- Prep work and load data ----------- #
# To clean up the memory of your current R session run the following line
source("C:/anu/wksp_prep.R")
# load basic libraries and salary dataset.  
# remove sal data
rm(saldata)


options(digits=16) # numeric values will have 7 decimal digits.
options(scipen=999) # probabilities will be in numeric notation, NOT scientific!
# e.g: p = 0.000043 NOT 4.3e-05

library(bit64)
library(plyr)


# read eventdetails file, which is combination of events.csv, app_events.csv,
# app_labels.csv and label_categories.csv
eventdets = fread("eventdetails_join.csv")

phbrand <- fread("phone_brand_device_model.csv")   
## 187,245 (~187k) rows & 3 columns.

# load training and test data
mytrain <- fread("gender_age_train.csv")
mytest <- fread("gender_age_test.csv")



# the long integer garbles storage. hence convert all device_id to string.
phbrand$phid <- as.character(phbrand$device_id)
mytest$phid <- as.character(mytest$device_id)
mytrain$phid <- as.character(mytrain$device_id)

# hence device_id is no longer needed.
phbrand$device_id <- NULL
mytest$device_id <- NULL
mytrain$device_id <- NULL 

# similar string conversion for event_id and app_id
#phevent$eventnum <- as.character(phevent$event_id)
#applabel$appnum <- as.character(applabel$app_id)


# test check to see if merge works.
# It does! :)
# h1 = mytest[1:50,]
# hmerge = join(h1, phbrand, by = "phid", type = "left", match = "first")

mytest$x = 0 # dummy variable, else the join does not work!
testevents = join(mytest, phbrand, by = "phid", type = "left", match = "first")
mytest$x = NULL
testevents$x = NULL
trainevents = join(mytrain, phbrand, by = "phid", type = "left", match = "first")


traineventdets =  join(trainevents, eventdets, by = "phid", type = "left", match = "first")
rm(mytrain,trainevents)     ## removing unnecessary tables to free memory.

testeventdets =  join(testevents, eventdets, by = "phid", type = "left", match = "first")
rm(mytest,testevents)     ## removing unnecessary tables to free memory.

rm(eventdets)


# writing the two merged dataframes as csv files.
write.csv(testeventdets, file = "testevents.csv", row.names = FALSE)
write.csv(traineventdets, file = "trainevents.csv", row.names = FALSE)




# some descriptive statistics
tgen <- as.data.frame(table(traineventdets$phone_brand, traineventdets$gender ))
tgen2 <- subset(tgen, Freq > 5)


tgen <- as.data.frame(table(traineventdets$phone_brand, traineventdets$gender ))
tgen2 <- subset(tgen, Freq > 5)


# the top 5 brands account for ~75% of the populace.
# å°ç±³ , ä¸‰æ˜Ÿ, åŽä¸º, OPPO, vivo


