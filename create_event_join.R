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


# ------------- part 1 ----------- #
applabel <- fread("app_labels.csv")                ## 459,943 (~459k) rows & 2 columns.
label_catg <- fread("label_categories.csv")        ## 930 rows & 2 columns. 
applabel$appnum <- as.character(applabel$app_id)
applabel$app_id <- NULL

library(plyr)
# combine tables applabel & label_catg
# now all appid values have a category name to match the label_id
apptotal = join(applabel, label_catg, by = "label_id" )
rm(applabel, label_catg)



# ------------- part 2 ----------- #
appevents <- fread("app_events.csv")               
## 32,473,067 (~32.5 million) rows & 4 columns.

appevents$eventnum <- as.character(appevents$event_id)
appevents$appnum <- as.character(appevents$app_id)
# these 2 steps take about 4 minutes.

appevents$event_id <- NULL
appevents$app_id <- NULL

# combine tables apptotal & appevents.
appdets = join(appevents, apptotal, by = "appnum", match = "first")
# processing takes full 2 minutes.
# 32 million rows & 6 variables.

# removing the basic tables to free up some memory.
rm(appevents,apptotal)



# ------------- part 3 ----------- #
phevent <- fread("events.csv")                     ## 3,252,950 (~3.2 million) rows & 5 columns.
phevent$phid <- as.character(phevent$device_id)
phevent$eventnum <- as.character(phevent$event_id)
phevent$device_id <- NULL
phevent$event_id <- NULL

# combine tables phevent & appdets.
# there are 3.2 million unique eventnum values in phevent.
eventdetails = join( phevent, appdets, by = "eventnum" , match = "first")
# dataframe with xx rows & yy columns.

sapply(eventdetails, function(x) length(unique(x)))
# eventnum    timestamp    longitude     latitude         phid       appnum 
# 3252950       588125         3588         3086        60865         4739 
# is_installed    is_active     label_id     category 
# 2            3          279          260 


write.csv(eventdetails, file = "eventdetails_join.csv", row.names = FALSE)
rm(appdets, phevent)

x1 = fread("eventdetails_join.csv")  # 0.288GB file read in 9 seconds.



# ------------- part 3 ----------- #
phbrand <- fread("phone_brand_device_model.csv")   ## 187,245 (~187k) rows & 3 columns.

# load data
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









# exploring number of unique values for each column.
sapply(appdets, function(x) length(unique(x)))
# output as below:
# appnum    is_installed    is_active     eventnum     label_id     category 
# 19237            1            2         1488096          363          339 


# combine tables mytest & phbrand.
phtrain = join(mytrain, phbrand, by = "phid", type = "left", match = "first" )

# similar for mytrain & phbrand.
phtest = join(mytest, phbrand, by = "phid", type = "left", match = "first" )

# remove


library(plyr)
apptotal = join(applabel, label_catg, by = "label_id" )




tchk = join(mytrain, phbrand, by = "phid", type = "left", match = "first" )
colnames(tchk) = c("trphid", "trdevid", "trgender", "trage", "trgroupx", "trcustgrpx",
                   "brdevid", "br_brand","brmodel")



# testing to see if a testids are present in phone-brand table.
chk1 = subset(phbrand, phid == "-5723359257508971431")

# testing to see if string conversion worked or not.
chktest = subset(mytest, phid == "-5723359257508971431")

colnames(set1) = c("device_id", "gender", "age", "groupx", "phid")
colnames(mytrain) = c("device_id", "gender", "age", "groupx", "phid", "custgrp")


# create subset of mytrain to check whether joins are working properly or not.
set1 = mytrain[1:100,]
join1 = sqldf("select set1.phid as 'devid', 
               set1.gender as 'gender', 
               set1.age as 'age', 
               set1.groupx as 'demog',
               phbrand.phone_brand as 'brand',
               phbrand.device_model as 'model'
               from set1, phbrand
               where set1.phid = phbrand.phid
               ")

brandnames = as.factor(phbrand$phone_brand)   # 131 levels.
devmodels = as.factor(phbrand$device_model)   # 1598 levels.


# creating join for training set.
trainjoin = sqldf("select mytrain.phid as 'devid', 
                  mytrain.gender as 'gender',
                  count(*) as 'count'
                  mytrain.age as 'age', 
                  mytrain.groupx as 'demog',
                  mytrain.custgrp as 'custgrp',
                  phbrand.phone_brand as 'brand',
                  phbrand.device_model as 'model'
                  from mytrain, phbrand
                  where mytrain.phid = phbrand.phid
                  group by mytrain.phid
                  ")


library(plyr)
tchk = join(mytrain, phbrand, by = "phid", type = "left", match = "first" )
colnames(tchk) = c("trphid", "trdevid", "trgender", "trage", "trgroupx", "trcustgrpx",
                   "brdevid", "br_brand","brmodel")

tchk$br_brand = as.factor(tchk$br_brand)
tchk$brmodel = as.factor(tchk$brmodel)


testchk = join(mytest, phbrand, by = "phid", type = "left", match = "first")
colnames(testchk) = c("tphid", "tdevid",  "tcustgrpx",
                   "brdevid", "br_brand","brmodel")

train_idx <- sample(1:nrow(tchk),10000,replace=FALSE)
validn <- tchk[train_idx,] # select all these rows
trainset <- tchk[-train_idx,]


## ===================================================================== ##
## ------- modified code from animal_shelter Kaggle Competition -------- ##
## ===================================================================== ##

# converting sexoutcome to same factor levels for both training
# and test sets
mytrain$custgrp <- factor(mytrain$group, 
                         levels = c("F23-", "F24-26","F27-28","F29-32", "F33-42", "F43+", 
                                    "M22-", "M23-26", "M27-28", "M29-31", "M32-38", "M39+"))

mytest$custgrp <- "M23-26"  # assuming default group is males in the 23-26 group

# expecting 12 classes for the test group for future predictions, 
# even though we have only one value (default) at the moment.
mytest$custgrp <-factor(mytest$custgrp, 
                         levels = c("F23-", "F24-26","F27-28","F29-32", "F33-42", "F43+", 
                                    "M22-", "M23-26", "M27-28", "M29-31", "M32-38", "M39+"))




# --------------------- Section - 2 -------------------- #
# ------------ some descriptive statistics ------------- #
# this section is simply to explore the data, and can be completely ignored.

summary(mytrain)

table(mytrain$AnimalType, mytrain$OutcomeType)
#     Adoption    Died   Euthanasia   Return_to_owner    Transfer
# Cat     4272    147        710          500             5505
# Dog     6497    50        845          4286             3917



# check how many unique values exist for each column.
sapply(mytrain, function(x) length(unique(x)))

sapply(mytest, function(x) length(unique(x)))


# check for NAs - both datasets have 0 NA values, so we do not need
# to apply any corrections.
sapply(mytrain, function(x) sum(is.na(x)))
sapply(mytest, function(x) sum(is.na(x)))

summary(mytrain)
head(mytest) 



# ---------------- end of descriptive statistics ----------------- #




#attach(mytrain)

# --------------------- Section - 4 -------------------- #
# ------- modelling using multinomial regression ------- #

library(nnet)

# multinomial regression as predictive model
mod <- multinom(OutcomeType ~ AnimalType + year + mth , 
                data = mytrain, maxit = 500)
# score = 1.17728


# model 1
mod1 <- multinom(trgroupx ~  brmodel ,
                 data = trainset, maxit = 700)


ctab1 = (table(tchk$br_brand, tchk$trgroupx))
ct2 = as.data.frame(ctab1)


ctes = as.data.frame(table(testchk$br_brand))


# create table to see how if any demographic groups favor a certain device model.
ctemp = subset(ct2, Freq > 0)

tab2 <- as.data.frame(xtabs(~ trgroupx + br_brand, data=tchk) )
head(tab2)

brand_temp = as.data.frame((table(tchk$br_brand)))
#brand_temp$Freq = brand_temp$Freq*100

brtemp = subset(brand_temp, Freq >100)

ggplot(data=mytrain, aes(x=finalage)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~OutcomeType, scales='free')


outvar1 = as.integer(mytrain$OutcomeType)
c1var = cor(outvar1, mytrain$finalage, method = "kendall")


# predict the probabilities "probs" associated with each option 
# and then draw a random number to make our actual selection
opfinal = predict(mod,mytest,"probs")
shelteropfinal = data.frame(opfinal)
submitdf = cbind(mytest$ID, shelteropfinal)

# format column headers to match submission requirements
colnames(submitdf) = c('ID', 'Adoption', 'Died',
                       'Euthanasia', 'Return_to_owner', 'Transfer')

# summary(submitdf[1])  # simply for checking

write.csv(submitdf, file = "finalage.csv", row.names = FALSE)

write.csv(mytrain, file = "anly_train.csv", row.names = FALSE)
write.csv(mytest, file = "anly_test.csv", row.names = FALSE)





# -------------------- Section - 1 ------------------- #
# ---------------- Formatting animal color ----------- #
# future exploration

animColor = data.frame(mytrain$Color)
colnames(animColor) = c('Color')


animColor$posblk = regexpr('Black', animColor$Color)
# if pattern is absent, pos = -1
# ~30% are black variations

animColor$poswhite = regexpr('White', animColor$Color) 
# ~45.5% are white variations

animColor$posbrn = regexpr('Brown', animColor$Color) 
# ~25% are brown variations

animColor$col2 = animColor$posblk + animColor$poswhite + animColor$posbrn
# only 7352 animals are not variations of White, Black or Brown.
# Hence 72.49% animals fall in these 3 color catgories.



# color patterns: White > Black > Brown
# black   brown   white   stdcolor
# N       N       N         Other
# Y       Y/N     N         Black
# N       Y       N         Brown
# all others = White

# these rules are implemented as below:
animColor$stdcolor = "White"
animColor$stdcolor[animColor$col2 == -3] = "Other" 
animColor$stdcolor[animColor$posblk == -1 & animColor$posbrn > 0
                   & animColor$poswhite == -1] = "Brown"
animColor$stdcolor[animColor$posblk > 0 & animColor$posbrn == -1
                   & animColor$poswhite == -1] = "Black"
animColor$stdcolor[animColor$posblk > 0 & animColor$posbrn > 0
                   & animColor$poswhite == -1] = "Black"
mytrain$stdcolor = animColor$stdcolor



# color patterns for the test dataset
animColtest = data.frame(mytest$Color)
colnames(animColtest) = c('Color')


animColtest$posblk = regexpr('Black', animColtest$Color)
# if pattern is absent, pos = -1
# ~30% are black variations

animColtest$poswhite = regexpr('White', animColtest$Color) 
# ~45% are white variations

animColtest$posbrn = regexpr('Brown', animColtest$Color) 
# ~24.8% are brown variations

animColtest$col2 = animColtest$posblk + animColtest$poswhite + animColtest$posbrn
# only 3155 animals are not variations of White, Black or Brown.
# Hence 72.46% animals fall in these 3 color catgories.





# these rules are implemented as below:
animColtest$stdcolor = "White"
animColtest$stdcolor[animColtest$col2 == -3] = "Other" 
animColtest$stdcolor[animColtest$posblk == -1 & animColtest$posbrn > 0
                   & animColtest$poswhite == -1] = "Brown"
animColtest$stdcolor[animColtest$posblk > 0 & animColtest$posbrn == -1
                   & animColtest$poswhite == -1] = "Black"
animColtest$stdcolor[animColtest$posblk > 0 & animColtest$posbrn > 0
                   & animColtest$poswhite == -1] = "Black"
mytest$stdcolor = animColtest$stdcolor









# -------------------- Section - 2b ------------------- #
# ---------------- Foramtting breedtype ----------- #
animbreed = data.frame(mytrain$Breed)
colnames(animbreed) = c('Breed')

animbreed$shair = regexpr('Shorthair', animbreed$Breed) #42.7%
animbreed$retr = regexpr('Retriever', animbreed$Breed) # 9.1%
animbreed$shep = regexpr('Shepherd', animbreed$Breed) #5.1%
animbreed$terr = regexpr('Terrier', animbreed$Breed) #6%
animbreed$pitb = regexpr('Pit Bull', animbreed$Breed) # 8.9% 


animbreed$br = animbreed$shair + animbreed$chihu + animbreed$retr +
                 animbreed$terr + animbreed$shep

animbreed$brtyp = "Shorthair"
animbreed$brtyp[animbreed$br == -6] = "Others"
animbreed$brtyp[animbreed$shair == -1  
                & animbreed$retr > 0] = "Retriever"
animbreed$brtyp[animbreed$shair == -1 
                & animbreed$retr == -1 & animbreed$pitb > 0] = "Pitbull"


animbreed$brtyp[animbreed$shair == -1 
                & animbreed$retr == -1 & animbreed$pitb == -1
                & animbreed$terr > 0] = "Terrier"

animbreed$brtyp[animbreed$shair == -1 
                & animbreed$retr == -1 & animbreed$pitb == -1
                & animbreed$terr == -1 & animbreed$shep > 0] = "Shepherd"


mytrain$brtyp = animbreed$brtyp
                



# similarly for mytest

animbrtest = data.frame(mytest$Breed)
colnames(animbrtest) = c('Breed')

animbrtest$shair = regexpr('Shorthair', animbrtest$Breed) #42.5%
animbrtest$retr = regexpr('Retriever', animbrtest$Breed) # 9.3%
animbrtest$shep = regexpr('Shepherd', animbrtest$Breed) #5.1%
animbrtest$terr = regexpr('Terrier', animbrtest$Breed) #6.4%
animbrtest$pitb = regexpr('Pit Bull', animbrtest$Breed) # 9.2% 


animbrtest$br = animbrtest$shair + animbrtest$retr +
  animbrtest$pitb + animbrtest$terr + animbrtest$shep

animbrtest$brtyp = "Shorthair"
animbrtest$brtyp[animbrtest$br == -6] = "Others"
animbrtest$brtyp[animbrtest$shair == -1 
                & animbrtest$retr > 0] = "Retriever"
animbrtest$brtyp[animbrtest$shair == -1 
                & animbrtest$retr == -1 & animbrtest$pitb > 0] = "Pitbull"


animbrtest$brtyp[animbrtest$shair == -1 
                & animbrtest$retr == -1 & animbrtest$pitb == -1
                & animbrtest$terr > 0] = "Terrier"

animbrtest$brtyp[animbrtest$shair == -1 
                & animbrtest$retr == -1 & animbrtest$pitb == -1
                & animbrtest$terr == -1 & animbrtest$shep > 0] = "Shepherd"


mytest$brtyp = animbrtest$brtyp





# -------------------- Section - 1 ------------------- #
# ---------------- Prep work and load data ----------- #
anage = data.frame(mytrain$AgeuponOutcome)
colnames(anage) = c('Age')


anage$yr = anage$Age
anage$factor1 = 99 # mutiplication factor
anage$factoryrs = 0 # search for 'years'
anage$factoryr = 0 # search for 'year'
anage$factormths = 0 # search for 'months'
anage$factormth = 0 # search for 'months'
anage$factorwks = 0 # search for 'weeks'
anage$factorwk = 0 # search for 'week'
anage$factordys = 0 # search for 'days'
anage$factordy = 0 # search for 'day'
anage$finage = 0


anage$factoryrs = regexpr('years', anage$Age) # look for the word 'years'
anage$factor1[anage$factoryrs > 0 ] = 1 # assign mulyiplying factor = 1
anage$yr = gsub('years', '', anage$yr) # delete the word 'years'

anage$factoryr = regexpr('year', anage$Age) # look for the word 'year'
anage$factor1[anage$factoryr > 0 ] = 1 # assign mulyiplying factor = 1
anage$yr = gsub('year', '', anage$yr) # delete the word 'years'


anage$factormths = regexpr('months', anage$Age) # look for the word 'years'
anage$factor1[anage$factormths > 0 ] = (0.0833) # assign mulyiplying factor = 1
anage$yr = gsub('months', '', anage$yr) # delete the word 'years'

anage$factormth = regexpr('month', anage$Age) # look for the word 'year'
anage$factor1[anage$factormth > 0 ] = 0.0833 # assign mulyiplying factor = 1
anage$yr = gsub('month', '', anage$yr) # delete the word 'years'


anage$factorwks = regexpr('weeks', anage$Age) # look for the word 'years'
anage$factor1[anage$factorwks > 0 ] = (0.0192) # assign mulyiplying factor = 1
anage$yr = gsub('weeks', '', anage$yr) # delete the word 'years'

anage$factorwk = regexpr('week', anage$Age) # look for the word 'years'
anage$factor1[anage$factorwk > 0 ] = (0.0192) # assign mulyiplying factor = 1
anage$yr = gsub('week', '', anage$yr) # delete the word 'years'

anage$factordys = regexpr('days', anage$Age) # look for the word 'years'
anage$factor1[anage$factordys > 0 ] = (0.0027) # assign mulyiplying factor = 1
anage$yr = gsub('days', '', anage$yr) # delete the word 'years'

anage$factordy = regexpr('day', anage$Age) # look for the word 'years'
anage$factor1[anage$factordy > 0 ] = (0.0027) # assign mulyiplying factor = 1
anage$yr = gsub('day', '', anage$yr) # delete the word 'years'


anage$yr = as.integer(anage$yr)
anage$finage = anage$yr*anage$factor1

mytrain$finalage = anage$finage





# similarly, calculate age for mytest
anagetest = data.frame(mytest$AgeuponOutcome)
colnames(anagetest) = c('Age')


anagetest$yr = anagetest$Age
anagetest$factor1 = 99 # mutiplication factor
anagetest$factoryrs = 0 # search for 'years'
anagetest$factoryr = 0 # search for 'year'
anagetest$factormths = 0 # search for 'months'
anagetest$factormth = 0 # search for 'months'
anagetest$factorwks = 0 # search for 'weeks'
anagetest$factorwk = 0 # search for 'week'
anagetest$factordys = 0 # search for 'days'
anagetest$factordy = 0 # search for 'day'
anagetest$finage = 0


anagetest$factoryrs = regexpr('years', anagetest$Age) # look for the word 'years'
anagetest$factor1[anagetest$factoryrs > 0 ] = 1 # assign mulyiplying factor = 1
anagetest$yr = gsub('years', '', anagetest$yr) # delete the word 'years'

anagetest$factoryr = regexpr('year', anagetest$Age) # look for the word 'year'
anagetest$factor1[anagetest$factoryr > 0 ] = 1 # assign mulyiplying factor = 1
anagetest$yr = gsub('year', '', anagetest$yr) # delete the word 'years'


anagetest$factormths = regexpr('months', anagetest$Age) # look for the word 'years'
anagetest$factor1[anagetest$factormths > 0 ] = (0.0833) # assign mulyiplying factor = 1
anagetest$yr = gsub('months', '', anagetest$yr) # delete the word 'years'

anagetest$factormth = regexpr('month', anagetest$Age) # look for the word 'year'
anagetest$factor1[anagetest$factormth > 0 ] = 0.0833 # assign mulyiplying factor = 1
anagetest$yr = gsub('month', '', anagetest$yr) # delete the word 'years'


anagetest$factorwks = regexpr('weeks', anagetest$Age) # look for the word 'years'
anagetest$factor1[anagetest$factorwks > 0 ] = (0.0192) # assign mulyiplying factor = 1
anagetest$yr = gsub('weeks', '', anagetest$yr) # delete the word 'years'

anagetest$factorwk = regexpr('week', anagetest$Age) # look for the word 'years'
anagetest$factor1[anagetest$factorwk > 0 ] = (0.0192) # assign mulyiplying factor = 1
anagetest$yr = gsub('week', '', anagetest$yr) # delete the word 'years'

anagetest$factordys = regexpr('days', anagetest$Age) # look for the word 'years'
anagetest$factor1[anagetest$factordys > 0 ] = (0.0027) # assign mulyiplying factor = 1
anagetest$yr = gsub('days', '', anagetest$yr) # delete the word 'years'

anagetest$factordy = regexpr('day', anagetest$Age) # look for the word 'years'
anagetest$factor1[anagetest$factordy > 0 ] = (0.0027) # assign mulyiplying factor = 1
anagetest$yr = gsub('day', '', anagetest$yr) # delete the word 'years'


anagetest$yr = as.integer(anagetest$yr)
anagetest$finage = anagetest$yr*anagetest$factor1

mytest$finalage = anagetest$finage







mcorrdf = mytrain
mcorrdf$OutcomeType = as.integer(mcorrdf$OutcomeType)
mcorrdf$AnimalType = as.integer(mcorrdf$AnimalType)
mcorrdf$gender = as.integer(mcorrdf$gender)
#mcorrdf$brtyp = as.factor(mytrain$brtyp)
library(corrgram)
corrgram(mytrain)








# --------------------- Section - 3 -------------------- #
# ------------- moving this section to the end --------- #
# ------------------ some pretty graphs ---------------- #

# chart 1 -- crime category by day of week 
# image stored as CrimeCategory_by_DayOfWeek.jpg
ggplot(data=mytrain, aes(x=AnimalType)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~OutcomeType)

# chart -- crime category by day of year 
# image file = Crimecount_byCategory&Yr.jpg
# note this image has a fixed scale, unlike the previous plot.
ggplot(data=mytrain, aes(x=AnimalType)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') +
  facet_wrap(~OutcomeType)

ggplot(data=mytrain, aes(x=OutcomeType)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') + 
  facet_wrap(~year)


ggplot(data=mytrain, aes(x=OutcomeType)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') + 
  facet_wrap(~AnimalType)

# ---------------- end of graphs ----------------- #




