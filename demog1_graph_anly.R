# Author - Anupama Rajaram



# -------------------- Section - 1 ------------------- #
# ---------------- Prep work and load data ----------- #
# To clean up the memory of your current R session run the following line
source("C:/anu/wksp_prep.R")
# load basic libraries.  


options(digits=16) # numeric values will have 7 decimal digits.
options(scipen=999) # probabilities will be in numeric notation, NOT scientific!
# e.g: p = 0.000043 NOT 4.3e-05




# read derived files for training and test datasets.
# which is combination of the following files:
# events.csv, app_events.csv, app_labels.csv, label_categories.csv
# not using the option " stringsAsFactors = TRUE"
mytest <- fread("testevents.csv", stringsAsFactors = TRUE)
mytrain<- fread("trainevents.csv", stringsAsFactors = TRUE)

# create an agegroup column for both test & training datasets:
# sub('.', '', listfruit)
mytrain$agebin <- sub('.', '', mytrain$group)



# creating a combination set to get unique values for 3 columns, 
# from both training and test sets.
px = subset(mytrain, select = c("phone_brand", "device_model", "category"))
py = subset(mytest, select = c("phone_brand", "device_model", "category"))
ptot = rbind( px,  py)

# phb1 = data.frame(unique(mytrain$phone_brand))
# p1 = append(mytrain$phone_brand, mytest$phone_brand, after = length(mytrain))
# phb1 = append()

# collect factor values from original training set.
phb1 = sqldf("select distinct phone_brand as 'brand' 
             from ptot")
phdev = sqldf("select distinct device_model as 'model' 
             from ptot")
appcatg = sqldf("select distinct category from ptot")


# remove unnecessary variables:
rm(ptot, px, py)


train_idx <- sample(1:nrow(mytrain),10000,replace=FALSE)
validn <- mytrain[train_idx,] # select all these rows
train <- mytrain[-train_idx,] # select all but these rows


# apply factor levels to the new_training, validation and test datasets.
validn$phone_brand = factor(validn$phone_brand, levels = phb1$brand)
train$phone_brand = factor(train$phone_brand, levels = phb1$brand)
mytest$phone_brand = factor(mytest$phone_brand, levels = phb1$brand)

validn$device_model = factor(validn$device_model, levels = phdev$model)
train$device_model = factor(train$device_model, levels = phdev$model)
mytest$device_model = factor(mytest$device_model, levels = phdev$model)

validn$category = factor(validn$category, levels = appcatg$category)
train$category = factor(train$category, levels = appcatg$category)
mytest$category = factor(mytest$category, levels = appcatg$category)


# library (nnet)
# b1 <- nnet(age ~ phone_brand + device_model, data = train, 
#            size=4, decay=0.001, maxit=500)
# 
# x = lm(age ~ phone_brand + device_model + category, data = train)
# pred1 = predict(x, validn, type = class)
# table(pred1, validn$age)


# =========================================================== #
# ============ Section 1 - Descriptive Statistics =========== #


# some descriptive statistics
tgen <- as.data.frame(table(mytrain$phone_brand, mytrain$group ))
tgen2 <- subset(tgen, Freq > 5)
# the top 5 brands account for ~75% of the populace.
# å°ç±³ , ä¸‰æ˜Ÿ, åŽä¸º, OPPO, vivo

tyoungm = subset(mytrain, group == "M23-26")
youngphb <- as.data.frame(table(tyoungm$phone_brand , tyoungm$device_model))
youngphb <- subset(youngphb, Freq > 0)

phsub <- as.data.frame(table(mytrain$phone_brand, mytrain$device_model, mytrain$group))
phsub <- subset(phsub, Freq > 0)

phsubp <- as.data.frame(table( mytrain$device_model, mytrain$group))
phsubp <- subset(phsubp, Freq > 0)


phgen <- as.data.frame(table(mytrain$phone_brand, mytrain$gender))

phgen2 <- subset(phgen, phgen$Freq > 1)


# looking for NAs
sapply(mytrain, function(x) sum(is.na(x)))

# check for unique values:
sapply(mytrain, function(x) length(unique(x)))


tdemog <- as.data.frame(table(mytrain$phone_brand, mytrain$group ))
colnames(tdemog)<- c("brand", "category", "freq")
# tdemog2 <- subset(tdemog, which(brand == "å°ç±³" | brand == "ä¸‰æ˜Ÿ" | 
#                              brand == "åŽä¸º" | brand == "OPPO" | 
#                              brand == "vivo" | brand == "è”æƒ³" |   
#                                brand == "é…·æ´¾"))
# # the top 5 brands account for ~75% of the populace.

x = lm(age ~ phone_brand + device_model + category, data = mytrain)




# =========================================================== #
# ============ Section 1 - Interactive Leaflet ============== #


validn$info = paste("Phone ID: ", "<br>",
                   validn[['phid']],  ", ", "<br>",
                   "Demographic = ", validn[['group']], "<br>", 
                   "Phone Brand = ", validn[['phone_brand']], 
                   sep='')

validnmap = subset(validn, validn$latitude > 1)


# create map to show univ details
map_demog <- leaflet(validn) %>% 
  setView(100.1, 34.19, zoom = 4) %>%
  addTiles() %>%
  addMarkers(108.87, 34.19, popup = "Manning Park, Canada") %>% 
  addMarkers(114.27, 30.60, popup = "M23-26, OPPO") 
map_demog


map_demogv <- leaflet(validnmap) %>% 
  setView(110.1, 34.19, zoom = 4) %>%
  addTiles() %>%
  addMarkers(~validnmap$longitude, ~validnmap$latitude, popup = ~validnmap$info,
             clusterOptions = markerClusterOptions())
  
map_demogv





# =========================================================== #
# ============ Section 1 - Graphical Exploration============= #


px = ggplot(data = mytrain, aes(x = phone_brand, fill = group)) +
  geom_bar(position = "dodge")
ggplotly(px)


hist(mytrain$age, breaks = 50)  #histogram of age distribution
# 26-30 has highest population.

pz = ggplot(data = mytrain, aes(x = phone_brand, fill = agebin))  +
  geom_bar(position = "dodge")
ggplotly(pz)
# the top 5 brands are:  å°ç±³ , ä¸‰æ˜Ÿ, åŽä¸º, OPPO, vivo


pz = ggplot(data = mytrain, aes(x = agebin, fill = gender))  +
  geom_bar(position = "dodge")
ggplotly(pz)
# men are distributed in the 23-26 and 32-38 age groups. 
# women are almost evenly distributed.


pz = ggplot(data = mytrain, aes(x = category, fill = gender))  +
  geom_bar(position = "dodge")
ggplotly(pz)


pxy = ggplot(mytrain, aes(category)) + geom_bar() +
  facet_wrap(~ gender)
ggplotly(pxy)
# top 5 categories are the same. predominantly NA


pxy = ggplot(mytrain, aes(device_model)) + geom_bar() +
  facet_wrap(~ gender)
ggplotly(pxy)


library(plyr)
ct1 = data.frame(table(mytrain$phone_brand, mytrain$age))
ct2 = subset(ct1, Freq > 10)