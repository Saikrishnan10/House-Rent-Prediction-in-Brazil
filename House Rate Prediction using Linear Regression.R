#Predict Rent amount of houses

#Installing and Loading the common library functions
install.packages("dplyr")
install.packages("caTools") 
install.packages("GGally") 
install.packages("ggplot2")
install.packages("ggiraph")
install.packages("ggiraphExtra")
install.packages("plyr")
install.packages("randomForest")
install.packages("caret")
install.packages("GGally")
install.packages("DescTools")
install.packages("e1071")
install.packages("ROCR")
install.packages("ca")
install.packages("igraph")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("tidyverse")  
install.packages("cluster")  
install.packages("factoextra") 
install.packages("class")
install.packages("DT")
install.packages("stringr")
install.packages("tidyr")
install.packages("cowplot")
install.packages("rpart")
install.packages("rpart.plot")

library(dplyr)
library(caTools) 
library(GGally) 
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)
library(plyr)
library(randomForest)
library(caret)
library(GGally)
library(DescTools)
library(e1071)
library(ROCR)
library(ca)
library(igraph)
library(tidyverse)
library(magrittr)
library(tidyverse)  
library(cluster)  
library(factoextra) 
library(class)
library(DT)
library(stringr)
library(tidyr)
library(cowplot)
library(rpart)
library(rpart.plot)



#Data selection

brazil_houses<-read.csv("C:/Users/mural/Desktop/DMML/House prediction Brazil/houses_to_rent_v2.csv")
head(brazil_houses)
colnames(brazil_houses)

#Data Cleaning and pre-processing

# 1) Column name and value changes

names(brazil_houses)<-c("city","area","rooms","bathroom","Parking Spaces","floor", "Pet's Allowed","furnish type","Homeowner's Association Fee","property Tax", "Fire Insuarance","Rent","Total Rent")
colnames(brazil_houses)
summary(brazil_houses)


unique(brazil_houses$city)


(brazil_houses$city[brazil_houses$city=="São Paulo"]<-"S?o Paulo")
(brazil_houses$`Pet's Allowed`[brazil_houses$`Pet's Allowed`=="acept"]<-"allowed")
(brazil_houses$`Pet's Allowed`[brazil_houses$`Pet's Allowed`=="not acept"]<-"not allowed")
(brazil_houses$floor[brazil_houses$floor=="-"]<-0)
brazil_houses$`Total Rent`<-NULL

head(brazil_houses)





#Checking na data

sum(is.na(brazil_houses))


#Data Transformation

str(brazil_houses)


# Factor Conversion

colnames(brazil_houses)

brazil_houses$city<-as.factor(brazil_houses$city)

brazil_houses$city

brazil_houses$rooms<-as.factor(brazil_houses$rooms)
brazil_houses$bathroom<-as.factor(brazil_houses$bathroom)
brazil_houses$floor<-as.numeric(brazil_houses$floor)
brazil_houses$floor[brazil_houses$floor>10] <- 0
brazil_houses$floor<-as.factor(brazil_houses$floor)
brazil_houses$`Pet's Allowed`<-as.factor(brazil_houses$`Pet's Allowed`)
levels(brazil_houses$`Pet's Allowed`<-factor(c('allowed'=1,'not allowed'=0)))

brazil_houses$`furnish type` <-as.factor(brazil_houses$`furnish type`)
levels(brazil_houses$`furnish type` <-factor(c('furnished'=1,'not furnished'=0)))


brazil_houses$`Parking Spaces` <-as.factor(brazil_houses$`Parking Spaces`)
brazil_houses$`Parking Spaces`

str(brazil_houses)

#Data Mining

# Data Splitting

set.seed(500)

house_split<-sample.split(brazil_houses$Rent, SplitRatio = 2/3)
housetraining_set<- subset(brazil_houses,house_split == TRUE)
housetesting_set<- subset(brazil_houses,house_split == FALSE)


# 3) Dependent and Independent variable relationship check
colnames(housetraining_set)
# 3.1) Continuous variable data frame 
brazil_cont_train<-data.frame(housetraining_set$area,housetraining_set$`Homeowner's Association Fee`,housetraining_set$`property Tax`,housetraining_set$`Fire Insuarance`,housetraining_set$Rent)
colnames(brazil_cont_train)


# 3.2) Categorical variable data frame
brazil_cat_train<-data.frame(housetraining_set$city,housetraining_set$rooms,housetraining_set$floor,housetraining_set$bathroom,housetraining_set$`Parking Spaces`,housetraining_set$`Pet's Allowed`,housetraining_set$`furnish type`)
colnames(brazil_cat_train)

# 3.3) Correlation with Continuous variables and dependent variable
cor_brazil_cont<-cor(brazil_cont_train)
rownames(cor_brazil_cont)<-c("Area","Owner Association-Fee","Property-Tax","Fire-Insurance","Rent")
colnames(cor_brazil_cont)<-c("Area","Owner Association-Fee","Property-Tax","Fire-Insurance","Rent")


#Heatmap for continuous variable correlation
heatmap(cor_brazil_cont,col=heat.colors(3))+legend(x="left", legend=c("min", "med", "max"),fill=heat.colors(3))


# 3.4) T-TEST on binary categorical variables and dependent variable
T_pet<-t.test(housetraining_set$Rent~housetraining_set$`Pet's Allowed`)
T_furnish<-t.test(housetraining_set$Rent~housetraining_set$`furnish type`)
T_pet
T_furnish



# 3.4) Anova on categorical variables with more than 2 levels and dependent variable
anova_city<-aov(housetraining_set$Rent~housetraining_set$city)
summary(anova_city)
anova_rooms<-aov(housetraining_set$Rent~housetraining_set$rooms)
summary(anova_rooms)
anova_floor<-aov(housetraining_set$Rent~housetraining_set$floor)
summary(anova_floor)
anova_bath<-aov(housetraining_set$Rent~housetraining_set$bathroom)
summary(anova_bath)
anova_parking<-aov(housetraining_set$Rent~housetraining_set$`Parking Spaces`)
summary(anova_parking)


#Pair-plot
pairlist(cor_brazil_cont)
ggpairs(as.data.frame(cor_brazil_cont),aes(col =  "red"),columnLabels  = c("Area","Owner Association-Fee","Property-Tax","Fire-Ins","Rent"))
colnames(brazil_cat_train)

#Categorical variable representation
boxplot(brazil_cat_train,col="brown",names = c("City","Rooms","Floors","Bathroom","Parking","Pet's allowed","Furnishing"))

# 5) Linear regression with independent variables with training data

colnames(housetraining_set)

#Linear regression Model

Brazil_House_model<-lm(Rent~city+rooms+floor+bathroom+`Parking Spaces`+`Fire Insuarance`,data = housetraining_set)
summary(Brazil_House_model)


predicted_linear<-predict(Brazil_House_model)
actual_linear<-housetraining_set$Rent
compare_linear<-data.frame(actual_linear,predicted_linear)
value_linear<-mean(abs(actual_linear-predicted_linear)/actual_linear)
value_linear
accuracy_linear<-1-value_linear

#Accuracy of the linear model
accuracy_linear


# 6) Random Forest with independent and dependent variable Rent

#Loading library for random forest classification

set.seed(1000)
housetraining_set
random_model_1<-randomForest(housetraining_set$Rent~housetraining_set$city+housetraining_set$rooms+housetraining_set$bathroom+housetraining_set$`Parking Spaces`+housetraining_set$`Fire Insuarance`)
random_model<-randomForest(housetraining_set$Rent~housetraining_set$city+housetraining_set$rooms+housetraining_set$bathroom+housetraining_set$`Parking Spaces`+housetraining_set$`Fire Insuarance`+housetraining_set$`Homeowner's Association Fee`)

#Prediction using random forest
predicted_random<-predict(random_model)
predicted_random
actual_random<-housetraining_set$Rent
actual_random
compare_random<-data.frame(actual_random,predicted_random)
compare_random
value_random<-mean((abs(compare_random$actual_random-compare_random$predicted_random)/compare_random$actual_random),na.rm = TRUE)
value_random

accuracy_random<-1-value_random
accuracy_random
compare_random


#Evaluation Metrics:

#Evaluating the model by applying in testing data for Multiple Linear regression
Brazil_House_model_test<-lm(Rent~city+rooms++floor+bathroom+`Parking Spaces`+`Fire Insuarance`,data = housetesting_set)
summary(Brazil_House_model_test)

predicted_linear_test<-predict(Brazil_House_model_test)
actual_linear_test<-housetesting_set$Rent
compare_linear_test<-data.frame(actual_linear_test,predicted_linear_test)
value_linear_test<-mean(abs(actual_linear_test-predicted_linear_test)/actual_linear_test)
value_linear_test
accuracy_linear_test<-1-value_linear_test

#Accuracy % of the linear regression for training vs testing data
accuracy_linear
accuracy_linear_test


#----------------------------------------------------------------------------------------------------------------------------------------
#Evaluating the model by applying in testing data for Random Forest regression

random_model_test<-randomForest(housetesting_set$Rent~housetesting_set$city+housetesting_set$rooms+housetesting_set$bathroom+housetesting_set$`Parking Spaces`+housetesting_set$`Fire Insuarance`+housetesting_set$`Homeowner's Association Fee`)

#Prediction using random forest
predicted_random_test<-predict(random_model_test)
predicted_random_test
actual_random_test<-housetesting_set$Rent
actual_random_test
compare_random_test<-data.frame(actual_random_test,predicted_random_test)
compare_random_test
value_random_test<-mean((abs(compare_random$actual_random-compare_random$predicted_random)/compare_random$actual_random),na.rm = TRUE)
value_random_test


accuracy_random_test<-1-value_random_test
accuracy_random_test
compare_random_test


#Tuning the random forest for testing data

mtry <- tuneRF(housetesting_set[-1],housetesting_set$Rent, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)


set.seed(100)
rf_test <-randomForest(housetesting_set$Rent~housetesting_set$city+housetesting_set$rooms+housetesting_set$bathroom+housetesting_set$`Parking Spaces`+housetesting_set$`Fire Insuarance`+housetesting_set$`Homeowner's Association Fee`,ntree=500,mtry=best.m,importance=TRUE)
print(rf_test)

#Prediction using tuned random forest model
predicted_random_tuned<-predict(rf_test)
predicted_random_tuned
actual_random_tuned<-housetesting_set$Rent
actual_random_tuned
compare_random_tuned<-data.frame(actual_random_tuned,predicted_random_tuned)
compare_random_tuned
value_random_tuned<-mean((abs(compare_random_tuned$actual_random_tuned-compare_random_tuned$predicted_random)/compare_random_tuned$actual_random_tuned),na.rm = TRUE)
value_random_tuned

accuracy_random_tuned<-1-value_random_tuned
accuracy_random_tuned

#Evaluate variable importance
importance(random_model_test)
varImpPlot(random_model_test)



#Accuracy % of the random forest regression for training vs testing data
accuracy_random
accuracy_random_test
accuracy_random_tuned


