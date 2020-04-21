#Loading the dataset
df <- read.csv("C:/Users/sayal/Downloads/Final_Data.csv")
View(df)
names(df)
summary(df)

names(df)[1]= 'CanineGroup'

#Converting CanioneGroup to factor variable
df$CanineGroup <- as.factor(df$CanineGroup)

#Printing the levels of Canine Group
levels(df$CanineGroup)

#Changing Gender to Factor variable
df$Gender <- as.factor(df$Gender)

#Printing the levels of Gender to Console
levels(df$Gender)

#Checking different groups for Canine
levels(df$CanineGroup)

#Question1

#Assigning different colors as per CanineGroup
my_cols <- c("#FF0000", "#0000FF","#228B22","#00FF00","#A9A9A9")
#Printing Scatterplot for X1 to X9
pairs(df[,c(2:10)], main='Draftsman plot' ,pch = c(1,16,9,12,14)[as.numeric(df$CanineGroup)], cex = 0.5, col = my_cols[df$CanineGroup], )
legend(-0.003,1.07,c("Cuons","GoldenJackal","IndianWolves","ModernDog","ThaiDogs"),pch=c(1,16,9,12,14), cex=0.7, text.font=2)


#Question2

#Calculating Distance Matrix

dist.df <- dist(df[,c(2:10)],method='euclidean')
dist.df

#Question3:

#Applying PCA function on the dataset
pca.df <- prcomp(df[,c(2:10)], scale=TRUE)

#Printing the results of pca to console
pca.df
#Printing the summary of the pca to console
summary(pca.df)

#If we look at PCA summary we get 92% variance in the first 3 columns. Thus, we can use these 3 variables instead of X1 to X9

#Plotting PCA

library(factoextra)
#Printing the Scree plot of PCA
fviz_eig(pca.df)


#Question 4.

#Changing levels of Gender to 0,1,2 
levels(df$Gender) <- c(0,1,2) 
#Changing Gender from ctegorical to numerical
df$Gender <- as.numeric(df$Gender)
#Creating matrix with scaled values
matstd.df <- scale(df[,2:11])
#Applying Kmeans for predicting 5 groups with 5 random points as starting points
(kmeans5.df <- kmeans(matstd.df,5,nstart = 5))
kmeans5.df$cluster
#Applying Kmeans for predicting 5 groups with 10 random points as starting points
(kmeans10.df <- kmeans(matstd.df,5,nstart = 10))
kmeans10.df$cluster
#Applying Kmeans for predicting 5 groups with 15 random points as starting points
(kmeans15.df <- kmeans(matstd.df,5,nstart = 15))
kmeans15.df$cluster

#If we look at the above results we can see that for maximum number of times we get cluster 1 with cluster 5 for datapoints so we can conclude that IndianWolves are related to Modern Dogs


#Converting gender back to factor variable
df$Gender <- as.factor(df$Gender)

#Question 5
library(psych)
vss(df[,-c(1,11)]) # See factor recommendation from Vss the recommended factor is 2
pc <- principal(df[,-c(1,11)], nfactors=2, rotate="varimax")
summary(pc)
round(pc$values, 3)
pc$loadings 
#From the loadings we can see that upto 2 RC factors explain about 86% of the variance.

pc$communality

#Plotting the EFA Plot
fa.diagram(pc) 

#Question 6- Discriminant Function Analysis 

library(MASS)
library(klaR)
library(dplyr)
new_data <- df[,-11]
sample_n(new_data, 10)
training_sample <- sample(c(TRUE, FALSE), nrow(new_data), replace = T, prob =
                            c(0.8,0.2))
train <- new_data[training_sample, ]
test <- new_data[!training_sample, ]
lda.new_data <- lda(CanineGroup ~ ., train)
lda.new_data #show results
plot(lda.new_data, col = as.integer(train$CanineGroup))
lda.train <- predict(lda.new_data)
train$lda <- lda.train$class
table(train$lda,train$CanineGroup)
lda.test <- predict(lda.new_data,test)
test$lda <- lda.test$class
table(test$lda,test$CanineGroup)


#Question7: logistic regression for each Canine group

#For Cuons

#Extracting dfata for cuons
df.cuons <- df[df$CanineGroup=='Cuons',]
df.cuons <- df.cuons[,-1]
#Changing the levels to 0&1 i.e Females and Males
levels(df.cuons$Gender) <- c(0,1,1)

#Applying Logistic regression to with all the variables for Cuons
fit.cuons <- glm(Gender~.,data=df.cuons,family = 'binomial')
#Applying stepwise regression to find best variables
final.fit.cuons <- step(fit.cuons)

#For cuons we get X3,X5,X6,X9 as most contributing variables to predict gender

#For ModernDog

#Extracting dfata for cuons
df.moderndog <- df[df$CanineGroup=='ModernDog',]
df.moderndog <- df.moderndog[,-1]
#Changing the levels to 0&1 i.e Females and Males
levels(df.moderndog$Gender) <- c(0,1,1)

#Applying Logistic regression to with all the variables for ModernDog
fit.moderndog <- glm(Gender~.,data=df.moderndog,family = 'binomial')
#Applying stepwise regression to find best variables
final.fit.moderndog <- step(fit.moderndog)

#For ModernDog we get variables X4,X8,X1,X7 as most contributing variables

#For GoldenJackal

#Extracting dfata for cuons
df.GoldenJackal <- df[df$CanineGroup=='GoldenJackal',]
df.GoldenJackal <- df.GoldenJackal[,-1]
#Changing the levels to 0&1 i.e Females and Males
levels(df.GoldenJackal$Gender) <- c(0,1,1)

#Applying Logistic regression to with all the variables for GoldenJackal
fit.GoldenJackal <- glm(Gender~.,data=df.GoldenJackal,family = 'binomial')
#Applying stepwise regression to find best variables
final.fit.GoldenJackal <- step(fit.GoldenJackal)

#From the above results we can say that for GoldenJackal we have X1,X6,X9 as he most contributing variables

#For IndianWolves

#Extracting dfata for cuons
df.IndianWolves <- df[df$CanineGroup=='IndianWolves',]
df.IndianWolves <- df.IndianWolves[,-1]
#Changing the levels to 0&1 i.e Females and Males
levels(df.IndianWolves$Gender) <- c(0,1,1)

#Applying Logistic regression to with all the variables for IndianWolves
fit.IndianWolves <- glm(Gender~.,data=df.IndianWolves,family = 'binomial')
#Applying stepwise regression to find best variables
final.fit.IndianWolves <- step(fit.IndianWolves)

#For IndianWolves we get most contributing factors as X7,X5,X2.

#If we analyze the above results we get most frequent variables as X1,X5,X6,X7,X9 as the contributing variables

#Hence we can fit a logistic regression with variables X1,X5,X6,X7,X9,X2


#Verifying the above results
df.all.except.thaidogs <- df[!(df$CanineGroup =='ThaiDogs'),]
df.all.except.thaidogs <- df.all.except.thaidogs[,-1]
#Changing the levels to 0&1 i.e Females and Males
levels(df.all.except.thaidogs$Gender) <- c(0,1,1)

#Applying Logistic regression to with all the variables for ThaiDogs
fit.all.except.thaidogs <- glm(Gender~.,data=df.all.except.thaidogs,family = 'binomial')
#Applying stepwise regression to find best variables
final.fit.all.except.thaidogs <- step(fit.all.except.thaidogs)

#From above results we get that X2 is the contributing variable. Thus we fit the new model with minimum number of parameters 

#Question9:

#Training the model with entire data and parameters X1,X5,X6,X7,X9,X2
df.all.except.thaidogs <- df[!(df$CanineGroup =='ThaiDogs'),]
df.all.except.thaidogs <- df.all.except.thaidogs[,-1]

#Extracting the relevant columns
df.all.except.thaidogs <-   df.all.except.thaidogs[,c('X1','X5','X6','X7','X9','X2','Gender')]

#Changing the levels to 0&1 i.e Females and Males
levels(df.all.except.thaidogs$Gender) <- c(0,1,1)
#Applying Logistic regression to with all the variables
fit.all.except.thaidogs <- glm(Gender~.,data=df.all.except.thaidogs,family ='binomial')
#Calculating the accuracy of Logistic Regression
cm <-table(df.all.except.thaidogs$Gender,as.factor(ifelse(test=as.numeric(fit.all.except.thaidogs$fitted.values>0.5) == 0, yes=1, no=0)))
accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
accuracy

library(pROC)
roc(df.all.except.thaidogs$Gender,ifelse(test=as.numeric(fit.all.except.thaidogs$fitted.values>0.5) == 0, yes=1, no=0),plot=TRUE)

#Question 8 answer: Area under curve is very less hence, the model has low accuracy.
#Ans for question 9a: We used Logistic Regression becsause we had binary outcome so Logistic
#Ans for question 9b: We get an accuracy of 43.28% for Logistic Regression


#Creating test data
df.test <- df[(df$CanineGroup =='ThaiDogs'),c('X1','X5','X6','X7','X9','X2','Gender')]

#Predicting the values
predicted_values <- predict(fit.all.except.thaidogs,newdata = df.test[,-c(7)])

#Predicting the values of Male & Female
predicted_values <- as.factor(ifelse(test=as.numeric(predicted_values>0.5) == 0, yes="Male", no="Female"))

predicted_values

df[df$CanineGroup=='ThaiDogs',]$Gender<-as.factor(ifelse(test=as.numeric(predicted_values>0.5) == 0, yes=2, no=1))


#Answer10: We have to create a linear regression model to predict Mandible Length i.e X1 

#Extracting all the data except for Thaidogs
names(df)
df.all.except.thaidogs <- df[!(df$CanineGroup =='ThaiDogs'),c(2:10)]

#Creating the model with X1 against all the variables
fit.lm <- lm(X1~.,data = df.all.except.thaidogs)
summary(fit.lm)
#From the above summary,if we look at adjusted R-squared value, we can conclude that the accuracy is around 95%

#Now we predict the values for Thai Dogs
#Creating test data
df.test.thaidogs <- df[(df$CanineGroup =='ThaiDogs'),c(2:10)]
#Predicting the values for X1
predicted_values <- predict(fit.lm,newdata =df.test.thaidogs[,-c(1)])

#Loading the required library
library(ggplot2)
#Plotting the predicted values with actual values
qplot(df.test.thaidogs$X1, predicted_values) + geom_abline(intercept=0,slope=1)










