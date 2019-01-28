#Importing basic libraries 
install.packages("data.table")
library(data.table) #used for reading and manipulation of data
install.packages("dplyr")
library(dplyr) #used for data manipulation and joining 
install.packages("ggplot2")
library(ggplot2) #used for plotting
install.packages("caret")
library(caret) #used for modelling
install.packages("corrplot")
library(corrplot) #used for making correlation plot
install.packages("cowplot")
library(cowplot)  #used for combining multipe plots
install.packages("GGally")
library(GGally)
#CAR(Companion to Applied Regression)
install.packages('car')
library(car)
install.packages('kableExtra')
library(kableExtra)
install.packages('ISLR')
library(ISLR)
install.packages('forecast')
library(forecast)
install.packages("MLmetrics")
library(MLmetrics)

#Import data and form a dataframe
df_prac<-as.data.frame(read.csv('cereal.csv'))
#Descriptive stats of the attributes
summary(df_prac)
#descriptive stats graphs to check the trend of each attribute.
par(mfrow=c(4,4))#To fit many graphs in a 4 by 4 matrix.#DO THIS: Expand the o/p area.
hist(df_prac$calories, prob=T)
hist(df_prac$protein)
hist(df_prac$fat)
hist(df_prac$sodium)
hist(df_prac$fiber)
hist(df_prac$carbo)
hist(df_prac$sugars)
hist(df_prac$potass)
hist(df_prac$vitamins)
hist(df_prac$shelf)
hist(df_prac$weight)
hist(df_prac$cups)
hist(df_prac$rating)
#Number of Columns 
ncol(df_prac)
#trimming columns to only necessary columns
df_analyze<-df_prac[,c(4:16)]
# Train test split
attach(df_analyze)
smp_size = floor(0.80*nrow(df_analyze))
set.seed(123)
smp_size 
train_ind = sample(seq_len(nrow(df_analyze)),size=smp_size)
train= df_analyze[train_ind,]
train
test = df_analyze[-train_ind,]

#Checking the datatypes for the columns in the dataframe
str(train)
#Creates a correlation matrix with circles as the intensity of the values 
#and colors as the positive and negative values
dev.off()
M<- cor(train)
corrplot(M,method="circle")
#Another way to represent the corr matrix with numbers below and circles above
corrplot.mixed(M,lower.col = "black",number.cex=.7)

#Correlaton matrix with linear trends
ggpairs(train, 
        lower = list(continuous = wrap("smooth", method = "lm")))
#Summary of the Linear model
summary(lm(rating ~ calories+protein+fat+sodium+fiber+carbo+sugars+potass+shelf+weight+cups, data = train))$coef

#Checking Collinearity using VIF(Variable inflation factor)
fitvif<-lm(rating ~ calories+protein+fat+sodium+fiber+carbo+sugars+potass+shelf+weight+cups, data = train)
kable(vif((fitvif)),format = "markdown",align = 'c',padding = 2)

finalfit<-lm(rating ~ protein+fat+sodium+sugars, data = train)
summary(finalfit)$coef 
summary(finalfit)
AIC(finalfit)
BIC(finalfit)

summary(finalfit)$r.squared
#Checking the VIF for the selected attributes 
kable(vif(finalfit), format = "markdown", padding = 2)
#checking the residuals vs fitted values
residualPlot(finalfit)
#Training data set RMSE
train_RMSE = sqrt(mean(finalfit$residuals^2))
train_RMSE

#Checking if the test and train MSEs.(Low Train MSE and High test MSE = Overfitting, high and high = underfitting)
predict_rating<-predict(finalfit, test)
predict_rating

#create a dataframe with actauls and predcited values for the test set and plot it.
actuals_preds<-data.frame(cbind(actuals=test$rating, predicteds=predict_rating))
actuals_preds
correlation_accuracy<-cor(actuals_preds)
correlation_accuracy
plot(actuals_preds)
#add a regression line b/w predicted and actuals.
abline(lm(actuals_preds$predicteds~actuals_preds$actuals),col="red")

accuracy(actauls_preds$predicteds,actauls_preds$actauls)
actauls_preds$predicteds
actauls_preds$actauls

Accuracy(y_pred = actuals_preds$predicteds,y_true = actuals_preds$actuals)

RMSE(actauls_preds$predicteds,actauls_preds$actauls)

test_RMSE<-sqrt(mean((predict_rating - test$rating)^2))
test_RMSE


#As the test and train RMSE vary a lot we use K-fold Cross Validation
# K fold Cross validation
controlparameters<-trainControl(method="cv",number=10,
                                savePredictions = TRUE,
                                classProbs = TRUE,
                                verboseIter = TRUE)


model<-train(rating ~ protein+fat+sodium+sugars, 
             data = train,
             method = "lm",
             trControl = controlparameters
             )
model  
model$resample
model$metric

predict_model_rating<-predict(model, test)
predict_model_rating


#Attempt to create confusion matrix
t<-table(actual=test$rating, predictions=predict_rating)
t
test$rating 


