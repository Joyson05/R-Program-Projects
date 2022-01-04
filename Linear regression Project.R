df_data = read.csv("C:\\Users\\Joyson\\Downloads\\LinearRegression\\1 Linear_Regression\\Dataset\\insurance.csv")
View(df_data)
str(df_data)
dim(df_data)
summary(df_data)
sum(is.na(df_data))
#label encoding
library(superml)
label <- LabelEncoder$new()
library(dplyr)#deep layer
char_col=names(select_if(df_data, is.character))
for (col in char_col) {df_data[, col] <- label$fit_transform(df_data[, col])}
df_data
#correlation
library(corrplot)
my_corr1=cor(df_data)
my_corr1
corrplot(my_corr1)
#EDA
plot(density(df_data$charges))
hist(df_data$charges)
#data is skewed too much so normalizing the data is needed.

#normalize
library(caret)
df1=select(df_data,-charges)
head(df1)
process=preProcess(as.data.frame(df1), method=c("range"))
norm_scale =predict(process, as.data.frame(df1))
norm_scale
boxplot(norm_scale)
norm_scale$charges=df_data$charges
head(norm_scale)
boxplot(norm_scale)
df1=norm_scale
head(df1)
#pairplot
plot(df1)

#outlier

outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

out_rem=remove_outliers(df1, )
dim(out_rem)
#random state
set.seed(100)
#sample split
library(caTools)
split=sample.split(out_rem,SplitRatio = 0.8)
split
Train=subset(out_rem,split=="TRUE")
Test=subset(out_rem,split=="FALSE")
#model building
my_model=lm(charges~.,data=Train)# ' .' is representing all independent variables.
summary(my_model)
#prediction
y_pred=predict(my_model,Test)
y_pred
Residual=Test$charges-y_pred
Residual
y_test=Test$charges
plot(Test$charges,type="l",col="red")
lines(y_pred,type="l",col="green")
#metrics

library(Metrics)
rmse(Test$charges, y_pred)


