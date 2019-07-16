rm(list = ls())
setwd('C:/Users/admin/Desktop')

#Read Data
bike_df = read.csv('day.csv',header = T)
x = c("ggplot2","corrgram","DMwR","caret","randomForest","unbalanced",
      "C50","dummies","e1071","Information","MASS","rpart","gbm","ROSE","lubridate"
      ,"dplyr","rsq","usdm","gplots","scales","psych")
lapply(x,require,character.only = TRUE)
rm(x)

#Understand the structure of data
str(bike_df)

#Removing unnecessary variables
bike_df$instant = NULL
bike_df$dteday = NULL
bike_df$holiday = NULL
bike_df$casual = NULL
bike_df$registered = NULL


#Understanding the distribution of values
table(bike_df$season)
table(bike_df$yr)
table(bike_df$mnth)
table(bike_df$weekday)
table(bike_df$workingday)
table(bike_df$weathersit)


#Data type conversion
bike_df$season = as.factor(bike_df$season)
bike_df$yr = as.factor(bike_df$yr)
bike_df$mnth = as.factor(bike_df$mnth)
bike_df$weekday = as.factor(bike_df$weekday)
bike_df$workingday = as.factor(bike_df$workingday)
bike_df$weathersit = as.factor(bike_df$weathersit)

bike_df$cnt = as.numeric(bike_df$cnt)



#Missing Value Analysis : 0 missing values
colSums(is.na(bike_df))

#Outlier Analysis
boxplot(bike_df$temp,bike_df$atemp,bike_df$hum,bike_df$windspeed)
boxplot(bike_df$cnt)

#Remove outliers present in hum and windspeed
numeric_index = sapply(bike_df, is.numeric)
numeric_data = bike_df[,numeric_index]
cnames = colnames(numeric_data)

for (i in cnames){
  print(i)
  val = bike_df[,i][bike_df[,i]%in%boxplot.stats(bike_df[,i])$out]
  print(length(val))
  bike_df = bike_df[which(!bike_df[,i]%in%val),]
}

dim(bike_df)
summary(bike_df)

#Feature Selection : removing atemp as high correlation with temp~0.99
corrgram(bike_df[,numeric_index],order = F,
         upper.panel = panel.pie,text.panel = panel.txt, main = "Correlation Plot")

bike_df$atemp = NULL



#Distribution of data
# Boxplot of rental count by year 
boxplot(cnt~yr,data=bike_df, main="Bike Rental", 
        xlab="Years", ylab="Rental Count")

# Boxplot of rental count by season 
boxplot(cnt~season,data=bike_df, main="Bike Rental", 
        xlab="Season", ylab="Rental Count")

# Boxplot of rental count by month 
boxplot(cnt~mnth,data=bike_df, main="Bike Rental", 
        xlab="Months", ylab="Rental Count")

# Boxplot of rental count by weather criteria 
boxplot(cnt~weathersit,data=bike_df, main="Bike Rental", 
        xlab="Weather", ylab="Rental Count")

# Boxplot of rental count by weekday 
boxplot(cnt~weekday,data=bike_df, main="Bike Rental", 
        xlab="Weekday", ylab="Rental Count")

#Plot b/w temperature and count
ggplot(bike_df,aes_string(x=bike_df$temp,y=bike_df$cnt))+
  geom_point(inherit.aes = TRUE,size=3)+
  theme_bw()+ylab("Rental Count")+xlab("Temperature")+ggtitle("Scatter Plot b/w Temperature and Rental Count")+
  theme(text=element_text(size = 15))+
  scale_x_continuous(breaks = pretty_breaks(10))+
  scale_y_continuous(breaks = pretty_breaks(10))

#Plot b/w humidity and count
ggplot(bike_df,aes_string(x=bike_df$hum,y=bike_df$cnt))+
  geom_point(inherit.aes = TRUE,size=3)+
  theme_bw()+ylab("Rental Count")+xlab("Humidity")+ggtitle("Scatter Plot b/w Humidity and Rental Count")+
  theme(text=element_text(size = 15))+
  scale_x_continuous(breaks = pretty_breaks(10))+
  scale_y_continuous(breaks = pretty_breaks(10))

#Plot b/w windspeed and count
ggplot(bike_df,aes_string(x=bike_df$windspeed,y=bike_df$cnt))+
  geom_point(inherit.aes = TRUE,size=3)+
  theme_bw()+ylab("Rental Count")+xlab("Windspeed")+ggtitle("Scatter Plot b/w Windspeed and Rental Count")+
  theme(text=element_text(size = 15))+
  scale_x_continuous(breaks = pretty_breaks(10))+
  scale_y_continuous(breaks = pretty_breaks(10))

#As distribution with count variable is randomly scattered, so doesn't capture
#any pattern
bike_df$hum = NULL
bike_df$windspeed = NULL

#Model Building

#Normality check
vif(bike_df[,])
vifcor(bike_df[,7:8],th = 0.9)

#Random Sampling
set.seed(1234)
train_index = sample(1:nrow(bike_df),0.8*nrow(bike_df))
train = bike_df[train_index,]
test = bike_df[-train_index,]

#Multiple Linear Regression
lm_model = lm(cnt~.,data = train)
summary(lm_model)

predictions_lr = predict(lm_model,test[,1:7])

mape = function(actual,predicted){
  mean(abs((actual - predicted)/actual))
}
mape(test[,8],predictions_lr) * 100
#R sq : 84.39; Accuracy : 100 - 19.44 = 80.56%
#error rate mape: 19.44%


#Decision Tree
dt_model = rpart(cnt~.,data = train,method = "anova")
predictions_dt = predict(dt_model,test[,1:7])
mape(test[,8],predictions_dt)*100
#Error rate mape: 23.82%, Accuracy : 100-23.82 = 76.18%


#Random Forest
model_rf = randomForest(cnt~.,
                        train,importance = TRUE, ntree = 300)

RF_predictions = predict(model_rf,test[,1:7])
mape(test[,8],RF_predictions)
#Error rate mape: 18.97% , Accuracy : 100-18.97 = 81.03%
#Rsquare : 83.9%

