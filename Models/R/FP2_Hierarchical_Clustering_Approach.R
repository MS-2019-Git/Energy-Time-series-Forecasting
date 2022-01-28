# Import Libraries
library(ggplot2)
library(ggfortify)
library(openxlsx)
library(corrplot)
library(forecast)
library(fpp2)
library(wavelets)
library(zoo)
library(ggpubr)
library(TSA)
library(xts)
library(timeSeries)
library(tseries)
library(MTS)
library(dplyr)
library(DataExplorer)

# Read energy consumption data of all states of US
setwd('C:/Users/acer/Desktop/Professional/ISB-AMPBA/Term 5/FP2')
energy_data <- read.csv("energy_ALL_STATES.csv")

summary(energy_data)
str(energy_data)

# Convert data type of Date column
energy_data$Date <- as.Date(paste0(as.character(energy_data$Date), '01'), format='%Y%m%d')
str(energy_data)
plot_missing(energy_data)

length(unique(energy_data$state))
table(energy_data$state)

# Ignore data that is incomplete time series
energy_data <- subset(energy_data, state != "DC")

energy_data$consumption[is.na(energy_data$consumption)] <- mean(energy_data$consumption,na.rm=TRUE)

# Missing Value Imputation
mean_consumption <- aggregate(energy_data$consumption, by = list(energy_data$state), FUN = mean)

# Category 1
data1 <- subset(mean_consumption, x <= 5000)
df1 <- subset(energy_data, state %in% data1$Group.1)

# Category 2
data2 <- subset(mean_consumption, x>5000 & x<10000)
df2 <- subset(energy_data, state %in% data2$Group.1)

# Category 3
data3 <- subset(mean_consumption, x > 10000)
df3 <- subset(energy_data, state %in% data3$Group.1)


# Model 1
df1_s1 <- subset(df1, state == 'CT')
m1_ts <- ts(df1_s1$consumption ,start=c(2001, 1),frequency=12)

s1_list <- unique(df1$state)
train_m1 <- window(m1_ts,end=c(2020,12), frequency=12)
test_m1 <- window(m1_ts,start=c(2021,1), frequency=12)

arima_fit <- auto.arima(train_m1)
arima_fit

#arima_fit.pred <-forecast(arima_fit,h=12)

columns1 = c("State","Model","RMSE","AIC","BIC")
out_df_cat1 = data.frame(matrix(nrow = 1,ncol = length(columns1))) 
colnames(out_df_cat1)<- columns1

for (x in s1_list){
  
  state_data <- subset(df1, state==x) 
  state.consumption <-  ts(state_data$consumption ,start=c(2001, 1),frequency=12)
  
  print(x)
  train_data1 <- window(state.consumption,end=c(2020,12), frequency=12)
  test_data1 <- window(state.consumption,start=c(2021,1), frequency=12)
  
  arima_fit.pred <-forecast(arima_fit,h=12)

  new_row = c(x,as.character(arima_fit),accuracy(arima_fit.pred$mean,test_data1)[2],AIC(arima_fit),BIC(arima_fit))

  out_df_cat1 <- rbind(out_df_cat1,new_row)

}


# Model 2
df2_s2 <- subset(df2, state == 'KY')
m2_ts <- ts(df2_s2$consumption ,start=c(2001, 1),frequency=12)

s2_list <- unique(df2$state)
train_m2 <- window(m2_ts,end=c(2020,12), frequency=12)
test_m2 <- window(m2_ts,start=c(2021,1), frequency=12)

arima_fit2 <- auto.arima(train_m2)
arima_fit2

#arima_fit.pred <-forecast(arima_fit,h=12)

columns2 = c("State","Model","RMSE","AIC","BIC")
out_df_cat2 = data.frame(matrix(nrow = 1,ncol = length(columns2))) 
colnames(out_df_cat2) <- columns2

for (x in s2_list){
  
  state_data <- subset(df2, state==x) 
  state.consumption <-  ts(state_data$consumption ,start=c(2001, 1),frequency=12)
  
  print(x)
  train_data2 <- window(state.consumption,end=c(2020,12), frequency=12)
  test_data2 <- window(state.consumption,start=c(2021,1), frequency=12)
  
  arima_fit.pred2 <-forecast(arima_fit2,h=12)
  
  new_row = c(x,as.character(arima_fit2),accuracy(arima_fit.pred2$mean,test_data2)[2],AIC(arima_fit2),BIC(arima_fit2))
  
  out_df_cat2 <- rbind(out_df_cat2,new_row)
  
}

# Model 3
df3_s3 <- subset(df3, state == 'OH')
m3_ts <- ts(df3_s3$consumption ,start=c(2001, 1),frequency=12)

s3_list <- unique(df3$state)
train_m3 <- window(m3_ts,end=c(2020,12), frequency=12)
test_m3 <- window(m3_ts,start=c(2021,1), frequency=12)

arima_fit3 <- auto.arima(train_m3)
arima_fit3

#arima_fit.pred <-forecast(arima_fit,h=12)

columns3 = c("State","Model","RMSE","AIC","BIC")
out_df_cat3 = data.frame(matrix(nrow = 1,ncol = length(columns3))) 
colnames(out_df_cat3)<- columns3

for (x in s3_list){
  
  state_data <- subset(df3, state==x) 
  state.consumption <-  ts(state_data$consumption ,start=c(2001, 1),frequency=12)
  
  print(x)
  train_data3 <- window(state.consumption,end=c(2020,12), frequency=12)
  test_data3 <- window(state.consumption,start=c(2021,1), frequency=12)
  
  arima_fit.pred3 <- forecast(arima_fit3,h=12)
  
  new_row = c(x,as.character(arima_fit3),accuracy(arima_fit.pred3$mean,test_data3)[2],AIC(arima_fit3),BIC(arima_fit3))
  
  out_df_cat3 <- rbind(out_df_cat3,new_row)
  
}


final_df1 <- out_df_cat1[2:nrow(out_df_cat1),]
final_df2 <- out_df_cat2[2:nrow(out_df_cat2),]
final_df3 <- out_df_cat3[2:nrow(out_df_cat3),]
