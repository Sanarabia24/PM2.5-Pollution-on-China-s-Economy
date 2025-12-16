### Thesis Topic ###
### Air Pollution in China ###

#Loading the pm2.5 pollution Dataset
china_pm2.5 <- read.csv("C:/Users/91988/OneDrive/Desktop/Project/PM2.5 data/PM2.5 Pollution data.csv")

#Loading the location data
loc <- read.csv("C:/Users/91988/OneDrive/Desktop/Project/PM2.5 data/aqi_locations_2016-02-04.csv")

#Changing the date from character format to date format
china_pm2.5$date <- as.Date(china_pm2.5$date, "%d-%m-%Y")

#Changing province column format from character to Factor
china_pm2.5$Province <- as.factor(china_pm2.5$Province)

#Selecting only pm2.5 column
china_pm2.5 <- china_pm2.5[,c(1,2,3,4)]

#Selecting the time period between 2014 to 2021
china_pm2.5 <- china_pm2.5[china_pm2.5$date > "2013-12-31" &
                             china_pm2.5$date < "2022-01-01",]

#merging the two files to get the coordinates
pm2.5_data <- merge(china_pm2.5, loc[,1:4], by.x = "AQI.Locations", 
                    by.y = "stationname")

#Initial Analysis
#numerical analysis
summary(pm2.5_data)

#graphical analysis
#Loading the ggplot package
library(ggplot2)

#Alternative PM2.5 graph
ggplot(data = pm2.5_data, aes(x = date, y = pm25, color = factor(Province[]))) +
  geom_line()+
  labs(x = 'Year',y = 'PM2.5', color = "Province",
       title = '2014-2021 PM2.5 Pollution')+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))


#Loading scales package to shorten the year in the graph
install.packages("scales")
library(scales)


ggplot(data = pm2.5_data, aes(x = date, y = pm25, color = factor(Province)))+
  geom_line()+
  labs(x = 'Year',y = 'PM2.5',
       title = 'PM2.5 Pollution (2014-2021)', color = 'Province/Municipality')+
  facet_wrap(.~ Province, ncol = 3, scales = "free")


#Converting the daily data to Quarterly
#Separating the year from the date column
pm2.5_data["Year"] <- format(pm2.5_data$date, format = "%Y")

library(zoo)
pm2.5_data$Quarter <- as.yearqtr(pm2.5_data$date)

#Loading the dplyr package 
library(dplyr)

#Finding the quantiles for the provinces (quarterly)
pm2.5_quant <- pm2.5_data %>% 
  group_by(Province, Quarter) %>% 
  summarize(count = n(),
            min = fivenum(pm25)[1],
            median = fivenum(pm25)[2],
            max = fivenum(pm25)[3],
            sum = sum(pm25, na.rm = TRUE),
            mean = mean(pm25, na.rm = TRUE))

pm25_qrtly <- pm2.5_data %>%
  group_by(Quarter, Province, Year) %>%
  summarize(PM25 = mean(pm25, na.rm = TRUE))

summary(pm25_qrtly)

#Graphical analysis
ggplot(data = pm2.5_data, aes(x = Quarter, y = pm25, color = factor(Province)))+
  geom_point()+
  labs(x = 'Quarter',y = 'PM2.5',
       title = 'PM2.5 Pollution (2014-2021)', color = 'Province/Municipality')+
  facet_wrap(.~ Province, ncol = 3, scales = "free")

#Calculating the quarterly average for each province            
#PM2.5_avg = pm2.5_data %>%
#  group_by(Quarter) %>%
#  summarise_at(vars(-Province), funs(mean(pm25, na.rm=TRUE)))


#Downloading the excel file to the desktop
install.packages("writexl")
library("writexl")
write_xlsx(GDP_quant,"C:/Users/91988/OneDrive/Desktop/Project/GDP quant1.xlsx")

write_xlsx(pm25_qrtly,"C:/Users/91988/OneDrive/Desktop/Project/updated data.xlsx")

### Main Analysis ###

#Loading the economic variables dataset
gdp_data <- read.csv("C:/Users/91988/OneDrive/Desktop/Project/Economy Variables/GDP Quarterly.csv")

#Changing City column format from character to Factor
gdp_data$Province <- as.factor(gdp_data$Province)

#Changing the Year from integer format to date format
gdp_data$Year <- as.Date(as.character(gdp_data$Year), format = "%Y")

#Separating the year from the date column
gdp_data$Year <- format(gdp_data$Year, format = "%Y")

#removing the comma's from the columns in order to convert 
#the class from character to numeric
gdp_data$GDP..100.million.yuan. <- as.numeric(gsub(",","",gdp_data$GDP..100.million.yuan.))

#Changing column names
gdp_data <- gdp_data %>% rename(GDP = GDP..100.million.yuan.)

#summary of the gdp data
summary(gdp_data)

#Finding the quantiles for the provinces (quarterly)
GDP_quant <- gdp_data %>% 
  group_by(Province) %>% 
  summarize(count = n(),
            min = fivenum(GDP)[1],
            median = fivenum(GDP)[2],
            max = fivenum(GDP)[3],
            sum = sum(GDP, na.rm = TRUE),
            mean = mean(GDP))

#Graphical analysis of GDP

ggplot(data = gdp_data, aes(x = Quarter, y = GDP)) +
  geom_point(aes(colour = Province), size = 1)+
  labs(color ='Province/Municipality', title = "Quarterly GDP from 2014-2021") +
  ylab("GDP") + xlab("Quarter") +
  theme(axis.text.x = element_text(angle = 90))

##Regression Analysis##

#Combining the pm2.5 data with the gdp data
#Changing the yearqtr format to character in the pm2.5 data
pm25_qrtly$Quarter <- as.character(pm25_qrtly$Quarter)

china_data <- pm25_qrtly %>% full_join(gdp_data)

#Graphical summary
pairs(~GDP + PM25, data = china_data)

#Plotting the data
plot(china_data$PM25, china_data$GDP, pch=20,xlab='PM2.5',ylab='GDP', 
     main = 'GDP vs PM25 from 2014-2021')

#Histogram for PM2.5
hist(china_data$PM25)

#Histogram for GDP
hist(china_data$GDP)

#Fitting a LM model
lm_mod <- lm(GDP ~ PM25, data = china_data)

summary(lm_mod)

#Scatterplot with regression line
plot(china_data$PM25, china_data$GDP, pch=20,xlab='PM2.5',ylab='GDP', 
     main = 'GDP vs PM2.5 Scatterplot')
abline(lm_mod, lwd=2, col="red")

ggplot(china_data, aes(y=GDP, x=PM25)) +geom_point()+
  geom_smooth(method = 'lm')+
  labs(title = "Linear Regression Scatter Plot") +
  ylab("PM2.5") + xlab("GDP")
  
#Fitting a Gaussian GLM
glm_mod <- glm(GDP ~ PM25,
               data = china_data, family = gaussian(link = "identity"))

summary(glm_mod)

#Estimating 95% CIs 
confint(lm_mod, level = 0.95) 
confint(glm_mod, level = 0.95) 

#Model Checking
#Extracting deviance 
glm_mod$deviance

#LRT-statistic 
glm_mod$deviance/463045812    #dispersion parameter of GLM model

#Using R to do the F test with the anova() function: 
anova(glm_mod,test="F")

#Checking the residual plots
par(mfrow=c(1,3)) 
plot(glm_mod,1) 
plot(glm_mod,2) 
plot(glm_mod,5)

#Fitting a Poisson GLM
glm_mod2 <- glm(offset(log(GDP)) ~ PM25,
               data = china_data, family = poisson(link = "log"))

summary(glm_mod2)

#Estimating 95% CIs 
confint(glm_mod2, level = 0.95) 

#Goodness of fit test 
1-pchisq(glm_mod2$deviance,glm_mod2$df.residual)

#Checking the residual plots
par(mfrow=c(1,3)) 
plot(glm_mod2,1) 
plot(glm_mod2,2) 
plot(glm_mod2,5)

#Fitting a Negative Binomial
install.packages("MASS")
library(MASS)

glm_mod3 <- glm.nb(offset(log(GDP)) ~ PM25,
               data = china_data)

summary(glm_mod3)

#Checking the residual plots
par(mfrow=c(1,3)) 
plot(glm_mod3,1) 
plot(glm_mod3,2) 
plot(glm_mod3,5)

#Goodness of fit test 
1-pchisq(glm_mod3$deviance,glm_mod3$df.residual)

#Estimating 95% CIs 
confint(glm_mod3, level = 0.95) 

#Fitting a Quasi-poisson Model
glm_mod4 <- glm(offset(log(GDP)) ~ PM25,
                data = china_data, 
                family = quasipoisson(link = "log"))

summary(glm_mod4)

#Checking the residual plots
par(mfrow=c(1,3)) 
plot(glm_mod4,1) 
plot(glm_mod4,2) 
plot(glm_mod4,5)

#Goodness of fit test 
1-pchisq(glm_mod4$deviance,glm_mod4$df.residual)

#Fitting a gamma model
glm_mod5 <- glm(GDP ~ PM25,
                data = china_data, 
                family = Gamma(link = "inverse"))

#Summarizing as gamma GLM
summary(glm_mod5)

#summarizing as exponential
summary(glm_mod5, dispersion = 1)

#Goodness of fit test 
1-pchisq(glm_mod5$deviance,glm_mod5$df.residual)

#Checking the residual plots
par(mfrow=c(1,3)) 
plot(glm_mod5,1) 
plot(glm_mod5,2) 
plot(glm_mod5,5)

#Model AIC's: 
AIC(glm_mod, glm_mod2, glm_mod3, glm_mod4, glm_mod5)

#Model Comparison
anova(glm_mod2, glm_mod4, test = "Chisq")
anova(glm_mod, glm_mod5, test = "F")

#observations: the beta's estimation and std.error and t-values are the same of the two models
#They use different algorithms 
#Our linear model explains an impact of 14.57%  of PM2.5 on GDP

#Predicting
gdp_val1 <-seq(min(china_data$GDP),max(china_data$GDP),length=352)
PM25_VAL1 <- seq(50.68,207.92, length.out = 352)

pois_pred <- predict(glm_mod3, newdata = data.frame(GDP = gdp_val1,
                                                    PM25 = PM25_VAL1),
                     type = "link", se.fit = TRUE)

#Plotting data 
plot(china_data$GDP,china_data$PM25, pch=20) 

#Predictions and CIs 
lines(gdp_val1,pois_pred$fit,col="blue",lwd=4) 
lines(gdp_val1,pois_pred$fit+1.96*pois_pred$se.fit,col="red",lwd=2,lty=4) 
lines(gdp_val1,pois_pred$fit-1.96*pois_pred$se.fit,col="red",lwd=2,lty=4)

predict(glm_mod3)
head(china_data)

# Installing and loading required packages 
install.packages("mgcv")
library(mgcv)

#Fitting a GAM Gaussian model
gam_mod <- gam(GDP ~ s(PM25, by = Province, k=30, bs='cs'),
               data = china_data,
               method = "REML",
               family = gaussian(link = "identity"))

summary(gam_mod)

#Checking residuals 
par("mar")
par(mar=c(2,2,2,2))
par(mfrow = c(2,2))
gam.check(gam_mod,pch=20)

#Fitting a poisson GAM model
gam_mod2 <- gam(GDP ~ s(PM25, by = Province, k=30, bs='cs'),
               data = china_data,
               family = poisson(link = "log"))

summary(gam_mod2)

#Checking residuals 
par(mar=c(2,2,2,2))
par(mfrow = c(2,2))
gam.check(gam_mod2,pch=20)

#Fitting a negative binomial GAM model
gam_mod3 <- gam(GDP ~ s(PM25, by = Province, k=30, bs='cs'),
                data = china_data,
                method = "REML",
                family = negbin(352))
                
summary(gam_mod3)

#Checking residuals 
par(mar=c(2,2,2,2))
par(mfrow = c(2,2))
gam.check(gam_mod3,pch=20)

#Fitting a quasipoisson GAM model
gam_mod4 <- gam(GDP ~ s(PM25, by = Province, k=30, bs='cs'),
                data = china_data,
                method = "REML",
                family = quasipoisson(link = "log"))

summary(gam_mod4)

#Checking residuals 
par(mar=c(2,2,2,2))
par(mfrow = c(2,2))
gam.check(gam_mod4,pch=20)

#Fitting a gamma GAM model
gam_mod5 <- gam(GDP ~ s(PM25, by = Province, k=30, bs='cs'),
                data = china_data,
                method = "REML",
                family = Gamma(link = "log"))

summary(gam_mod5)

#Checking residuals 
par(mar=c(2,2,2,2))
par(mfrow = c(2,2))
gam.check(gam_mod5,pch=20)

#predictions
gdp_val <-seq(min(china_data$GDP),max(china_data$GDP),length=352)
PM25_VAL <- seq(50.68,207.92, length.out = 352)

summary(china_data)
pred.val <- predict(gam_mod5, newdata = data.frame(GDP = gdp_val, PM25 = PM25_VAL),
                    type = "link", se.fit = TRUE)

#Plotting data 
plot(china_data$GDP,china_data$PM25, pch=20) 

#Predictions and CIs 
lines(PM25_VAL,pred.val$fit,col="blue",lwd=4) 
lines(PM25_VAL,pred.val$fit+1.96*pred.val$se.fit,col="red",lwd=2,lty=4) 
lines(PM25_VAL,pred.val$fit-1.96*pred.val$se.fit,col="red",lwd=2,lty=4)

#Extracting the AIC's
AIC(gam_mod, gam_mod2, gam_mod3, gam_mod4, gam_mod5)
predict(gam_mod)

## TIME SERIES ANALYSIS ##

##TIME SERIES FOR PM2.5 for the Provinces

ts_data <- china_data[,c("Quarter","PM25")]

ggplot(data = ts_data, aes(x = Quarter, y = PM25)) +
  geom_point()+
  labs(title = "PM2.5 Concentration from 2014-2021") +
  ylab("PM2.5") + xlab("Quarter") +
  theme(axis.text.x = element_text(angle = 90))

summary(ts_data)

#Periodogram
spec.pgram(ts_data, log = 'no', na.action = na.pass)

#converting the PM2.5 data to a TS data
qrtly.ts <- ts(ts_data[,'PM25'], start=c(2014,1), end = c(2021,4), frequency = 4)
qrtly.ts

#plotting the ts data
plot(qrtly.ts, lwd=2, col = 'blue', xlab='Year', ylab = 'Quarterly PM2.5 Concentration', 
     main = 'PM2.5 Time Series Plot')

#Checking the ACF & PACF for the data
par(mfrow=c(1,2)) 
acf(qrtly.ts, main = 'ACF'); pacf(qrtly.ts, main = 'PACF')

#Fitting ARMA models

#Installing and loading the forecast package
install.packages("forecast")
library(forecast)

#Automatically estimating the orders
auto.arima(qrtly.ts, max.d = 0, seasonal = FALSE)

#ARMA Model 1 – AR(1)
arma1 <- arima(qrtly.ts, order = c(1,0,0))
arma1

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arma1)

#ARMA Model 2 – ARMA(1,1)
arma2 <- arima(qrtly.ts, order = c(1,0,1))
arma2

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arma2)

#ARMA Model 3 – ARMA(2,2)
arma3 <- arima(qrtly.ts, order = c(2,0,2))
arma3

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arma3)

#ARMA Model 4 – AR(3)
arma4 <- arima(qrtly.ts, order = c(3,0,0))
arma4

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arma4)

#ARMA Model 5 – MA(2)
arma5 <- arima(qrtly.ts, order = c(0,0,2))
arma5

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arma5)

#ARIMA
par(mfrow=c(1,2))
plot(qrtly.ts)    #before differencing
plot(diff(qrtly.ts))   #after differencing

par(mfrow=c(1,2))
acf(diff(qrtly.ts), main = 'ACF'); pacf(diff(qrtly.ts), main = 'PACF')

#Automatically estimating the orders
auto.arima(qrtly.ts, seasonal = FALSE)

#ARIMA Model 1 – AR(1,1,0)
arima1 <- arima(qrtly.ts, order = c(1,1,0))
arima1

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arima1)

#ARIMA Model 2 – MA(0,1,1)
arima2 <- arima(qrtly.ts, order = c(0,1,1))
arima2

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arima2)

#ARIMA Model 3 – ARIMA(1,1,1)
arima3 <- arima(qrtly.ts, order = c(1,1,1))
arima3

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arima3)

#ARIMA Model 4 – ARIMA(2,1,0)
arima4 <- arima(qrtly.ts, order = c(2,1,0))
arima4

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arima4)

#ARIMA Model 5 – ARIMA(0,1,2)
arima5 <- arima(qrtly.ts, order = c(0,1,2))
arima5

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(arima5)

## PREDICTIONS ##
#PM2.5
pred_PM2.5 <- predict(arima3, n.ahead = 50) 
pred_PM2.5$pred

#Plotting the predictions 
plot(qrtly.ts, xlab = "Quarter", ylab = "PM2.5 Concentration",
     main = "PM2.5 Prediction for Chinese Provinces")
lines(pred_PM2.5$pred, col = "blue", lwd=2)
lines(pred_PM2.5$pred+1.96*pred_PM2.5$se, col = "red", lwd=2)
lines(pred_PM2.5$pred-1.96*pred_PM2.5$se, col = "red", lwd=2)

##TIME SERIES FOR GDP of the Provinces

gdpts_data <- china_data[,c("Quarter","GDP")]

ggplot(data = gdpts_data, aes(x = Quarter, y = GDP)) +
  geom_point()+
  labs(title = "PM2.5 Concentration from 2014-2021") +
  ylab("GDP") + xlab("Quarter") +
  theme(axis.text.x = element_text(angle = 90))

summary(gdpts_data)

#Periodogram
spec.pgram(gdpts_data, log = 'no')

#converting the PM2.5 data to a TS data
gdp_qrtly.ts <- ts(gdpts_data[,'GDP'], start=c(2014,1), end = c(2021,4), frequency = 4)
gdp_qrtly.ts

#plotting the ts data
plot(gdp_qrtly.ts, lwd=2, col = 'blue', xlab='Quarter', ylab = 'Quarterly GDP', 
     main = 'GDP Time Series Plot')

#Checking the ACF & PACF for the data
par(mfrow=c(1,2)) 
acf(gdp_qrtly.ts, main = 'ACF'); pacf(gdp_qrtly.ts, main = 'PACF')

#Fitting ARMA models
#Automatically estimating the orders
auto.arima(gdp_qrtly.ts, max.d = 0, seasonal = FALSE)

#ARMA Model 1 – AR(1)
gdp_arma1 <- arima(gdp_qrtly.ts, order = c(1,0,0))
gdp_arma1

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arma1)

#ARMA Model 2 – ARMA(1,1)
gdp_arma2 <- arima(gdp_qrtly.ts, order = c(1,0,1))
gdp_arma2

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arma2)

#ARMA Model 3 – ARMA(2,2)
gdp_arma3 <- arima(gdp_qrtly.ts, order = c(2,0,2))
gdp_arma3

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arma3)

#ARMA Model 4 – AR(3)
gdp_arma4 <- arima(gdp_qrtly.ts, order = c(3,0,0))
gdp_arma4

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arma4)

#ARMA Model 5 – MA(2)
gdp_arma5 <- arima(gdp_qrtly.ts, order = c(0,0,2))
gdp_arma5

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arma5)

#ARIMA
par(mfrow=c(1,2))
plot(gdp_qrtly.ts)    #before differencing
plot(diff(gdp_qrtly.ts))   #after differencing

par(mfrow=c(1,2))
acf(diff(gdp_qrtly.ts), main = 'ACF'); pacf(diff(gdp_qrtly.ts), main = 'PACF')

#Automatically estimating the orders
auto.arima(gdp_qrtly.ts, seasonal = FALSE)

#ARIMA Model 1 – AR(1,1,0)
gdp_arima1 <- arima(gdp_qrtly.ts, order = c(1,1,0))
gdp_arima1

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arima1)

#ARIMA Model 2 – MA(0,1,1)
gdp_arima2 <- arima(gdp_qrtly.ts, order = c(0,1,1))
gdp_arima2

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arima2)

#ARIMA Model 3 – ARIMA(1,1,1)
gdp_arima3 <- arima(gdp_qrtly.ts, order = c(1,1,1))
gdp_arima3

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arima3)

#ARIMA Model 4 – ARIMA(2,1,0)
gdp_arima4 <- arima(gdp_qrtly.ts, order = c(2,1,0))
gdp_arima4

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arima4)

#ARIMA Model 5 – ARIMA(0,1,2)
gdp_arima5 <- arima(gdp_qrtly.ts, order = c(3,1,0))
gdp_arima5

#Residual diagnostics
par(mar=c(2,2,1,1))
tsdiag(gdp_arima5)

## PREDICTIONS ##
#GDP
pred_gdp <- predict(gdp_arima4, n.ahead = 50) 
pred_gdp$pred

#Plotting the predictions 
plot(gdp_qrtly.ts, xlab = "Quarter", ylab = "GDP",
     main = "GDP Prediction for Chinese Provinces")
lines(pred_gdp$pred, col = "blue", lwd=2)
lines(pred_gdp$pred+1.96*pred_gdp$se, col = "red", lwd=2)
lines(pred_gdp$pred-1.96*pred_gdp$se, col = "red", lwd=2)


## DLM Method for GDP ##

#Installing & loading dlm package
install.packages("dlm")
library(dlm)

#Fitting DLM (including both a trend and a seasonal component)
dlm_model <- dlmModPoly(order = 2) + dlmModSeas(frequency = 4) 
dlm_model$GG; dlm_model$W; dlm_model$FF

#Encoding the form of the model we wish to fit
buildFun<-function(x){ 
  diag(W(dlm_model))[2:3]<-exp(x[1:2]) 
  V(dlm_model)<-exp(x[3]) 
  return(dlm_model) }

#Fitting MLE
fit <- dlmMLE(gdp_qrtly.ts, parm=rep(0,3),build=buildFun) 
fit$par

dlm_model_fitted <- buildFun(fit$par) 
V(dlm_model_fitted)
W(dlm_model_fitted)

#Smoothing
model_smooth <- dlmSmooth(gdp_qrtly.ts, mod = dlm_model_fitted) 
summary(model_smooth)

dim(model_smooth$s)

x <- cbind(gdp_qrtly.ts, dropFirst(model_smooth$s[,c(1,3)])) 
colnames(x) <- c("GDP", "Trend", "Seasonal") 
plot(x, type = 'o', main = "GDP from 2014-2021")
model_smooth$s

#Forecasting
model_Filt <- dlmFilter(gdp_qrtly.ts, mod = dlm_model_fitted) 
summary(model_Filt)

dim(model_Filt$m)

x <- ts.union(window(gdp_qrtly.ts, start = 2014), 
                window(model_Filt$f, start = 2014)) 
par(mar = c(4,4,2,2)) 
plot(x, plot.type = "single", type = 'o', 
       col = c("black","brown", "brown", "yellow", "yellow"),
       ylab="GDP")

model_Fore <- dlmForecast(model_Filt, nAhead = 3)
summary(model_Fore)

dim(model_Fore$a)
dim(model_Fore$f)

sqrtR <- sapply(model_Fore$R,function(x)sqrt(x[1,1])) 
pl <- model_Fore$a[,1]+qnorm(0.05,sd=sqrtR) 
pu <- model_Fore$a[,1]+qnorm(0.95,sd=sqrtR) 

x <- ts.union(window(gdp_qrtly.ts,start=2020), 
                window(model_smooth$s[,1],start=2020), 
                model_Fore$a[,1], 
                model_Fore$f,pl,pu) 

par(mar=c(4,4,2,2)) 
plot(x,plot.type="single",type='o',pch=c(1,0,20,3,NA,NA),
       col = c("darkgrey", "darkgrey", "brown", "brown", "blue", "blue"), 
       ylab = "GDP") 
legend("bottomright", legend = c("Observed", 
                                 "Smoothed (deseasonalized)", 
                                 "Forecast", "90% interval"), 
       bty = 'n', pch = c(1, 0, 20, NA), lty = 1, 
       col = c("darkgrey", "darkgrey", "brown", "blue"))

#Residual Checking
par(mar=c(4,2,2,2)); tsdiag(model_Filt)

#PM2.5
#Fitting DLM (including both a trend and a seasonal component)
dlm_model_tian <- dlmModPoly(order = 2) + dlmModSeas(frequency = 4) 
dlm_model_tian$GG; dlm_model_tian$W; dlm_model_tian$FF

#Encoding the form of the model we wish to fit
buildFun_tian<-function(x){ 
  diag(W(dlm_model_tian))[2:3]<-exp(x[1:2]) 
  V(dlm_model_tian)<-exp(x[3]) 
  return(dlm_model_tian) }

#Fitting MLE
fit_tian <- dlmMLE(qrtly.ts, parm=rep(0,3),build=buildFun_tian) 
fit_tian$par

dlm_model_fitted_tian <- buildFun_tian(fit_tian$par) 
V(dlm_model_fitted_tian)
W(dlm_model_fitted_tian)

#Smoothing
model_smooth_tian <- dlmSmooth(qrtly.ts, mod = dlm_model_fitted_tian) 
summary(model_smooth_tian)

dim(model_smooth_tian$s)

x <- cbind(qrtly.ts, dropFirst(model_smooth_tian$s[,c(1,3)])) 
colnames(x) <- c("GDP", "Trend", "Seasonal") 
plot(x, type = 'o', main = "GDP from 2014-2019")

#Forecasting
model_Filt_tian <- dlmFilter(qrtly.ts, mod = dlm_model_fitted_tian) 
summary(model_Filt_tian)

dim(model_Filt_tian$m)

x1 <- ts.union(window(qrtly.ts, start = 2014), 
              window(model_Filt_tian$f, start = 2014)) 
par(mar = c(4,4,2,2)) 
plot(x1, plot.type = "single", type = 'o', 
     col = c("black","brown", "brown", "yellow", "yellow"),
     ylab="GDP")

model_Fore_tian <- dlmForecast(model_Filt_tian, nAhead = 3)
summary(model_Fore_tian)

dim(model_Fore_tian$a)
dim(model_Fore_tian$f)

sqrtR_tian <- sapply(model_Fore_tian$R,function(x)sqrt(x[1,1])) 
pl_tian <- model_Fore_tian$a[,1]+qnorm(0.05,sd=sqrtR_tian) 
pu_tian <- model_Fore_tian$a[,1]+qnorm(0.95,sd=sqrtR_tian) 

x1 <- ts.union(window(qrtly.ts,start=2018), 
              window(model_smooth_tian$s[,1],start=2018), 
              model_Fore_tian$a[,1], 
              model_Fore_tian$f,pl_tian,pu_tian) 

par(mar=c(4,4,2,2)) 
plot(x1,plot.type="single",type='o',pch=c(1,0,20,3,NA,NA),
     col = c("darkgrey", "darkgrey", "brown", "brown", "blue", "blue"), 
     ylab = "GDP") 
legend("bottomright", legend = c("Observed", 
                                 "Smoothed (deseasonalized)", 
                                 "Forecast", "90% interval"), 
       bty = 'n', pch = c(1, 0, 20, NA), lty = 1, 
       col = c("darkgrey", "darkgrey", "brown", "blue"))

#Residual Checking
par(mar=c(4,2,2,2)); tsdiag(model_Filt_tian)
