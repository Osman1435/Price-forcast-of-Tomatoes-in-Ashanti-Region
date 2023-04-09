library(janitor)
library(zoo)
library(lubridate)
library(dplyr)

bm =attach(Data)
bm
class(Data)
class(bm)



#Converting into time series
bm_ts = ts(Data$QUARTERS,start = 1995,end = c(2020,2),frequency = 4)
bm_ts
bm_trans = ts(transformed_data$`TRANSFORMED QUARTERS DATA`,start = 1994,end = 2020,frequency = 4)
bm_trans

tsdisplay(bm_ts)


#DESCRIPTIVE STATISTICS
summary(bm_ts) 
var(bm_ts)

skewness(bm_ts)
shapiro.test(bm_ts)

#Graphical statistics
ggplot(bm_trans,xlab = 'YEAR' , ylab = 'PRICES (GHS)',main = 'QUARTERLY PTICES OF TOMATO IN ASHANTI REGION')
plot(bm_trans)

decompose(bm_trans)
plot(decompose(bm_trans))
plot(decompose(bm_trans))
nsdiffs(bm_trans)
#ACF and PACF of the data
ggPacf(bm_trans)
ggAcf(bm_trans)
adf.test(bm_trans)
kpss.test(bm_trans)
pp.test(bm_trans)
isSeasonal(bm_ts,test="combined",freq = 4)
combined_test(bm_ts,freq = 4)
kw(bm_ts,freq = 4,diff = T,residuals = F,autoarima = T)
mk.test(bm_ts)
sens.slope(bm_ts)
welch(bm_ts,freq = 4)
#Test for normality in the data
hist(bm_trans,xlab = 'PRICES (GHS)',main = 'QUARTERLY PTICES OF TOMATO IN ASHANTI REGION')
plotNormalHistogram(bm_ts)
boxplot(bm_trans,xlab = "YEARS",ylab = "PRICES")
boxcox(lm( (bm_trans) ~ 1))
BoxCox.lambda(bm_ts)
bm_trans = log(bm_ts)
bm_trans
boxcox(lm( (bm_trans) ~ 1))
#trial
qqnorm(bm_trans,xlab = "YEARS",ylab = "PRICES")
qqnorm(bm_trans)
qqline(bm_trans,col="blue")

shapiro.test(bm_ts)
a = decompose(bm_ts)
plot(a)
#After transformation
hist(bm_trans)
boxplot(bm_trans)
boxcox(lm( bm_trans ~ 1))
shapiro.test(difbm_trans)

#stationarity TEST 
adf.test(bm_trans)
kpss.test(bm_trans)
pp.test(bm_trans)

#Removing seasonality in the data
decomp = stl(bm_trans,'periodic')
DESbm_trans <- seasadj(decomp)
plot(decomp)

#Removing trend and seasoanlity in the data
difbm_trans = diff(DESbm_trans, lag = 4)
difbm_trans
ggtsdisplay(difbm_trans)
autoplot(difbm_trans,xlab = 'YEARS',ylab = 'PRICES (GHS)')
nsdiffs(difbm_trans)
#Check for seasonality after differencing 
adf.test(difbm_trans)
kpss.test(difbm_trans)
pp.test(difbm_trans)


#ACF and PACF of differenced data
ggPacf(difbm_trans)
ggAcf(difbm_trans)


#FITTING ARIMA MODEL 
   # i. model selection
model1 = auto.arima(difbm_trans,stepwise = FALSE,approximation = FALSE,seasonal = FALSE,trace = TRUE,allowdrift = FALSE)
summary(model1)

m1 = Arima(difbm_trans,order = c(3,0,2))
m1
m2 = Arima(bm_ts,order = c(2,0,4))
m2
summary(m1)
ggPacf(residuals(m2),lag.max = 32)
ggAcf(residuals(m2),lag.max = 32)
checkresiduals(m1)
m2
shapiro.test(difbm_trans)
welch(difbm_trans)
model2 = auto.arima(DESbm_trans,stepwise = FALSE,approximation = FALSE,seasonal = FALSE,trace = TRUE,allowdrift = TRUE)
q1 = Arima(difbm_trans,order = c(2,0,4))
summary(q1)
Box.test(difbm_trans,lag = 4,type = 'Ljung')
  # ii. model diagnostics
autoplot(m1)#inspect roots
autoplot(m2)
autoplot(m3)

ggPacf(residuals(q1),lag.max = 32)
ggAcf(residuals(q1),lag.max = 32)


#FITTING SARIMA MODEL 
# i. model selection
model3 = auto.arima(difbm_trans,stepwise = FALSE,approximation = FALSE,trace = TRUE,allowdrift = FALSE)
model3

m1=Arima(bm_trans,order=c(1,0,1),seasonal = c(2,1,0))
m1

boxplot(difbm_trans~cycle(difbm_trans))

p1 = Arima(difbm_trans,order = c(1,0,0),seasonal = c(0,0,1),include.mean = TRUE)
summary(p1)
 model3 = auto.arima(bm_trans,stepwise = TRUE,approximation = FALSE,trace = TRUE,allowdrift = FALSE)
p2 = Arima(difbm_trans,order = c(3,0,1),seasonal = c(0,0,1),include.mean = TRUE)
p2
summary(p2)
p2 = auto.arima(bm_ts)
p2
forecast(p2,4*8)
plot(forecast(p2,4*8))
shapiro.test(m1$residuals)

# ii. model diagnostics
autoplot(p1)
plot(p1) #inspect roots
ggPacf(residuals(Arima(difbm_trans,order=c(5,0,0))))
ggAcf(residuals(Arima(difbm_trans,order=c(5,0,0)),lag.max = 20))
checkresiduals(p2)
shapiro.test(p2$residuals)
ggtsdisplay(p2$residuals)
shapiro.test(checkresiduals((Arima(difbm_trans,order=c(3,0,1),seasonal = c(0,0,1)))))

sarima(difbm_trans,p = 3,0,1,0,0,1,4)
















#split data into train and test 









bm_train <- window(bm_ts,end = c(2011,4))
bm_train

p1 = Arima(bm_train,order = c(3,0,1),seasonal = c(0,0,1),include.mean = TRUE)
f = forecast::forecast(p1,h = 4*9)
f
plot(f)
q1 = Arima(difbm_trans,order = c(2,0,4))

bm_test <- window(bm_ts,start = 2012)
bm_test

#train the model
train_sarima1 = model2 = auto.arima(bm_train,stepwise = FALSE,approximation = FALSE,trace = TRUE,allowdrift = FALSE) 

train_sarima2 = model2 = auto.arima(bm_train,stepwise = FALSE,approximation = FALSE,seasonal = FALSE,trace = TRUE,allowdrift = FALSE) 

#produce forecasts
myforecasts <- forecast::forecast(p1,h = 4*8)
myforecasts <- forecast::forecast(train_sarima1,h = 4*12)

myforecasts
autoplot(myforecasts)

accuracy(myforecasts)
#plot the forecasts
autoplot(myforecasts) + autolayer(bm_test) 
my= forecast::Arima(bm_ts,order = c(1,0,1),seasonal = c(2,1,0),include.mean = TRUE )
my
myplot(difbm_trans)
lines(fitted(forecast(p2,4*8)),col="blue")
myforecasts$lower

 