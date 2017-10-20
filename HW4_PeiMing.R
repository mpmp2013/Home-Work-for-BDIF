#HW4.1
library(rjson)
json_file = "http://crix.hu-berlin.de/data/crix.json"
json_data = fromJSON(file=json_file)
lst <- lapply(json_data,function(x){
  df<-data.frame(date=x$date,price=x$price)
  return(df)
})
crix_data_frame <- Reduce(rbind,lst)
crix_data_frame <- crix_data_frame[-1,]
load(file = "ecrix.RData")
load(file = "efcrix.RData")
length(ecrix)=length(crix_data_frame$price)
length(efcrix)=length(crix_data_frame$price)
ecrix_data_frame <- as.data.frame(ecrix)
efcrix_data_frame <- as.data.frame(efcrix)
#install.packages("dplyr")
library(dplyr)
sum_crix <- cbind(crix_data_frame,ecrix_data_frame,efcrix_data_frame)
#figure3
ts.plot(sum_crix$price)
lines(sum_crix$price,col="black",lwd=0.5)
lines(sum_crix$ecrix,col="blue",lwd=1)
lines(sum_crix$efcrix,col="red",lwd=1)
#figure4
crixreturn <- diff(log(crix_data_frame$price))
ts.plot(crixreturn)
#figure5
hist(crixreturn,col = "grey",breaks = 20,freq = FALSE,ylim = c(0,25),xlab = NA)
lines(density(crixreturn),lwd=1)
mu = mean(crixreturn)
sigma = sd(crixreturn)
x = seq(-4, 4, length = 100)
curve(dnorm(x, mean = mean(crixreturn), sd = sd(crixreturn)), add = TRUE, col = "darkblue", 
      lwd = 1)
qqnorm(crixreturn)
qqline(crixreturn, col = "blue", lwd = 2)
#figure6
Box.test(crixreturn, type = "Ljung-Box", lag = 20)
adf.test(crixreturn, alternative = "stationary")
kpss.test(crixreturn, null = "Trend")
par(mfrow = c(1, 2))
autocorr = acf(crixreturn, lag.max = 20, ylab = "Sample Autocorrelation", main = NA, 
               lwd = 2, ylim = c(-0.3, 1))
print(cbind(autocorr$lag, autocorr$acf))
Box.test(crixreturn, type = "Ljung-Box", lag = 1, fitdf = 0)
Box.test(autocorr$acf, type = "Ljung-Box")
autopcorr = pacf(crixreturn, lag.max = 20, ylab = "Sample Partial Autocorrelation", 
                 main = NA, ylim = c(-0.3, 0.3), lwd = 2)

#HW4.2
par(mfrow = c(1, 1))
auto.arima(crixreturn)
fit1 = arima(crixreturn, order = c(1, 0, 1))
tsdiag(fit1)
Box.test(fit1$residuals, lag = 1)

aic = matrix(NA, 6, 6)
for (p in 0:4) {
  for (q in 0:3) {
    a.p.q = arima(crixreturn, order = c(p, 0, q))
    aic.p.q = a.p.q$aic
    aic[p + 1, q + 1] = aic.p.q
  }
}
aic

bic = matrix(NA, 6, 6)
for (p in 0:4) {
  for (q in 0:3) {
    b.p.q = arima(crixreturn, order = c(p, 0, q))
    bic.p.q = AIC(b.p.q, k = log(length(crixreturn)))
    bic[p + 1, q + 1] = bic.p.q
  }
}
bic
fit4 = arima(crixreturn, order = c(2, 0, 3))
tsdiag(fit4)
Box.test(fit4$residuals, lag = 1)
fitr4 = arima(crixreturn, order = c(2, 1, 3))
tsdiag(fitr4)
Box.test(fitr4$residuals, lag = 1)
fit202 = arima(crixreturn, order = c(2, 0, 2))
tsdiag(fit202)
tsdiag(fit4)
tsdiag(fitr4)
AIC(fit202, k = log(length(crixreturn)))
AIC(fit4, k = log(length(crixreturn)))
AIC(fitr4, k = log(length(crixreturn)))
fit202$aic
fit4$aic
fitr4$aic
fit202 = arima(crixreturn, order = c(2, 0, 2))
crpre = predict(fit202, n.ahead = 30)
dates = seq(as.Date("02/08/2014", format = "%d/%m/%Y"), by = "days", length = length(crixreturn))
plot(crixreturn, type = "l", xlim = c(0, 1200), ylab = "log return", xlab = "days", 
     lwd = 1)
lines(crpre$pred, col = "red", lwd = 3)
lines(crpre$pred + 2 * crpre$se, col = "red", lty = 3, lwd = 3)
lines(crpre$pred - 2 * crpre$se, col = "red", lty = 3, lwd = 3)
