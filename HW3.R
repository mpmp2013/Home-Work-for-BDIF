#install.packages("digest",repos='http://cran.us.r-projest.org')
library(digest)

#HW3.1
digest("I learn a lot from this class when I am proper listening to the professor","sha256")
digest("I do not learn a lot from this class when I am absent and playing on my Iphone","sha256")

#HW3.3
library(rjson)
json_RAM <- toJSON(RAM_size,method = "C")

#HW3.4
#install.packages("rjson", repos="http://cran.us.r-project.org")
library(rjson)
json_file = "http://crix.hu-berlin.de/data/crix.json"
json_data = fromJSON(file=json_file)
lst <- lapply(json_data,function(x){
  df<-data.frame(date=x$date,price=x$price)
  return(df)
})
crix_data_frame <- Reduce(rbind,lst)
plot(crix_data_frame$date,crix_data_frame$price)

#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)
ts.plot(crix_data_frame$price)
Acf(crix_data_frame$price)

for(i in 1:length(crix_data_frame$price)){
  crixreturn[i] <- log(crix_data_frame$price[i+1]/crix_data_frame$price[i])
}
ts.plot(crixreturn)
Box.test(crixreturn, type = "Ljung-Box", lag = 20)
autocorr = acf(crixreturn, lag.max = 20, ylab = "Sample Autocorrelation", main = NA, 
               lwd = 2, ylim = c(-0.3, 1))
Acf(crixreturn)
Pacf(crixreturn)
arima(crixreturn,order = c(2,0,2))




