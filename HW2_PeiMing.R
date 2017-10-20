#HW2.1.plot
library(readr)
RAM_size <- read_csv("~/R data/Home-Work-for-BDIF/RAM_size.csv")
plot(RAM_size,type="l",xlab = "time",ylab = "size",lwd=3)

#HW2.2.Splines
splines.reg.l1 = smooth.spline(x = RAM_size$year, y = RAM_size$Byte, spar = 0.2)  # lambda = 0.2
splines.reg.l2 = smooth.spline(x = RAM_size$year, y = RAM_size$Byte, spar = 1)  # lambda = 1
splines.reg.l3 = smooth.spline(x = RAM_size$year, y = RAM_size$Byte, spar = 2)  # lambda = 2
lines(splines.reg.l1, col = "red", lwd = 1)  # regression line with lambda = 0.2
lines(splines.reg.l2, col = "green", lwd = 1)  # regression line with lambda = 1
lines(splines.reg.l3, col = "blue", lwd = 1)  # regression line with lambda = 2

#HW2.3.Poisson
lambda=4
x=6
dpois(x,lambda)

lambda=5
x=0
dpois(x,lambda)
