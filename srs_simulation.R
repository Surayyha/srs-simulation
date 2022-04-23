#  Scenario 1: f1(x) = N(0,1)
## generate simulation data 500
install.packages("locfit")
library(locfit)
library(tibble)
library(ggplot2)
w=rbinom(5000,1,0.5)
sam1_500 <- as_tibble(x=w*rnorm(5000, mean = 0, sd = 1)+(1-w)*rnorm(5000,mean=5,sd=0.5))
d1 <- density(sam1_500$value)
h1 <- hist(sam1_500$value)
loc1<-locfit( ~ lp(sam1_500$value))
plot(loc1)
plot(d1)
loc_fit=fitted(loc1)
predict(locfit, newdata=targage, se.fit=TRUE)

ggplot(data = sam1_500,aes(x=value))+
  geom_histogram(aes(y = ..density..))+
  geom_line(aes(y=dnorm(value,0,1)))+
  geom_density()+
  geom_line(aes(y=loc_fit))

plot(sam1_500[order(sam1_500)], fitted(loc1)[order(sam1_500)], type = "l")
            

## sample from histogram
bins=with(h1,sample(length(mids),250,p=density,replace=TRUE)) # choose a bin
result=runif(length(bins),h1$breaks[bins],h1$breaks[bins+1]) # sample a uniform in it
hist(result,add=TRUE,bord=3)


## sample from kde
x.new <- rnorm(250, sample(sam1_500$value, size = 250, replace = TRUE), d1$bw)
plot(d1)
lines(density(x.new), col = "blue")

## ise for Scenario 1
splxy = splinefun(d1$x, (d1$y - dnorm(d1$x))^2)
integrate(splxy, lower = min(sam1_500$value), upper = max(sam1_500$value))
