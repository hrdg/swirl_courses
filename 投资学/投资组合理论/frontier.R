library("quantmod")
#szSymbols <- c("000001.sz","600036.SH")
d1<-getSymbols("002066.sz",src="yahoo",from="2014-1-1",to=Sys.Date()) 
d2<-getSymbols("600099.ss",src="yahoo",from="2014-1-1",to=Sys.Date()) 
#Asset1
rtn1=diff(log(`002066.SZ`$`002066.SZ.Adjusted`))
r1 =mean(na.omit(rtn1))
sigma1=sd(na.omit(rtn1))
#Asset2
rtn2=diff(log(`600099.SS`$`600099.SS.Adjusted`))
r2 =mean(na.omit(rtn2))
sigma2=sd(na.omit(rtn2))

rho =cor(na.omit(rtn1),na.omit(rtn2))
covar =rho*sigma1*sigma2 #cov(rtn1,rtn2)
p =seq(0, 1, by = 0.1)#c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)# percent of Asset1
##Portfolio return as function of percent of Asset1##
variance=p^2*sigma1^2+(1-p)^2*sigma2^2+2*p*(1-p)*covar
sigma=sqrt(variance)
portfolio_return=r1*p+r2*(1-p)
cbind(p,sigma,return)
plot(sigma,portfolio_return,type="o")
