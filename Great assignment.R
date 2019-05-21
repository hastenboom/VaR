#----------------------------Part one:preparation-----------------------------------

#----------------definiting the Weibull loglikelihood function-----------------------
x=dat
weibull_loglik <- function(parm){
  n <- length(x)
  gamma <- parm[1]
  lambda <- parm[2]
  loglik <- sum(dweibull(vec, shape=gamma, scale=lambda, log=TRUE))
  return(-loglik)
}
weibull <- nlm(weibull_loglik, p = c(1,1), hessian=TRUE)
#----------------------------------------------------------------------

#-----download necessary datasets
library(quantmod)
stocks=c("601818.ss","600015.ss","601009.ss","601998.ss","601169.ss")
            #光大，华夏，南京，中信，北京
selstock=function(stock,begin,end)
{
  num=length(stock);
  stockdata = NA;
  for(i in 1:num)
  {
    data1=getSymbols(stock[i],src="yahoo",from=begin,to=end,auto.assign = F)
    stockdata=cbind(stockdata,data1)
  }
  data2=NA;
  for(i in 1:num)
  {
    data2=cbind(data2,stockdata[,((i-1)*6+5)])
  }
  return(data2[,2:ncol(data2)])#return close price only
}
data1.pir=selstock(stocks,begin="2015-05-21",end="2019-05-21")
#---------------------------------------

#-----converting into daily return
daily=function(x)
{
  y=diff(x)/lag(x)
}

data2.re=apply(data1.pir,2,FUN=daily)
colnames(data2.re)=c("18re","15re","09re","98re","69re")
#--------------------------------

#------------------Part two:Calculating VaR applying weibull-----------------------------
data2.re=data.frame(date=as.Date(rownames(data2.re)),data2.re)
data3.raw=data2.re[data2.re$date>as.Date("2016-05-21"),]

n=nrow(data3.raw)
wVaR=NA;
alpha=0.05;

index=NA
for(i in 1:n)
{
  i=1
  index[i]=which(data3.raw$date[i]==data2.re$date)
  re=sort(data2.re$re[(index[i]-90):(index[i]-1)])
  x=exp(re)
  
  weibull_loglik=function(pars)
    {
      n=length(x)
      gamma=pars[1]
      lambda=pars[2]
      loglik=sum(dweibull(vec, shape=gamma, scale=lambda, log=TRUE))
      return(-loglik)
    }
  weibull=nlm(weibull_loglik, p = c(1,1), hessian=TRUE)
  wVaR[i]=log(qweibull(alpha,weibull$pars[1],weibull$pars[2]))
  
}








