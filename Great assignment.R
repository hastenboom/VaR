#----------------------------Part one:preparation-----------------------------------

#-----download necessary datasets
library(quantmod)
stocks=c("601398.ss","601288.ss","601988.ss","601939.ss","601328.ss")
#工商，农业，中行，建设，交通
selstock=function(stock,begin,end,ty)
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
  if(ty=="close"){return(data2[,2:ncol(data2)])}#return close price only
  else if(ty=="all"){return(stockdata)};#return all of the information
}
data1.pir=selstock(stocks,begin="2013-05-21",end="2019-05-21",ty="close")
#---------------------------------------

#-----converting into daily return
daily=function(x)
{
  y=diff(x)/lag(x)
}

data2.re=apply(data1.pir,2,FUN=daily)
colnames(data2.re)=c("1398re","1288re","1988re","1939re","1328re")
#--------------------------------

#------------Part two:Calculating VaR applying weibull & History--------------------
data2.re=data.frame(date=as.Date(rownames(data2.re)),data2.re)
data3.raw=data2.re[data2.re$date>as.Date("2018-05-21"),]

n=nrow(data3.raw)
wVaR=NA;
alpha=0.05;
#Xi contains return and VaR later
x1=data3.raw[,c(1,2)];x2=data3.raw[,c(1,3)];x3=data3.raw[,c(1,4)]
x4=data3.raw[,c(1,5)];x5=data3.raw[,c(1,6)]

#a function that calculates historical VaR and VaR based on weibull distribution
  calVaR=function(x,ty)
  {
    index1=NA;index2=NA;hVaR=NA
    for(i in 1:n)
    {
      index1[i]=which(x$date[i]==data2.re$date)
      index2=which(colnames(x)[2]==colnames(data2.re))
      re=sort(data2.re[(index1[i]-252):(index1[i]-1),index2])
      y=exp(re)
      hVaR[i]=(re[trunc(252*alpha)]+re[trunc(252*alpha)+1])/2
      weibull_loglik=function(pars)
      {
        n=length(y)
        gamma=pars[1]
        lambda=pars[2]
        loglik=sum(dweibull(y, shape=gamma, scale=lambda, log=TRUE))
        return(-loglik)
      }
      weibull=nlm(weibull_loglik, p = c(1,1), hessian=TRUE)
      wVaR[i]=log(qweibull(alpha,weibull$estimate[1],weibull$estimate[2]))
    }
    if(ty=="His"){return(hVaR)}
    else if(ty=="wei"){return(wVaR)}
  }

#-------------------Part three:adding amount amplitude data---------
hVaR1398=calVaR(x1,"His");x1=data.frame(x1,hVaR1398,default=NA)
x1[x1$X1398re<x1$hVaR1398,4]=1;
x1[x1$X1398re>=x1$hVaR1398,4]=0;

xx1=selstock(stocks[1],begin="2013-05-21",end="2019-05-21",ty="all");

xx1=xx1[index(xx1)%in%x1$date,c(3,4,6)];
xx2=data.frame(xx1,amplitude=xx1$X601398.SS.High-xx1$X601398.SS.Low)
x1=data.frame(x1,xx2$X601398.SS.Volume,xx2$X601398.SS.High.1)
colnames(x1)[c(5,6)]=c("volume","amplitude")
#standardization on volume since whose unit is too great.
x1$volume=(x1$volume-mean(x1$volume))/sd(x1$volume)
