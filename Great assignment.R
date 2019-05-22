#----------------------------Part one:preparation-----------------------------------

#-----download necessary datasets
library(quantmod)
stocks=c("601398.ss","601288.ss","601988.ss","601939.ss","601328.ss")
#工商，农业，中行，建设，交通
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
colnames(data2.re)=c("1398re","1288re","1988re","1939re","1328re")
#--------------------------------

#------------------Part two:Calculating VaR applying weibull-----------------------------
data2.re=data.frame(date=as.Date(rownames(data2.re)),data2.re)
data3.raw=data2.re[data2.re$date>as.Date("2016-05-21"),]

n=nrow(data3.raw)
wVaR=NA;
alpha=0.05;
#Xi contains return and VaR later
x1=data3.raw[,c(1,2)];x2=data3.raw[,c(1,3)];x3=data3.raw[,c(1,4)]
x4=data3.raw[,c(1,5)];x5=data3.raw[,c(1,6)]

weiVaR=function(x)
{
  index1=NA;index2=NA;
  for(i in 1:n)
  {
      index1[i]=which(x$date[i]==data2.re$date)
      index2=which(colnames(x)[2]==colnames(data2.re))
      re=sort(data2.re[(index1[i]-90):(index1[i]-1),index2])
      y=exp(re)
      
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
  return(wVaR)
}
#Xi contains return and VaR
VaR1398=weiVaR(x1);x1=data.frame(x1,VaR1398)
VaR1288=weiVaR(x2);x2=data.frame(x2,VaR1288)
VaR1988=weiVaR(x3);x3=data.frame(x3,VaR1988)
VaR1939=weiVaR(x4);x4=data.frame(x4,VaR1939)
VaR1328=weiVaR(x5);x5=data.frame(x5,VaR1328)
VaR=data.frame(x1$date,VaR1288,VaR1328,VaR1398,VaR1939,VaR1988)
colnames(VaR)[1]="date"
#-----------

#-----------------------Part three:plotting-------------------------
library(reshape);library(reshape2);library(ggplot2)
VaR.melt=melt(VaR,id="date")
ggplot(VaR.melt,aes(x=date,y=value,colour=variable))+geom_line()

#----------------------Part four:back testing---------------------
sum(x1[2]<x1[3])
sum(x2[2]<x2[3])
sum(x3[2]<x3[3])
sum(x4[2]<x4[3])
sum(x5[2]<x5[3])
