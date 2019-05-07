# Historical simulation
xdata=getSymbols("600007.ss",src="yahoo",from="1999-03-12",to="2018-03-30",auto.assign = F)
colnames(xdata)=c("open","high","low","close","volume","adjusted")

xdata=data.frame(date=index(xdata),xdata)
xdata$re=NA;
xdata$re[2:nrow(xdata)]=diff(xdata$close)/xdata$close

#those data calculted VaR(present)
cdata=xdata[xdata$date>as.Date("2017-12-31"),]
n=nrow(cdata)


hVaR=NA;
zVaR=NA;
wVaR=NA;

alpha=0.05;

for(i in 1:n)
{
  
  index[i]=which(cdata$date[i]==xdata$date)
  re=sort(xdata$re[(index[i]-252):(index[i]-1)])
  hVaR[i]=(re[trunc(252*alpha)]+re[(trunc(252*alpha)+1)])/2
  zVaR[i]=qnorm(alpha,mean(re),sd(re))

  
}

VaR=data.frame(date=cdata$date,return=cdata$re,hVaR=hVaR,zVaR=zVaR)
VaR_melt=melt(VaR,id="date")
ggplot(VaR_melt,aes(x=date,y=value,color=variable))+geom_line()
