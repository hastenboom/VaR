#backtesting
if((1-pbinom(nrow(VaR[VaR$return<VaR$hVaR,]),252,0.05))>alpha)
  {print("Reject H0:m/p=alpha")
  else
  {print("Not Reject H0:m/p=alpha")}

  
if(1-pnorm(nrow(VaR[VaR$return<VaR$zVaR,]),252*alpha,252*alpha*(1-alpha)>alpha)
   {print("Reject H0:m/p=alpha")
   else
   {print("Not Reject H0:m/p=alpha")}
       
