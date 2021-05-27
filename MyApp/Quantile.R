library(tidyverse)

calcquant<-function(DF,ResponseV,X1)
{
  if (ResponseV=="Total_Loan_Amount" && X1=='None')
  {quantile(ConsumerData$EDN_INST,na.rm=TRUE)}
  else if (ResponseV=="loan_ratio" && X1=='None')
    {quantile(ConsumerData$loan_ratio,na.rm=TRUE)}
  else if (ResponseV=="payment_ratio" && X1=='None')
  {quantile(ConsumerData$payment_ratio,na.rm=TRUE)}
  else if (ResponseV=="Total_Loan_Amount" && X1=="GENDER")
  {do.call("rbind", tapply(ConsumerData$EDN_INST, ConsumerData$HHSEX, quantile))} 
  else if (ResponseV=="Total_Loan_Amount" && X1=="RACE")
    {do.call("rbind", tapply(ConsumerData$EDN_INST, ConsumerData$RACE, quantile))}
  else if (ResponseV=="Total_Loan_Amount" && X1=="AGE")
    {do.call("rbind", tapply(ConsumerData$EDN_INST, ConsumerData$AGECL, quantile))}
  else if (ResponseV=="loan_ratio" && X1=="GENDER")
    {do.call("rbind", tapply(ConsumerData$loan_ratio, ConsumerData$HHSEX, quantile))}
  else if (ResponseV=="loan_ratio" && X1=="RACE")
    {do.call("rbind", tapply(ConsumerData$loan_ratio, ConsumerData$RACE, quantile))}
  else if (ResponseV=="loan_ratio" && X1=="AGE")
    {do.call("rbind", tapply(ConsumerData$loan_ratio, ConsumerData$AGECL, quantile))}
  else if (ResponseV=="payment_ratio" && X1=="GENDER")
    {do.call("rbind", tapply(ConsumerData$payment_ratio, ConsumerData$HHSEX, quantile,na.rm=TRUE))}
  else if (ResponseV=="payment_ratio" && X1=="RACE")
    {do.call("rbind", tapply(ConsumerData$payment_ratio, ConsumerData$RACE, quantile,na.rm=TRUE))}
  else if (ResponseV=="payment_ratio" && X1=="AGE")
    {do.call("rbind", tapply(ConsumerData$payment_ratio, ConsumerData$AGECL, quantile,na.rm=TRUE))}
}

