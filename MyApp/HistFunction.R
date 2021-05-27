library(tidyverse)


hist_student_loans<-function(DF,ResponseV,X1,X2)
{
  if (ResponseV == "Total_Loan_Amount"){
  if (X1=="None") { 
    if (X2=="None") {
      ggplot(data=DF) + geom_histogram(aes(EDN_INST))+ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
    }
    else if (X2 == "GENDER"){
      ggplot(data=DF) + geom_histogram(aes(EDN_INST,fill=factor(HHSEX)))+ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +facet_wrap(~HHSEX)
    }
    else if (X2 == "AGE"){
      ggplot(data=DF) + geom_histogram(aes(EDN_INST,fill=factor(AGECL)))+ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +facet_wrap(~AGECL)
    }
    else if (X2 == "RACE"){
      ggplot(data=DF) + geom_histogram(aes(EDN_INST,fill=factor(RACE)))+scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ylab("Percentage of People with Loan")+xlab("Total Amountof Loan")+ggtitle("Total Student Loan Amounts")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~RACE)
    }
  }
  else if (X1 == "GENDER"){
    if (X2=="None"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
    }
    else if (X2 == "AGE"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~AGECL)
      
    }
    else if (X2 == "RACE"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~RACE)   
    }
    else if (X2 == "GENDER"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~HHSEX)
    }
    
  }
  else if (X1 == "AGE"){
    if (X2=="None"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
    }
    else if (X2 == "RACE"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~RACE)
    }
    else if (X2 == "GENDER"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~HHSEX)
    }
    else if (X2 == "AGE"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Total Student Loan Amounts")+ylab("Percentage of People with Loan") + xlab("Total Amount of Loan") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~AGECL)  
    }
  }
  else if (X1 == "RACE"){
    if (X2=="None"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ylab("Percentage of People with Loan")+xlab("Total Amountof Loan")+ggtitle("Total Student Loan Amounts") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
    }
    else if (X2 == "GENDER"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ylab("Percentage of People with Loan")+xlab("Total Amountof Loan")+ggtitle("Total Student Loan Amounts") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~HHSEX)
    }
    else if (X2 == "AGE"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ylab("Percentage of People with Loan")+xlab("Total Amountof Loan")+ggtitle("Total Student Loan Amounts") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~AGECL)
    }
    else if (X2 == "RACE"){
      ggplot(data=DF,aes(EDN_INST,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ylab("Percentage of People with Loan")+xlab("Total Amountof Loan")+ggtitle("Total Student Loan Amounts")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~RACE)
    }
  }
}
  else if(ResponseV == "loan_ratio"){
    if (X1=="None") { 
      if (X2=="None") {
        ggplot(data=DF) + geom_histogram(aes(loan_ratio))+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      }
      else if (X2 == "GENDER"){
        ggplot(data=DF) + geom_histogram(aes(loan_ratio,fill=factor(HHSEX)))+gggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +facet_wrap(~HHSEX)
      }
      else if (X2 == "AGE"){
        ggplot(data=DF) + geom_histogram(aes(loan_ratio,fill=factor(AGECL)))+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +facet_wrap(~AGECL)
      }
      else if (X2 == "RACE"){
        ggplot(data=DF) + geom_histogram(aes(loan_ratio,fill=factor(RACE)))+scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~RACE)
      }
    }
    else if (X1 == "GENDER"){
      if (X2=="None"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      }
      else if (X2 == "AGE"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~AGECL)
        
      }
      else if (X2 == "RACE"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~RACE)   
      }
      else if (X2 == "GENDER"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~HHSEX)
      }
      
    }
    else if (X1 == "AGE"){
      if (X2=="None"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      }
      else if (X2 == "RACE"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~RACE)
      }
      else if (X2 == "GENDER"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~HHSEX)
      }
      else if (X2 == "AGE"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~AGECL)  
      }
    }
    else if (X1 == "RACE"){
      if (X2=="None"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      }
      else if (X2 == "GENDER"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~HHSEX)
      }
      else if (X2 == "AGE"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~AGECL)
      }
      else if (X2 == "RACE"){
        ggplot(data=DF,aes(loan_ratio,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Loan Ratio")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~RACE)
      }
    }    
  }
  else if(ResponseV == "payment_ratio"){
    if (X1=="None") { 
      if (X2=="None") {
        ggplot(data=DF) + geom_histogram(aes(payment_ratio))+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      }
      else if (X2 == "GENDER"){
        ggplot(data=DF) + geom_histogram(aes(payment_ratio,fill=factor(HHSEX)))+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +facet_wrap(~HHSEX)
      }
      else if (X2 == "AGE"){
        ggplot(data=DF) + geom_histogram(aes(payment_ratio,fill=factor(AGECL)))+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +facet_wrap(~AGECL)
      }
      else if (X2 == "RACE"){
        ggplot(data=DF) + geom_histogram(aes(payment_ratio,fill=factor(RACE)))+scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +gggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~RACE)
      }
    }
    else if (X1 == "GENDER"){
      if (X2=="None"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      }
      else if (X2 == "AGE"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~AGECL)
        
      }
      else if (X2 == "RACE"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~RACE)   
      }
      else if (X2 == "GENDER"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(HHSEX))) + geom_histogram(position="dodge2")+ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="GENDER",labels=c("Male","Female"),values=c("1"="red","2"="blue"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~HHSEX)
      }
      
    }
    else if (X1 == "AGE"){
      if (X2=="None"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      }
      else if (X2 == "RACE"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~RACE)
      }
      else if (X2 == "GENDER"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~HHSEX)
      }
      else if (X2 == "AGE"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(AGECL))) + geom_histogram(position="dodge2") +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_fill_manual(name="AGE",labels=c("<35","35-44","45-54","55-64","65-74","75+"),values=c("1"="red","2"="blue","3" ="green","4"="purple","5"="orange","6"="pink"))+scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+facet_wrap(~AGECL)  
      }
    }
    else if (X1 == "RACE"){
      if (X2=="None"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE))
      }
      else if (X2 == "GENDER"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~HHSEX)
      }
      else if (X2 == "AGE"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio") + scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~AGECL)
      }
      else if (X2 == "RACE"){
        ggplot(data=DF,aes(payment_ratio,fill=factor(RACE))) + geom_histogram(position="dodge2") + scale_fill_manual(name="RACE",labels=c("White","Black","Hispanic","Other"),values=c("5"="purple","3"="blue","2"="green","1"="red")) +ggtitle("Ratio of Student Loan Amount to Income")+ylab("Percentage of People with Loan") + xlab("Payment Ratio")+scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + facet_wrap(~RACE)
      }
    }    
  }
}

