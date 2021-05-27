library(shiny)
library(tidyverse)
library(rsconnect)

ConsumerData <- read_csv("SCFP2019.csv")

ConsumerData %>% 
  mutate(ConsumerData, loan_ratio = EDN_INST/INCOME) %>%
  mutate(ConsumerData, payment_ratio = (PAYEDU1 + PAYEDU2 +PAYEDU3 +PAYEDU4 +PAYEDU5+PAYEDU6)*12/INCOME)->ConsumerData

ConsumerData<-select(ConsumerData,EDN_INST,PAYEDU1,PAYEDU2,PAYEDU3,PAYEDU4,PAYEDU5,PAYEDU6,PAYEDU7,INCOME,RACE,HHSEX,AGECL,loan_ratio,payment_ratio)

ConsumerData <- ConsumerData[ConsumerData$EDN_INST!=0,]

#Quantile Function
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

#import Histogram Function

source("HistFunction.R")

#App.R

ui <- fluidPage(
  titlePanel("Student Loan App"),
  sidebarLayout(
    sidebarPanel(
      selectInput('plot1', h4('Choose the Variable to Plot'), choices=c("Total_Loan_Amount","loan_ratio","payment_ratio"),selected="Total_Loan_Amount"),
      selectInput('add1', h4('Choose the Variable to Plot'),choices=c("None","GENDER","RACE","AGE"),selected="None"),
      selectInput('add2', h4('Choose the Variable to Plot'),choices=c("None","GENDER","RACE","AGE"),selected="None"),
    ),
  mainPanel(
    tabsetPanel(
    tabPanel('Plot',plotOutput(outputId = "LoanPlot")),
    tabPanel("Quantiles",tableOutput(outputId = "Qtable"))
     )
    )
  )
)


server <- function(input,output){
  output$LoanPlot <- renderPlot({
        x1=input$plot1
        x2=input$add1
        x3=input$add2
        
  hist_student_loans(DF=ConsumerData,x1,x2,x3)
  })
  output$Qtable<-renderTable({
    x1=input$plot1
    x2=input$add1
    
    calcquant(DF=ConsumerData,x1,x2)
    
  })
}

shinyApp(ui=ui, server=server)

