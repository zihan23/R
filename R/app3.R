#shiny application
library(shiny)
library(shinythemes)
StockData<-read.csv(file="/Users/zihanwang/Documents/Zihan/Columbia/2018 Fall/Intro to Probability and Statistics/R/StockData.csv",header=TRUE, sep=",")
StockData<-StockData[,-1]
ui<-fluidPage(
  navbarPage(
    themeSelector(),
    tabPanel('PreVisualization',
  #title
  titlePanel("Investment in Healthcare Industry"),
  
  #sidebar with slider input
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
      #           "Numbers of bins:"
      #         min=1,
      #         max=50,
      #         value=30)
      selectInput("stock1","Stock:",
                  choices = colnames(StockData[1:10])),
      hr(),
      helpText("Data frm Stockdata,")
    ),
    
    
    #show a plot of the genrated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
      #           "Numbers of bins:"
      #         min=1,
      #         max=50,
      #         value=30)
      selectInput("stock2","Stock:",
                  choices = colnames(StockData[1:10])),
      hr(),
      helpText("Data from Stockdata,")
    ),
    
    
    #show a plot of the genrated distribution
    mainPanel(
      plotOutput("normalPlot")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("confidence",
                  "confidence interval:",
                  min=0.01,
                  max=1.00,
                  value=0.50),
      #selectInput("stock","Stock:",
      #           choices= colnames(StockData[2:11])),
      hr(),
      helpText("Data from Stockdata,")
    ),
    
    
    #show a plot of the genrated distribution
    mainPanel(
      tableOutput("table")
    ))
  ),
  
  tabPanel('Inner-Industry Analysis',
  titlePanel("Compare of Stock in Healthcare Industry"),
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
      #           "Numbers of bins:"
      #         min=1,
      #         max=50,
      #         value=30)
      selectInput("stock3","Stock:",
                  choices= colnames(StockData[1:10])),
      selectInput("stock4","Stock:",
                  choices= colnames(StockData[1:10])),
      hr(),
      helpText("Data from Stockdata,")
    ),
    
    
    #show a plot of the genrated distribution
    mainPanel(
      textOutput("ttestresult")   ##
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      p("Select the inputs for the Dependent Variable"),
      selectInput("DepVar", "Dependent Variables", choices = colnames(StockData[1:10])),
      p("Select the inputs for the Independent Variable"),
      selectInput("IndVar", "Independent Variables", choices = colnames(StockData[1:10]))
    ),
    mainPanel(
      verbatimTextOutput(outputId = "RegDes1"),
      plotOutput(outputId = "RegPlot1"),
      plotOutput(outputId = "RegResi1")
      #plotOutput("hist")
    )
  )),
  
  
  tabPanel('Time Series Analysis',
  titlePanel(" Regression Analysis of Stock Price against time"),
  #show regression result
  sidebarLayout(
    sidebarPanel(
      p("Select the inputs for the Stock"),
      selectInput("Stock5", "Stock", choices = colnames(StockData[1:10])),
      p("Select the inputs for the Factors"),
      selectInput("Factor", "Factors", choices = colnames(StockData[11:21]))
    ),
    mainPanel(
      verbatimTextOutput(outputId = "RegDes2"),
      plotOutput(outputId = "RegPlot2"),
      plotOutput(outputId = "RegResi2")
      #plotOutput("hist")
    )
  )
)))




#define server logic
server<-function(input,output){
  
  output$distPlot<-renderPlot({
    #generate bins
    x<-StockData[1:10]
    #bins<-seq(min(x),max(x)...)
    bins<-20
    #draw histogram
    hist(StockData[,input$stock1], breaks = bins, main = input$stock1, xlab = "frequency", ylab = "log_return", col ='blue', border = 'white')
  })
  
  output$normalPlot<-renderPlot({
    #generate bins
    x<-StockData[1:10]
    #bins<-seq(min(x),max(x)...)
    bins<-20
    #draw histogram
    qqnorm(StockData[,input$stock2], main = input$stock2, xlab = "frequency", ylab = "log returns", col ='red')
  })
  
  
  output$ttestresult<-renderPrint({
    print(t.test(StockData[,input$stock3],StockData[,input$stock4])[1])
    print(t.test(StockData[,input$stock3],StockData[,input$stock4])[2])
  })
  
  
  ci_mean<-function(data,alpha)
  {
    n<-length(data)
    xbar<-mean(data)
    df<-length(data)-1
    s<-sd(data)
    error<-qnorm(1-alpha/2)*s/sqrt(n)
    first_value<-qchisq(1/2*alpha,df)
    second_value<-qchisq(1-1/2*alpha,df)
    upper_mean<-xbar+error
    lower_mean<-xbar-error
    upper_var<-df*s^2/first_value
    lower_var<-df*s^2/second_value
    ci<-c(lower_mean,upper_mean,lower_var,upper_var)
    return(ci)
  }
  
  tableformation<-function(data,alpha)
  {
    lower1<-rep(0,10)
    upper1<-rep(0,10)
    lower2<-rep(0,10)
    upper2<-rep(0,10)
    for (i in (1:10))
    {
      a<-ci_mean(StockData[,i],alpha)
      lower1[i]<-a[1]
      upper1[i]<-a[2]
      lower2[i]<-a[3]
      upper2[i]<-a[4]
      
    }
    data1<-data.frame(c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD'),lower1,upper1,lower2,upper2)
    colnames(data1)<-c("stock name","lower-mean","upper-mean","lower-variance","upper-variance")
    rownames(data1)<-c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD')
    return(format(data1,digits=4))
  }
  
  
  output$table <- renderTable({
    tableformation(StockData,1-input$confidence)})
  
  lm2 = reactive({lm(StockData[,input$Stock5]~StockData[,input$Factor],data=StockData)})
  
  output$RegDes2 <- renderPrint({
    print(paste0("The results of regression between ", input$Stock5, " against ", input$Factor, " are:"))
    print(summary(lm2()))})
  output$RegPlot2 <- renderPlot({
    with(StockData, plot(StockData[,input$Factor], StockData[,input$Stock5]))
    abline(lm2())
  })
  
  output$RegResi2 <- renderPlot({
    plot(resid(lm2())) 
  })
  
  lm1 = reactive({lm(StockData[,input$DepVar]~StockData[,input$IndVar],data=StockData)})
  
  output$RegDes1 <- renderPrint({
    print(paste0("The results of regression between ", input$DepVar, " against ", input$IndVar, " are:"))
    print(summary(lm1()))})
  output$RegPlot1 <- renderPlot({
    with(StockData, plot(StockData[,input$IndVar], StockData[,input$DepVar]))
    abline(lm1())
  })
  
  output$RegResi1 <- renderPlot({
    plot(resid(lm1())) 
  })
  
  
  
  
}

#run application
shinyApp(ui=ui,server = server)
