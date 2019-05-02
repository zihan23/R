#shiny application
library(shiny)

ui<-fluidPage(
  
  #title
  titlePanel("HW11"),
  
  #sidebar with slider input
  sidebarLayout(
    sidebarPanel(
      #sliderInput("bins",
      #           "Numbers of bins:"
      #         min=1,
      #         max=50,
      #         value=30)
      selectInput("stock1","Stock:",
                  choices = colnames(StockData)),
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
                  choices = colnames(StockData)),
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
      #sliderInput("bins",
      #           "Numbers of bins:"
      #         min=1,
      #         max=50,
      #         value=30)
      selectInput("stock3","Stock:",
                  choices= colnames(StockData)),
      selectInput("stock4","Stock:",
                  choices= colnames(StockData)),
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
      sliderInput("confidence",
                  "confidence interval:",
                  min=0.01,
                  max=1.00,
                  value=0.50),
      #selectInput("stock","Stock:",
      #           choices= colnames(StockData)),
      hr(),
      helpText("Data from Stockdata,")
    ),
    
    
    #show a plot of the genrated distribution
    mainPanel(
      tableOutput("table")
    )
  )
  
)



#define server logic
server<-function(input,output){
  
  output$distPlot<-renderPlot({
    #generate bins
    x<-StockData
    #bins<-seq(min(x),max(x)...)
    bins<-20
    #draw histogram
    hist(StockData[,input$stock1], breaks = bins, main = input$stock1, xlab = "frequency", ylab = "log_return", col ='blue', border = 'white')
  })
  
  output$normalPlot<-renderPlot({
    #generate bins
    x<-StockData
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
    s<-sd(data)
    error<-qnorm(1-alpha/2)*s/sqrt(n)
    upper_mean<-xbar+error
    lower_mean<-xbar-error
    ci<-c(lower_mean,upper_mean)
    return(ci)
  }
  
  tableformation<-function(data,alpha)
  {
    lower<-rep(0,10)
    upper<-rep(0,10)
    for (i in (1:10))
    {
      a<-ci_mean(StockData[,i],alpha)
      lower[i]<-a[1]
      upper[i]<-a[2]
    }
    data1<-data.frame(c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD'),lower,upper)
    colnames(data1)<-c("stock name","lower-tail","upper-tail")
    rownames(data1)<-c('AET','BSX','CELG','CVS','JNJ','MYL','MTD','HSIC','XRAY','GILD')
    return(format(data1,digits=4))
  }
  
  output$table <- renderTable({
    tableformation(StockData,1-input$confidence)
    
  })
  
  
  
}

#run application
shinyApp(ui=ui,server = server)
