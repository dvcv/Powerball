library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  headerPanel("Powerball"),
  #textOutput("intro"),
  sidebarLayout(
    
    sidebarPanel(
      textOutput("intro"),
      sliderInput("tickets", "Games", 1, 10400, 1,100),
      numericInput("tickets2", NULL, 1, 1,10400,100),
      helpText("Note: Only values between 1 and 10400 are accepted."),
      textOutput("grandPrize"),
      sliderInput("jackpot", "Jackpot", 60000000, 1000000000, 100000000,1000000),
      actionButton("play", "Play")
    ),mainPanel(tabsetPanel(
      tabPanel("Win Loss Comparison",plotOutput("plot1")),
      tabPanel("Winning Tickets",plotOutput("plot2")),
      tabPanel("Ticket Winnings",plotOutput("plot3")),
      tabPanel("Spent/Won/Lost",plotOutput("plot4")),
      tabPanel("Best Ticket",verbatimTextOutput("text1")),
      tabPanel("Summary",verbatimTextOutput("text2"))
      
   )
  )
 )
)


server <- function(input, output, session) {
  output$intro = renderText(
    print("This is a simple powerball simulator representing many games played over a lifetime. Assuming
          one ticket was played per game using quickpick numbers, how many games of powerball do you wish to play?")
    )
  
  observe({
    updateSliderInput(session,"tickets",value=input$tickets2)
  })
  
  observe({
    GRANDPRIZE <<- input$jackpot
  })
  
  play <- eventReactive(input$play, {
    input$tickets
  })
  GRANDPRIZE=100000000
  win.ticket=0
  winnings=0
  sum.5w.1r=0
  sum.5w=0
  sum.4w.1r=0
  sum.4w=0
  sum.3w.1r=0
  sum.3w=0
  sum.2w.1r=0
  sum.1w.1r=0
  sum.1r=0
  
  
  data = reactive({
    n = play()
    winning.ticket=0
    winning.ticket=list(winning.ticket)
    for (i in 1:n){ 
      winning.ticket[[i]]= c(sort(sample(69,5)),sample(26,1))
    }
    ticket=0
    ticket=list(ticket)
    for (i in 1:n){ 
      ticket[[i]]= c(sort(sample(69,5)),sample(26,1))
    }
    win.ticket=0
    winnings=0
    sum.5w.1r=0
    sum.5w=0
    sum.4w.1r=0
    sum.4w=0
    sum.3w.1r=0
    sum.3w=0
    sum.2w.1r=0
    sum.1w.1r=0
    sum.1r=0
    win.ticket=0
    win.ticket=list(win.ticket)
    for(i in 1:n){
      if (sum(ticket[[i]]==winning.ticket[[i]])==6){
        sum.5w.1r=sum.5w.1r+1
        win.ticket[[i]]=ticket[[i]]}
      else if(sum(ticket[[i]][1:5]==winning.ticket[[i]][1:5])==5){
        sum.5w=sum.5w+1
        win.ticket[[i]]=ticket[[i]]}
      else if(sum(ticket[[i]][1:5]%in%winning.ticket[[i]][1:5])==4 && sum(ticket[[i]][6]==winning.ticket[[i]][6])==1){
        sum.4w.1r=sum.4w.1r+1
        win.ticket[[i]]=ticket[[i]]}
      else if(sum(ticket[[i]][1:5]%in%winning.ticket[[i]][1:5])==4){
        sum.4w=sum.4w+1
        win.ticket[[i]]=ticket[[i]]}
      else if(sum(ticket[[i]][1:5]%in%winning.ticket[[i]][1:5])==3 && sum(ticket[[i]][6]==winning.ticket[[i]][6])==1){
        sum.3w.1r=sum.3w.1r+1
        win.ticket[[i]]=ticket[[i]]}
      else if(sum(ticket[[i]][1:5]%in%winning.ticket[[i]][1:5])==3){
        sum.3w=sum.3w+1
        win.ticket[[i]]=ticket[[i]]}
      else if(sum(ticket[[i]][1:5]%in%winning.ticket[[i]][1:5])==2 && sum(ticket[[i]][6]==winning.ticket[[i]][6])==1){
        sum.2w.1r=sum.2w.1r+1
        win.ticket[[i]]=ticket[[i]]}
      else if(sum(ticket[[i]][1:5]%in%winning.ticket[[i]][1:5])==1 && sum(ticket[[i]][6]==winning.ticket[[i]][6])==1){
        sum.1w.1r=sum.1w.1r+1
        win.ticket[[i]]=ticket[[i]]}
      else if(sum(ticket[[i]][6]==winning.ticket[[i]][6])==1){
        sum.1r=sum.1r+1
        win.ticket[[i]]=ticket[[i]]
      }
    }
    win.ticket<<-split(unlist(win.ticket), ceiling(seq_along(unlist(win.ticket))/6))
    winnings<<-sum.5w.1r*GRANDPRIZE+sum.5w*1000000+sum.4w.1r*50000+sum.4w*100+sum.3w.1r*100+sum.3w*7+sum.2w.1r*7+sum.1w.1r*4+sum.1r*4
    sum.5w.1r<<-sum.5w.1r
    sum.5w<<-sum.5w
    sum.4w.1r<<-sum.4w.1r
    sum.4w<<-sum.4w
    sum.3w.1r<<-sum.3w.1r
    sum.3w<<-sum.3w
    sum.2w.1r<<-sum.2w.1r
    sum.1w.1r<<-sum.1w.1r
    sum.1r<<-sum.1r
    
  })
  
  output$plot1 = renderPlot({
    data()
    x=c(sum.5w.1r,sum.5w,sum.4w.1r,sum.4w,sum.3w.1r,sum.3w,sum.2w.1r,sum.1w.1r,sum.1r)
    x = sum(x)
    barplot(c(x,play()-x),main = "Win Loss Comparison", names.arg = c("Winning Tickets", "Losing Tickets"), las = 1, col=rainbow(7))
    
  })
  
  output$plot2 = renderPlot({
    data()
    x=c(sum.5w.1r,sum.5w,sum.4w.1r,sum.4w,sum.3w.1r,sum.3w,sum.2w.1r,sum.1w.1r,sum.1r)
    x=c(x,sum(x))
    if(sum(x)==0){
      barplot(x,axes = F, ylim = c(0,1),las=2,names.arg=c("5W 1R","5W","4W 1R","4W","3W 1R","3W","2W 1R","1W 1R","1R","Total"),col=rainbow(9),main=paste("Winning Tickets"),ylab="Winning Tickets")
      axis(side = 2, at = c(0,1))   
   }else barplot(x,las=2,names.arg=c("5W 1R","5W","4W 1R","4W","3W 1R","3W","2W 1R","1W 1R","1R","Total"),col=rainbow(9),main=paste("Winning Tickets"),ylab="Winning Tickets")
  })
  
  output$plot3 = renderPlot({
    data()
    y=c("Grand Prize","1,000,000","50,000","100","100","7","7","4","4","Total")
    x=c(sum.5w.1r*GRANDPRIZE, sum.5w*1000000, sum.4w.1r*50000, sum.4w*100,sum.3w.1r*100, sum.3w*7, sum.2w.1r*7, sum.1w.1r*4, sum.1r*4,winnings)
    if(winnings==0){
      barplot(x,axes = F,ylim = c(0,1),las=2,names.arg=c("5W 1R","5W","4W 1R","4W","3W 1R","3W","2W 1R","1W 1R","1R","Total"),col=rainbow(9),main=paste("Ticket Winnings"),ylab="Money Won")
      axis(side = 2, at = c(0,1))
    }else barplot(x,las=2,names.arg=c("5W 1R","5W","4W 1R","4W","3W 1R","3W","2W 1R","1W 1R","1R","Total"),col=rainbow(9),main=paste("Ticket Winnings"),ylab="Money Won")
  })
  
  output$plot4 = renderPlot({
    data()
    n = play()
    barplot(c(2*n,winnings,abs(2*n-winnings)),col=c("green","red","gray"),names.arg=c("Spent","Won","Lost"),ylab="Money")
  })
  
  output$text1 = renderPrint({
    data()
    x=c(sum.5w.1r,sum.5w,sum.4w.1r,sum.4w,sum.3w.1r,sum.3w,sum.2w.1r,sum.1w.1r,sum.1r)
    best.ticket=x
    q=best.ticket[7]
    w=best.ticket[6]
    best.ticket[6]=q
    best.ticket[7]=w
    la=TRUE
    while(la){
      if(length(which(best.ticket==0))==9){
        cat("\n","No Tickets Won","\n","\n")
        break
      }
      best.ticket=which(best.ticket!=0)[1]
      if (best.ticket==1){
        cat("\n","Your best ticket had  5 Whites + Powerball Matched ($Grand Prize)","\n","\n")
        break
      }
      if(best.ticket==2){ 
        cat("\n","Your best ticket had  5 Whites Matched ($1,000,000","\n","\n")
        break
      }
      if(best.ticket==3){ 
        cat("\n","Your best ticket had  4 Whites + Powerball Matched ($50,000)","\n","\n")
        break
      }
      if(best.ticket==4){ 
        cat("\n","Your best ticket had  4 Whites Matched ($100)","\n","\n")
        break
      }
      if(best.ticket==5){ 
        cat("\n","Your best ticket had  3 Whites + Powerball Matched ($100)","\n","\n")
        break
      }
      if(best.ticket==6){ 
        cat("\n","Your best ticket had  2 Whites + Powerball Matched ($7)","\n","\n")
        break
      }
      if(best.ticket==7){
        cat("\n","Your best ticket had  3 Whites Matched ($7)","\n","\n")
        break
      }
      if(best.ticket==8){ 
        cat("\n","Your best ticket had  1 White + Powerball Matched ($4)","\n","\n")
        break
      }
      if(best.ticket==9){ 
        cat("\n","Your best ticket had  Power Ball Matched ($4)","\n","\n")
        break
      }
    }
  })
  
  output$text2 = renderPrint({
    data()
    winning2 = c(sum.5w.1r*GRANDPRIZE, sum.5w*1000000, sum.4w.1r*50000, sum.4w*100, sum.3w.1r*100, sum.3w*7, sum.2w.1r*7, sum.1w.1r*4, sum.1r*4)
    x=c(sum.5w.1r,sum.5w,sum.4w.1r,sum.4w,sum.3w.1r,sum.3w,sum.2w.1r,sum.1w.1r,sum.1r)
    x=c(sum(x))
    n = play()
    if(as.integer((2*n)-winnings)>=0){
      cat(  "\n", "In your lifetime you spent $", as.integer(2*n),", won $", as.integer(winnings),
            ", and lost $", (2*n)-winnings,".","\n",
            "From the",  n , "ticket(s) you purchased", x, "ticket(s) were winners.","\n","\n")
            m = matrix(c(
            sum.5w.1r, winning2[1],  
            sum.5w,    winning2[2],  
            sum.4w.1r, winning2[3],  
            sum.4w,    winning2[4],  
            sum.3w.1r, winning2[5],  
            sum.3w,    winning2[6],  
            sum.2w.1r, winning2[7],  
            sum.1w.1r, winning2[8],  
            sum.1r,    winning2[9]     
            ),nrow = 9,ncol = 2, byrow = T)
            colnames(m) <- c( "Freq", "Won")
            rownames(m) <- c("5 White+Powerball",    
                             "5 White",  
                             "4 White+Powerball",  
                             "4 White",    
                             "3 White+Powerball",   
                             "3 White",     
                             "2 White+Powerball",      
                             "1 White+Powerball",    
                             "Powerball ")
            as.table(m)
    }else{
      cat(  "\n", "In your lifetime you spent $", as.integer(2*n),", won $", as.integer(winnings),
            ", and profited $", -((2*n)-winnings),".","\n",
            "From the",  n , "ticket(s) you purchased", x, "ticket(s) were winners.","\n","\n")
              m = matrix(c(
              sum.5w.1r, winning2[1],  
              sum.5w,    winning2[2],  
              sum.4w.1r, winning2[3],  
              sum.4w,    winning2[4],  
              sum.3w.1r, winning2[5],  
              sum.3w,    winning2[6],  
              sum.2w.1r, winning2[7],  
              sum.1w.1r, winning2[8],  
              sum.1r,    winning2[9]     
            ),nrow = 9,ncol = 2, byrow = T)
            colnames(m) <- c( "Freq", "Won")
            rownames(m) <- c("5 White+Powerball",    
                             "5 White",  
                             "4 White+Powerball",  
                             "4 White",    
                             "3 White+Powerball",   
                             "3 White",     
                             "2 White+Powerball",      
                             "1 White+Powerball",    
                             "Powerball ")
            as.table(m)
  
    }
  })
  
}

shinyApp(ui = ui, server = server)