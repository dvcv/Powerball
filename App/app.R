library(shiny)
ui <- fluidPage(
  headerPanel("Powerball"),
  textOutput("intro"),
  textOutput("howManyTickets"),
  sliderInput("tickets", "Games", 1, 10400, 1000,5),
  actionButton("play", "Play"),
  plotOutput("plot1"),
  plotOutput("plot2"),
  plotOutput("plot3"),
  titlePanel("Best Ticket"),
  textOutput("text1"),
  titlePanel("Summary"),
  textOutput("text2")
  
  
)


server <- function(input, output, session) {
  output$intro = renderText(
    print("This is a simple powerball simulater representing many games played over a lifetime where 1 ticket was played per game. 
          Assuming you played quickpick numbers every time.")
    )
  
  output$howManyTickets = renderText(
    print("How many games of powerball do you wish to play?"))
  
  play <- eventReactive(input$play, {
    input$tickets
  })
  GRANDPRIZE=60000000
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
    length(win.ticket)
    x=c(sum.5w.1r,sum.5w,sum.4w.1r,sum.4w,sum.3w.1r,sum.3w,sum.2w.1r,sum.1w.1r,sum.1r)
    x=c(x,sum(x))
    barplot(x,las=2,names.arg=c("5W 1R","5W","4W 1R","4W","3W 1R","3W","2W 1R","1W 1R","1R","Total"),col=rainbow(10),main=paste("Winning Tickets"),ylab="Winning Tickets")
  })
  
  output$plot2 = renderPlot({
    data()
    y=c("Grand Prize","1,000,000","50,000","100","100","7","7","4","4","Total")
    x=c(sum.5w.1r*GRANDPRIZE, sum.5w*1000000, sum.4w.1r*50000, sum.4w*100,sum.3w.1r*100, sum.3w*7, sum.2w.1r*7, sum.1w.1r*4, sum.1r*4,winnings)
    barplot(x,las=2,names.arg=c("5W 1R","5W","4W 1R","4W","3W 1R","3W","2W 1R","1W 1R","1R","Total"),col=rainbow(10),main=paste("Ticket Winnings"),ylab="Money Won")
  })
  
  output$plot3 = renderPlot({
    data()
    n = play()
    barplot(c(2*n,winnings,abs(2*n-winnings)),col=c(44:46),names.arg=c("Spent on Tickets","Won","Lost"),ylab="Money")
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
        cat("No Tickets Won")
        break
      }
      best.ticket=which(best.ticket!=0)[1]
      if (best.ticket==1){
        cat("Your best ticket had  5 Whites + Powerball Matched ($Grand Prize)")
        break
      }
      if(best.ticket==2){ 
        cat("Your best ticket had  5 Whites Matched ($1,000,000")
        break
      }
      if(best.ticket==3){ 
        cat("Your best ticket had  4 Whites + Powerball Matched ($50,000)")
        break
      }
      if(best.ticket==4){ 
        cat("Your best ticket had  4 Whites Matched ($100)")
        break
      }
      if(best.ticket==5){ 
        cat("Your best ticket had  3 Whites + Powerball Matched ($100)")
        break
      }
      if(best.ticket==6){ 
        cat("Your best ticket had  2 Whites + Powerball Matched ($7)")
        break
      }
      if(best.ticket==7){
        cat("Your best ticket had  3 Whites Matched ($7)")
        break
      }
      if(best.ticket==8){ 
        cat("Your best ticket had  1 White + Powerball Matched ($4)")
        break
      }
      if(best.ticket==9){ 
        cat("Your best ticket had  Power Ball Matched ($4)")
        break
      }
    }
  })
  
  output$text2 = renderPrint({
    data()
    x=c(sum.5w.1r,sum.5w,sum.4w.1r,sum.4w,sum.3w.1r,sum.3w,sum.2w.1r,sum.1w.1r,sum.1r)
    x=c(sum(x))
    n = play()
    if(as.integer((2*n)-winnings)>=0){
    cat(  "In your lifetime you spent $", as.integer(2*n),"and won $", as.integer(winnings),"", ".\n",
          "From the",  n , "ticket(s) you purchased", x, "ticket(s) were winners.","\n",
          "You lost $", (2*n)-winnings,".","\n","\n",
          "You got 5 White + Powerball   ", sum.5w.1r, "time(s).","\n",
          "You got 5 White               ", sum.5w, "time(s).","\n",
          "You got 4 White + Powerball   ", sum.4w.1r, "time(s).","\n",
          "You got 4 White               ", sum.4w, "time(s),","\n",
          "You got 3 White + Powerball   ", sum.3w.1r, "time(s).","\n",
          "You got 3 White               ", sum.3w, "time(s),","\n",
          "You got 2 White + Powerball   ", sum.2w.1r, "time(s).","\n",
          "You got 1 White + Powerball   ", sum.1w.1r, "time(s).","\n",
          "You got only Powerball             ", sum.1r, "time(s)."
      )
    }else{
      cat(  "In your lifetime you spent $", as.integer(2*n),"and won $", as.integer(winnings),"", ".\n",
            "From the",  n , "ticket(s) you purchased", x, "ticket(s) were winners.","\n",
            "You won $", -((2*n)-winnings),".","\n","\n",
            "You got 5 White + Powerball   ", sum.5w.1r, "time(s).","\n",
            "You got 5 White               ", sum.5w, "time(s).","\n",
            "You got 4 White + Powerball   ", sum.4w.1r, "time(s).","\n",
            "You got 4 White               ", sum.4w, "time(s),","\n",
            "You got 3 White + Powerball   ", sum.3w.1r, "time(s).","\n",
            "You got 3 White               ", sum.3w, "time(s),","\n",
            "You got 2 White + Powerball   ", sum.2w.1r, "time(s).","\n",
            "You got 1 White + Powerball   ", sum.1w.1r, "time(s).","\n",
            "You got only Powerball             ", sum.1r, "time(s)."
      )
      
    }
  })
  
}

shinyApp(ui = ui, server = server)