#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(geosphere)
library(knitr)

set.seed(12771)
options(scipen = 99999999)

load("taxi.RData")

thm = function() {
  theme_gray(base_family = "黑體-繁 中黑") + theme(text=element_text(size=18))
}

# 從 Google Maps API 導航的資料裡，取得開車的部分。

taxi.trip = data.frame()

for (i in 1:length(taxi)) {
  x1 = taxi[i][[1]]$routes[[1]]$legs[[1]]$start_location$lat[1]
  x2 = taxi[i][[1]]$routes[[1]]$legs[[1]]$end_location$lat[1]
  y1 = taxi[i][[1]]$routes[[1]]$legs[[1]]$start_location$lng[1]
  y2 = taxi[i][[1]]$routes[[1]]$legs[[1]]$end_location$lng[1]
  
  if (taxi[i][[1]]$routes[[1]]$legs[[1]]$steps[[1]]$travel_mode != "DRIVING") {
    next
  }
  
  Temp = data.frame(i = i,
                    d = distm(c(y1,x1),c(y2,x2),fun=distHaversine)[1],
                    m = taxi[i][[1]]$routes[[1]]$legs[[1]]$distance$value[1],
                    t = round(taxi[i][[1]]$routes[[1]]$legs[[1]]$duration$value[1]/60,2))
  taxi.trip = rbind(taxi.trip,Temp,stringsAsFactors=F)
  rm(x1);rm(y1);rm(x2);rm(y2);rm(Temp)
  
}



taxi.trip = mutate(taxi.trip,fee = round((100+round(ifelse((m-1000)<0,0,m-1000)/250,0)*5),0),
                   nightfee=round((100+round(ifelse((m-800)<0,0,m-800)/200,0)*5),0))



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("酒後計程車費補貼策略成本估計"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # sliderInput("cars",
         #             "估計一日最小最大車次：",
         #             min = 1,
         #             max = 800,
         #             value = c(1,100)),
        numericInput("carsmin",
                   "你覺得一天可能的最少車次：",value = 1),
        numericInput("carsmax",
                   "你覺得一天可能的最大車次：",value = 150),
        numericInput("carsmean",
                   "你覺得一天可能平均有多少車次：",value = 75),
         selectInput("plan","補貼策略：",choices = c("固定金額","固定比例"),multiple = F,selected = "固定比例"),
        numericInput("money",
                   "補貼比例或金額：",value = 0.1),
         selectInput("selfpay","有最小自付額嗎？",choices = c("是","否"),multiple = F,selected = "是"),
        numericInput("selfpaymoney",
                   "最小自付額（元）：",value = 100)
      ),
      
      # Show a plot of the generated distribution
      
      mainPanel(
        
        #
        
        plotOutput("distPlot"),
        textOutput("text1"),
        plotOutput("distPlot2"),
         textOutput("text2")#,
        # plotOutput("distPlot3"),
        # textOutput("text3")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
     s.mean = as.numeric(input$carsmean)
     s.cars = rnorm(n = 365,mean = s.mean,sd=s.sd) %>% round(.,0)
     s.cars <<- ifelse(s.cars<0,1,s.cars)
     
     #日計程車費成本分佈
     taxi.est = data.frame(cars=s.cars)
     taxi.est$taxiFee=0
     for ( i in 1:nrow(taxi.est)) {
       taxi.est$taxiFee[i] = sum(taxi.trip$fee[sample(nrow(taxi.trip),abs(taxi.est$cars[i]),replace=T)])
     }
     taxi.est<<-taxi.est
     
     #年計程車費成本分佈
     taxi.est.y = data.frame()
     for ( i in 1:30) {
       Temp1 = s.cars
       Temp2 = sapply(X = Temp1,FUN = function (x) {taxi.trip$fee[sample(nrow(taxi.trip),abs(x),replace=T)] %>% sum()})
       
       taxi.est.y = rbind(taxi.est.y,
                          cbind(sum(Temp1),sum(Temp2)))
       rm(Temp1);rm(Temp2)
     }
     names(taxi.est.y) = c("cars","taxiFee")
     taxi.est.y <<- taxi.est.y
     
     ggplot(taxi.est,aes(x=taxiFee))+geom_density()+ggtitle("Single Day Total Taxi fee")
     
   })
   
   output$text1 <- renderText({
     s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
     s.mean = as.numeric(input$carsmean)
     paste0("單日計程車費成本可能落在", quantile(taxi.est$taxiFee[order(taxi.est$taxiFee)],0.1),"元到", quantile(taxi.est$taxiFee[order(taxi.est$taxiFee)],0.9),"元之間。（90%信賴區間）")
   })
   
   output$distPlot2 <- renderPlot({
     
     s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
     s.mean = as.numeric(input$carsmean)
     # s.cars = rnorm(n = 365,mean = s.mean,sd=s.sd)
     # s.cars = ifelse(s.cars<0,0,s.cars)
     
     ggplot(taxi.est.y,aes(x=taxiFee))+geom_density()+ggtitle("Single Years Total Taxi fee")
   })
   
   output$text2 <- renderText({
     s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
     s.mean = as.numeric(input$carsmean)
     paste0("一年的計程車費成本可能落在", quantile(taxi.est.y$taxiFee[order(taxi.est.y$taxiFee)],0.1)[1],"元到", quantile(taxi.est.y$taxiFee[order(taxi.est.y$taxiFee)],0.9)[1],"元之間。（90%信賴區間）")
   })
   
   # output$distPlot3 <- renderPlot({
   #   
   #   s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
   #   s.mean = as.numeric(input$carsmean)
   #   # s.cars = rnorm(n = 365,mean = s.mean,sd=s.sd)
   #   # s.cars = ifelse(s.cars<0,0,s.cars)
   #   
   #   if (input$plan == "固定金額") {
   #     Temp1 = 1
   #   } else {
   #     Temp1 = 2
   #   }
   #   
   #   Temp2 = as.numeric(input$money) 
   #   
   #   if (input$selfpay=="是") {
   #     Temp3 = as.numeric(input$selfpaymoney)
   #   } else {
   #     Temp3 = 0
   #   }
   #   
   #   if (Temp1==1) {
   #     if (Temp3!=0) {
   #       Temp = taxi.est.y-Temp3
   #       Temp = ifelse(Temp<0,0,Temp)
   #       Temp = ifelse((Temp-Temp2)<0,Temp,Temp2)
   #       Temp <<- Temp
   #     } else {
   #       Temp = taxi.est.y
   #       Temp = ifelse((Temp-Temp2)<0,Temp,Temp2)
   #       Temp <<- Temp
   #     }
   #   } else {
   #     if (Temp3!=0) {
   #       Temp = taxi.est.y-Temp3
   #       Temp = ifelse(Temp<0,0,Temp)
   #       Temp = Temp*Temp2
   #       Temp <<- Temp
   #     } else {
   #       Temp = taxi.est.y
   #       Temp = Temp*Temp2
   #       Temp <<- Temp
   #     }
   #   }
   #   
   #   ggplot(Temp,aes(x=taxiFee))+geom_density()+ggtitle("Single Years Total Payout")
   # })
   # 
   # output$text3 <- renderText({
   #   s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
   #   s.mean = as.numeric(input$carsmean)
   #   paste0("一年的補貼成本可能落在", quantile(Temp$taxiFee[order(Temp$taxiFee)],0.1)[1],"元到", quantile(Temp$taxiFee[order(Temp$taxiFee)],0.9)[1],"元之間。（90%信賴區間）")
   # })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

