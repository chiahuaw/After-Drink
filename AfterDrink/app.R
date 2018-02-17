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

#load("taxi.RData")
load("taxiTrip.RData")

thm = function() {
  theme_gray(base_family = "黑體-繁 中黑") + theme(text=element_text(size=18))
}




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
                   "補貼比例或金額：",value = 0.5),
         selectInput("selfpay","有最小自付額嗎？",choices = c("是","否"),multiple = F,selected = "否"),
        numericInput("selfpaymoney",
                   "最小自付額（元）：",value = 0)
      ),
      
      # Show a plot of the generated distribution
      
      mainPanel(
        
        #
        HTML("<p>這是1月底時看見臉書社群「關心金門者」上在討論酒後代駕或計程車費補貼的事，來具體減少金門酒駕的問題。我一開始只是好奇成本是多少，所以用了常態分配、GoogleMapsAPI取得計程車模擬路徑，來計算可能的計程車成本。</p>"),
        HTML("<p>因為缺乏市場需求數量的資料，造成在酒後計程車資補貼的成本估計上，不同人估計的需求數量會有很大的落差。例如不常喝酒的人，可能認為一天平均會有10台車的需求；喝喝酒的人卻認為一天平均會有100台車。之類的。</p>"),
        HTML("<p>需求量會直接影響酒後計程車資的成本（或代駕），而我又沒有資料可以用；再加上不同補貼策略、金額也會影響成本，大家認為可以接受的成本可能也不一樣。總之就是變數很大。</p>"),
        HTML("<p>為了減少討論時的成本觀念誤差，我把原先分析報告型式的估計，改寫成網頁的版本，用的服務是 Shinyapps。利用最大值、最小值和平均值來建立個別不同人猜測的可能需求數，再據以計算出車資成本，以及套用補貼策略後的補貼成本。</p>"),
        HTML("<p>這當然不是預測，而是一個把個人猜測量化、估計成本的東西。是個玩具。希望大家玩得開心。</p>"),
        HTML("<p>以下分別是輸入左側的數值後，計算出來的單日計程車資成本、年計程車資成本、以及年補貼成本的機率密度分佈。</p>"),
        plotOutput("distPlot"),
        HTML(paste0("<p><H3>",textOutput("text1"),"</H3></p>")),
        plotOutput("distPlot2"),
        HTML(paste0("<p><H3>",textOutput("text2"),"</H3></p>")),
        plotOutput("distPlot3"),
        HTML(paste0("<p><H3>",textOutput("text3"),"</H3></p>")),
        HTML(paste0("<p><H3>",textOutput("text4"),"</H3></p>"))
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
   
   output$distPlot3 <- renderPlot({

     s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
     s.mean = as.numeric(input$carsmean)
     # s.cars = rnorm(n = 365,mean = s.mean,sd=s.sd)
     # s.cars = ifelse(s.cars<0,0,s.cars)

     Temp.plan = input$plan
     Temp.money = input$money
     Temp.self = input$selfpay
     Temp.pay = input$selfpaymoney
       
     if (Temp.plan == "固定金額") {
       
       if (Temp.self=="否") {
         Temp.pay = 0
       } 
       
       Temp.y=data.frame()
       for (t in 1:30) {
         Temp = data.frame(cars=s.cars)
         Temp$taxiFee=0
         for ( i in 1:nrow(Temp)) {
           Temp$taxiFee[i] = taxi.trip$fee[sample(nrow(taxi.trip),abs(Temp$cars[i]),replace=T)] %>% 
           {.-Temp.pay} %>% 
             ifelse(.<0,0,.) %>% 
             ifelse(.<Temp.money,.,Temp.money) %>% 
             sum()
         }
         Temp = data.frame(cars = sum(Temp$cars),taxiFee=sum(Temp$taxiFee))
         Temp.y =rbind(Temp.y,Temp)
         rm(Temp)
       }
       
       Temp.y <<- Temp.y
       
       
     } else {
       
       if (Temp.self=="否") {
         Temp.pay = 1
       } 
       
       Temp.y=data.frame()
       for (t in 1:30) {
         Temp = data.frame(cars=s.cars)
         Temp$taxiFee=0
         for ( i in 1:nrow(Temp)) {
           Temp$taxiFee[i] = taxi.trip$fee[sample(nrow(taxi.trip),abs(Temp$cars[i]),replace=T)] %>% 
           {.-Temp.pay} %>% 
             ifelse(.<0,0,.) %>% 
             {.*Temp.money} %>% 
             sum()
         }
         Temp = data.frame(cars = sum(Temp$cars),taxiFee=sum(Temp$taxiFee))
         Temp.y =rbind(Temp.y,Temp)
         rm(Temp)
       }
       
       Temp.y <<- Temp.y
       
     }

     ggplot(Temp.y,aes(x=taxiFee))+geom_density()+ggtitle("Single Years Total Payout")
   })

   output$text3 <- renderText({
     s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
     s.mean = as.numeric(input$carsmean)
     paste0("一年的補貼成本可能落在", quantile(Temp.y$taxiFee[order(Temp.y$taxiFee)],0.1)[1],"元到", quantile(Temp.y$taxiFee[order(Temp.y$taxiFee)],0.9)[1],"元之間。（90%信賴區間）")
   })
   
   output$text4 <- renderText({
     s.sd = abs(as.numeric(input$carsmax)-as.numeric(input$carsmin))/2
     s.mean = as.numeric(input$carsmean)
     Temp.t = taxi.est.y/2
     paste0("一年的代駕成本可能落在", quantile(Temp.t$taxiFee[order(Temp.t$taxiFee)],0.1)[1],"元到", quantile(Temp.t$taxiFee[order(Temp.t$taxiFee)],0.9)[1],"元之間。（90%信賴區間）")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

