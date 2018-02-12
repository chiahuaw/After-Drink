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
         textInput("carsmin",
                   "你覺得一天可能的最少車次：",value = 1),
         textInput("carsmax",
                   "你覺得一天可能的最大車次：",value = 100),
         textInput("carsmean",
                   "你覺得一天可能平均有多少車次：",value = 50),
         selectInput("plan","補貼策略：",choices = c("固定金額","固定比例"),multiple = F,selected = "固定比例"),
         textInput("money",
                   "補貼比例或金額：",value = 0.1),
         selectInput("selfpay","有最小自付額嗎？",choices = c("是","否"),multiple = F,selected = "是"),
         textInput("selfpaymoney",
                   "最小自付額（元）：",value = 100)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

