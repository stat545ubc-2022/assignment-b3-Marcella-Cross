library(shiny)
library(tidyverse)
library(DT)

chicks <- read.csv("chickData")

ui <- fluidPage(
  column(6,offset = 3, titlePanel("Chicken Development by Diet for the First 21 Days")),
  tags$figure(
    align = "center",
    tags$img(
      src = "Hatching-Chicks.jpeg",
      alt = "Picture Chicken Development")),
  sidebarLayout(sidebarPanel(
      sliderInput("TimeDays","Days of Age", 0,21,value=c(1,21),post="Days", step=2),
      checkboxGroupInput("DietType","Diet Type",choices=c("1","2","3","4"), selected=c("1","2","3","4"),inline=TRUE)),
    mainPanel(tabsetPanel(
        tabPanel("Histogram", plotOutput("chick_hist")),
        tabPanel("Scatter",plotOutput("scat.plot")),
        tabPanel("Data Table", DT::dataTableOutput("summary"))))))




server <- function(input, output) {
  output$chick_hist <- renderPlot({
    chicks %>% filter(Time>input$TimeDays[1] &
                     Time<input$TimeDays[2]&
                     Diet==input$DietType) %>%
      ggplot(aes(weight)) +geom_histogram(col=I("black"), fill="sandybrown")})
  output$scat.plot <- renderPlot(ggplot(chicks, aes(Time,weight)) +geom_point(aes(color=factor(Diet))))
  output$summary <- DT::renderDataTable(chicks)

}

shinyApp(ui = ui, server = server)
