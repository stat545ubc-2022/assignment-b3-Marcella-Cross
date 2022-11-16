library(shiny)
library(tidyverse)
library(DT)

#load data that is saved in the same folder as the app code.
chicks <- read.csv("chickData")

ui <- fluidPage(
  #this creates a title feature that is aesthetic and informative.
  column(6,offset = 3, titlePanel("Chicken Development by Diet for the First 21 Days")),
  #This adds an image to the shiny app that is aesthetic and makes the page look more user friendly
  tags$figure(
    align = "center",
    tags$img(
      src = "Hatching-Chicks.jpeg",
      alt = "Picture Chicken Development")),
  #This is a sidebar that contains two elements, a slider, and a checkbox list for interaction with the histogram.
  sidebarLayout(sidebarPanel(
      #The slider allows the user to manipulate the age range of the chickens to see how their weight distribution changes.
      sliderInput("TimeDays","Days of Age", 0,21,value=c(1,21),post="Days", step=2),
      #This check box list allows the user to select the diet types and see how each diet affects the weight distribution of the chicks.
      checkboxGroupInput("DietType","Diet Type",choices=c("1","2","3","4"), selected=c("1","2","3","4"),inline=TRUE)),
       #This creates a tab system that displays an interactive histogram, a scatter plot, and a data table. These are useful for better understanding the data and helping the user to visualize the results of how diet is affecting the weight development of chicks over the period of 21 days.
       mainPanel(tabsetPanel(
        tabPanel("Histogram", plotOutput("chick_hist")),
        tabPanel("Scatter",plotOutput("scat.plot")),
        tabPanel("Data Table", DT::dataTableOutput("summary"))))))



#This is the code for the histogram, the scatterplot, and the datatable as discussed above.
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
