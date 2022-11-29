library(shiny)
library(tidyverse)
library(DT)
library(png)
library(jpeg)

#load data that is saved in the same folder as the app code.
chicks <- read.csv("chickData")

ui <- fluidPage(
  br(),
  br(),
  #this creates a title feature that is aesthetic and informative.
  column(4,offset = 4, titlePanel("Chicken Development by Diet")),
  br(),
  #This adds an image to the shiny app that is aesthetic and makes the page look more user friendly
  tags$figure(
    align = "center",
    tags$img(
      src = "Hatching-Chicks.jpeg",
      alt = "Picture Chicken Development")),
  br(),
  #Here I will make some text that will tell the user what the function of the app is and how to use it.
  tags$hr(style="border-color: sandybrown;"),
  h2("How to use this app:"),
  strong("This app displays a histogram, scatter plot, and a data table for visualization of chick development over the first 21 days. The bar can be toggled to show certain day ranges, and the check boxes may be used to select the diets of interest. For a longer term image of chicken development, images of chickens from 1 day to 10 weeks of age can be selected. Enjoy!"),
  tags$hr(style="border-color: sandybrown;"),
  #This is a sidebar that contains three elements, a slider, a checkbox list for interaction with the histogram as well as a select menu for the image selection.
  sidebarLayout(sidebarPanel(
      #The slider allows the user to manipulate the age range of the chickens to see how their weight distribution changes.
      sliderInput("TimeDays","Days of Age", 0,21,value=c(1,21),post="Days", step=2),
      #This check box list allows the user to select the diet types and see how each diet affects the weight distribution of the chicks.
      checkboxGroupInput("DietType","Diet Type",choices=c("1","2","3","4"), selected=c("1","2","3","4"),inline=TRUE),
      #This select input will allow the user to choose an age and have an image display.
      selectInput("select",label = h3("Chicken Development Over the Weeks"),
                  choices = c("One Day", "One Week", "Two Weeks","Three Weeks", "Four Weeks", "Five Weeks", "Six Weeks","Seven Weeks", "Eight Weeks","Nine Weeks","Ten Weeks"),
                  selected = "One_Day"),
        uiOutput("Images")),
       #This creates a tab system that displays an interactive histogram, a scatter plot, and a data table. These are useful for better understanding the data and helping the user to visualize the results of how diet is affecting the weight development of chicks over the period of 21 days.
       mainPanel(tabsetPanel(
        tabPanel("Histogram", plotOutput("chick_hist")),
        tabPanel("Scatter",plotOutput("scat.plot")),
        tabPanel("Data Table", DT::dataTableOutput("summary"))))),
    #This code is for the links to the two resources used.
    tags$hr(style="border-color: sandybrown;"),
    a(href="https://www.thepioneerchicks.com/chick-growth-guide-with-pictures/",
    "Link to Weekly Development Resource"),
    br(),
    a(href="https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ChickWeight.html",
    "Link to the original data set"),
    tags$hr(style="border-color: sandybrown;"))



#This is the code for the histogram, the scatterplot, datatable, and the image code as discussed above.
server <- function(input, output) {
  #This creates a reactive set of filtered data for repeated use.
  filtered.data <- reactive({chicks %>% filter(Time>input$TimeDays[1] &
                                                 Time<input$TimeDays[2]&
                                                 Diet==input$DietType)})
  #This codes for the interactive histogram
  output$chick_hist <- renderPlot({
    filtered.data() %>%
      ggplot(aes(weight)) +geom_histogram(col=I("black"), fill="sandybrown")})
  #This codes for the interactive scatter plot
  output$scat.plot <- renderPlot({filtered.data() %>%
    ggplot(aes(Time,weight)) +geom_point(size=5, aes(color=factor(Diet)))+scale_colour_discrete("Diet")})
  #This codes for the interactive data table
  output$summary <- DT::renderDataTable(filtered.data())
  #This codes for the image selection by chicken age
   output$Images <- renderUI({
    if (input$select=="One Day") {img(height = 300, width = 500, src="1.day.png")}
    else if (input$select=="One Week") {img(height = 300, width = 500, src="1.week.png")}
    else if (input$select=="Two Weeks") {img(height = 300, width = 500, src="1.2.week.png")}
    else if (input$select=="Three Weeks") {img(height = 300, width = 500, src="3.week.png")}
    else if (input$select=="Four Weeks") {img(height = 300, width = 500, src="4.week.png")}
    else if (input$select=="Five Weeks") {img(height = 300, width = 500, src="5.week.png")}
    else if (input$select=="Six Weeks") {img(height = 300, width = 500, src="6.week.png")}
    else if (input$select=="Seven Weeks") {img(height = 300, width = 500, src="7.week.png")}
    else if (input$select=="Eight Weeks") {img(height = 300, width = 500, src="8.week.png")}
    else if (input$select=="Nine Weeks") {img(height = 300, width = 500, src="9.week.png")}
    else if (input$select=="Ten Weeks") {img(height = 300, width = 500, src="10.week.png")}
    })
}

shinyApp(ui = ui, server = server)
