library(shinydashboard)
library(shiny)
library(ggplot2)


#A Simple Dashboard I have put together so there's not plotting functions lying around the place
#It's a bit cleaner!!
#Dashboard is a bit poor, Rshiny Skills not up to scratch!
#can be run on any dataset that contains the "TARGET_CHAR" varaible
Run_Dashboard <- function(Inp_Data){
  
  factors <- names(Inp_Data)[ sapply(Inp_Data, is.factor) ]
  numerics <- names(Inp_Data)[ !sapply(Inp_Data, is.factor) ]

  ui <- dashboardPage(
    dashboardHeader(title='Dashboard'),
    dashboardSidebar(
      sidebarMenu(id = 'tabs',
        menuItem("Variable Selection", tabName = "MTG",
          selectInput(inputId = "numeric",
                      label = "Numeric Variable Selection",
                      choices = numerics),
          selectInput(inputId = "categ",
                   label = "Categorical Variable Selection",
                   choices = factors))
        )),
    dashboardBody(
    tabItems(
      #selected = 1,
      tabItem(
      tabName = "MTG", class = "active",
      fluidRow(
            box(plotOutput("Hist", height = 350) , title = "Numeric Variable Histogram"),
            box(plotOutput("Boxp", height = 350) , title = "Numeric Variable Boxplot"),
            box(plotOutput("BAR", height = 350) , title = "Categorical Variable Barchart"),
            box(plotOutput("PIE", height = 350) , title = "Categorical Variable Pie Chart")
        )
      )
    )
  ))
  
  
  server <- function(input, output) {

    output$Hist <- renderPlot({ggplot(data=Inp_Data) + geom_histogram(aes_string(x=input$numeric, color='TARGET_CHAR'), fill='white', bins=30, position = position_stack(reverse = TRUE))})
    output$Boxp <- renderPlot({ggplot(data=Inp_Data) + geom_boxplot(aes_string(y=input$numeric))})
    output$BAR <- renderPlot({ggplot(data=Inp_Data) + geom_bar(aes_string(x=input$categ, fill='TARGET_CHAR'), position = position_stack(reverse = TRUE))})
    output$PIE <- renderPlot({ggplot(data=Inp_Data) + geom_bar(aes_string(x='TARGET_CHAR', fill=input$categ), position = position_fill()) + coord_polar("y", start=0) + facet_wrap(~ TARGET_CHAR)})
    
    }

    shinyApp(ui, server)

}


Run_Dashboard(RL)
