library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(leaflet)
library(rgdal)
library(janitor)

setwd("~/R")
getwd()

children <- c(0, 1, 2, 3)
maxbase <-c(7320, 10980, 15410, 15410)
maxcredit <-c(560, 3733, 6164, 6935)
rate <-c(.0765, .34, .4, .45)
PO_rate <-c(.0765, .1598, .2106, .2106)
PO_s <-c(9160, 20130, 20130, 20130)
PO_m <-c(15290, 26260, 26260, 26260)
end_s <-c(16480, 43492, 49399, 53057)
end_m <-c(22610, 49622, 55529, 59187)

calc <- data.frame(children, maxbase, maxcredit, rate, PO_rate, PO_s, PO_m, end_s, end_m)
  
eitc <- function(child, status, AGI) {
    calc1 <- calc %>%
      filter(children==child)
    if(AGI<calc1$maxbase){
      fed <- round(AGI * calc1$rate, 0)
    }
    if(AGI>calc1$maxbase & AGI<calc1$PO_s & status=="S"){
      fed <- round(calc1$maxcredit, 0)
    }
    if(AGI>calc1$maxbase & AGI<calc1$PO_m & status=="MFJ"){
      fed <- round(calc1$maxcredit, 0)
    }
    if(AGI>calc1$PO_m & status=="MFJ"){
      fed <- round((calc1$end_m - AGI)*calc1$PO_rate, 0)
    }
    if(AGI>calc1$PO_s & status=="S"){
      fed <- round((calc1$end_s - AGI)*calc1$PO_rate, 0)
    }
    if(fed<0){
      fed <- 0
    }
    mich6 <- round(fed*.06, 0)
    mich20 <- round(fed*.2, 0)
    mich30 <- round(fed*.3, 0)
    print(paste("Federal EITC: ", fed))
    print(paste("Current Michigan EITC (6%): ", mich6))
    print(paste("Current Michigan EITC (20%): ", mich20))
    print(paste("Current Michigan EITC (30%): ", mich30))
}
View(graph_df)

load("eitc_calc.Rda")

i <- ggplot(graph_df, aes(income)) +
  geom_line(aes(y=fedM0)) +
  geom_line(aes(y=fedM1)) +
  geom_line(aes(y=fedM2)) +
  geom_line(aes(y=fedM3)) +
  geom_line(aes(y=fedS0)) +
  geom_line(aes(y=fedS1)) +
  geom_line(aes(y=fedS2)) +
  geom_line(aes(y=fedS3)) +
  scale_fill_distiller(palette="Blues") +
  theme_classic() + ggtitle("Value of federal EITC") +
  xlab("Adjusted Gross Income (AGI)") +
  ylab("Credit")
i

ui <- fluidPage(
  titlePanel("EITC Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("status", label=h3("Filing status: "),
                  choices=list("Single/Head of household"=1,
                               "Married filing jointly"=2),
                  selected=1),
      selectInput("number", label=h3("Number of children: "),
                  choices=list("None"=1, "One"=2, "Two"=3, "Three+"=4),
                  selected=1),
      textInput("income", label=h3("Adjusted gross income: ", "$"),
                  value = "")
    ),
    mainPanel(
      plotOutput("eitc_plot")
    )
  )
)

server <- function(input, output, session) {
  output$fs <- renderPrint({input$status})
  output$num <- renderPrint({input$number})
  output$AGI <- renderPrint({input$income})
  credit <- reactive({
    if(input$status==1 & input$number==1){
      credit=graph_df$fedS0
    }
    if(input$status==1 & input$number==2){
      credit=graph_df$fedS1
    }
    if(input$status==1 & input$number==3){
      credit=graph_df$fedS2
    }
    if(input$status==1 & input$number==4){
      credit=graph_df$fedS3
    }
    if(input$status==2 & input$number==1){
      credit=graph_df$fedM0
    }
    if(input$status==2 & input$number==2){
      credit=graph_df$fedM1
    }
    if(input$status==2 & input$number==3){
      credit=graph_df$fedM2
    }
    if(input$status==2 & input$number==4){
      credit=graph_df$fedM3
    }
    output$eitc_plot <- renderPlot({
      a <- ggplot(data=graph_df, aes(x=income, y=credit), ) +
        geom_line()
      a
    })
  })
}

shinyApp(ui, server)
