library(rsconnect)
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
library(dashboardthemes)

load("eitc_calc.Rda")
#rsconnect::deployApp('C:/Users/HP/Documents/R/EITC_Calculator')

ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = "MI EITC dashboard"),
  dashboardSidebar(
    selectInput("status", label=h3("Filing status: "),
                choices=list("Single/Head of household"=1,
                             "Married filing jointly"=2),
                selected=1),
    selectInput("number", label=h3("Number of children: "),
                choices=list("None"=1, "One"=2, "Two"=3, "Three+"=4),
                selected=1),
    textInput("income", label=h3("Adjusted gross income: ", "$"),
              value = "20000")
  ),
  dashboardBody(
    #tags$head(tags$style(HTML('
    #  .main-header .logo {
    #    font-family: "Georgia", Times, "Times New Roman", serif;
    #    font-weight: bold;
    #    font-size: 24px;
    #  }
    #'))),
    tags$style(
      ".small-box.bg-yellow { background-color: #96c9cc !important; }",
      ".small-box.bg-navy { background-color: #28556b !important; }",
      ".small-box.bg-maroon { background-color: #f05922 !important; }",
      ".small-box.bg-blue { background-color: #bad645 !important; }"
               ),
    
    column(width = 4, 
           fluidRow(valueBoxOutput("fed", width=12)),
           fluidRow(valueBoxOutput("mi6", width=12)),
           fluidRow(valueBoxOutput("mi20", width=12)),
           fluidRow(valueBoxOutput("mi30", width=12))
    ),
           box(
             plotOutput("eitc_plot"),
             p("*Estimates assume all income is from earnings. Income from capital gains, unemployment insurance, or other ineligible sources may reduce federal and state EITC.")
           )
  )
)

server <- function(input, output, session) {
  
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
    fed
  }
  mich6 <- function(x){
    round(x*.06, 0)
  }
  mich20 <- function(x){
    round(x*.2, 0)
  }
  mich30 <- function(x){
    round(x*.3, 0)
  }
  #Need to clean up later#
  a <- eventReactive(input$number, {as.numeric(input$number)-1})
  b <- eventReactive(input$status, {
    if(input$status=="1"){"S"}
    else if(input$status=="2"){"MFJ"}
  })
  c <- eventReactive(input$income, {as.numeric(input$income)})
  arg1 <- reactive({
    as.numeric(input$status)
  })
  arg2 <- reactive({
    as.numeric(input$number)
  })
  arg3 <- reactive({
    as.data.frame(graph_df %>% filter(income == input$income))
  })
  #########################
  output$fed <- renderValueBox({
    valueBox(
      paste0("$", eitc(a(), b(), c())), "Federal EITC", 
      icon = icon("bookmark", lib = "glyphicon"), 
      color = "blue", width = 12
    )
  })
  output$mi6 <- renderValueBox({
    fed <- eitc(a(), b(), c())
    valueBox(
      paste0("$", mich6(fed)), "Current MI EITC (6%)", 
      icon = icon("bookmark", lib = "glyphicon"),
      color = "yellow", width = 12
    )
  })
  output$mi20 <- renderValueBox({
    fed <- eitc(a(), b(), c())
    valueBox(
      paste0("$",  mich20(fed)), "Restored MI EITC (20%)", 
      icon = icon("bookmark", lib = "glyphicon"),
      color = "maroon", width = 12
    )
  })
  output$mi30 <- renderValueBox({
    fed <- eitc(a(), b(), c())
    valueBox(
      paste0("$", mich30(fed)), "Increased MI EITC (30%)", 
      icon = icon("bookmark", lib = "glyphicon"),
      color = "navy", width = 12
    )
  })
  output$eitc_plot <- renderPlot({
    if(arg1()==1 & arg2()==1){
      p <- ggplot() +
        geom_line(data = graph_df, aes(x=income, y=fedS0),
                  color="#96c9cc", size=1) +
        geom_point(data = arg3(), aes(x=income, y=fedS0),
                   color="#28556b", size=4)
    }
    else if(arg1()==2 & arg2()==1){
      p <- ggplot() +
        geom_line(data = graph_df, aes(x=income, y=fedM0),
                  color="#96c9cc", size=1)+
        geom_point(data = arg3(), aes(x=income, y=fedM0),
                   color="#28556b", size=4) 
    }
    else if(arg1()==1 & arg2()==2){
      p <- ggplot() +
        geom_line(data = graph_df, aes(x=income, y=fedS1),
                  color="#96c9cc", size=1)+
        geom_point(data = arg3(), aes(x=income, y=fedS1),
                   color="#28556b", size=4) 
    }
    else if(arg1()==2 & arg2()==2){
      p <- ggplot() +
        geom_line(data = graph_df, aes(x=income, y=fedM1),
                  color="#96c9cc", size=1)+
        geom_point(data = arg3(), aes(x=income, y=fedM1),
                   color="#28556b", size=4) 
    }
    else if(arg1()==1 & arg2()==3){
      p <- ggplot() +
        geom_line(data = graph_df, aes(x=income, y=fedS2),
                  color="#96c9cc", size=1)+
        geom_point(data = arg3(), aes(x=income, y=fedS2),
                   color="#28556b", size=4) 
    }
    else if(arg1()==2 & arg2()==3){
      p <- ggplot() +
        geom_line(data = graph_df, aes(x=income, y=fedM2),
                  color="#96c9cc", size=1)+
        geom_point(data = arg3(), aes(x=income, y=fedM2),
                   color="#28556b", size=4) 
    }
    else if(arg1()==1 & arg2()==4){
      p <- ggplot() +
        geom_line(data = graph_df, aes(x=income, y=fedS3),
                  color="#96c9cc", size=1)+
        geom_point(data = arg3(), aes(x=income, y=fedS3),
                   color="#28556b", size=4) 
    }
    else if(arg1()==2 & arg2()==4){
      p <- ggplot() +
        geom_line(data = graph_df, aes(x=income, y=fedM3),
                  color="#96c9cc", size=1)+
        geom_point(data = arg3(), aes(x=income, y=fedM3),
                   color="#28556b", size=4) 
    }
    p +
      theme_classic() + ggtitle("Federal EITC value") +
      xlab("Adjusted Gross Income ($)") + ylab("Credit ($)") +
      theme(
        axis.title = element_text(color = "#28556b", size = 15),
        axis.text = element_text(size=12),
        title = element_text(color = "#28556b", 
                             size = 18, face = "bold")) +
      scale_y_continuous(labels=scales::dollar_format()) +
      scale_x_continuous(labels=scales::dollar_format())  + 
      xlim(0,60000) + ylim(0,7000)
  })
}

shinyApp(ui, server)