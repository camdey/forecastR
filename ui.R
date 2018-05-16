library(shiny)
library(ggplot2)
library(DT)
library(shinyjs)


fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Support Ticket Forecaster"),
  
  fluidRow(
    column(12,
           # mainPanel(
           plotOutput('plot')
           # )
    )
  ),
  fluidRow(
    # column(4,
    sidebarPanel(
      tags$div(class="header", checked=NA,
               tags$a(href="https://linktoyourdata.com", "Click here to download latest data")
      ),
      uiOutput("ui"),
      sliderInput('forecastPeriod', 'Forecast Period (weeks)', min = 4, max = 104, 
                  value = 52, step = 1, round = TRUE),
      sliderInput('chatAverage', 'Average Chat Tickets per Week', min = 0, max = 1000,
                  value = 0, step = 10, round = TRUE),
      dateInput('graphStart', 'Start Date', value = '2015-01-01', min = '2012-01-01', max = '2017-12-01', startview = 'year', weekstart = 1),
      radioButtons('retailerForecast', 'Retailer Forecast Method', selected = 0,
                   inline = FALSE,choiceNames = c("Default", "Growth"), choiceValues = c(0, 1)),
      numericInput('retailerAverage', 'Current Net New Retailers per month', value = 250, min = 0, max = 1000, step = 5),
      numericInput('retailerGrowth', 'Net New Retailer Growth (% per month)', value = 2, min = -100, max = 100, step = 0.1),
      downloadButton("downloadData", "Download Table"),
      downloadButton("downloadPlot", "Download Graph")
    ),
    # ),
    column(8,
           DT::dataTableOutput("table")
    )
  )
)