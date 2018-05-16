# library(shiny)
# library(shinythemes)
library(dplyr)
# library(readr)
# library(shinydashboard)
# library(rsconnect)
library(ggplot2)
# library(RPostgres)
library(DBI) # dbGetQuery
library(forecast)
library(magrittr) # for renaming stupid column
library(lubridate) # date stuff
library(stringr)
library(DT)
library(shinyjs)


### use for debugging
# chatAverage = 0
# forecastPeriod = 52
# graphStart = '2018-01-01'
# retailerAverage = 100
# retailerGrowth = 1.01
# input <- data.frame(chatAverage = as.numeric(chatAverage), forecastPeriod = as.numeric(forecastPeriod), graphStart)

function(input, output, session) {
  file <- reactiveValues(
    upload_state = NULL
  )
  # radio button toggle
  observeEvent(input$retailerForecast, {
    # Change the following line for more examples
    toggleState("retailerAverage")
    toggleState("retailerGrowth")
  })
  observeEvent(input$csvFile, {
    file$upload_state <- 'uploaded'
  })
  
  output$plot <- renderPlot({
    ############# Load Data #############
    inFile <- input$csvFile
    
    # print(input$csvFile)
    # print(file$upload_state)
    
    mainDir <- "/home/rstudio"
    subDir <- "files"
    
    # create file directories
    if (file.exists(file.path(mainDir, subDir))){
      setwd(file.path(mainDir, subDir))
    } else {
      dir.create(file.path(mainDir, subDir))
      setwd(file.path(mainDir, subDir))
      file.copy('/home/rstudio/R/raw_ticket_data-1.csv', '/home/rstudio/files/raw_ticket_data-1.csv')
    }
    
    # if no file uploaded, select local file with highest suffix number
    if (is.null(inFile) || is.null(file$upload_state)){
      # get array position of file with highest suffix
      file_suffixes <- as.numeric(str_replace(str_replace(list.files(pattern="*.csv"), ".csv", ""), "raw_ticket_data-", ""))
      # get string of file name
      latest_file <- list.files(pattern="*.csv")[which(file_suffixes == max(file_suffixes))]
      # use string to load file
      tickets.df <- read.csv(paste(latest_file, sep = ''), header = T)
    }
    # else use data from uploaded file
    else if (file$upload_state == 'uploaded'){
      tickets.df <- read.csv(inFile$datapath, header = TRUE)
      
      file_strings <- matrix(unlist(str_split(list.files(pattern="*.csv"), '-')), ncol = 2, byrow = TRUE)[,2]
      file_strings <- file_strings %>%
        str_replace(".csv", "")
      suffix <- max(as.numeric(file_strings))
      
      # add one to max suffix number for new file
      suffix = max(suffix[which(is.na(suffix)=='FALSE')])+1
      # get directory, save to local file
      local_dir = getwd()
      write.csv(tickets.df, sprintf(str_c(local_dir, "/raw_ticket_data-%d.csv"), suffix))
      # reset upload state to avoid new files being created on input
      file$upload_state = NULL
    }
    
    # drop unnecessary columns
    # tickets.df <- tickets.df %>% 
    #   transmute(
    #     week,
    #     total_adjusted_tickets,
    #     retailer_count
    #   )
    
    # fileInput using most recent date from dataframe in placeholder string
    outputString <- "fileInput('csvFile', 'Choose CSV File', multiple = FALSE, accept = c('text/csv', 
    'text/comma-separated-values,text/plain', '.csv'), 
    placeholder = sprintf('Most recent week included: %s', as.Date(tail(tickets.df$week, 1))))"
    
    # set directory back
    setwd('/home/rstudio/R')
    
    output$ui <- renderUI({
      eval(parse(text = outputString))
    })
    
    ############# Format Data #############
    tickets.df <- rename(tickets.df, tickets = total_adjusted_tickets)
    # for whatever reason, the integer variables aren't behaving unless specifically specified as int.
    tickets.df$retailer_count <- as.integer(tickets.df$retailer_count)
    tickets.df$total_chat <- as.integer(tickets.df$total_chat)
    tickets.df$tickets <- as.integer(tickets.df$tickets)
    tickets.ts <- ts(tickets.df$tickets, frequency = 52, start = c(2012, 1))
    
    ############# Forecast Retailers #############
    if (input$retailerForecast == 0) {
      
      tickets.df$retailer_count
      retailers.ts <- ts(tickets.df$retailer_count, frequency = 52, start = c(2012, 1))
      
      retailer_stlm.model <- stlm(retailers.ts, method = "arima")
      retailer_stlm.forecast <- forecast(retailer_stlm.model, h = input$forecastPeriod)
      # plot(retailer_stlm.forecast)
      retailer_forecast <- as.vector(retailer_stlm.forecast$mean)
    }
    
    # else using Steph's numbers from the board
    else {
      # current number of retailers
      current_retailers <- tickets.df$retailer_count[which(as.Date(tickets.df$week) == max(as.Date(tickets.df$week)))]
      # create matrix
      retailer_forecast <- as.vector(NA)
      for (i in 1:input$forecastPeriod){
        if (i == 1) {
          retailer_forecast[i] = round(current_retailers + (round(input$retailerAverage/4.3) * (input$retailerGrowth/100+1)))
        } else {
          retailer_forecast[i] = round(retailer_forecast[i-1] + (round(input$retailerAverage/4.3) * (input$retailerGrowth/100+1) ^ i))
        }
      } 
    }
    
    
    ############# ARIMA Model #############
    # Create matrix of numeric predictors
    xreg <- cbind(tickets.df$total_chat, 
                  tickets.df$retailer_count)
    
    # create model
    ticket_stlm.model <- stlm(tickets.ts, method = "arima", xreg = xreg, robust = TRUE)
    chat_average = as.matrix(rep(input$chatAverage, input$forecastPeriod))
    xreg_p <- cbind(chat_average, retailer_forecast)
    ticket_stlm.forecast <- forecast(ticket_stlm.model, h = input$forecastPeriod, newxreg = xreg_p)
    # plot(ticket_stlm.forecast)
    
    # export forecasts to dataframe
    STLMforecast.df <- data.frame(ticket_stlm.forecast)
    
    ############# GGPLOT #############
    # rename prediction dataframe columns
    STLMforecast.df <- rename(STLMforecast.df, estimate = Point.Forecast, low80 = Lo.80, high80 = Hi.80, low95 = Lo.95, high95 = Hi.95)
    # str(STLMforecast.df)
    
    # create dataframe with observed and predicted ticket numbers
    graph.df <- as.data.frame(c(as.vector(tickets.df$tickets[which(as.character(tickets.df$week) >= paste(input$graphStart[1], sep = ''))]), 
                                as.vector(round(STLMforecast.df$estimate))))
    # rename stupid column
    graph.df <- graph.df %T>% {colnames(.)[1] = "tickets"}
    # str(graph.df)
    
    # create buffer to offset predictions when combining dataframes
    buffer = as.matrix(rep(NA, length(graph.df$tickets) - as.numeric(paste(input$forecastPeriod[1], sep = ''))))
    
    # add buffer to beginning of each column
    for (i in 2:length(STLMforecast.df)) {
      graph.df[names(STLMforecast.df[i])] <- as.vector(rbind(buffer, as.matrix(round(STLMforecast.df[i]))))
    }
    
    # add a column with row numbers
    graph.df$rownumber <- as.numeric(rownames(graph.df))
    # add a column with predicted/observed factor
    graph.df$source <- ifelse(graph.df$rownumber > (length(graph.df$tickets) - as.numeric(paste(input$forecastPeriod[1], sep = ''))),
                              "predicted", "observed")
    # create timeseries for graph
    forecast.ts=ts(graph.df$tickets, frequency = 52, 
                   start = c(lubridate::year(paste(input$graphStart[1], sep = '')), lubridate::month(paste(input$graphStart[1], sep = ''))))
    # plot.ts(forecast.ts)
    Time=1:length(graph.df$tickets)
    
    # ggplot of forecasts
    fc <- qplot(x = Time, y = tickets, data=graph.df,
                main = "Ticket Volume by Week",
                col = source,
                geom = c("line"),
                xlab = "",
                ylab = "Tickets per Week \n",
                size = I(1.04))
    # fc
    
    # plotting prediction interval fill black/white theme, legend direction, removing legend title 
    fc2 <- fc + geom_ribbon(aes(ymin = graph.df$low95, ymax = graph.df$high95),
                            size = 0,
                            fill = "turquoise2",
                            alpha = 0.3,
                            show.legend = F) + 
      geom_ribbon(aes(ymin = graph.df$low80, ymax = graph.df$high80),
                  size = 0,
                  fill = "turquoise3",
                  alpha = 0.3,
                  show.legend = F) + 
      theme_bw() + 
      theme(legend.direction = "vertical") + 
      labs(col = "")
    # fc2
    
    # changing axis text, legend position, etc
    fc3 <- fc2 + theme(axis.title = element_text(size = "16", color = "gray26",hjust = 0.5)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.title = element_text(size=16,face = "bold",vjust = 2)) +
      theme(legend.position = c(0.33, 0.78)) +
      theme(legend.text = element_text(colour = "gray26", size = 11, face = "bold")) +
      theme(legend.background = element_rect(fill = "white", size = 0),legend.margin = margin(t = 0.01, b = 0.01, r = 0.01, l = 0.01, unit = 'mm')) 
    # fc3
    
    # generate breaks and labels
    graphEnd = Sys.Date() + as.integer(paste(input$forecastPeriod[1], sep = ''))*7
    weeks <- as.numeric(round(difftime(graphEnd, paste(input$graphStart[1], sep = ''), units = "weeks")))
    breaks = (1:(round(weeks/26)+1)*26)-25
    labels <- NA
    for(i in 1:length(breaks)){
      labels[i] <- paste(
        str_trunc(months(round_date(as.Date(paste(input$graphStart[1], sep = ''))+(breaks[i]*7)-1, "month")), 3, ellipsis = ''), 
        str_trunc(year(round_date(as.Date(paste(input$graphStart[1], sep = ''))+(breaks[i]*7)-1, "month")), 2, "left", ellipsis = ''), 
        sep = '-')
    }
    
    
    # changing the x-axis labels to show month-year instead of weeks
    fc4 <- fc3 + scale_x_continuous(breaks = breaks,
                                    labels = labels,
                                    expand = c(0,0)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.1))
    # fc4
    
    
    # adjust x.axis labels, add space to right margin
    fc5 <- fc4 + theme(axis.text.x = element_text(angle = 0, vjust = 0.1)) + theme(plot.margin = unit(c(0.5, 1.3, 0.5, 0.5), "cm"))
    # fc5
    
    fc6 <- fc5 + theme(axis.text.x = element_text(size = 11)) +
      theme(axis.text.y = element_text(face = "bold",size = 12)) +
      # scale_y_continuous(limits = c(500, 2200), breaks = c(500, 1000, 1500, 2000), minor_breaks = c(750, 1250, 1750)) + 
      theme(panel.grid.minor = element_line(color = "grey82", size = 0.1)) + 
      theme(panel.grid.major = element_line(color = "grey52", size = 0.1)) +
      theme(legend.position = "none")
    
    
    print(fc6)
    
    # generate future dates
    last_date = as.Date(tickets.df$week[length(tickets.df$week)])
    week <- as.Date(NA)
    for (i in 1:length(STLMforecast.df$estimate)){
      week[i] = as.Date(last_date + (7*i))
    }
    rownames(STLMforecast.df) <- NULL
    STLMforecast.df <- rename(STLMforecast.df, 'predicted' = 'estimate', 'low 80%' = 'low80', 'high 80%' = 'high80', 'low 95%' = 'low95', 'high 95%' = 'high95')
    forecast_output <- cbind(week, round(retailer_forecast), round(STLMforecast.df))
    forecast_output <- rename(forecast_output, "retailers" = "round(retailer_forecast)", "tickets" = "predicted")
    output$table <- DT::renderDataTable(DT::datatable(forecast_output))
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("forecast.csv", sep = "")
      },
      content = function(file) {
        write.csv(forecast_output, file, row.names = FALSE)
      }
    )
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("plot.jpg", sep = "")
      },
      content = function(file) {
        ggsave(plot = last_plot(), device = "jpeg", file = file, width = 16.8, height = 6.96)
      }
    )
    # reset CSV file if provided to prevent new copies being created when input parameters are changed
    reset('csvFile')
  })
}