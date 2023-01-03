
library(tidyverse)
library(shiny)
library(httr)
companyData <- function(company, start_day)
{
  start_day_POSIX = as.numeric(as.POSIXct(start_day, format="%Y-%m-%d"))
  temp = tempfile()
  yahooHandle = httr::handle('https://finance.yahoo.com/quote/')
  url = paste('https://finance.yahoo.com/quote/', company, '/history?p=', company, sep = "")
  raw_html = httr::content(httr::GET(url, handle = yahooHandle),"text")
  url =  paste0("https://query1.finance.yahoo.com/v7/finance/download/", company, "?period1=", start_day_POSIX, "&period2=", as.integer(as.POSIXct(Sys.time(), format="%Y-%m-%d %h:%m:%s")), "&interval=1d&events=history")
  #period1 is the starting point and period2 is the end point
  #print(url)
  writeBin(httr::content(httr::GET(url, handle = yahooHandle), "text", encoding = "UTF-8"), temp, useBytes = T)
  suppressWarnings(read.csv(temp) %>% tibble::as_tibble())
}
out = companyData("AAPL", "2019-11-20") 



#Shiny app 
ui <- fluidPage(
  #Application title
  titlePanel("Yahoo Financial Data"),
  
  # Inputs
  sidebarLayout(
    sidebarPanel(
      tags$h1("Note: Must input both company ticker and starting date to get results"),
      textInput("companysymbol", "Enter a company ticker (ex. AAPL)"),
      textInput("dateinput", "Enter a origin date (ex. 2019-11-20)"),
      actionButton("button0",
                   label = "Search")
    ),
    
    # Outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Data", textOutput("info1"), dataTableOutput("datatibble")),
        tabPanel("Summary Tibble", textOutput("info2"), dataTableOutput("summarytibble")),
        tabPanel("Visualizations", textOutput("info3"), plotOutput(outputId = "graph1", click = "plot1_clicked1"), verbatimTextOutput("clickedinfo1"), 
                 plotOutput(outputId = "graph2", click = "plot1_clicked2"), verbatimTextOutput("clickedinfo2"))
      )
    )
  )
)


server <- function(input, output) {
  
  # Events occur when button is clicked
  observeEvent(input$button0, {
    # displaying information for user and time stamp for today
    output$info1 <- renderText({ paste0("This is a data tibble containing the information for ", toupper(input$companysymbol), " starting from " , input$dateinput , " to ", as.POSIXct(Sys.time(), format="%Y-%m-%d")) })
    output$info2 <- renderText({ "This is a summary tibble displaying the average among the columns for Open, Close, Low, and High for each Year between the given dates" })
    output$info3 <- renderText({ "These visualizations display the various columns in the data over time, try clicking on a point in the graph to see the corresponding values" })
    
    # sending user input into function to get data
    dat <- companyData(toupper(input$companysymbol), input$dateinput)
    dat <- dat %>% 
      mutate(Year = format(as.Date(Date, format="%Y-%m-%d"),"%Y"))
    
    # For summary tibble, displaying the mean Open, Close, High, and Low for each year
    datAv <- dat %>% 
      summarise(OpenAverage=mean(Open), CloseAverage=mean(Close), HighAverage=mean(High), LowAverage=mean(Low))
    
    output$summarytibble <- renderDataTable({ datAv })
    
    dat$posix = (as.POSIXct(dat$Date, format="%Y-%m-%d"))
    
    # displaying data and data visualization
    output$datatibble <- renderDataTable({ dat })
    vals1 <- c("Open" = "red", "Close" = "blue")
    vals2 <- c("Low" = "red", "High" = "blue")
    
    output$graph1 <- renderPlot({ ggplot(data=dat, aes(x=posix, y=Open, color="Open")) + geom_point() + geom_point(data=dat, aes(x=posix, y=Close, color="Close")) + ggtitle("Open and Close Prices over Time") + geom_hline(data=datAv, aes(yintercept=(CloseAverage+OpenAverage)/2)) + xlab("Time") + ylab("Price") + labs(color="Legend") +
        scale_color_manual(values = vals1)})
    
    output$graph2 <- renderPlot({ ggplot(data=dat, aes(x=posix, y=High, color="High")) + geom_point() + geom_point(data=dat, aes(x=posix, y=Low, color="Low")) + ggtitle("Low and High Prices over Time") + geom_hline(data=datAv, aes(yintercept=HighAverage), col="Blue") + geom_hline(data=datAv, aes(yintercept=LowAverage), col="Red") + xlab("Time") + ylab("Price") + labs(color="Legend") +
        scale_color_manual(values = vals2)})
    
    
    # displaying information of clicked point
    output$clickedinfo1 <- renderPrint({ nearPoints(dat, input$plot1_clicked1, addDist = TRUE) })
    output$clickedinfo2 <- renderPrint({ nearPoints(dat, input$plot1_clicked2, addDist = TRUE) })
    
    
  })
  
  
}

#Run the application 
shinyApp(ui = ui, server = server)
