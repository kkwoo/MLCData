#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(httr)
library(dygraphs)   # zoomable graphs
library(data.table) # like using data.table
library(shiny)
library(stringi)

MLCH7Data <- function (startDate, endDate) {
    # view-source:https://www.mlc.com.au/masterkeyWeb/execute/UnitPricesWQO?openAgent&reporttype=HistoricalDateRange&product=MasterKey%20Super%20Fundamentals&fund=MLC%20Horizon%207%20-%20Accelerated%20Growth%20Portfolio&begindate=01/08/2014&enddate=28/08/2099&
    urlStr <- paste0("https://www.mlc.com.au/masterkeyWeb/execute/UnitPricesWQO?openAgent&reporttype=HistoricalDateRange&product=MasterKey%20Super%20Fundamentals&fund=MLC%20Horizon%207%20-%20Accelerated%20Growth%20Portfolio",
                     "&begindate=", startDate, # 01/08/2014
                     "&enddate=", endDate)  # 28/08/2099
    rawData <- GET(urlStr)  
    return (fread(sapply(rawData[6], intToUtf8), skip=5, sep=","))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("MLC Horizon 7 - Accelerated Growth Portfolio"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("date1", "Date:", value = as.Date("01/02/2014", format = "%d/%m/%Y")),
            dateInput("date2", "Date:", value = Sys.Date())
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            dygraphOutput("dygraph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ds01 <- reactive({
        proc02 <- MLCH7Data(as.character(input$date1, format = "%d/%m/%Y"),
                            as.character(input$date2, format = "%d/%m/%Y"))[, .(as.POSIXct(V3, format = "%d %b %Y"), V4, as.double(stri_replace_all(V5, regex="\";", "")))]
        # proc02 <- MLCH7Data(globalStart, globalEnd)[, .(as.POSIXct(V3, format = "%d %b %Y"), V4, as.double(stri_replace_all(V5, regex="\";", "")))]
    })
    output$dygraph <- renderDygraph({
        dygraph(ds01(),
                main = "i02.title") %>%
            dyOptions(useDataTimezone = FALSE) %>%
            # dyBarChart() %>%
            dyRoller(rollPeriod = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
