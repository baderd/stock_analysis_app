#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

library(data.table)
library(plotly)
library(tidyquant)
library(reshape2)
library(heatmaply)

source("functions.R")
source("server_stocks.R")


# Define UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Stocks DIY"),
    dashboardSidebar(
        sidebarMenu(
            id = "menu",
            menuItem("Compare stocks", tabName = "compare"),
            menuItem("Portfolio analysis", tabName = "portfolio"),
            menuItem("Rawdata", tabName = "data"),
            menuItem("Info about the app", tabName = "info")
        ),
        textInput(inputId = "startdate", "Start date", today() - 90),
        textInput(inputId = "enddate", "End date", today()),
        textInput(inputId = "alphakey", "API key AlphaVantage", NULL),
        conditionalPanel(
            condition = "input.menu == 'compare'",
            textInput(inputId = "stock1", "Yahoo symbol 1", "GOOG"),
            textInput(inputId = "stock2", "Yahoo symbol 2", "NFLX"),
        ),
        conditionalPanel(
            condition = "input.menu == 'portfolio'",
            HTML("&emsp;Compare the following list of <br>
                &emsp;Yahoo symbols (1 symbol per line)"),
            textAreaInput(
                inputId = "stocklist", 
                label = "Enter Yahoo symbol list", 
                value = "GOOG\nNFLX\nAMZN\nEVT.DE",
                height = "200px"
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "portfolio",
                h2("Analyze portfolio of stocks and funds"),
                h3("Validated input"),
                tableOutput("tab_input_filtered"),
                h3("Correlation of monthly returns"),
                plotlyOutput("corheatmap"),
            ),
            tabItem(tabName = "compare",
                h2("Compare price development"),
                plotlyOutput("plot_compare_relative_prices"),
                HTML("<br>"),
                h2("Plot monthly returns"),
                plotlyOutput("plot_compare_monthly_returns"),
                HTML("<br>"),
                h2("Statistics"),
                HTML("Capital Asset Pricing Model."),
                tableOutput("tab_capm"),
                HTML("<br>")
            ),
            tabItem(
                tabName = "data",
                h2("Input data"),
                DT::dataTableOutput("tab_rawdata"),
                HTML("<br>")
            ),
            tabItem(
                tabName = "info",
                h2("Analyze  stocks using Yahoo finance data"),
                HTML("Stock data is queried by the yahoo fincance API with R tidyquant package."),
                br(),
                a("Link to Yahoo Finance", href = "https://finance.yahoo.com/"),
                br(),
                a("Link to tidyquant package", href = "https://business-science.github.io/tidyquant/"),
                br(),
                h3("Rate of return"),
                a("Link to Wikipedia \"Rate of return\"", href = "https://en.wikipedia.org/wiki/Rate_of_return"),
                br(),
                HTML("
In finance, return is a profit on an investment. 
It comprises any change in value of the investment, and/or cash flows which the 
investor receives from the investment, such as interest payments or dividends. 
It may be measured either in absolute terms (e.g., dollars) or as a percentage 
of the amount invested. The latter is also called the holding period return.
<br>
A loss instead of a profit is described as a negative return, assuming the 
amount invested is greater than zero.
<br>
The rate of return is a profit on an investment over a period of time, 
expressed as a proportion of the original investment.[2] The time period is 
typically a year, in which case the rate of return is referred to as 
the annual return.
                    ")
            )
        )
    )
)




# Run the application 
shinyApp(ui = ui, server = server_stocks)
