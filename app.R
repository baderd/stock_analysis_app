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
                HTML(readLines("shiny_info.html"))
            )
        )
    )
)




# Run the application 
shinyApp(ui = ui, server = server_stocks)
