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
library(DT)
library(crosstalk)
library(plotly)
library(tidyquant)
library(reshape2)
library(heatmaply)

source("functions.R")
source("server_stocks.R")
tmp_key_file <- file.path("~/Documents/api_key_alphavantage.txt")
if (!file.exists(tmp_key_file)) {
    tmp_key <- NULL
} else {
    tmp_key <- readLines(tmp_key_file)
}

# Define UI ----
# #+ ----
ui <- dashboardPage(
    dashboardHeader(title = "Stocks DIY"),
    dashboardSidebar(
        sidebarMenu(
            id = "menu",
            menuItem("Compare stocks", tabName = "compare"),
            menuItem("Portfolio creation", tabName = "portfolio_create"),
            menuItem("Portfolio analysis", tabName = "portfolio_plot"),
            menuItem("Rawdata", tabName = "data"),
            menuItem("Info about the app", tabName = "info")
        ),
        hr(),
        textInput(inputId = "startdate", "Start date", today() - 90),
        textInput(inputId = "enddate", "End date", today()),
        textInput(inputId = "alphakey", "API key AlphaVantage", tmp_key),
        HTML(
            '<a href=\"https://www.alphavantage.co/support/#api-key\">Get you  API key here!</a>'
        ),
        hr(),
        conditionalPanel(
            condition = "input.menu == 'portfolio_create'"
        )
    ),
    dashboardBody(
        tabItems(
            # ui portfolio create ---------------------------------------------
            tabItem(
                tabName = "portfolio_create",
                h2("Analyze portfolio of stocks and funds"),
                fluidRow(
                    box(
                        HTML("Compare the following list of Yahoo symbols (1 symbol per line)"),
                        textAreaInput(
                            inputId = "stocklist", 
                            label = "Enter Yahoo symbol list", 
                            value = "GOOG \nNFLX \nAMZN \nEVT.DE \nMTUAY \nMRK \nBMWYY \nBNTX",
                            height = "200px"
                        )
                    ),
                    box(
                    h3("Validated input"),
                    tableOutput("tab_input_filtered")
                )
            )
        ),
        # ui portfolio plot -----------------------------------------------
        tabItem(
            tabName = "portfolio_plot",
            h2("Analyze portfolio of stocks and funds"),
            h3("Compare relative price development"),
            plotlyOutput("plot_portfolio_relative_prices"),
            h3("Correlation of monthly returns"),
            plotlyOutput("corheatmap"),
        ),
        # ui compare ----------------------------------------------------
        tabItem(tabName = "compare",
            h1("Compare 2 stocks"),
            textInput(
                inputId = "text_search_symbol", 
                "Enter search text for yahoo symbols (API key needed)", 
                "EVT.DE"
            ),
            DT::DTOutput("tab_search_result"),
            HTML("<br>"),
            h2("Compare price development"),
            div(
                style="display:inline-block",
                textInput(inputId = "stock1", "Yahoo symbol 1", "GOOG")
            ),
            div(
                style="display:inline-block",
                textInput(inputId = "stock2", "Yahoo symbol 2", "NFLX")
            ),
            HTML("<br>"),
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
        # ui rawdata -----------------------------------------------------
        tabItem(
            tabName = "data",
            h2("Input data"),
            DT::dataTableOutput("tab_rawdata"),
            HTML("<br>")
        ),
        # ui info -------------------------------------------------------
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
