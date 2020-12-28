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


tmp_key_file <- "~/Documents/api_key_alphavantage.txt"
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
    hr(),
    conditionalPanel(
      condition = "input.menu == 'portfolio_create'"
    )
  ),
  dashboardBody(
    tabItems(
      # ui compare ----------------------------------------------------
      tabItem(
        tabName = "compare",
        fluidPage(
          h1("Compare 2 stocks"),
          box(
            title = "Step1: Search the symbol of your favorite stock",
            width = 12,
            HTML("You get best search results with an ALphaVantage API key."),
            a(href="https://www.alphavantage.co/support/#api-key", "Get your API key here!"),
            br(),
            HTML("Alternatively search for symbols directly on yahoo finance:"),
            a(href="https://finance.yahoo.com/", "Link to Yahoo Finance"),
            br(),
            br(),
            textInput(
              inputId = "text_search_symbol",
              "Enter search text for yahoo symbols",
              "GOOG"
            ),
            DT::DTOutput("tab_search_result")
          ),
          box(
            title = "Step2: Compare price development",
            width = 12,
            div(
              style="display:inline-block",
              textInput(inputId = "stock1", "Yahoo symbol 1", "GOOG")
            ),
            div(
              style="display:inline-block",
              textInput(inputId = "stock2", "Yahoo symbol 2", "EVTCY")
            ),
            HTML("<br>"),
            plotlyOutput("plot_compare_relative_prices")
          ),
          box(
            title = "Step3: Advanced financial statistics",
            width = 12,
            column(
              width = 9,
              h3("Plot monthly returns"),
              HTML(readLines("info_rate_of_return.html")),
              br(),
              plotlyOutput("plot_compare_monthly_returns")
            ),
            column(
              width = 3,
              h3("Capital Asset Pricing Model"),
              tableOutput("tab_capm")
            )
          )
        )
      ),

      # ui portfolio create ---------------------------------------------
      tabItem(
        tabName = "portfolio_create",
        fluidPage(
          h2("Analyze portfolio of stocks and funds"),
          fluidRow(
            shinydashboard::box(
              title = "Create your portfolio",
              HTML("Compare the following list of Yahoo symbols (1 symbol per line)"),
              textAreaInput(
                inputId = "stocklist",
                label = "Enter Yahoo symbol list",
                value = "GOOG \nNFLX \nAMZN \nEVTCY \nMTUAY \nMRK \nBMWYY \nBNTX",
                height = "300px"
              )
            ),
            shinydashboard::box(
              title = "Upload custom file",
              HTML(
                "If your favorite stock or funds are not available on yahoo finance, ",
                "you can also upload your own stock price data in OHLC format. ",
                "Read more about this option at the tab: Info about the app."
              ),
              br(),
              fileInput(
                inputId = "file_portfolio_upload",
                label = "Choose CSV File",
                multiple = FALSE,
                accept = c(
                  "text/csv", "text/comma-separated-values,text/plain", ".csv"
                )
              ),
              HTML(
                "Expected column headers:<br>",
                "ISIN, name, date, open, high, low, close, volume"
              ),
              br(),
            ),
            box(
              textInput(
                inputId = "text_portfolio_search",
                "Enter search text for yahoo symbols",
                "Evotec"
              ),
              DT::DTOutput("tab_portfolio_search")
            )
          ),
          fluidRow(
            box(
              title = "Validated input",
              width = 12,
              DT::DTOutput("tab_input_filtered", height = "auto")
            )
          )
        )
      ), # end tabitem

      # ui portfolio plot -----------------------------------------------
      tabItem(
        tabName = "portfolio_plot",
        h2("Analyze portfolio of stocks and funds"),
        h3("Compare relative price development"),
        plotlyOutput("plot_portfolio_relative_prices", height = "500px"),
        h3("Correlation of monthly returns"),
        plotlyOutput("corheatmap", height = "700px"),
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
