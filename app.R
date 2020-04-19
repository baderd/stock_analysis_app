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
library(plotly)
library(tidyquant)
library(reshape2)
library(heatmaply)
# library(tidyverse)

my_stocks <- data.frame(
    fond_name = c("Magellan C", "Asian growth"), 
    symbol = c("0P00000PM7.F", "y")
)




# Define UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Stocks for bioinformatics"),
    dashboardSidebar(
        sidebarMenu(
            id = "menu",
            menuItem("Compare stocks", tabName = "compare"),
            menuItem("Portfolio analysis", tabName = "portfolio")
        ),
        textInput(inputId = "startdate", "Start date", "2015-01-01"),
        textInput(inputId = "enddate", "End date", "2020-04-01"),
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
            ),
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "compare",
                HTML("Stock data is queried by the yahoo fincance API with R tidyquant package."),
                h2("Statistics"),
                HTML("Capital Asset Pricing Model."),
                # dataTableOutput("tab_capm"),
                tableOutput("tab_capm"),
                HTML("<br>"),
                h2("Plot monthly returns"),
                plotlyOutput("plot2Lines"),
                HTML("<br>")
            ),
            tabItem(
                tabName = "portfolio",
                h2("Analyze portfolio of stocks and funds"),
                h3("Correlation of monthly returns"),
                plotlyOutput("corheatmap"),
            )
        )
    )
)

# Define server ----

get_monthly_stock_returns <- function(
    stock_symbol, 
    column_output, 
    startdate = "2015-01-01", 
    enddate = "2020-04-01"
){
    stock_symbol %>%
        tq_get(get = "stock.prices", from = startdate, to = enddate) %>%
        tq_transmute(
            select  = adjusted, 
            mutate_fun = periodReturn, 
            period = "monthly", 
            col_rename = column_output, 
            type = "arithmetic"
        )
}

server <- function(input, output) {
    
    # compare 2 stocks ------------------------------------------------------
    tib_combo_return <- reactive({
        tib_monthly_return1 <- get_monthly_stock_returns(
            input$stock1, "monthly_return1",
            input$startdate,
            input$enddate
        )
        tib_monthly_return2 <- get_monthly_stock_returns(
            input$stock2, "monthly_return2", 
            input$startdate,
            input$enddate
        )
        tib_combo_return <- dplyr::left_join(
            tib_monthly_return1, tib_monthly_return2, 
            by = c("date" = "date")
        )
        return(tib_combo_return)
    })
    
    # Form output summary table
    y <- reactive({
        tib_capm <-  tib_combo_return() %>%
            tq_performance(
                Ra = monthly_return1, 
                Rb = monthly_return2, 
                performance_fun = table.CAPM
            )
        df_res <- data.frame(
            Metic = names(tib_capm),
            Value = as.numeric(tib_capm)
        )
        return(df_res)
    })
    
    output$tab_capm <- renderTable({
        y()
    })
    
    # line plot 
    output$plot2Lines <- renderPlotly({
        plot_ly(tib_combo_return(), x = ~date) %>%
            add_lines(name = input$stock1, y = ~monthly_return1) %>%
            add_lines(name = input$stock2, y = ~monthly_return2) %>%
            layout(yaxis = list(title = "Monthly returns"))
    })
    
    # build portfolio stats:  ------------------------------------------------
    #
    # startdate = "2015-01-01" 
    # enddate = "2020-04-01"
    # vec_input_symbols <- c("FB", "NFLX", "AMZN") 
    tib_multi <- reactive({
        vec_input_symbols <- unlist(
            strsplit(gsub(" ", "", input$stocklist), "\n")
        )
        tib_multi <- vec_input_symbols %>% 
            tq_get(
                get = "stock.prices", from = input$startdate, to = input$enddate
            ) %>% 
            group_by(symbol)
        
        # add monthly return
        tib_multi <- dplyr::left_join(
            tib_multi %>%
                tq_transmute(
                    select  = adjusted, 
                    mutate_fun = periodReturn, 
                    period = "monthly", 
                    col_rename = "monthly_return", 
                    type = "arithmetic"
                ),
            tib_multi, 
            by = c("symbol", "date")
        )
        return(tib_multi)
    })
    # compute correlation between assets
    output$corheatmap <- renderPlotly({
        tab_wide <- dcast(
            tib_multi(), 
            substring(date, 1, 7) ~ symbol, 
            value.var = "monthly_return"
        )
        mat <- tab_wide[, -1]
        heatmaply_cor(
            # cor(mat[rowSums(is.na(mat)) == 0,], method = "pearson"),
            cor(mat, method = "pearson"),
            key.title = "Pearson\ncorrelation"   
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
