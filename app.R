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




# Define UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Stocks for bioinformatics"),
    dashboardSidebar(
        sidebarMenu(
            id = "menu",
            menuItem("Portfolio analysis", tabName = "portfolio"),
            menuItem("Compare stocks", tabName = "compare"),
            menuItem("Info about the app", tabName = "info")
        ),
        textInput(inputId = "startdate", "Start date", "2018-01-01"),
        textInput(inputId = "enddate", "End date", "2020-04-01"),
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
        ),
        conditionalPanel(
            condition = "input.menu == 'compare'",
            textInput(inputId = "stock1", "Yahoo symbol 1", "GOOG"),
            textInput(inputId = "stock2", "Yahoo symbol 2", "NFLX"),
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




# Define server -------------------------------------------------------------

get_monthly_stock_returns <- function(
    stock_symbol, 
    column_output = "monthly_return", 
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
    output$tab_input_filtered <- renderTable({
        vec_input_symbols <- unlist(
            strsplit(gsub(" ", "", input$stocklist), "\n")
        )
        tib_multi_stocks <- vec_input_symbols %>% 
            tq_get(
                get = "stock.prices", from = input$startdate, to = input$enddate
            ) %>% 
            group_by(symbol)
        # Check was could be matched
        tmp_tab <- data.frame(Yahoo_symbol = vec_input_symbols)
        tmp_filtered <- data.frame(
            Yahoo_symbol = unique(tib_multi_stocks$symbol),
            valid_input = TRUE
        )
        res <- merge(tmp_tab, tmp_filtered, by = "Yahoo_symbol", all = T)
        return(res)
    })
    
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
        # browser()
        heatmaply_cor(
            # cor(mat[rowSums(is.na(mat)) == 0,], method = "pearson"),
            cor(mat, method = "pearson", use = "pairwise.complete.obs"),
            key.title = "Pearson\ncorrelation"   
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
