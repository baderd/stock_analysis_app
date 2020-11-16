# Define server -------------------------------------------------------------

server_stocks <- function(input, output) {
  
  # TAB compare 2 stocks ------------------------------------------------------
  # 
  # search
  output$tab_search_result <- DT::renderDT(
    get_name_from_symbol(
      keywords = input$text_search_symbol, key = input$alphakey
    ),
    rownames = FALSE, 
    options = list(pageLength = 6)
  )
  # 
  # plain stock prices
  tab_prices <- reactive(get_stockprices_table(
    c(input$stock1, input$stock2), 
    input$startdate,
    input$enddate
  ))
  
  # out: plot_compare_relative_prices ----------------------------------------
  # 
  # tricky to implement if stocks do not have same date
  output$plot_compare_relative_prices <- renderPlotly({
    dt <- tab_prices()
    tmp_min_date <- dt[date == min(date), .(price_first_date = adjusted), by = yahoo_symbol]
    dt <- dt[tmp_min_date, on = "yahoo_symbol"]
    dt[, price_relative_to_first_date := adjusted/price_first_date]
    # browser()
    
    plot_ly(
      data = dt, 
      x = ~date, 
      y = ~price_relative_to_first_date, 
      color = ~yahoo_symbol, 
      colors = c("dodgerblue", "orange"),
      text = ~paste0("Adjusted price: ", round(adjusted, 2))
    ) %>%
      add_lines() %>%
      layout(
        shapes = list(shape_hline(1)),
        yaxis = list(title = "Relative price to first datum", type = "log")
      )
  })
  
  # period returns 2 stocks ---------------------------------------------
  # 
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
  
  # out: tab_capm ---------------------------------------------------------
  # 
  # Form output summary table
  output$tab_capm <- renderTable({
    tib_capm <-  tib_combo_return() %>%
      tq_performance(
        Ra = monthly_return1, 
        Rb = monthly_return2, 
        performance_fun = table.CAPM
      )
    df_res <- data.frame(
      Metric = names(tib_capm),
      Value = as.numeric(tib_capm)
    )
    return(df_res)
  })
  
  
  # line plot 
  output$plot_compare_monthly_returns <- renderPlotly({
    plot_ly(tib_combo_return(), x = ~date) %>%
      add_lines(name = input$stock1, y = ~monthly_return1) %>%
      add_lines(name = input$stock2, y = ~monthly_return2) %>%
      layout(yaxis = list(title = "Monthly returns"))
  })
  
  #+ ----
  # TAB build portfolio stats:  ------------------------------------------------
  #
  # startdate = "2015-01-01" 
  # enddate = "2020-04-01"
  # vec_input_symbols <- c("FB", "NFLX", "AMZN") 
  # 
  vec_input_symbols <- reactive({
    unlist(
      strsplit(gsub(" ", "", input$stocklist), "\n")
    )
  })
  
  # validate input --------------------------------------------------------
  output$tab_input_filtered <- renderTable({
    tib_multi_stocks <- vec_input_symbols() %>% 
      tq_get(
        get = "stock.prices", from = input$startdate, to = input$enddate
      ) %>% 
      group_by(symbol)
    # Check was could be matched
    tmp_tab <- data.frame(Yahoo_symbol = vec_input_symbols())
    tmp_filtered <- data.frame(
      Yahoo_symbol = unique(tib_multi_stocks$symbol),
      valid_input = TRUE
    )
    res <- merge(tmp_tab, tmp_filtered, by = "Yahoo_symbol", all = T)
    return(res)
  })
  
  # portfolio prices -----------------------------------------------------
  # 
  # plain stock prices
  tab_portfolio_prices <- reactive(get_stockprices_table(
    vec_input_symbols(), 
    input$startdate,
    input$enddate
  ))
  
  
  # period returns ---------------------------------------------------------
  tib_multi <- reactive({
    # vec_input_symbols <- unlist(
    #   strsplit(gsub(" ", "", input$stocklist), "\n")
    # )
    tib_multi <- vec_input_symbols() %>% 
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
  
  # out: plot_portfolio_relative_prices --------------------------------------
  # 
  output$plot_portfolio_relative_prices <- renderPlotly({
    dt <- tab_portfolio_prices()
    tmp_min_date <- dt[date == min(date), .(price_first_date = adjusted), by = yahoo_symbol]
    dt <- dt[tmp_min_date, on = "yahoo_symbol"]
    dt[, price_relative_to_first_date := adjusted/price_first_date]
    # browser()
    
    # build shared data object
    shared_portfolio <- highlight_key(dt, ~yahoo_symbol, group = "portfolio") 
    
    # build plot
    p <- shared_portfolio %>%
      plot_ly(
        x = ~date, 
        y = ~price_relative_to_first_date, 
        color = I("grey40"),
        split = ~yahoo_symbol, 
        hoverinfo = "text",
        text = ~paste0(
          "SYMBOL: ", yahoo_symbol,
          "<br>Relative price: ", round(price_relative_to_first_date, 2),
          "<br>Price: ", round(adjusted, 2),
          "<br>Date: ", date
        )
      ) %>%
      add_lines(showlegend = FALSE) %>%
      layout(
        hovermode = "closest",
        shapes = list(shape_hline(1)),
        yaxis = list(title = "Relative price to first datum", type = "log")
      ) %>%
      highlight(
        on = "plotly_selected",
        off = "plotly_doubleclick",
        dynamic = TRUE,
        selectize = TRUE
      )
  })
  
  # out: corheatmap--------------------------------------------------------
  # 
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
  
  
  #+ ----
  # TAB rawdata ------------------------------------------------------------
  # 
  output$tab_rawdata <- DT::renderDataTable(
    {
      get_stockprices_table(
        unique(c(input$stock1, input$stock2, vec_input_symbols()))
      )
    }, 
    rownames = FALSE, 
    filter = "top", 
    extensions = 'Buttons',
    options = list(
      pageLength = 20,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel')
    )
  )
  
  # end ---- 
}