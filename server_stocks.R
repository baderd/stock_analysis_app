# Define server -------------------------------------------------------------

server_stocks <- function(input, output) {

  # TAB compare 2 stocks ------------------------------------------------------
  #
  # search
  output$tab_search_result <- DT::renderDT({
    shiny::req(input$text_search_symbol)
    tmp_tab <- get_name_from_symbol(
      keywords = input$text_search_symbol, key = input$alphakey
    )
    res <- DT::datatable(
      tmp_tab, rownames = FALSE, options = list(pageLength = 6)
    )
    return(res)
  })

  # plain stock prices
  tab_prices <- reactive({
    shiny::req(input$stock1, input$stock2)
    res <- get_stockprices_table(
      c(input$stock1, input$stock2),
      input$startdate,
      input$enddate
    )
    return(res)
  })

  # out: plot_compare_relative_prices ----------------------------------------
  #
  # tricky to implement if stocks do not have same date
  output$plot_compare_relative_prices <- renderPlotly({
    shiny::req(tab_prices())
    dt <- tab_prices()
    # check for small input
    if (length(unique(dt$date)) < 2) {
      return(plot_ly())
    }
    tmp_min_date <- dt[
      date == min(date),
      .(price_first_date = adjusted),
      by = yahoo_symbol
      ]
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


  # out: line plot period returns ------------------------------------------
  output$plot_compare_monthly_returns <- renderPlotly({
    plot_ly(tib_combo_return(), x = ~date) %>%
      add_lines(name = input$stock1, y = ~monthly_return1) %>%
      add_lines(name = input$stock2, y = ~monthly_return2) %>%
      layout(yaxis = list(title = "Monthly returns"))
  })



  #+ ----
  # TAB build portfolio ----------------------------------------------------
  #
  # startdate = "2015-01-01"
  # enddate = "2020-04-01"
  # vec_input_symbols <- c("FB", "NFLX", "AMZN")
  #
  vec_input_symbols <- reactive({
    shiny::req(input$stocklist)
    unlist(
      strsplit(gsub(" +", "", input$stocklist), "\n")
    )
  })


  # portfolio search --------------------------------------------------
  #
  output$tab_portfolio_search <- DT::renderDT({
    shiny::req(input$text_portfolio_search)
    tmp_tab <- get_name_from_symbol(
      keywords = input$text_portfolio_search, key = input$alphakey
    )
    res <- DT::datatable(
      tmp_tab[, c("symbol", "name", "type", "region"), with = F],
      rownames = FALSE, options = list(pageLength = 6)
    )
    return(res)
  })


  # portfolio prices -----------------------------------------------------
  #
  # plain stock prices
  tab_portfolio_prices <- reactive({
    shiny::req(vec_input_symbols())

    res <- get_stockprices_table(
      vec_input_symbols(),
      input$startdate,
      input$enddate
    )
    # search for trivial stock names
    vec_all_symbols <- as.character(unique(
      res[!is.na(yahoo_symbol), yahoo_symbol]
    ))
    if (length(vec_all_symbols) > 0 & nrow(TAB_STOCK_LISTING) > 0) {
      tmp_tab_names <- TAB_STOCK_LISTING[
        symbol %in% vec_all_symbols, .(yahoo_symbol = symbol, name)
        ]
      res <- merge(
        tmp_tab_names,
        res,
        by = "yahoo_symbol",
        all = T
      )
    }
    # fill remaining names
    res[is.na(name), name := yahoo_symbol]

    return(res)
  })


  # load custom potfolio ------------------------------------------------
  tab_custom_potfolio <- reactiveVal(value = data.table())
  observeEvent(
    eventExpr = input$file_portfolio_upload,
    handlerExpr = {
      tmp_tab <- data.table::fread(
        input$file_portfolio_upload$datapath
      )
      tmp_tab[, date := as.Date(date, format = "%Y-%m-%d")]

      # update value
      tab_custom_potfolio(tmp_tab)
    }
  )

  # merge API and custom data ----------------------------------------------
  #
  tab_all_prices <- reactive({
    shiny::req(tab_portfolio_prices(), tab_custom_potfolio())
    tab_yahoo <- tab_portfolio_prices()
    tab_custom <- tab_custom_potfolio()

    if (nrow(tab_custom) > 0) {
      res <- merge.data.table(
        tab_yahoo,
        tab_custom,
        by = intersect(
          names(tab_yahoo), names(tab_custom)
        ),
        all = TRUE
      )
      # browser()
    } else {
      res <- tab_yahoo
    }
    return(res)
  })


  #+ ----
  #
  # out: validate input --------------------------------------------------------
  tab_validated_input <- reactiveVal(value = data.table())
  observeEvent(
    eventExpr = tab_all_prices(),
    handlerExpr = {
      # browser()
      tmp_tab <- merge(
        tab_all_prices(),
        data.table(name = vec_input_symbols()),
        by = "name",
        all = T
      )

      # count entries
      tmp_keys <- "name"
      if ("ISIN" %in% names(tmp_tab)) {
        tmp_keys <- c("ISIN", tmp_keys)
      }
      if ("yahoo_symbol" %in% names(tmp_tab)) {
        tmp_keys <- c("yahoo_symbol", tmp_keys)
      }
      res <- tmp_tab[!is.na(close), .(number_of_dates = .N), by = tmp_keys]
      res[, valid := number_of_dates > 1]

      # update result table
      tab_validated_input(res)
    }
  )
  output$tab_input_filtered <- DT::renderDataTable(
    expr = {
      shiny::req(tab_validated_input())
      res <- DT::datatable(
        tab_validated_input(),
        rownames = FALSE,
        options = list(pageLength = nrow(tab_validated_input()))
      )
      return(res)
    }
  )

  # out: plot_portfolio_relative_prices --------------------------------------
  #
  output$plot_portfolio_relative_prices <- renderPlotly({
    # browser()
    shiny::req(tab_all_prices())
    dt <- tab_all_prices()

    # compute realitve price to first valid date with price info available
    dt[
      !is.na(close),
      first_valid_date := min(date, na.rm = T),
      by = name
      ]
    tmp_min_date <- dt[
      date == first_valid_date, .(price_first_date = close), by = name
      ]
    dt <- dt[tmp_min_date, on = "name"]
    dt[, price_relative_to_first_date := close/price_first_date]
    # browser()

    # build shared data object
    shared_portfolio <- highlight_key(dt, ~name, group = "portfolio")

    # build plot
    p <- shared_portfolio %>%
      plot_ly(
        x = ~date,
        y = ~price_relative_to_first_date,
        color = I("grey40"),
        split = ~name,
        hoverinfo = "text",
        text = ~paste0(
          "Name: ", name,
          "<br>SYMBOL: ", yahoo_symbol,
          "<br>Relative price: ", round(price_relative_to_first_date, 2),
          "<br>Price: ", round(close, 2),
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
    return(p)
  })

  # out: corheatmap--------------------------------------------------------
  #
  # compute correlation between assets
  output$corheatmap <- renderPlotly({
    shiny::req(tab_all_prices())
    tab_wide <- dcast(
      tab_all_prices(),
      date ~ name,
      value.var = "close"
    )
    mat <- tab_wide[, -1]
    # browser()
    heatmaply_cor(
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
        unique(c(input$stock1, input$stock2, vec_input_symbols())),
        startdate = input$startdate,
        enddate = input$enddate
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
