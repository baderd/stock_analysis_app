# R functions

#' get_stockprices_table
#' 
#' "stock.prices": Get the open, high, low, close, volume and adjusted stock 
#' prices for a stock symbol from Yahoo Finance. 
#' Wrapper for quantmod::getSymbols().
#' 
get_stockprices_table <- function(
  stock_symbols,  
  startdate = today() - 66, 
  enddate = today(), 
  ...
) {
  x <- tq_get(
    x = stock_symbols, get = "stock.prices", from = startdate, to = enddate, ...
  )
  x <- as.data.table(x)
  x[, symbol := factor(symbol)]
  return(x)
}

shape_hline <- function(y = 0, color = "black") {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color)
  )
}


get_monthly_stock_returns <- function(
  stock_symbol, 
  column_output = "monthly_return", 
  startdate = today() - 30, 
  enddate = today()
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

get_name_from_symbol <- function(
  keywords, key = input$alphakey, ...
){
  # browser()
  if (is.null(key) | is.na(key) | nchar(key) == 0 
    | is.null(keywords) | is.na(keywords) | nchar(keywords) == 0) {
    tab <- data.frame()
  } else {
    alphavantager::av_api_key(key)
    tab <- alphavantager::av_get(
      keywords = keywords, av_fun = "SYMBOL_SEARCH", ...
    )
  }
  return(tab)
}

