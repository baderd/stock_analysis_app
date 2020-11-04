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
  keywords, ...
){
  alphavantager::av_get(keywords = keywords, av_fun = "SYMBOL_SEARCH", ...)
}

