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
  setnames(x, "symbol", "yahoo_symbol")
  x[, yahoo_symbol := factor(yahoo_symbol)]
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
  keywords, key = input$alphakey, return_only_exact_name = FALSE, ...
){
  library(data.table)
  # browser()
  if (is.null(key) | is.na(key) | nchar(key) == 0) {
    tab <- data.frame(Warning = "No API key provided!")
  } else if (is.null(keywords) | is.na(keywords) | nchar(keywords) == 0) {
    tab <- data.frame(Warning = "No search provided!")
  } else {
    alphavantager::av_api_key(key)
    tab <- as.data.table(alphavantager::av_get(
      keywords = keywords, av_fun = "SYMBOL_SEARCH", ...
    ))
    if (return_only_exact_name) {
      exact_name <- tab[symbol == keywords, name]
      return(exact_name)
    }
  }
  return((tab))
}

#' load_and_merge_ohlc_from_finanzen_net
#'
#' Save files with formatted names:
#' "ohlc_ISIN_easy_name.csv"
#'
#' Example link for download:
#' https://www.finanzen.net/fonds/historisch/amundi-funds-top-european-players-a-c-lu1883868819
#'
load_and_merge_ohlc_from_finanzen_net <- function(
  dir_input = "~/Documents/ebase/",
  pattern_input =  "ohlc",
  basename_output = "merged_finanzen_net.csv"
){
  tmp_input_files <- list.files(
    dir_input, pattern = pattern_input, full.names = T
  )
  tab_merged <- rbindlist(lapply(
    tmp_input_files, function(fn) {
      tmp_tab <- fread(fn, sep = ",")
      tmp_tab[, bname := basename(fn)]
      tmp_tab[, ISIN := tstrsplit(bname, "_", keep = 2)]
      tmp_tab[,
        name := gsub(
          paste0("ohlc_", ISIN, "_(.*)\\.csv"), "\\1", bname
        ),
        by = Datum
        ]
      tmp_tab[, name := gsub("_", " ", name)]
      tmp_tab[, bname := NULL]
    }
  ))
  setnames(
    tab_merged,
    c("date", "open", "close", "high", "low", "volume", "ISIN", "name")
  )
  tab_merged[, date := as.Date(date, format = "%d.%m.%Y")]
  setcolorder(tab_merged, c("ISIN", "name"))

  fwrite(tab_merged, file = file.path(dir_input, basename_output))
}
# load_and_merge_ohlc_from_finanzen_net()


load_and_merge_stock_listings <- function() {
  library(data.table)
  file_stock_lists <- list.files(
    path = "~/Documents/ebase/",
    pattern = "listing_status_stocks_and_etfs_.+.csv",
    full.names = T
  )
  TAB_STOCK_LISTING <- rbindlist(lapply(file_stock_lists, fread), fill = T)
  TAB_STOCK_LISTING <- unique(TAB_STOCK_LISTING[, .(symbol, name, exchange, assetType)])
  fwrite(TAB_STOCK_LISTING, file = "listing_status_stocks_and_etfs.csv")
}
# load_and_merge_stock_listings()

