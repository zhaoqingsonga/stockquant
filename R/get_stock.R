
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




#' 获取股票数据
#'
#' 这个函数从 Yahoo Finance 获取指定股票代码的历史数据。
#' 用户可以指定股票代码、开始日期和结束日期来获取相应的股票历史数据。
#'
#' @param stock_code 一个字符型参数，指定股票代码，例如 "AAPL" 或 "000001.SS"。
#' @param start_date 一个字符型参数，指定开始日期，格式为 "YYYY-MM-DD"（默认为 "2020-01-01"）。
#' @param end_date 一个字符型参数，指定结束日期，格式为 "YYYY-MM-DD"（默认为当前日期）。
#' @return 返回一个 xts 对象，包含指定股票的历史数据，包括开盘价、最高价、最低价、收盘价、成交量等。
#' @examples
#' \dontrun{
#' stock_data <- getStocks("AAPL", "2020-01-01", "2021-01-01")
#' }
#' @export
getStocks <- function(stock_code, start_date = "2020-01-01", end_date = Sys.Date()) {
  # 使用 tryCatch 函数来捕获可能出现的错误
  stock_data <- tryCatch({
    # 尝试获取股票数据
    data <- getSymbols(stock_code, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    # 为数据列命名
    names(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    return(data)
  }, error = function(e) {
    # 如果出现错误，打印错误信息并返回 NULL
    message(paste("Failed to retrieve data for", stock_code, ":", e$message))
    return(NULL)
  })

  return(stock_data)
}



#' 获取多个股票的历史数据
#'
#' 该函数根据提供的股票代码列表（stocklist）获取多个股票的历史数据，并返回一个包含股票数据的列表。每个列表项包含一个股票的时间序列数据和该股票的名称。
#'
#' @param stocklist 一个字符向量，包含需要获取历史数据的股票代码。默认为 \code{c("600104.SS", "600028.SS")}。
#' @param start 一个日期字符串，表示获取历史数据的开始日期。默认为 \code{"2020-01-01"}。
#' @param end 一个日期字符串，表示获取历史数据的结束日期。默认为当前日期（\code{Sys.Date()}）。
#'
#' @return 返回一个列表，其中每个元素对应一个股票的历史数据，列表的名字是股票的代码，每个元素是一个数据框或时间序列对象。
#'         每个数据框包含该股票的历史数据，并添加了一个名为 \code{name} 的列，表示股票的名称。
#'
#' @examples
#' \dontrun{
#' stock_data <- getListStocks(c("600104.SS", "600028.SS"))
#' }
#'
#' @export
getListStocks <- function(stocklist = c("600104.SS", "600028.SS"),
                          start = "2020-01-01",
                          end = Sys.Date()) {
  listC <- list()
  for (iname in stocklist) {
    listC[[iname]] <- getStocks(iname, start, end)
    listC[[iname]]$name <- NA
    listC[[iname]]$name <- iname
  }
  return(listC)
}







