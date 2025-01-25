#' 生成MACD买卖点信号
#'
#' 该函数通过计算MACD指标来生成股票买卖点信号。基于MACD线和信号线的交叉情况，
#' 当MACD线从下向上穿越信号线时，标记为买入信号（1）；当MACD线从上向下穿越信号线时，标记为卖出信号（0）。
#' 买入后会持续持有，直到遇到卖出信号。
#'
#' @param stock_data 股票数据，通常是一个xts或zoo对象，包含了股票的历史数据（如收盘价）。
#' @return 返回一个包含买卖点标记的data.frame，包含如下列：
#' \item{Date}{股票数据的日期}
#' \item{Close}{股票的收盘价}
#' \item{MACD}{计算得到的MACD线}
#' \item{Signal_Line}{计算得到的信号线}
#' \item{Trade_Signal}{买卖点标记，1表示买入信号，0表示卖出信号，NA表示没有信号}
#' @examples
#' \dontrun{
#' macd_signal(stock_data)
#' }
macd_signal <- function(stock_data) {
  # 提取收盘价
  close_prices <- Cl(stock_data)

  # 计算MACD指标
  macd_data <- MACD(close_prices, 12, 26, 9, wilder = FALSE)
  macd_line <- macd_data$macd
  signal_line <- macd_data$signal

  # 创建信号列
  signals <- data.frame(Date = zoo::index(stock_data),
                        Close = close_prices,
                        MACD = macd_line,
                        Signal_Line = signal_line,
                        MACD_Trade = NA_real_)

  # 找出交叉点：判断MACD线从下向上穿越信号线（买入信号）
  buy_signal <- macd_line > signal_line & lag(macd_line) <= lag(signal_line)

  # 找出交叉点：判断MACD线从上向下穿越信号线（卖出信号）
  sell_signal <- macd_line < signal_line & lag(macd_line) >= lag(signal_line)

  # 买入信号时标记为1，卖出信号时标记为0
  signals$MACD_Trade[buy_signal] <- 1
  signals$MACD_Trade[sell_signal] <- 0

  # 处理买入后持有的标记：从买入点后开始持有标记为1，直到卖出
  signals$MACD_Trade <- zoo::na.locf(signals$MACD_Trade, fromLast = FALSE, na.rm = FALSE)

  # 卖出后空仓：在卖出后，设置为0（空仓状态）
  signals$MACD_Trade[sell_signal] <- 0

  return(signals)
}


#' 生成SMA买卖点信号
#'
#' 该函数通过计算短期SMA和长期SMA，基于SMA交叉情况生成买卖点信号。
#' 当短期SMA从下向上穿越长期SMA时，标记为买入信号（1）；当短期SMA从上向下穿越长期SMA时，标记为卖出信号（0）。
#' 买入后会持续持有，直到遇到卖出信号。
#'
#' @param stock_data 股票数据，通常是一个xts或zoo对象，包含了股票的历史数据（如收盘价）。
#' @param short_window 短期SMA的窗口大小，默认为50。
#' @param long_window 长期SMA的窗口大小，默认为200。
#' @return 返回一个包含买卖点标记的data.frame，包含如下列：
#' \item{Date}{股票数据的日期}
#' \item{Close}{股票的收盘价}
#' \item{SMA_Short}{计算得到的短期SMA}
#' \item{SMA_Long}{计算得到的长期SMA}
#' \item{Trade_Signal}{买卖点标记，1表示买入信号，0表示卖出信号，NA表示没有信号}
#' @examples
#' \dontrun{
#' sma_signal(stock_data)
#' }
sma_signal <- function(stock_data, short_window = 50, long_window = 200) {
  # 提取收盘价
  close_prices <- Cl(stock_data)

  # 计算短期和长期SMA
  sma_short <- SMA(close_prices, short_window)  # 短期SMA
  sma_long <- SMA(close_prices, long_window)   # 长期SMA

  # 创建信号列
  signals <- data.frame(Date = zoo::index(stock_data),
                        Close = close_prices,
                        SMA_Short = sma_short,
                        SMA_Long = sma_long,
                        SMA_Trade = NA_real_)  # 初始Trade_Signal为NA

  # 找出交叉点：判断短期SMA从下向上穿越长期SMA（买入信号）
  buy_signal <- sma_short > sma_long & lag(sma_short) <= lag(sma_long)

  # 找出交叉点：判断短期SMA从上向下穿越长期SMA（卖出信号）
  sell_signal <- sma_short < sma_long & lag(sma_short) >= lag(sma_long)

  # 买入信号时标记为1，卖出信号时标记为0
  signals$SMA_Trade[buy_signal] <- 1
  signals$SMA_Trade[sell_signal] <- 0

  # 处理买入后持有的标记：从买入点后开始持有标记为1，直到卖出
  signals$SMA_Trade <- zoo::na.locf(signals$SMA_Trade, fromLast = FALSE, na.rm = FALSE)

  # 卖出后空仓：在卖出后，设置为0（空仓状态）
  signals$SMA_Trade[sell_signal] <- 0

  return(signals)
}

#' RSI交易信号函数
#'
#' 计算基于RSI（相对强弱指标）的买卖交易信号。买入信号发生在RSI小于指定超卖阈值，卖出信号发生在RSI大于指定超买阈值。
#'
#' @param stock_data xts格式的股票数据，需包含收盘价（Close）。
#' @param rsi_period 整数，RSI的计算周期（默认值为14）。
#' @param overbought 阈值，大于此值时触发卖出信号（默认值为70）。
#' @param oversold 阈值，小于此值时触发买入信号（默认值为30）。
#' @return 包含日期、RSI值以及交易信号的data.frame，其中Trade_Signal列标记交易信号：1表示买入，0表示卖出。
#' @examples
#' library(quantmod)
#' getSymbols("AAPL")
#' rsi_signals <- rsi_signal(AAPL)
#' head(rsi_signals)
#' @export
rsi_signal <- function(stock_data, rsi_period = 14, overbought = 70, oversold = 30) {
  # 提取收盘价
  close_prices <- Cl(stock_data)

  # 计算RSI指标
  rsi_values <- RSI(close_prices, n = rsi_period)

  # 创建信号列
  signals <- data.frame(Date = zoo::index(stock_data),
                        Close = close_prices,
                        RSI = rsi_values,
                        RSI_Trade = NA_real_)

  # 找出买入信号：当RSI小于超卖阈值
  buy_signal <- rsi_values < oversold

  # 找出卖出信号：当RSI大于超买阈值
  sell_signal <- rsi_values > overbought

  # 买入信号时标记为1，卖出信号时标记为0
  signals$RSI_Trade[buy_signal] <- 1
  signals$RSI_Trade[sell_signal] <- 0

  # 处理买入后持有的标记：从买入点后开始持有标记为1，直到卖出
  signals$RSI_Trade <- zoo::na.locf(signals$RSI_Trade, fromLast = FALSE, na.rm = FALSE)

  # 卖出后空仓：在卖出后，设置为0（空仓状态）
  signals$RSI_Trade[sell_signal] <- 0

  return(signals)
}


#' 基于成交量突破的交易信号生成函数
#'
#' 该函数根据短期和长期成交量均线的突破生成买卖信号。若成交量突破均线，则生成买入信号；若成交量回落至均线以下，则生成卖出信号。
#'
#' @param stock_data 一个包含股票历史数据的 xts 对象，必须包含收盘价（Cl）和成交量（Vo）数据。
#' @param short_window 短期成交量均线的窗口大小，默认值为5（即5日均线）。
#' @param long_window 长期成交量均线的窗口大小，默认值为20（即20日均线）。
#'
#' @return 返回一个数据框，其中包含日期、收盘价、成交量、短期和长期成交量均线，以及生成的交易信号。
#'         交易信号列的值为1表示买入信号，0表示卖出信号，NA表示没有信号。
#'
#' @examples
#' # 假设你有一个包含历史数据的 stock_data 对象
#' # result <- volume_breakout_signal(stock_data)
#'
volume_signal <- function(stock_data, short_window = 10, long_window = 30) {
  # 提取收盘价和成交量
  close_prices <- Cl(stock_data)
  volume <- Vo(stock_data)

  # 计算短期和长期成交量均线
  ma_volume_short <- SMA(volume, short_window)  # 短期成交量均线
  ma_volume_long <- SMA(volume, long_window)    # 长期成交量均线

  # 创建信号列
  signals <- data.frame(Date = zoo::index(stock_data),
                        Close = close_prices,
                        Volume = volume,
                        MA_Volume_Short = ma_volume_short,
                        MA_Volume_Long = ma_volume_long,
                        VOLUME_Trade = NA_real_)  # 初始VOLUME_Trade为NA

  # 找出成交量突破：当成交量大于短期和长期均线时（买入信号）
  buy_signal <- volume > ma_volume_short & volume > ma_volume_long

  # 找出成交量回落：当成交量小于短期和长期均线时（卖出信号）
  sell_signal <- volume < ma_volume_short & volume < ma_volume_long

  # 买入信号时标记为1，卖出信号时标记为0
  signals$VOLUME_Trade[buy_signal] <- 1
  signals$VOLUME_Trade[sell_signal] <- 0

  # 处理买入后持有的标记：从买入点后开始持有标记为1，直到卖出
  signals$VOLUME_Trade <- zoo::na.locf(signals$VOLUME_Trade, fromLast = FALSE, na.rm = FALSE)

  # 卖出后空仓：在卖出后，设置为0（空仓状态）
  signals$VOLUME_Trade[sell_signal] <- 0

  return(signals)
}


#' 生成股票交易信号
#'
#' 该函数根据输入的股票数据，计算多个技术指标（如SMA、MACD、RSI、成交量等）并返回相应的买卖信号。
#' 用于分析股票市场的多种技术指标，并为每个指标生成买卖信号列。
#'
#' @param stock_data 包含股票历史数据的`xts`或`data.frame`对象，通常包括收盘价和成交量等信息。
#' @return 返回一个包含日期、收盘价、成交量和多个交易信号的`data.frame`。信号列包括：
#'   \describe{
#'     \item{SMA_Trade}{基于简单移动平均（SMA）的交易信号，1表示买入，0表示卖出，NA表示无信号。}
#'     \item{MACD_Trade}{基于MACD的交易信号，1表示买入，0表示卖出，NA表示无信号。}
#'     \item{RSI_Trade}{基于RSI的交易信号，1表示买入，0表示卖出，NA表示无信号。}
#'     \item{VOLUME_Trade}{基于成交量的交易信号，1表示买入，0表示卖出，NA表示无信号。}
#'   }
#' @examples
#' \dontrun{
#' # 示例：加载某支股票数据并计算交易信号
#' getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = Sys.Date())
#' signals <- stock_signal(AAPL)
#' head(signals)
#' }
#' @export
stock_signal <- function(stock_data) {
  # 提取收盘价和成交量
  close_prices <- Cl(stock_data)
  volume <- Vo(stock_data)

  # 创建信号列
  signals <- data.frame(Date = zoo::index(stock_data),
                        Close = close_prices,
                        Volume = volume,
                        SMA_Trade = sma_signal(stock_data)$SMA_Trade,
                        MACD_Trade = macd_signal(stock_data)$MACD_Trade,
                        RSI_Trade = rsi_signal(stock_data)$RSI_Trade,
                        VOLUME_Trade = volume_signal(stock_data)$VOLUME_Trade
  )

  return(signals)
}




