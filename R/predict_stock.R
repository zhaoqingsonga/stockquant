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


#' 股票价格预测函数
#'
#' 使用 Prophet 模型对股票数据进行预测。此函数读取股票数据、拟合 Prophet 模型，并预测未来一段时间的股票价格。
#'
#' @param stock_data 数据框，包含股票历史数据。默认值为 `stock_data_ss50[["600048.SS"]]`。
#' 数据框中需包含以下列：
#'   - `Date`：日期列，表示股票的交易日期。
#'   - `Adjusted`：调整后收盘价。
#' @param prediction_period 整数，表示预测的天数。默认值为 30。
#'
#' @return 返回一个 xts 对象，包含预测的未来日期及相应的预测价格。
#'
#' @examples
#' # 示例：对股票代码 "600048.SS" 的数据进行30天的预测
#' result <- stock_prediction_prophet(stock_data = stock_data_ss50[["600048.SS"]], prediction_period = 30)
#' print(result)
#'
#' @import prophet
#' @import dplyr
#' @export
stock_prediction_prophet <- function(stock_data=stock_data_hs300[["603290.SS"]],
                                     prediction_period = 30) {
  library(prophet)
  library(dplyr)

  # 读取数据
  data <- as.data.frame(stock_data)
  stockname<-data$name[1]

  data$Date <- rownames(data)
  # 准备 prophet 模型需要的数据格式
  df <- data %>%
    select(ds = all_of("Date"), y = all_of("Adjusted")) %>%
    mutate(ds = as.Date(ds),
           y=as.numeric(y))
  # df$ds<-lubridate::with_tz(df$ds, tzone = "Asia/Shanghai")

  # 创建并拟合 prophet 模型
  m <- prophet()
  m <- fit.prophet(m, df)

  # 创建未来日期数据框
  future <- make_future_dataframe(m, periods = prediction_period)

  # 进行预测
  forecast <- predict(m, future)

  # 提取未来预测结果
  future_forecast <- forecast %>%
    filter(ds > max(df$ds)) %>%
    select(ds, yhat) %>%
    mutate(yhat = round(yhat, 2))
  names(future_forecast)<-c("Date",stockname)
  #加一天真实数据
  last_true<-tail(df,1)
  names(last_true)<-names(future_forecast)
  last_true[,2]<-round(last_true[,2],2)
  future_forecast<-rbind(last_true,future_forecast)
  #转化为xts类型
  xts_data <- xts(future_forecast[stockname], order.by = future_forecast$Date)
  return(xts_data)
}

#' 股票价格预测函数（使用 Holt-Winters 模型）
#'
#' 该函数用于对给定的股票数据进行价格预测，主要使用 Holt-Winters 模型。
#' 如果 Holt-Winters 模型拟合失败，将自动改用 ARIMA 模型进行预测。
#'
#' @param stock_data 股票数据，默认为 `stock_data_ss50[["600048.SS"]]`。
#'                   数据应为包含日期和调整后价格的格式。
#' @param prediction_period 预测的周期数，默认为 30 天。
#'
#' @return 返回一个 `xts` 对象，包含预测的未来日期和对应的股票价格。
#'
#' @import dplyr
#' @import xts
#' @import stats
#' @import forecast
#'
#' @examples
#' \dontrun{
#' # 假设 stock_data_ss50 是已经存在的股票数据集
#' result <- stock_prediction_hw(stock_data = stock_data_ss50[["600048.SS"]], prediction_period = 30)
#' print(result)
#' }
#'
#' 使用 Holt-Winters 或 ARIMA 模型预测股票价格
#'
#' 该函数使用 Holt-Winters 或 ARIMA 模型对股票数据进行预测。
#' 如果 Holt-Winters 模型拟合失败，则尝试使用 ARIMA 模型。
#'
#' @param stock_data 包含股票数据的 xts 对象。
#' @param prediction_period 预测周期，默认为 30 天。
#' @param frequency 时间序列频率。如果为 NULL，则尝试自动检测。
#' @param alpha Holt-Winters 模型的 alpha 参数，默认为 NULL，表示自动选择。
#' @param beta Holt-Winters 模型的 beta 参数，默认为 NULL，表示自动选择。
#' @param gamma Holt-Winters 模型的 gamma 参数，默认为 NULL，表示自动选择。
#'
#' @return 包含预测价格的 xts 对象。
#'
#' @examples
#' # 假设 stock_data_ss50 是包含股票数据的 xts 对象
#' predictions <- stock_prediction_hw(stock_data = stock_data_ss50[["600048.SS"]], prediction_period = 60)
#' print(predictions)
stock_prediction_hw <- function(stock_data = stock_data_ss50[["600048.SS"]], prediction_period = 30, frequency = NULL, alpha = NULL, beta = NULL, gamma = NULL) {
  library(dplyr)
  library(xts)
  library(forecast)

  # 读取数据
  data <- as.data.frame(stock_data)
  stockname <- data$name[1]  # 获取股票名称
  data$Date <- rownames(data)

  # 准备时间序列模型需要的数据格式
  df <- data %>%
    select(Date, Adjusted) %>%
    mutate(Date = as.Date(Date),
           Adjusted=as.numeric(Adjusted)
           )

  # 清理数据，处理缺失值
  df <- na.omit(df)

  # 确定时间序列频率
  if (is.null(frequency)) {
    # 尝试自动检测频率（可能会出错，需要进一步处理）
    frequency <- findfrequency(df$Adjusted)
    if (frequency == 1) {
      frequency <- 365 # 如果检测到频率为 1，则假设为日度数据
    }
  }

  # 设置时间序列索引
  ts_data <- ts(as.numeric(df$Adjusted), frequency = frequency, start = c(as.numeric(format(min(df$Date), "%Y")),
                                                                          as.numeric(format(min(df$Date), "%m"))))

  # 尝试使用 Holt-Winters 模型，并处理可能的错误
  hw_model <- tryCatch({
    HoltWinters(ts_data, alpha = alpha, beta = beta, gamma = gamma)
  }, error = function(e) {
    message("Error in HoltWinters model: ", e$message)
    return(NULL)
  })

  # 如果 Holt-Winters 失败，尝试使用 ARIMA 模型
  if (is.null(hw_model)) {
    arima_model <- tryCatch({
      auto.arima(ts_data)
    }, error = function(e) {
      message("Error in ARIMA model: ", e$message)
      return(NULL)
    })
    if (is.null(arima_model)) {
      stop("Both Holt-Winters and ARIMA models failed.")
    }
    forecast_result <- forecast(arima_model, h = prediction_period)
  } else {
    # 如果 Holt-Winters 成功，进行预测
    forecast_result <- forecast(hw_model, h = prediction_period)
  }

  # 提取未来预测结果
  future_forecast <- data.frame(
    Date = seq(max(df$Date) + 1, by = "day", length.out = prediction_period),
    Predicted_Price = round(forecast_result$mean, 2)
  )

  #加一天真实数据
  last_true<-tail(df,1)
  names(last_true)<-names(future_forecast)
  last_true[,2]<-round(last_true[,2],2)
  future_forecast<-rbind(last_true,future_forecast)

  # 转化为 xts 类型
  xts_data <- xts(future_forecast$Predicted_Price, order.by = future_forecast$Date)
  colnames(xts_data) <- stockname
  return(xts_data)
}


#error
#' 股票价格预测函数（LSTM 时间序列方法）
#'
#' 使用 LSTM（长短期记忆网络）对股票数据进行预测。此函数读取股票数据并预测未来一段时间的股票价格。
#'
#' @param stock_data 数据框，包含股票历史数据。
#' 数据框中需包含以下列：
#'   - `Date`：日期列，表示股票的交易日期。
#'   - `Adjusted`：调整后收盘价。
#' @param prediction_period 整数，表示预测的天数。默认值为 30。
#'
#' @return 返回一个 xts 对象，包含预测的未来日期及相应的预测价格。
#'
#' @examples
#' # 示例：对股票数据进行30天的预测
#' result <- stock_prediction_lstm(stock_data = stock_data, prediction_period = 30)
#' print(result)
#'
#' @import dplyr
#' @import keras
#' @import tensorflow
#' @import xts
#' @export
stock_prediction_lstm <- function(stock_data, prediction_period = 30) {
  library(dplyr)
  library(keras)
  library(tensorflow)
  library(xts)

  # 读取数据
  data <- as.data.frame(stock_data)
  stockname <- data$name[1]  # 获取股票名称

  data$Date <- rownames(data)
  # 准备 LSTM 模型需要的数据格式
  df <- data %>%
    select(Date, Adjusted) %>%
    mutate(Date = as.Date(Date))

  # 标准化数据
  scaler <- function(x) (x - min(x)) / (max(x) - min(x))
  inverse_scaler <- function(x, original) x * (max(original) - min(original)) + min(original)

  scaled_data <- scaler(df$Adjusted)

  # 创建时间序列数据集
  create_dataset <- function(data, look_back) {
    x <- list()
    y <- list()
    for (i in seq_len(length(data) - look_back)) {
      x[[i]] <- data[i:(i + look_back - 1)]
      y[[i]] <- data[i + look_back]
    }
    list(
      x = array(unlist(x), dim = c(length(x), look_back, 1)),
      y = array(unlist(y), dim = c(length(y), 1))
    )
  }

  look_back <- 10
  dataset <- create_dataset(scaled_data, look_back)

  # 构建 LSTM 模型
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(look_back, 1), return_sequences = FALSE) %>%
    layer_dense(units = 1)

  model %>% compile(
    optimizer = "adam",
    loss = "mean_squared_error"
  )

  # 训练模型
  model %>% fit(
    x = dataset$x,
    y = dataset$y,
    epochs = 50,
    batch_size = 32,
    verbose = 1
  )

  # 预测未来数据
  predictions <- c()
  input_seq <- tail(scaled_data, look_back)

  for (i in 1:prediction_period) {
    pred <- model %>% predict(array(input_seq, dim = c(1, look_back, 1)))
    predictions <- c(predictions, pred)
    input_seq <- c(input_seq[-1], pred)
  }

  # 反标准化预测结果
  predicted_prices <- inverse_scaler(predictions, df$Adjusted)

  # 创建预测结果数据框
  future_forecast <- data.frame(
    Date = seq(max(df$Date) + 1, by = "day", length.out = prediction_period),
    Predicted_Price = round(predicted_prices, 2)
  )

  # 转化为 xts 类型
  xts_data <- xts(future_forecast$Predicted_Price, order.by = future_forecast$Date)
  colnames(xts_data) <- stockname

  return(xts_data)
}





