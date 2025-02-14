#' 绘制股票分析图（包含残差分析和蜡烛图）
#'
#' 该函数获取指定股票的历史数据，进行线性回归残差分析，并生成蜡烛图，保存为 PDF 文件。
#'
#' @param stockname 字符串类型，表示股票代码，默认为 \code{"600048.SS"}。
#'
#' @return 无返回值。函数生成一个 PDF 文件，包含蜡烛图、残差的箱线图和直方图。
#'
#' @details
#' 函数获取指定股票的日频历史数据，并对其进行分析：
#' \itemize{
#'   \item 蜡烛图：显示股票的开盘、最高、最低和收盘价的变化趋势。
#'   \item 箱线图：展示线性回归残差的分布特征。
#'   \item 直方图：展示残差的分布情况。
#' }
#'
#' @examples
#' \dontrun{
#' plot_stock("600048.SS")
#' }
#'
#' @export
plot_stock <- function(stockname = "600048.SS") {
  # 获取股票数据
  ss001 <- getStocks(stock = stockname, start = "2022-01-01", end = Sys.Date())

  # 计算残差
  mydata <- na.omit(ss001)
  mydata$number <- 1:nrow(mydata)
  mylm <- lm(Adjusted ~ number, mydata)
  mydata$residuals <- as.vector(mylm$residuals)

  # 保存 PDF 图表，宽度为高度的两倍
  pdf(paste(stockname, ".pdf", sep = ""), width = 12, height = 6)

  # 调整布局，3 行 1 列
  #par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))  # 调整边距

  # 绘制蜡烛图
  library(quantmod)
  chartSeries(ss001, name = paste("蜡烛图:", stockname), theme = chartTheme("white"))


  # 箱线图(残差)
  myboxplot <- boxplot(mydata$residuals, xlab = paste("残差箱线图:", stockname))
  abline(h = tail(mydata$residuals, 1), col = "red")

  # 箱线图(价格)
  myboxplot_price <- boxplot(mydata$Adjusted, xlab = paste("价格箱线图:", stockname))
  abline(h = tail(mydata$Adjusted, 1), col = "red")


  # 直方图残差
  hist(mydata$residuals, breaks = 100,
       col = 'darkgray', border = 'white',
       xlab = paste("残差直方图:", stockname))
  abline(v = tail(mydata$residuals, 1), col = "red")
  abline(v = myboxplot$stats[2], col = "blue")
  abline(v = myboxplot$stats[3], col = "blue")
  abline(v = myboxplot$stats[4], col = "blue")

  # 直方图价格
  hist(mydata$Adjusted, breaks = 100,
       col = 'darkgray', border = 'white',
       xlab = paste("价格直方图:", stockname))
  abline(v = tail(mydata$Adjusted, 1), col = "red")
  abline(v = myboxplot_price$stats[2], col = "blue")
  abline(v = myboxplot_price$stats[3], col = "blue")
  abline(v = myboxplot_price$stats[4], col = "blue")



  dev.off()
}



#' 画曲线图
#'
#' 该函数用于绘制股票数据的曲线图，并添加不同方法的平滑曲线。
#'
#' @param ssm 股票数据，默认为 `stock_data_ss50[[2]]`。函数会将其转换为数据框，并从行名中提取日期信息。
#'
#' @return 返回一个使用 `ggplot2` 包创建的曲线图对象，图中包含股票调整价格的折线图以及两种平滑曲线（GLM 和 GAM）。
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_smooth
#' @importFrom stats as.Date
#' @importFrom utils data.frame
#'
#' @examples
#' \dontrun{
#' # 假设 stock_data_ss50 已经定义
#' plot <- drawStockLine(stock_data_ss50[[2]])
#' print(plot)
#' }
#'
drawStockLine<-function(ssm=stock_data_ss50[[2]]){
  ssm<-data.frame(ssm)
  ssm$Date<-as.Date(rownames(ssm))
  p <- ggplot(ssm,aes(x=as.Date(Date),y=as.numeric(Adjusted)))
  p<-p + geom_line()#+xlab(last(getsellpoint(ssm)$judge,1))
  p<-p+geom_smooth(method="glm")
  p<-p+geom_smooth(method="gam",color="black")
  #p<-p+geom_smooth(method="loess",color="red")
  p
}


#function函数箱线图残差
drawStockBoxplot<-function(df=getpoint_sz50[[1]]){
  p<-ggplot(df,aes(residuals))
  p<-p+geom_boxplot()
  p+geom_vline(aes(xintercept=tail(df,1)$residuals),color="red")
}

#函数箱线图价格
drawStockBoxplot.Adj<-function(df=getpoint_sz50[[1]]){
  df$Adjusted<-as.numeric(df$Adjusted)
  p<-ggplot(df,aes(Adjusted))
  p<-p+geom_boxplot()
  p<-p+geom_vline(aes(xintercept=tail(df,1)$Adjusted),color="red")
  p<-p+geom_vline(aes(xintercept=tail(df,2)$Adjusted[1]),color="blue")#前1天
  p<-p+geom_vline(aes(xintercept=tail(df,6)$Adjusted[1]),color="green")#前5天
  p
}


#' 绘制并保存多个股票数据的 Prophet 预测交互式图表到 HTML 文件
#'
#' 该函数接收包含多个股票数据的列表，为每个股票数据拟合 Prophet 模型进行预测，
#' 生成交互式图表，并将所有图表整合到一个 HTML 文件中，同时为每个图表添加对应的股票名称作为标题。
#'
#' @param data 一个列表，列表中的每个元素代表一个股票的时间序列数据。
#'             列表元素的名称将作为图表的标题。默认为 `stock_data_ss50`。
#' @param filename 字符型，指定要保存的 HTML 文件的名称。默认为 `"fifty_plots.html"`。
#'
#' @return 无返回值，函数的主要作用是将生成的 HTML 文件保存到指定路径。
#'
#' @importFrom prophet prophet fit.prophet make_future_dataframe dyplot.prophet
#' @importFrom htmltools tagList tags save_html
#' @importFrom htmlwidgets as.tags
#' @importFrom dplyr %>%
#'
#' @export
plot_prophet <- function(data = stock_data_ss50, filename = "fifty_plots.html") {
  library(prophet)
  # 创建一个空列表用于存储图表
  plots <- list()

  # 模拟生成 50 组数据并创建对应的 Prophet 模型和预测
  for (iname in names(data)) {
    # 生成模拟数据
    data_sub <- data.frame(data[[iname]])
    data_sub <- data.frame(ds = as.Date(rownames(data_sub)), y = as.numeric(data_sub$Adjusted))

    # 创建并拟合 Prophet 模型
    m <- prophet()
    m <- fit.prophet(m, data_sub)

    # 创建未来日期数据框并进行预测
    future <- make_future_dataframe(m, periods = 30)
    forecast <- predict(m, future)

    # 生成交互式图表
    plot <- dyplot.prophet(m, forecast)

    # 将图表添加到列表中
    plots[[iname]] <- plot
  }

  html_content <- htmltools::tagList(
    htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$title("Stock Prophet Plots")
      ),
      htmltools::tags$body(
        mapply(function(plot_obj, stock_name) {
          # 为每个图表添加标题和图表本身
          htmltools::tagList(
            htmltools::tags$h3(stock_name),  # 显示股票名称作为标题
            htmltools::as.tags(plot_obj)    # 将图表转换为 HTML 标签
          )
        }, plots, names(plots))
      )
    )
  )

  # 保存为 HTML 文件
  htmltools::save_html(html_content, filename)
}

#' 绘制 Holt-Winters 预测图
#'
#' 该函数接收一个包含多个股票数据的列表，为每个股票数据使用 Holt-Winters 模型进行预测，并绘制交互式图表。
#'
#' @param data 包含多个股票数据的列表。每个元素是一个 xts 对象，表示股票的历史数据。
#' @param prediction_period 预测的周期，默认为 30 天。
#' @param filename 生成的 HTML 文件名，默认为 "hw_plots.html"。
#'
#' @return 返回保存预测图的 HTML 文件。
#'
#' @examples
#' # 假设有一个包含 xts 对象的股票数据列表 stock_data
#' plot_hw(data = stock_data, filename = "hw_stock_plots.html")
plot_hw <- function(data=stock_data_ss50[1:2], prediction_period = 30, filename = "hw_plots.html") {
  library(forecast)
  library(dygraphs)
  library(htmltools)

  # 创建一个空列表用于存储图表
  plots <- list()

  # 对列表中的每个股票数据进行预测和绘图
  for (iname in names(data)) {
    # 提取当前股票数据
    stock_data <- data[[iname]]

    # 使用 stock_prediction_hw 函数进行预测
    forecast_data <- stock_prediction_hw(stock_data, prediction_period)

    # 生成交互式图表
    plot <- dygraph(forecast_data, main = paste("预测图：", iname)) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
      dySeries(name = iname, label = "预测价格")

    # 将图表添加到列表中
    plots[[iname]] <- plot
  }

  # 创建 HTML 内容
  html_content <- tagList(
    tags$html(
      tags$head(
        tags$title("Stock Holt-Winters Forecast Plots")
      ),
      tags$body(
        mapply(function(plot_obj, stock_name) {
          tagList(
            tags$h3(stock_name),  # 显示股票名称作为标题
            as.tags(plot_obj)      # 将图表转换为 HTML 标签
          )
        }, plots, names(plots))
      )
    )
  )

  # 保存为 HTML 文件
  save_html(html_content, filename)
}
