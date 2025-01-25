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



#

