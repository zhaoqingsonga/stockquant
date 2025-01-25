#' 计算带有加权的五数概括（九数概括）
#'
#' 此函数基于R语言的五数概括（min, Q1, median, Q3, max）计算一个扩展的九数概括。
#' 该函数首先通过1.5倍四分位距计算上下限，并根据这些限制调整五数概括中的最大值和最小值。
#' 然后根据上四分位数和下四分位数的差距，计算加权的中位数加值，生成九数概括。
#'
#' @param data 向量类型的数据，默认为1到20的整数向量。用于计算五数概括的输入数据。
#'
#' @return 返回一个包含九个值的向量，分别是：
#' 1. 最小值
#' 2. 下四分位数
#' 3. 中位数
#' 4. 加数1
#' 5. 加数2
#' 6. 上四分位数
#' 7. 加数3
#' 8. 加数4
#' 9. 最大值
#'
#' @examples
#' boxfivenum9(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' boxfivenum9(1:100)
boxfivenum9 <- function(data = 1:20) {
  # 计算五数概括
  five <- fivenum(data)
  IQR <- five[4] - five[2]  # 四分位距（IQR）
  upperHinge <- five[4] + 1.5 * IQR  # 上四分位数修正
  lowerHinge <- five[2] - 1.5 * IQR  # 下四分位数修正

  # 修正五数中的最大值和最小值，确保它们在上下限范围内
  five[c(1, 5)] <- pmin(pmax(five[c(1, 5)], lowerHinge), upperHinge)

  # 定义计算加权值的函数（用于生成加权的上四分位数和下四分位数）
  calculate_add <- function(from, to) {
    c(from + (to - from) * 1/3, from + (to - from) * 2/3)
  }

  # 计算下四分位数加权值和上四分位数加权值
  add12 <- calculate_add(five[3], five[4])
  add34 <- calculate_add(five[4], five[5])

  # 返回九数概括
  return(c(five[1:3], add12, five[4], add34, five[5]))
}

#' 获取买入或卖出点
#'
#' 该函数根据输入的时间序列数据，通过线性回归计算残差，并使用九数概括法生成相应的买卖信号。还会计算每日的涨幅。
#'
#' @param ss001 一个包含股票调整后收盘价（Adjusted）的数据框或 xts 对象。
#'
#' @return 返回一个包含以下字段的数据框或 xts 对象：
#' \item{Q1}{Q1值，通过拟合值加上残差的九数概括得出。}
#' \item{Q2}{Q2值，通过拟合值加上残差的九数概括得出。}
#' \item{Q3}{Q3值，通过拟合值加上残差的九数概括得出。}
#' \item{Q4}{Q4值，通过拟合值加上残差的九数概括得出。}
#' \item{Q5}{Q5值，通过拟合值加上残差的九数概括得出。}
#' \item{Q6}{Q6值，通过拟合值加上残差的九数概括得出。}
#' \item{Q7}{Q7值，通过拟合值加上残差的九数概括得出。}
#' \item{Q8}{Q8值，通过拟合值加上残差的九数概括得出。}
#' \item{Q9}{Q9值，通过拟合值加上残差的九数概括得出。}
#' \item{judge}{根据残差的值判断买入或卖出信号，可能的值有："strongbuy", "buy", "holdbuy", "holdselldown",
#' "holdsell", "holdsellup", "selldown", "sell", "sellup", "strongsell"。}
#' \item{increase}{每日涨幅，计算方法为每日收盘价的变化百分比。}
#'
#' @examples
#' # 假设 ss001 是一个包含股票调整后收盘价的 xts 数据
#' result <- getsellpoint(ss001)
#'
#' @export
#'

getsellpoint <- function(ss001) {
  # 删除缺失值并重建数据框
  mydata <- na.omit(ss001)
  mydata<-data.frame(mydata)
  mydata$number <-1:nrow(mydata)
  # 线性回归计算拟合值和残差
  mylm <- lm(Adjusted ~ number, data = mydata)
  mydata$residuals <- as.vector(residuals(mylm))
  mydata$fitted.values <- as.vector(fitted(mylm))

  # 获取九数概括的值
  myboxninenum <- boxfivenum9(as.vector(mydata$residuals))

  # 直接计算 Q1 到 Q9 的值
  mydata$Q1 <- mydata$fitted.values + myboxninenum[1]
  mydata$Q2 <- mydata$fitted.values + myboxninenum[2]
  mydata$Q3 <- mydata$fitted.values + myboxninenum[3]
  mydata$Q4 <- mydata$fitted.values + myboxninenum[4]
  mydata$Q5 <- mydata$fitted.values + myboxninenum[5]
  mydata$Q6 <- mydata$fitted.values + myboxninenum[6]
  mydata$Q7 <- mydata$fitted.values + myboxninenum[7]
  mydata$Q8 <- mydata$fitted.values + myboxninenum[8]
  mydata$Q9 <- mydata$fitted.values + myboxninenum[9]

  #要增加的列
  mydata$judge <- NA

  # 使用 cut 函数，并确保将因子转换为字符型
  myjudge<-cut(mydata$residuals,
               breaks = c(-Inf, myboxninenum, Inf),
               labels = c("strongbuy", "buy", "holdbuy", "holdselldown",
                          "holdsell", "holdsellup", "selldown",
                          "sell", "sellup", "strongsell"))

  #myjudge<-xts(myjudge,order.by=index(mydata))
  mydata$judge <-as.character(myjudge)
  # 计算每日涨幅
  #mydata$increase <- dailyReturn(mydata$Adjusted)
  todayP<-as.numeric(mydata$Adjusted)
  beforeP<-c(todayP[1],todayP[-length(todayP)])
  mydata$increase<-(todayP-beforeP)/beforeP*100
  #转化为xts类型
  #mydata<-xts(mydata,order.by=as.Date(row.names(mydata)))
  return(mydata)
}


#' 从股票数据中删除最后 n 天数据
#'
#' 该函数从传入的股票数据中删除最后 n 天的数据。
#'
#' @param stock_data_ss50 一个列表，每个元素是一个时间序列数据（例如 xts 对象），表示股票的每日数据。
#' @param deleN 一个整数，表示要删除的天数。如果 deleN 为 0，则返回原始数据。
#'
#' @return 返回一个列表，包含每个股票去除最后 n 天的数据。
#'
#' @examples
#' # 假设 stock_data_ss50 是一个包含股票数据的列表
#' # 删除每个股票数据的最后 5 天数据
#' result <- deleFromEnd(stock_data_ss50, deleN = 5)
#'
#' @export
deleFromEnd<-function(sz50,deleN=0){
  if(deleN==0){return(sz50)}
  num<-deleN-1
  mydata<-lapply(sz50,function(x) x[-(nrow(x)-num):-nrow(x),])
  return(mydata)
}





#' 更新和保存股票数据
#'
#' 该函数将新的股票数据添加到已有的数据框 `saved_statable` 中，并去除重复的日期项，按日期排序后保存。
#'
#' @param my 数据框，默认值为 `statable`，表示需要更新的数据。此数据框应包含股票数据，并有一个 `Date` 列。
#'
#' @return 返回更新后的 `saved_statable` 数据框，包含去重和排序后的股票数据。
#'
#' @examples
#' updated_data <- satstock(new_data)
satstock <- function(my = statable) {
  setwd("E:/FangCloudSync/R_WD360/Project/Stocks/shangzheng50")  # 设置工作目录
  load("statalbe")  # 加载已有的统计数据

  # 将新的数据与已有的数据合并
  saved_statable <- rbind(saved_statable, my)

  # 转换为数据框并去重
  saved_statable <- data.frame(saved_statable)
  saved_statable <- saved_statable[!duplicated(saved_statable$Date), ]

  # 按日期排序
  saved_statable <- saved_statable[order(saved_statable$Date), ]

  # 保存更新后的数据
  save(saved_statable, file = "statalbe")

  return(saved_statable)  # 返回更新后的数据框
}


#' 检查股票在指定日期范围内的买入和卖出情况
#'
#' 该函数根据输入的股票数据和指定的日期范围，
#' 检查在前一段时间（before）和当前时间（current）内，
#' 指定类型的股票（如“strongsell”）是否在两个时间段内都有出现，
#' 或者在某一时间段内出现。
#'
#' @param stock_data_ss50 数据框或xts对象，包含股票的历史数据，默认使用 `mydata` 数据集。
#' @param before 前一个时间段的天数，默认为1，表示在当前时间之前的1天。
#' @param current 当前时间段的天数，默认为0，表示当前时间。
#' @param stock_type 股票的买入或卖出类型，默认为 "strongsell"，表示强烈卖出类型。
#'
#' @return 返回一个列表 `mylist`，其中包含以下三个元素：
#' \item{common}{在两个时间段内都有出现的股票代码。}
#' \item{in_}{在当前时间段内出现，但在前一个时间段内未出现的股票代码。}
#' \item{out_}{在前一个时间段内出现，但在当前时间段内未出现的股票代码。}
#'
#' @examples
#' \dontrun{
#' # 假设已经加载了 stock_data_ss50 数据
#' result <- check_in_out(stock_data_ss50 = mydata, before = 5, current = 0, stock_type = "strongsell")
#' print(result)
#' }
check_in_out <- function(stock_data_ss50 = mydata,
                         before = 1,
                         current = 0,
                         stock_type = "strongsell") {
  myd1 <- deleFromEnd(stock_data_ss50, before)
  myd2 <- deleFromEnd(stock_data_ss50, current)

  # 获取前一个时间段的股票卖出点
  myd1 <- lapply(myd1, getsellpoint)
  myd1_last <- lapply(myd1, tail, 1)
  myd1_last <- data.table::rbindlist(myd1_last)

  # 获取当前时间段的股票卖出点
  myd2 <- lapply(myd2, getsellpoint)
  myd2_last <- lapply(myd2, tail, 1)
  myd2_last <- data.table::rbindlist(myd2_last)

  # 根据股票类型筛选
  type1 <- myd1_last$name[myd1_last$judge == stock_type]
  type2 <- myd2_last$name[myd2_last$judge == stock_type]

  # 返回三类股票：在两个时间段内都有出现的、在当前时间段内出现的、在前一个时间段内出现的
  mylist <- list(
    common = intersect(type1, type2),  # 两个时间段都出现的股票
    in_ = setdiff(type2, type1),       # 仅当前时间段出现的股票
    out_ = setdiff(type1, type2)       # 仅前一个时间段出现的股票
  )

  return(mylist)
}


#' 按类求平均 increase
#'
#' 该函数用于按前一天的分类统计当天的 increase，计算不同类型下 increase 的平均值，并将结果整理成一个数据框返回。
#'
#' @param sz50 一个包含股票数据的列表，默认为 stock_data_ss50。
#' @return 一个数据框，包含日期、SZindex 以及不同类型下的 increase 平均值，若某类型无数据则用 NA 填充。
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_type_increase(sz50 = stock_data_ss50)
#' print(result)
#' }
get_type_increase<-function(sz50=stock_data_ss50){
  # 计算每个股票数据的卖点信息
  # 对输入的 sz50 列表中的每个元素应用 getsellpoint 函数，得到每个股票的卖点相关信息
  getpoint_sz50<-lapply(sz50,getsellpoint)

  # 计算删除最后一天数据后的卖点信息
  # 先使用 deleFromEnd 函数删除 sz50 列表中每个元素的最后一天数据，再应用 getsellpoint 函数
  getpoint_sz50_before<-lapply(deleFromEnd(sz50,1),getsellpoint)

  # 获取每个股票数据的最后一天信息
  # 对 getpoint_sz50 列表中的每个元素取最后一行数据
  getpoint_sz50_last<-lapply(getpoint_sz50,tail,1)
  #获取日期
  stockDate<-rownames(getpoint_sz50_last[[1]])

  # 获取删除最后一天数据后的最后一天信息
  # 对 getpoint_sz50_before 列表中的每个元素取最后一行数据
  getpoint_sz50_before_last<-lapply(getpoint_sz50_before,tail,1)

  ######这一步很关键
  # 将 getpoint_sz50_last 列表合并为一个数据框
  # 使用 data.table::rbindlist 函数将列表中的元素按行合并为一个数据框
  getpoint_sz50_last<-data.table::rbindlist(getpoint_sz50_last)

  # 将 getpoint_sz50_before_last 列表合并为一个数据框
  # 使用 data.table::rbindlist 函数将列表中的元素按行合并为一个数据框
  getpoint_sz50_before_last<-data.table::rbindlist(getpoint_sz50_before_last)

  # 按前一天的 judge 类型对当天的 increase 求平均
  # 使用 aggregate 函数，按 getpoint_sz50_before_last 的 judge 列进行分组，对 getpoint_sz50_last 数据求均值
  typeagv<-aggregate(getpoint_sz50_last$increase,list(type=getpoint_sz50_before_last$judge),mean)

  typeagv<-data.frame(type=typeagv$type,increase=typeagv$x)

  # 提取不同类型的 increase 值
  # 从 typeagv 数据框中提取不同 judge 类型对应的 increase 值
  addstrongbuy<-typeagv[typeagv$type=="strongbuy",]$increase
  addbuy<-typeagv[typeagv$type=="buy",]$increase
  addholdbuy<-typeagv[typeagv$type=="holdbuy",]$increase

  addholdselldown<-typeagv[typeagv$type=="holdselldown",]$increase
  addholdsell<-typeagv[typeagv$type=="holdsell",]$increase
  addholdsellup<-typeagv[typeagv$type=="holdsellup",]$increase
  addselldown<-typeagv[typeagv$type=="selldown",]$increase
  addsell<-typeagv[typeagv$type=="sell",]$increase
  addsellup<-typeagv[typeagv$type=="sellup",]$increase
  addstrongsell<-typeagv[typeagv$type=="strongsell",]$increase

  # 构建最终的数据框
  # 包含日期、SZindex 以及不同类型的 increase 平均值，若某类型无数据则用 NA 填充
  curincrease<-data.frame(Date=stockDate,
                          SZindex=getpoint_sz50_last[1,]$increase,
                          strongbuy=ifelse(length(addstrongbuy)==0,NA,addstrongbuy),
                          buy=ifelse(length(addbuy)==0,NA,addbuy),
                          holdbuy=ifelse(length(addholdbuy)==0,NA,addholdbuy),
                          holdselldown=ifelse(length(addholdselldown)==0,NA,addholdselldown),
                          holdsell=ifelse(length(addholdsell)==0,NA,addholdsell),
                          holdsellup=ifelse(length(addholdsellup)==0,NA,addholdsellup),
                          selldown=ifelse(length(addselldown)==0,NA,addselldown),
                          sell=ifelse(length(addsell)==0,NA,addsell),
                          sellup=ifelse(length(addsellup)==0,NA,addsellup),
                          strongsell=ifelse(length(addstrongsell)==0,NA,addstrongsell))

  # 返回最终的数据框
  return(curincrease)
}



#' 统计不同类型的股票数据数量
#'
#' 该函数用于对输入的股票数据列表进行处理，计算每种类型的股票数据数量，并将结果整理成一个数据框返回。
#' 它会对输入列表中的每个元素应用 `getsellpoint` 函数，提取最后一行数据，然后统计不同类型的出现次数。
#'
#' @param sz50 一个包含股票数据的列表，默认为 `stock_data_ss50`。列表中的每个元素应该是一个数据框或可处理的数据结构，
#'             能够被 `getsellpoint` 函数处理。
#' @return 一个数据框，包含日期和不同类型股票数据的统计数量。列名包括 `Date`、`strongbuy`、`buy`、`holdbuy`、
#'         `holdselldown`、`holdsell`、`holdsellup`、`selldown`、`sell`、`sellup` 和 `strongsell`。
#' @export
#' @examples
#' \dontrun{
#' result <- sta_type_number(sz50 = stock_data_ss50)
#' print(result)
#' }
sta_type_number <- function(sz50 = stock_data_ss50) {
  ## calculate each
  # 对输入的 sz50 列表中的每个元素应用 getsellpoint 函数，得到每个股票的卖点相关信息
  getpoint_sz50 <- lapply(sz50, getsellpoint)

  # get the last of list
  # 对 getpoint_sz50 列表中的每个元素取最后一行数据
  getpoint_sz50_last <- lapply(getpoint_sz50, tail, 1)

  # 获取股票数据的日期
  stockDate <- rownames(getpoint_sz50_last[[1]])

  ###### 这一步很关键
  # 将 getpoint_sz50_last 列表合并为一个数据框
  # 使用 data.table::rbindlist 函数将列表中的元素按行合并为一个数据框
  getpoint_sz50_last <- data.table::rbindlist(getpoint_sz50_last)

  # getpoint_sz50_last$name <- names(getpoint_sz50)

  # 类统计 ----
  # 统计不同类型的出现次数，并将日期信息合并
  statable <- c(Date = stockDate, table(getpoint_sz50_last$judge))

  # 构建最终的数据框
  curstatable <- data.frame(
    Date = statable["Date"],
    strongbuy = statable["strongbuy"],
    buy = statable["buy"],
    holdbuy = statable["holdbuy"],
    holdselldown = statable["holdselldown"],
    holdsell = statable["holdsell"],
    holdsellup = statable["holdsellup"],
    selldown = statable["selldown"],
    sell = statable["sell"],
    sellup = statable["sellup"],
    strongsell = statable["strongsell"]
  )

  # 返回最终的数据框
  return(curstatable)
}


#' 获取每只股票的阶段
#'
#' 该函数用于计算给定股票数据集中每只股票的卖出点，并提取每只股票的最新卖出点信息，包括日期、股票名称和判断结果。
#'
#' @param sz50 一个数据框或列表，包含股票数据。默认值为 `stock_data_ss50`。
#' @return 一个数据框，包含股票的日期、名称和判断结果。
#' @details
#' - 使用 `getsellpoint` 函数计算每只股票的卖出点。
#' - 通过 `lapply` 和 `tail` 函数提取每只股票的最新卖出点。
#' - 使用 `data.table::rbindlist` 将列表转换为数据框。
#' - 重组数据框以匹配所需格式。
#' @examples
#' # 假设 stock_data_ss50 是一个已定义的股票数据集
#' result <- get_each_stage()
#' head(result)
#'
#' @export
get_each_stage <- function(sz50 = stock_data_ss50) {
  ## calculate each stock's sell point
  getpoint_sz50 <- lapply(sz50, getsellpoint)
  # get the last element of each list
  getpoint_sz50_last <- lapply(getpoint_sz50, tail, 1)
  stockDate<-rownames(getpoint_sz50_last[[1]])
  ###### This step is crucial
  getpoint_sz50_last <- data.table::rbindlist(getpoint_sz50_last)
  # Prepare column names
  mydd_name <- c("Date", as.vector(t(data.frame(getpoint_sz50_last)[c("name")])))
  # Prepare data frame
  mydd <- data.frame(t(data.frame(JU = c(stockDate,as.vector(t(data.frame(getpoint_sz50_last)[c("judge")]))))))
  colnames(mydd) <- mydd_name
  return(mydd)
}

#' 判断买入卖出信号
#'
#' 该函数用于根据输入的价格数据判断买入和卖出信号。具体规则为：
#' - 如果当前值小于等于6，下一值大于等于7，判断为买入信号（-1）。
#' - 如果当前值大于等于9，下一值小于等于8，判断为卖出信号（0）。
#' - 如果当前值大于等于10，下一值小于等于9，判断为卖出信号（0）。
#'
#' @param vd 一个数值向量，表示价格数据序列。
#'
#' @return 返回一个长度与输入向量相同的向量，第一个值为 NA，后续值为对应的信号值（-1 或 0）。
#'
#' @examples
#' judge_buy_sell(c(6,6,7,8,9,10,9,8,7,7,7,6))
judge_buy_sell <- function(vd = c(6, 6, 7, 8, 9, 10, 9, 8, 7, 7, 7, 6)) {
  vd<-as.numeric(vd)
  vd1 <- vd[1:(length(vd) - 1)]  # 当前值
  vd2 <- vd[-1]  # 下一值
  vd3 <- rep(NA, length(vd) - 1)  # 初始化信号向量，去掉第一个值（NA）

  # 判断买入信号：当前值 <= 6 且 下一值 >= 7
  vd3[vd1 <= 6 & vd2 >= 7] <- -1

  # 判断卖出信号：当前值 >= 9 且 下一值 <= 8
  vd3[vd1 >= 9 & vd2 <= 8] <- 0

  # 判断卖出信号：当前值 >= 10 且 下一值 <= 9
  vd3[vd1 >= 10 & vd2 <= 9] <- 0

  return(c(NA, vd3))  # 返回结果，前面加上NA
}


#' 股票交易资金模拟处理函数
#'
#' 此函数用于模拟股票交易过程中资金的变化情况。根据给定的买卖信号、涨幅数据和初始资金，
#' 模拟资金在不同交易阶段的增长或减少，最终返回每个交易阶段结束后的资金总额。
#'
#' @param buysell 一个数值向量，代表买卖信号。其中，-1 表示全仓买入信号，0 表示清仓信号，其他值可按需自定义但在本函数逻辑中不影响交易操作。
#' @param increase 一个数值向量，与 `buysell` 长度相同，代表每个交易阶段对应的涨幅（百分比）。
#' @param mymoney 初始资金，默认为 10000，是一个数值类型的值。
#' @return 一个数值向量，长度与 `buysell` 和 `increase` 相同，代表每个交易阶段结束后持有的资金总额。
#' @export
#'
#' @examples
#' buysell <- c(3,4,5,-1,5,5,0,1,2,3,-1,9,7,6,7,0,2)
#' increase <- c(3.1,0.4,-5,0.9,1.5,5,8,1,2,3,4,1.9,7,6,7,6,2)
#' result <- stock_treat(buysell, increase, mymoney = 10000)
#' print(result)
stock_treat<-function(buysell=c(3,4,5,-1,5,5,0,1,2,3,-1,9,7,6,7,0,2),
                      increase=c(3.1,0.4,-5,0.9,1.5,5,8,1,2,3,4,1.9,7,6,7,6,2),
                      mymoney=10000){
  # 初始化一个长度与 buysell 相同的向量，用于存储每个阶段的资金总额
  allmoney<-rep(0,length(buysell))
  # 标记是否处于全仓买入状态，初始为未全仓买入
  myin<-FALSE
  # 遍历每个交易阶段
  for(i in 1:length(buysell)){
    # 如果处于全仓买入状态
    if(myin){
      # 根据当前阶段的涨幅更新资金总额
      mymoney<-mymoney*(100+increase[i])/100
    }
    # 记录当前阶段结束后的资金总额
    allmoney[i]<-mymoney
    # 当收到全仓买入信号时
    if(buysell[i]==-1&!is.na(buysell[i])){
      # 标记为全仓买入状态
      myin<-TRUE
    }
    # 当收到清仓信号时
    if(buysell[i]==0&!is.na(buysell[i])){
      # 标记为未全仓买入状态
      myin<-FALSE
    }
  }
  # 返回每个阶段结束后的资金总额向量
  return(allmoney)
}

