library(openxlsx)
# #update_stock_data
#   write.table(read.xlsx("HS300.xlsx"),"data/HS300.txt",row.names=FALSE)
#   write.table(read.xlsx("SS50.xlsx"),"data/SS50.txt",row.names=FALSE)
HS300<-read.xlsx("HS300.xlsx")
SS50<-read.xlsx("SS50.xlsx")
save(HS300,file="data/HS300.rda")
save(SS50,file="data/SS50.rda")

  # stock_data_ss50<-getListStocks(SS50$code)
  # stock_data_hs300<-getListStocks(HS300$code)
  # save(stock_data_hs50,file="E:/FangCloudSync/R_WD360/Project/quantstock/data/stock_data_ss50.rda")
  # save(stock_data_hs300,file="E:/FangCloudSync/R_WD360/Project/quantstock/data/stock_data_hs300.rda")
