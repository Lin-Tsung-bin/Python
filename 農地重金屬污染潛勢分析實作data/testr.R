rm(list = ls()) #清除所有

# 資料匯入
data_EPA <- read.csv("環保署列管污染農地_utf8.csv", fileEncoding = "utf8")
data_TARI <- read.csv("農地重金屬超標未列管_utf8.csv", fileEncoding = "utf8")

#-----------------------------------------------------
# PART1–環保署資料實作
# 環保署「土壤及地下水列管場址」資料，提供有公告列管的農地污染控制場址地號，截至2016-12-18共計5485筆。
# 1. 使用 dplyr 套件製作敘述統計表

library(dplyr)
#因為沒有全部要用，擷取需要用到的資訊，並另外命名為英文
data_EPA <- data_EPA %>% 
  select(county=1, coordinate=3, area=7, control_date=8, free_date=10)

# 預覽資料
head(data_EPA)

summarise_data <- data_EPA %>% group_by(county) %>% 
  summarise(count = n(),                                  # 所有案件數
            sum_area = sum(area),                        # 場址面積總和
            control_area = sum(area[free_date=="無"]),   # 列管面積總和 (平方公尺)
            free_area = sum(area[free_date!="無"]),      # 解除面積總和 (平方公尺)
            count_control = sum(free_date=="無"),        # 列管案件
            count_free = sum(free_date!="無")) %>%       # 解除列管案件
  mutate(avg_area = sum_area/count,                      # 平均場址面積 (平方公尺)
         ratio_control = sprintf("%1.0f%%",count_control/count*100), # 列管比例
         ratio_free = sprintf("%1.0f%%",count_free/count*100))       # 解除列管比例

summarise_data # 列印
#-----------------------------------------------------

# 2.計算平均列管月份
data_EPA <- mutate(data_EPA,
                   control_date = as.Date(control_date), # 把列管時間轉成日期格式
                   free_date = as.Date(free_date), # 把列管時間轉成日期格式
                   # 把尚未解除列管的時間帶資料收集截止時間"2016-12-18"，以便計算列管時間
                   free_date = replace(free_date, 
                                       which(is.na(free_date)), 
                                       as.Date("2016-12-18"))
)


# 計算列管月份
data_EPA$totol_month <- NA
for(i in 1:nrow(data_EPA)){
  tryCatch({ # 因為資料裡有其中一筆沒有列管時間，我們加tryCatch讓他忽略錯誤，讓迴圈可以正常執行完
    data_EPA$totol_month[i] <- 
      length(seq(from=data_EPA$control_date[i], to=data_EPA$free_date[i], by='month')) 
  }, error=function(e){})
}

# 計算各縣市列管中與解除列管的平均列管月份
avg_month <- data_EPA %>% group_by(county) %>% 
  summarise(control_month = mean(totol_month[free_date=="2016-12-18"], na.rm = TRUE) %>% 
              round(), 
            free_month = mean(totol_month[free_date!="2016-12-18"], na.rm = TRUE) %>% 
              round())
# 因為資料中有一筆沒有列管時間，因此把參數 na.rm 改為TRUE，才可以運算

# 把無列管中的平均月份帶0
avg_month[is.na(avg_month)] <- 0

# 合併回summarise_data
summarise_data <- summarise_data %>% 
  left_join(avg_month, by="county")

avg_month # 計算平均列管月份

#------------------------------------------------------------
# 3.計算全國加總
tmp <- colSums(summarise_data[,2:8]) %>% t() %>% as.data.frame() %>% 
  mutate(ratio_control = sprintf("%1.0f%%",count_control/count*100),
         ratio_free = sprintf("%1.0f%%",count_free/count*100)) %>% 
  cbind(.,colMeans(summarise_data[,11:12]) %>% round() %>% t() %>% as.data.frame()) %>%
  mutate(county="全國") %>% 
  select(12,1:11)

summarise_data <- rbind(tmp, summarise_data)

tmp #計算全國加總

# -------------------------------------------------
# 4.將欄位改成中文敘述並進行展示
tmp2 <- 
  summarise_data %>% 
  select(`全台縣市`=county,
         `案件數`=count,
         `場址面積總和`=sum_area,
         `列管面積總和`=control_area,
         `解除列管面積總和`=free_area,
         `平均場址面積`=avg_area,
         `列管案件`=count_control,
         `解除列管案件`=count_free,
         `列管案件比`=ratio_control,
         `解除列管案件比`=ratio_free,
         `列管中_平均列管月份`=control_month,
         `解除列管_平均列管月份`=free_month) %>% 
  arrange(desc(`案件數`))

tmp2 #列印

# ----------------------------------------------
# 5.使用 plotly 套件繪製統計圖表
# Bar Chart
library(plotly)

plot_ly(summarise_data, x = ~county, y = ~count_control, type = "bar") %>% 
  layout(title = "所有案件",
         xaxis = list(title = '全台縣市'),
         yaxis = list(title = '案件數'),
         width = 750, height = 400)

# x軸依據案件數做排序
plot_ly(summarise_data, 
        x = ~reorder(county, -count_control), 
        y = ~count_control, type = "bar") %>% 
  layout(title = "所有案件",
         xaxis = list(title = '全台縣市'),
         yaxis = list(title = '案件數'),
         width = 750, height = 400)


# Grouped Bar Chart
# with Hover Text and Rotated Labels
plot_ly(summarise_data, 
        x = ~reorder(county, -count_control), 
        y = ~count_control, type = 'bar',
        name = '列管案件', text = ~ratio_control) %>%
  add_trace(y = ~count_free, name = '解除列管案件', text = ~ratio_free) %>%
  layout(xaxis = list(title = "", tickangle = -45), 
         yaxis = list(title = 'count'), barmode = 'group',
         width = 800, height = 400)


# Pie Chart
#列管縣市太多，很多面積很小，取場址面積大於中位數的來畫圖
summarise_data1 <- summarise_data %>% 
  arrange(sum_area %>% desc) %>%  # 降冪排序
  filter(sum_area > median(sum_area)) %>%  # 篩選大於中位數者
  slice(-1) # 移除全國總和數據

plot_ly(summarise_data1, labels = ~county, values = ~sum_area, type = 'pie',
        textinfo = 'label+percent') %>%
  layout(title = '場址面積總和',  width = 800, height = 500,
         margin=list(l = 100, r = 50, b = 100, t = 100, pad = 4))

#---------------------------------------------
# 6.場址座標轉經緯度
# 把TWD97位址資訊抓出來，分割成x軸與y軸
head(data_EPA,3) #可以看到座標欄位裡面有：和，

TWD97 <- data_EPA$coordinate %>% as.character() %>% 
  stringr::str_split(.,'[，：]',simplify = TRUE) %>% 
  as.data.frame(., stringsAsFactors=FALSE) %>% select(x=2,y=4) 

# 將經緯度資訊從TWD97轉為WGS84
# 匯入轉碼程式包並用source()載入
# 利用轉碼函式將TWD97座標轉成經緯度座標系統
source("https://raw.githubusercontent.com/snexuz/TWD97TM2toWGS84/master/TWD97TM2toWGS84.R")
WGS84 <- TWD97TM2toWGS84(TWD97$x, TWD97$y) %>% as.data.frame()

#合併回原本的data
data_EPA$coordinate <- NULL
data_EPA <- cbind(data_EPA,TWD97,WGS84)
head(data_EPA)