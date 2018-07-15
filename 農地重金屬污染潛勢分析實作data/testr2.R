rm(list = ls()) #清除所有

data_EPA <- read.csv("環保署列管污染農地_utf8.csv", fileEncoding = "utf8")
data_TARI <- read.csv("農地重金屬超標未列管_utf8.csv", fileEncoding = "utf8")

#---------------------------------------------
# PART2–農試所資料實作
# 農委會農業試驗所（農試所）之「土壤品質及生產力調查」資料。原始資料是1992年至2008年間進行的全國土壤採樣調查資料，累積約13萬筆，涵蓋78萬公頃表土，包含鎘、鉻、銅、鎳、鉛、鋅等六項重金屬有效性之調查。該資料是以250公尺*250公尺網格（6.25公頃）為單位。
# 由於農試所資料與環保署現今重金屬管制標準檢測方法不同，前者是透過0.1M鹽酸方法，後者是經過王水消化法，需要進行公式轉換，在此提供933筆環保數超標數據。

# 預覽資料
head(data_TARI)

# ----------------------------------------------
#1.重金屬轉換公式
#農試所以 0.1M 鹽酸萃取抽出分析土壤中的重金屬濃度，其數據轉為王水消化法的推估值 (AR，以下為農試所使用的迴歸轉換公式) 後，再利用環保署現行管制標準進行篩檢。

#重金屬名稱	重金屬轉換公式	環保署農地管制值
#銅	CUAR = 2.035*CU + 11.884	200
#鋅	ZNAR = 2.487*ZN + 89.711	600
#鎘	CDAR = 1.4578*CD + 0.0323	5
#鎘	CRAR = 17.35*CR+ 31.91	250
#鎳	NIAR = 5.13*NI+ 14.56	200
#鉛	PBAR = 2.811*PB+ 6.715	500

data_TARI <- data_TARI %>% 
  mutate(CUAR = 2.035*CU + 11.884,
         CUAR_OVER = ifelse(CUAR>200,1,0),
         ZNAR = 2.487*ZN + 89.711,
         ZNAR_OVER = ifelse(ZNAR>600,1,0),
         CDAR = 1.4578*CD + 0.0323,
         CDAR_OVER = ifelse(CDAR>5,1,0),
         CRAR = 17.35*CR+ 31.91,
         CRAR_OVER = ifelse(CRAR>250,1,0),
         NIAR = 5.13*NI+ 14.56,
         NIAR_OVER = ifelse(NIAR>200,1,0),
         PBAR = 2.811*PB+ 6.715,
         PBAR_OVER = ifelse(PBAR>500,1,0),
         OVER = ifelse(CUAR_OVER+ZNAR_OVER+CDAR_OVER+CRAR_OVER+
                         NIAR_OVER+PBAR_OVER>=1, 1, 0))

#------------------------------------------------------------
# 2.計算農地樣區是否有超標情形
# mindistance (與最近的環保署列管農地距離，單位公尺)
# Is_monitored (境內是否有環保署列管農地。1有，0無)
# minover (方圓1公里內其他超標點總數)

# 以農試所樣區資料為主體，計算每一個樣區與環保署列管農地的最短距離
for(i in 1:nrow(data_TARI)){
  data_TARI$mindistance[i] <- 
    ((data_TARI$XLO[i]-as.integer(data_EPA$x))^2+
       (data_TARI$YLO[i]-as.integer(data_EPA$y))^2) %>% 
    sqrt %>% min(na.rm = TRUE)
}


# 以農試所樣區為主體，計算每一個樣區方圓一公里內有多少個超標樣區
for(i in 1:nrow(data_TARI)){
  tmp <- sqrt((data_TARI$XLO[i]-data_TARI$XLO)^2+(data_TARI$YLO[i]-data_TARI$YLO)^2)
  
  data_TARI$minover[i] <- sum(tmp<=1000, na.rm = TRUE)
}

# 以農試所樣區為主體，計算每一個樣區境內是否有環保署列管農地
data_TARI <- data_TARI %>% mutate(Is_monitored=ifelse(mindistance<=200, 1, 0))

#---------------------------------------------------
#3.與環保署的資料合併，計算統計指標

library(scales)
# 計算各縣市統計 `group_by(county)`
stat_over <- 
  data_TARI %>% group_by(county) %>%
  summarise(`重金屬超標`=sum(OVER),
            `超標未曾列管`=sum(ifelse(OVER+Is_monitored==1,1,0))) %>%
  mutate(`超標未曾列管比例`=percent(`超標未曾列管`/`重金屬超標`),
         `超標列管比例`=percent(1-`超標未曾列管`/`重金屬超標`)) %>% 
  arrange(desc(`重金屬超標`))

# 加入全國訊息
stat_over <-
  data_TARI %>% 
  summarise(`重金屬超標`=sum(OVER),
            `超標未曾列管`=sum(ifelse(OVER+Is_monitored==1,1,0))) %>%
  mutate(`超標未曾列管比例`=percent(`超標未曾列管`/`重金屬超標`),
         `超標列管比例`=percent(1-`超標未曾列管`/`重金屬超標`)) %>% 
  mutate(county="全國") %>% 
  select(5, 1:4) %>%    # 使用mutate新增的欄位會排在最後面，利用select把county的順序往前移 
  full_join(stat_over)

stat_over

# --------------------------------------------------

#PART3–用leaflet畫地圖
#地圖一：兩份資料交叉比對

library(leaflet)

#先把要點上圖的資料篩選出來
EPA <- data_EPA %>% filter(free_date=="2016-12-18") %>% select(lat, lon, area)
TARI <- data_TARI %>% select(lat = Y,lon = X, area = AREA)


leaflet() %>% setView(lng=120.58,lat=23.58,zoom=8) %>% 
  addProviderTiles("Esri.WorldImagery") %>%
  addCircles(data = EPA, color = "red", 
             lng = ~lon, lat = ~lat, weight = 1, radius = ~sqrt(area)/2) %>%
  addCircles(data = TARI, color = "yellow",
             lng = ~lon, lat = ~lat, weight = 1, radius = ~sqrt(area)/2)

# -----------------------------------------------------

#地圖二：用log(mindistance)畫地圖
TARI <- data_TARI %>% 
  select(lat = Y,lon = X, area = AREA ,mindistance) %>%
  mutate(log_mindistance=log(mindistance))

leaflet() %>% 
  addCircles(data=TARI,lng = ~lon, lat = ~lat, 
             radius = ~sqrt(area)/2, weight = 1,
             fill=TRUE, fillOpacity = 0.8,
             color=~colorNumeric(c("#CD0000", "#FFFFFF","#0B752F"),
                                 log_mindistance)(log_mindistance)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addLegend(position = 'topleft',
            pal =colorNumeric(c("#CD0000", "#FFFFFF","#0B752F"),
                              domain=TARI$log_mindistance),
            values=TARI$log_mindistance,
            title = 'log-mindistance')