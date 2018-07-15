# R Leaflet 地圖套件：繪製網頁互動式地圖，呈現經緯度座標資料
#  https://blog.gtwang.org/r/r-leaflet-interactive-map-package-tutorial/

library(leaflet)

# 方法一:使用傳統的 R 語法
#map <- leaflet()
#map <- addTiles(map)
#map <- addMarkers(map, lng=120.239, lat=22.992, popup="訊息方塊的文字說明")
#map

# 方法二: %>% 管線運算子語法
map <- leaflet() %>%
  addTiles() %>%  # 加上預設的地圖資料
  addMarkers(lng=120.239, lat=22.992, popup="訊息方塊的文字說明")
map  # 繪製地圖



# 使用 setView 設定地圖的位置與縮放比例：
m <- leaflet() %>%
  addTiles() %>%
  setView(-71.0382679, 42.3489054, zoom = 18)
m


# 使用fitBounds來變更地圖的顯示區域：
m %>% fitBounds(-72, 40, -70, 43)


# 清除了指定的方形區域之後，就會顯示最大的區域，亦即全球地圖：
m %>% clearBounds()


# 當資料來源為一般的 data frame 時，leaflet 會自動尋找 data frame 中名稱類似 lng、longitude 與 lat、latitude 作為經緯度座標，以下是一個以圓圈標示資料的範例：
set.seed(3)
point.df <- data.frame(
  Lat = 22.992 + rnorm(10)/800,    # 10是點數
  Long = 120.239 + rnorm(10)/800
)
m <- leaflet(point.df) %>%
  addTiles() %>%
  setView(lng = 120.239, lat = 22.992, zoom = 17)
m %>% addCircles()

# 亦可使用公式的方式明確指定經緯度座標的變數名稱：
m %>% addCircles(lng = ~Long, lat = ~Lat)


# 公式介面
set.seed(3)
point.df <- data.frame(
  lat = 22.97 + rnorm(100)/800,   # 100是點數
  long = 120.23 + rnorm(100)/800,
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m <- leaflet(point.df) %>%
  addTiles() %>%
  setView(lng = 120.23, lat = 22.97, zoom = 17)
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)