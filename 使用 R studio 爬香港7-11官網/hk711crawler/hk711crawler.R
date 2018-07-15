library(tidyverse)
library(magrittr)  
library(httr)

# magrittr 套件導入一種管線（pipe）的語法，
# 以串流的方式來處理資料的運算，讓程式碼更簡潔、語意更清楚。

# magrittr 套件中最重要的功能就是 %>% 管線運算子，
# 它的作用在於將左側的運算結果傳遞至右側函數的第一個參數

hk711api = "https://www.7-eleven.com.hk/api/store"

res = hk711api %>% GET()

df = res %>%
  content() %>%
  .$data %>%
  do.call(rbind,.) %>%
  as.data.frame() #%>%

# df %>% View

df %<>% mutate(id=row_number())

df %>% View

df$Store %>% map(length)

StoreDf = df$Store %>%
  do.call(rbind,.) %>%
  as.data.frame() %>%
  cbind(.,id=df$id)

StoreDf %>% View

StoreDf$District %>% map(length)

DistrictDf = StoreDf$District %>%
  do.call(rbind,.) %>%
  as.data.frame() %>%
  cbind(.,id=df$id)


StoreDf$Services %>% map(length)

StoreDf = StoreDf %>%
  mutate(ServicesLen = map(Services,length)) %>%
  mutate(Services =map(Services, ~as.data.frame(do.call(rbind,.)))) %>%
  mutate(Services =map2(Services, id, function(df, id){
    df$id  =rep(id,NROW(df))
    df
  })) %>%
  filter(ServicesLen > 0) %>%
  .$Services %>%
  do.call(rbind,.)

df$Geolocation %>% map(length)


GeolocationDf = df$Geolocation %>%
  do.call(rbind,.) %>%
  as.data.frame() %>%
  cbind(.,id=df$id)

GeolocationDf %>% View

final_sdf = df %>% 
  select(Region, id) %>%
  inner_join(StoreDf) %>%
  inner_join(DistrictDf) %>%
  inner_join(GeolocationDf) %>%
  select(-District, -Services)

cube = ServicesDf %>%
  inner_join(final_sdf) 

cube %<>% 
  map(unlist) %>%
  do.call(data.frame,.)

final_sdf %<>%
  map(unlist) %>%
  do.call(data.frame,.)

cube %>%
  ggplot(mapping = aes(x=Region,fill=Name)) + geom_bar()

cube %>% View

final_sdf %>%
  ggplot(mapping = aes(x=Longitude,y=Latitude)) + geom_point()
