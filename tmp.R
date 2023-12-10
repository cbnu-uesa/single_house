
library(tidyverse)
library(httr)
library(jsonlite)

setwd("D:/Analysis/single_house/")


### 실거래가 지오코딩
rentprice <- read_csv("raw_input/seoul_rentprice_apt.csv", skip = 15, locale = locale(encoding = "euc-kr")) %>%
  filter(전월세구분 == "전세") %>%
  mutate(id = row_number()) %>%
  mutate(address = paste(시군구, 번지))

rent_address <- rentprice %>%
  select(address) %>%
  distinct()



url<-"https://naveropenapi.apigw.ntruss.com/map-geocode/v2/geocode" # geocoding
naver_API_ID<-"to2ixvid2l"
naver_API_key<-"wTlv5OOPIltTbwWqIG7SaxTlnqqTiT17bOGj7j5a"

add_final <- data.frame()

for (i in 1:nrow(rent_address)){
  
  address <- rent_address$address[i]
  
  geocode_json<-GET(url=url,
                    query=list(query = address),
                    add_headers("X-NCP-APIGW-API-KEY-ID"=naver_API_ID,
                                "X-NCP-APIGW-API-KEY" =naver_API_key,
                                Accept="application/json")) %>%
    content(as="text") %>%
    fromJSON()
  
  if(geocode_json$meta$totalCount != 0){
    tmp <- data.frame(address = address, 
                      x = geocode_json$addresses$x,
                      y = geocode_json$addresses$y)
  } else {
    tmp <- data.frame(address = address, 
                      x = NA,
                      y = NA)
  }
  
  add_final <- rbind(add_final, tmp)
  print(sprintf("%d개 중 %d번째 데이터를 습득했습니다.", nrow(rent_address), i))
  
}

tmp <- add_final %>% distinct(address, .keep_all = TRUE) # 중복제거
final <- left_join(rentprice, tmp, by = c("address"))

write_delim(final, "raw_input/seoul_rentprice_apt.txt", delim = '\t')


## 편의점 추출

## 필요 함수 정의
getSafeMapAPI<-function(key, api_url){
  
  data <- data.frame()
  safedata_API_key <- key
  
  tmp1 <- GET(url=api_url,
              query=list(serviceKey = safedata_API_key,
                         numOfRows = 10,
                         pageNo = 1,
                         dataType = "json")) %>%
    content(as="text") %>%
    fromJSON()
  
  pageNum <- ceiling(tmp1$response$body$totalCount/10)
  
  for (i in 1:pageNum){
    
    tmp <- GET(url=api_url,
               query=list(serviceKey = safedata_API_key,
                          numOfRows = 10,
                          pageNo = i,
                          dataType = "json")) %>%
      content(as="text") %>%
      fromJSON()
    data <- rbind(data, tmp$response$body$items)
    print(sprintf("%d개 중 %d번째 데이터를 습득했습니다.", pageNum, i))
  }
  
  return(data)
  
}


safedata_API_key<-"F5FM17I5-F5FM-F5FM-F5FM-F5FM17I5P4"
api_url = "http://safemap.go.kr/openApiService/data/getConvenienceStoreData.do"

data <- data.frame()

tmp1 <- GET(url=api_url,
            query=list(serviceKey = safedata_API_key,
                       numOfRows = 10,
                       pageNo = 1,
                       dataType = "json")) %>%
  content(as="text") %>%
  fromJSON()

pageNum <- ceiling(tmp1$response$body$totalCount/10)

for (i in 101:pageNum){
  
  tmp <- GET(url=api_url,
             query=list(serviceKey = safedata_API_key,
                        numOfRows = 10,
                        pageNo = i,
                        dataType = "json")) %>%
    content(as="text") %>%
    fromJSON()
  data <- rbind(data, tmp$response$body$items)
  print(sprintf("%d개 중 %d번째 데이터를 습득했습니다.", pageNum, i))
}

conv <-data %>%
  filter(FCLTY_TY == "편의점")


write_delim(conv, file = "clean_input/convenient_store_230818.txt", delim = '\t')


a<-data %>%
  group_by(FCLTY_TY) %>%
  summarise(n = n())


safedata_API_key<-"F5FM17I5-F5FM-F5FM-F5FM-F5FM17I5P4"
conv_store <- getSafeMapAPI(key = safedata_API_key,
                            api_url = "http://safemap.go.kr/openApiService/data/getConvenienceStoreData.do")
university <- getSafeMapAPI(key = safedata_API_key, 
                            api_url = "http://safemap.go.kr/openApiService/data/getUniversity.do")

conv <-conv_store %>% distinct()


write_delim(conv_store, file = "clean_input/convenient_store_230818.txt', sep = '\t')

