
library(tidyverse)
library(sf)
library(lmtest)
library(sandwich)
library(readxl)
library(glue)
library(rgeoda)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Analysis/single_house")

## 데이터 가져오기
seoul <- read_sf("clean_input/seoul.gpkg") %>%
  st_make_valid() %>%
  st_transform(5179) %>%
  mutate(area = as.numeric(st_area(geom))/1000000)
dong_center <- st_centroid(seoul)

# 1) 접근성 관련 데이터
subway <- read_csv("raw_input/seoul_subway.csv") %>%
  st_as_sf(coords = c("환승역X좌표", "환승역Y좌표"), crs = 4326) %>% st_transform(5179) %>%
  st_join(seoul, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(행정동코드) %>% 
  summarise(n_subway = n())

busstop <-read_csv("raw_input/seoul_busstop.csv") %>%
  mutate(x = gsub("﻿", "", 좌표X),
         y = gsub("﻿", "", 좌표Y)) %>%
  filter(`정류장지역유형(0:서울/1:경기/2:인천)`=="﻿0") %>%
  filter(`가상정류장여부`=="﻿0") %>%
  filter(`정류장이용여부`=="﻿1") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_transform(5179) %>%
  st_join(seoul, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(행정동코드) %>% 
  summarise(n_busstop = n())

busstop_topis <-read_csv("raw_input/seoul_busstop_topis.csv", locale = locale(encoding = 'euc-kr')) %>%
  #filter(정류장_유형 %in% c("중앙차로","일반차로")) %>%
  st_as_sf(coords = c("경도", "위도"), crs = 4326) %>% st_transform(5179) %>%
  st_join(seoul, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(행정동코드) %>% 
  summarise(n_busstop = n())


# 2) 주택관련 데이터
housetype <- read_delim("raw_input/seoul_housetype_2021.txt", delim ='\t', locale = locale(encoding = 'utf-8')) %>%
  mutate(adm_cd2 = adm_cd / 10) %>% 
  mutate(adm_cd2 = case_when(adm_cd2 == 1117073 ~ 1117068,  # 오류2동
                             adm_cd2 == 1117074 ~ 1117068,  # 항동 -> 오류2동
                             adm_cd2 == 1125075 ~ 1125051,  # 강일동 -> 오류2동
                             adm_cd2 == 1125076 ~ 1125052,  # 상일1동 -> 오류2동
                             adm_cd2 == 1125077 ~ 1125052,  # 상일2동 -> 오류2동
                             TRUE ~ adm_cd2) 
  ) %>%
  group_by(adm_cd2, type_nm) %>% summarise(house_cnt = sum(house_cnt)) %>%
  pivot_wider(id_cols = adm_cd2, names_from = type_nm, values_from = house_cnt)

housearea <- read_delim("raw_input/seoul_housearea_2021.txt", delim ='\t', locale = locale(encoding = 'utf-8')) %>%
  mutate(adm_cd2 = adm_cd / 10) %>% 
  mutate(adm_cd2 = case_when(adm_cd2 == 1117073 ~ 1117068,  # 오류2동
                             adm_cd2 == 1117074 ~ 1117068,  # 항동 -> 오류2동
                             adm_cd2 == 1125075 ~ 1125051,  # 강일동 -> 오류2동
                             adm_cd2 == 1125076 ~ 1125052,  # 상일1동 -> 오류2동
                             adm_cd2 == 1125077 ~ 1125052,  # 상일2동 -> 오류2동
                             TRUE ~ adm_cd2) 
  ) %>%
  group_by(adm_cd2) %>% summarise(area_under40= sum(house_cnt))

houseage <- read_delim("raw_input/seoul_houseage_2021.txt", delim ='\t', locale = locale(encoding = 'utf-8')) %>%
  mutate(adm_cd2 = adm_cd / 10) %>% 
  mutate(adm_cd2 = case_when(adm_cd2 == 1117073 ~ 1117068,  # 오류2동
                             adm_cd2 == 1117074 ~ 1117068,  # 항동 -> 오류2동
                             adm_cd2 == 1125075 ~ 1125051,  # 강일동 -> 오류2동
                             adm_cd2 == 1125076 ~ 1125052,  # 상일1동 -> 오류2동
                             adm_cd2 == 1125077 ~ 1125052,  # 상일2동 -> 오류2동
                             TRUE ~ adm_cd2) 
  ) %>%
  group_by(adm_cd2) %>% summarise(age_over30= sum(house_cnt))

house <- housetype %>%
  left_join(housearea, by = c("adm_cd2")) %>%
  left_join(houseage, by = c("adm_cd2"))


# 3) 동별 아파트 평균 전세 실거래가
rentprice <- read_delim("raw_input/seoul_rentprice_apt.txt", delim ='\t') %>%
  mutate(price_m2 = `보증금(만원)`/`전용면적(㎡)`)

sf_rentprice <- rentprice %>%
  filter(x > 0) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_transform(5179)

rent_dong <- st_join(sf_rentprice, seoul, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(행정동코드) %>% 
  summarise(avg_price = mean(price_m2))


# 4) 동별 최단거리 대학
sf_univ <- read_delim("raw_input/university_kedi_20220310.txt", delim =',', locale = locale(encoding = 'utf-8')) %>%
  filter(학교구분 == "대학" & 지역 == "서울" & 학교상태 == "기존") %>% 
  filter(x > 0) %>%
  st_as_sf(coords = c("x", "y"), crs = 5179)

min_dist <- st_distance(dong_center, sf_univ) %>%
  apply(1, min)

dist_univ <- data.frame(행정동코드 = dong_center$행정동코드, dist_univ = min_dist)
univ_dong <-sf_univ %>%
  st_join(seoul, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(행정동코드) %>% 
  summarise(n_univ = n())


# 5) 편의점
sf_conv <- read_delim("raw_input/convenient_store_230818.txt", delim ='\t', locale = locale(encoding = 'utf-8')) %>%
  st_as_sf(coords = c("X", "Y"), crs = 3857) %>% st_transform(5179) %>%
  st_join(seoul, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(행정동코드) %>% 
  summarise(n_conv = n())

# 6) 공원수
park <- read_csv("raw_input/park_count.csv")
park <- park %>%
  rename(park = NUMPOINTS) %>%
  select(행정동코드, park)

# 7) POI
poi <- read_csv("raw_input/poi_data_not_na.csv", locale = locale(encoding = 'cp949')) %>%
  filter(영업상태구분코드 == 1) %>%
  select(개방서비스명, `좌표정보(x)`, `좌표정보(y)`)

hospital <- poi %>%
  filter(개방서비스명 %in% c("병원", "의원")) %>%
  st_as_sf(coords = c("좌표정보(x)", "좌표정보(y)"), crs = 2097) %>% st_transform(5179) %>%
  st_join(seoul, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(행정동코드) %>% 
  summarise(n_hos = n())

restaurant <- poi %>%
  filter(개방서비스명 %in% c("단란주점영업", "일반음식점", "휴게음식점")) %>%
  st_as_sf(coords = c("좌표정보(x)", "좌표정보(y)"), crs = 2097) %>% st_transform(5179) %>%
  st_join(seoul, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(행정동코드) %>% 
  summarise(n_rest = n())

# 7) 총종사자수
work <- read_csv("raw_input/work_people.csv") # 총종사자수 = 2020년 / 총종사자수_2021 = 2021년
work$행정동명 <- paste0(work$시군구, " ", work$행정동)

work<- work %>%
  select(행정동명, 총종사자수_2021) %>%
  subset(substr(work$행정동명, nchar(work$행정동명)-1, nchar(work$행정동명)) != "소계")

# 행정동코드 부여
hjd_code_data <- read_excel("raw_input/hjd_code.xlsx", sheet = 1, skip = 1)
str(hjd_code_data)

hjd_code_data$H_DNG_CD<- as.integer(hjd_code_data$H_DNG_CD)
hjd_code_data$행정동명 <- paste0(hjd_code_data$CT_NM, " ", hjd_code_data$H_DNG_NM)
hjd_code_data <- hjd_code_data %>%
  rename(행정동코드 = H_DNG_CD) %>% # 행정동코드 H_DNG_CD는 행자부행정동코드
  rename(통계청행정동코드 = H_SDNG_CD)

work <- left_join(work, hjd_code_data[, c("행정동명", "통계청행정동코드")], by = "행정동명")
work <- work %>%
  select(통계청행정동코드, 총종사자수_2021) %>%
  rename(emp_21 = 총종사자수_2021)


# 6) 총가구수
household <- read_csv("raw_input/all_house.csv")
household$행정동명 <- paste0(household$시군구, " ", household$행정동)
household<- household %>%
  select(-시군구, -행정동) %>%
  select(행정동명, everything()) %>%
  subset(substr(household$행정동명, nchar(household$행정동명)-1, nchar(household$행정동명)) != "소계")

household<- household[household$행정동명 != "구로구 항동",]


# 행정동코드 부여
hjd_code_data <- read_excel("raw_input/hjd_code.xlsx", sheet = 1, skip = 1)
str(hjd_code_data)

hjd_code_data$H_DNG_CD<- as.integer(hjd_code_data$H_DNG_CD)
hjd_code_data$행정동명 <- paste0(hjd_code_data$CT_NM, " ", hjd_code_data$H_DNG_NM)
hjd_code_data <- hjd_code_data %>%
  rename(행정동코드 = H_DNG_CD) %>% # 행정동코드 H_DNG_CD는 행자부행정동코드
  rename(통계청행정동코드 = H_SDNG_CD)

household <- left_join(household, hjd_code_data[, c("행정동명", "통계청행정동코드")], by = "행정동명")
household <- household %>%
  select(통계청행정동코드, 소계) %>%
  rename(household = 소계)


# 6) 시민생활
raw_life <- read_excel("raw_input/2023.6월_10개 관심집단 수.xlsx")  %>%
  mutate(행정동코드 = as.numeric(행정동코드))

all <- raw_life %>%
  group_by(행정동코드) %>%
  summarise(single = sum(`1인가구수`),
            single_s = sum(`외출-커뮤니케이션이 모두 적은 집단(전체)`),
            pop = sum(`총인구`))

young_1 <- raw_life %>%
  filter(연령대 < 30 & 연령대 >= 20) %>%
  group_by(행정동코드) %>%
  summarise(single_2029 = sum(`1인가구수`),
            single_2029_s = sum(`외출-커뮤니케이션이 모두 적은 집단(전체)`))

young_2 <- raw_life %>%
  filter(연령대 < 40 & 연령대 >= 30) %>%
  group_by(행정동코드) %>%
  summarise(single_3039 = sum(`1인가구수`),
            single_3039_s = sum(`외출-커뮤니케이션이 모두 적은 집단(전체)`))

young <- raw_life %>%
  filter(연령대 < 40 & 연령대 >= 20) %>%
  group_by(행정동코드) %>%
  summarise(single_2039 = sum(`1인가구수`),
            single_2039_s = sum(`외출-커뮤니케이션이 모두 적은 집단(전체)`))

middle <- raw_life %>%
  filter(연령대 < 65 & 연령대 >= 40) %>%
  group_by(행정동코드) %>%
  summarise(single_4064 = sum(`1인가구수`),
            single_4064_s = sum(`외출-커뮤니케이션이 모두 적은 집단(전체)`))

older <- raw_life %>%
  filter(연령대 >= 65) %>%
  group_by(행정동코드) %>%
  summarise(single_65 = sum(`1인가구수`),
            single_65_s = sum(`외출-커뮤니케이션이 모두 적은 집단(전체)`))


# 7) 도시활력(생활인구)
raw_vital <- read_delim("clean_input/LOCAL_PEOPLE_DONG_202304.txt", delim  = '|', 
                        locale = locale(encoding = 'cp949')) %>%
  mutate(시간대구분 = as.integer(시간대구분))
admincode <- seoul_emd <-read_sf("clean_input/seoul_emd.shp") %>%
  st_drop_geometry() %>%
  mutate(adm_cd3 = as.integer(substr(adm_cd2, 1, 8)),
         adm_cd = as.integer(adm_cd)) %>%
  mutate(adm_cd3 = case_when(adm_cd3 == 11305595 ~ 11305590, # 강북구 번1동 ~ 3동, 강북구 수유1동 ~ 3동
                             adm_cd3 == 11305603 ~ 11305600,
                             adm_cd3 == 11305608 ~ 11305606,
                             adm_cd3 == 11305615 ~ 11305610,
                             adm_cd3 == 11305625 ~ 11305620,
                             adm_cd3 == 11305635 ~ 11305630,
                             TRUE ~ adm_cd3)) %>%
  mutate(adm_cd = ifelse(adm_cd == 1117073, 1117068, adm_cd)) %>%
  select(adm_nm, adm_cd, adm_cd3)

weekday<-c(20230403, 20230404, 20230405, 20230406, 20230410, 20230411, 20230412, 20230413, 20230417, 20230418, 20230419, 20230420, 20230424, 20230425, 20230426, 20230427)

day_vital <-raw_vital %>%
  filter(기준일ID %in% weekday) %>%
  filter(시간대구분 > 9 & 시간대구분 < 16) %>%
  group_by(행정동코드) %>%
  summarise(day_vitality = mean(총생활인구수)) %>%
  left_join(admincode, by = c("행정동코드" = "adm_cd3")) %>%
  select(adm_cd, day_vitality)

night_vital <-raw_vital %>%
  filter(기준일ID %in% weekday) %>%
  filter(시간대구분 > 18 & 시간대구분 < 23) %>%
  group_by(행정동코드) %>%
  summarise(night_vitality = mean(총생활인구수)) %>%
  left_join(admincode, by = c("행정동코드" = "adm_cd3")) %>%
  select(adm_cd, night_vitality)



# 변수통합
analysis <- seoul %>%
  left_join(household, by = c("행정동코드" = "통계청행정동코드")) %>%
  left_join(all, by = c("행정동코드")) %>%
  left_join(young, by = c("행정동코드")) %>%
  left_join(young_1, by = c("행정동코드")) %>%
  left_join(young_2, by = c("행정동코드")) %>%
  left_join(middle, by = c("행정동코드")) %>%
  left_join(older, by = c("행정동코드")) %>%
  left_join(day_vital, by = c("행정동코드" = "adm_cd")) %>%
  left_join(night_vital, by = c("행정동코드" = "adm_cd")) %>%
  left_join(subway, by = c("행정동코드")) %>%
  left_join(busstop_topis, by = c("행정동코드")) %>%
  left_join(house, by = c("행정동코드" = "adm_cd2")) %>%
  left_join(rent_dong, by = c("행정동코드")) %>%
  left_join(dist_univ, by = c("행정동코드")) %>%
  left_join(univ_dong, by = c("행정동코드")) %>%
  left_join(sf_conv, by = c("행정동코드")) %>%
  left_join(hospital, by = c("행정동코드")) %>%
  left_join(restaurant, by = c("행정동코드")) %>%
  left_join(park, by = c("행정동코드")) %>%
  left_join(work, by = c("행정동코드" = "통계청행정동코드")) %>%
  mutate(sgg_cd = substr(행정동코드, 1, 5))

analysis[is.na(analysis)]<-0
analysis <- analysis %>% filter(행정동명 != "강동구 둔촌1동")


analysis$전체[analysis$전체 == 0]<-1
analysis$single_2029_s[analysis$single_2029_s == 0]<-0.1
analysis$single_3039_s[analysis$single_3039_s == 0]<-0.1
analysis$single_65_s[analysis$single_65_s == 0]<-0.1
analysis<- analysis %>%
  mutate(vital_type = case_when(day_vitality > mean(day_vitality) & night_vitality > mean(night_vitality) ~ 2, # 번화가
                                day_vitality > mean(day_vitality) & night_vitality < mean(night_vitality) ~ 3, # 업무중심지구
                                day_vitality < mean(day_vitality) & night_vitality > mean(night_vitality) ~ 1, # 주거지구
                                day_vitality < mean(day_vitality) & night_vitality < mean(night_vitality) ~ 4)) # 비활성화지역


hist(log(analysis$single_2039_s/analysis$single), freq=FALSE, xlab="ln(num_single)")
plot(analysis['n_rest'])


## 회귀분석
# 모형 1: 인구 대비 1인 가구 전체, 모형 2: 인구 대비 청년 1인 가구, 모형 3: 인구 대비 중장년 1인 가구, 모형 4: 인구 대비 노인 1인 가구
# 모형 5: 인구 대비 고립 1인 가구 전체, 모형 6: 인구 대비 고립 청년 1인 가구, 모형 7: 인구 대비 고립 중장년 1인 가구, 모형 8: 인구 대비 고립 노인 1인 가구

# 모형 추정식: 1인 가구 ~ 중소형 비율 + 아파트 비율 + 평균 전세가격 + 지하철 밀도 + 버스정류장 밀도 + 대학 밀도 + 종사자밀도 + 병원 밀도 + 음식점 및 주점 밀도 + 공원 밀도
dep <- "single"
denominator <- "pop"

form <- glue("log({dep}*100/{denominator}) ~ I(area_under40/전체)+ I(아파트/전체) + I(avg_price/100) + I(n_subway/area) + I(n_busstop/area) + I(n_univ/area) + I((emp_21/1000)/area) + I(n_hos/area)+ I(n_rest/area) + I(park/area)")
reg <- lm(form, data = analysis)
summary(reg)

nb <- nb2listw(poly2nb(analysis), style = "W")
moran.test(analysis$single_4064, listw=nb)
moran.test(reg$residuals, listw=nb)
lm.LMtests(reg, listw = nb, test = "all", zero.policy=TRUE)

lag_reg <- lagsarlm(form, data = analysis, listw = nb)
err_reg <- errorsarlm(form, data = analysis, listw = nb)
sac_reg <- sacsarlm(form, data = analysis, listw = nb)

summary(lag_reg)
summary(err_reg)

stargazer(lag_reg, title = "results", alight=TRUE, type="text")

#####

dep <- "single"
denominator <- "pop"

form <- glue("log({dep}*100/{denominator}) ~ I(area_under40/전체)+ I(아파트/전체) + I(avg_price/100) + I(n_subway/area) + I(n_busstop/area) + I(n_univ/area) + I((emp_21/1000)/area) + I(n_hos/area)+ I(n_rest/area) + I(park/area)")
reg <- lm(form, data = analysis)
summary(reg)

hist(reg$residuals, freq=FALSE, xlab="residuals")

car::qqPlot(reg)
plot(resid(reg))
qqnorm(reg$residuals)
qqline(reg$residuals, col = 2)
shapiro.test(reg$residuals)
car::vif(reg)
lmtest::bptest(reg)
lmtest::dwtest(reg)
robust_reg <- lmtest::coeftest(reg, vcovCL(reg, cluster = analysis$sgg_cd))


## 공간적 자기상관성 테스트
library(spdep)
library(spatialreg)
coords<-st_centroid(st_geometry(analysis), of_largest_polygon=TRUE)
k1 <- knn2nb(knearneigh(coords))
all.linked <- max(unlist(nbdists(k1, coords)))
nb <- nb2listw(dnearneigh(coords, 0, all.linked), style = "W")

nb <- nb2listw(poly2nb(analysis), style = "W")

moran.test(analysis$single_s, listw=nb)


## 공간계량경제모형 수행
lm.LMtests(reg, listw = nb, test = "all", zero.policy=TRUE)

lag_reg <- lagsarlm(form, data = analysis, listw = nb)
err_reg <- errorsarlm(form, data = analysis, listw = nb)
sac_reg <- sacsarlm(form, data = analysis, listw = nb)

summary(lag_reg)
summary(err_reg)
summary(sac_reg)


## 
library(stargazer)

out_path<-glue("C:/Users/Kyusang/Desktop/{dep}.html")
stargazer(err_reg, title = "results", alight=TRUE, type="html", out = out_path)


## 지도화
w <- queen_weights(analysis)
lisa <- local_moran(w, analysis['single_2029_s']/analysis['pop'])

lisa_color <- lisa_colors(lisa)
lisa_label <- lisa_labels(lisa)
lisa_cluster <- lisa_clusters(lisa)

plot(st_geometry(analysis),
     col = sapply(lisa_cluster, function(x){return(lisa_color[[x+1]])}),
     border = "#333333", lwd=0.2)
title(main = "Local Moran Map of One-Person Household per Person")
legend('topleft', legend = lisa_label, fill = lisa_color, border = "#eeeeee")

## 단계구분도
library(tmap)

analysis <- analysis %>%
  mutate()

tm_shape(analysis) +
  tm_fill("single_2039", style = "jenks", n = 6, 
          legend.hist = FALSE, title = "Age 20-39",
          palette = "-RdBu") +
  tm_borders() +
  tm_layout(legend.outside = FALSE,
            legend.outside.position = "right") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"))


## leaflet을 사용한 시각화
library(leaflet)
analysis_wgs84 <- st_transform(analysis, 4326)

leaflet() %>%
  addTiles() %>% # OSM 베이스맵을 활용할 때 사용
  setView(lng=127, lat=37.6, zoom=11) %>%
  addPolygons(
    data = analysis_wgs84,
    fillColor = ~colorNumeric(palette = "YlOrRd", domain = analysis_wgs84$n_hos)(n_hos),
    fillOpacity = 0.7,
    color = "white",
    weight = 1, 
    label = ~paste(analysis_wgs84$행정동명, ": ", n_hos),
    group = "day_vitality"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(palette = "YlOrRd", domain = analysis_wgs84$n_hos),
    values = analysis_wgs84$n_hos,
    title = "Value",
    group = "day_vitality")


leaflet() %>%
  addTiles() %>% # OSM 베이스맵을 활용할 때 사용
  setView(lng=127, lat=37.6, zoom=11) %>%
  addPolygons(
    data = analysis_wgs84,
    fillColor = ~colorNumeric(palette = "YlOrRd", domain = analysis_wgs84$day_vitalit)(day_vitality),
    fillOpacity = 0.7,
    color = "white",
    weight = 1, 
    label = ~paste(analysis_wgs84$행정동명, ": ", day_vitality),
    group = "day_vitality"
  ) %>%
  addPolygons(
    data = analysis_wgs84,
    fillColor = ~colorNumeric(palette = "YlOrRd", domain = analysis_wgs84$night_vitality)(night_vitality),
    fillOpacity = 0.7,
    color = "white",
    weight = 1, 
    label = ~paste(analysis_wgs84$행정동명, ": ", night_vitality),
    group = "night_vitality"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(palette = "YlOrRd", domain = analysis_wgs84$day_vitality),
    values = analysis_wgs84$day_vitality,
    title = "Value",
    group = "day_vitality"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(palette = "YlOrRd", domain = analysis_wgs84$night_vitality),
    values = analysis_wgs84$night_vitality,
    title = "Value",
    group = "night_vitality"
  ) %>%
  addLayersControl(
    baseGroups = c("OSM"),
    overlayGroups =  c("day_vitality", "night_vitality"),
    position = c("bottomright"))






