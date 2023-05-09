# 2번 문제) 2020년 3월~2022년 7월 일간 데이터 내 대한민국 코로나 일별 
# 발생자수와 일별 사망자 수를 대상으로 R code로 시계열분석 수행
# rm(list = ls())
library(dplyr)
library(stringr)
library(corrgram)
library(ggplot2)
setwd("C:/Rwork/project1")
# 데이터 불러오기
#covid라는 data.fram()형 변수 생성
covid20 <- data.frame()
covid21 <- data.frame()
covid22 <- data.frame()

## file list
#src_dir에는 파일 위치를, src_file에는 파일 리스트를 담음
src_dir <- c("C:/Rwork/project1/data")
src_file20 <- list.files(src_dir,pattern="*-2020.csv")
src_file20
src_file21 <- list.files(src_dir,pattern="*-2021.csv")
src_file21
src_file22 <- list.files(src_dir,pattern="*-2022.csv")
src_file22

#for문을 돌려서 모든 파일을 하나의 변수, covid로 만듦(컬럼추가해서 파일명을 넣음)
for (i in 1:length(src_file21)){
  if( i <=length(src_file20)){
    covid_temp20 <- read.csv(
      paste0(src_dir, "/", src_file20[i]),
      sep=",",
      header=T,
      stringsAsFactors=F)
    
    # 새로운 컬럼 date 생성
    covid_temp20$date <- src_file20[i]
    
    # rbind covid_temp to covid data.frame.. bind_rows?
    covid20 <-  bind_rows(covid20,covid_temp20)
  }
  
  if( i <=length(src_file21)){
    covid_temp21 <- read.csv(
      paste0(src_dir, "/", src_file21[i]),
      sep=",",
      header=T,
      stringsAsFactors=F)
    
    # 새로운 컬럼 date 생성
    covid_temp21$date <- src_file21[i]
    
    # rbind covid_temp to covid data.frame.. bind_rows?
    covid21 <-  rbind(covid21,covid_temp21)
  }
  
  if( i <=length(src_file22)){
    covid_temp22 <- read.csv(
      paste0(src_dir, "/", src_file22[i]),
      sep=",",
      header=T,
      stringsAsFactors=F)
    
    # 새로운 컬럼 date 생성
    covid_temp22$date <- src_file22[i]
    
    # rbind covid_temp to covid data.frame.. bind_rows?
    covid22 <-  rbind(covid22,covid_temp22)
  }
  
  print(i) # for progress check
}

# covid20 %>% View()
# covid21 %>% View()
# covid22 %>% View()

# 한국 추출
korea20 <- covid20 %>% subset(Country_Region == "Korea, South"|Country_Region == "Republic of Korea"|
                              Country.Region == "Korea, South"|Country.Region == "Republic of Korea"|Country_Region == "South Korea"|Country.Region == "South Korea")
korea21 <- covid21 %>% subset(Country_Region == "Korea, South"|Country_Region == "Republic of Korea"|Country_Region == "South Korea")
korea22 <- covid22 %>% subset(Country_Region == "Korea, South"|Country_Region == "Republic of Korea"|Country_Region == "South Korea")

# 필요 데이터 추출
korea20 <- subset(korea20, select=c(Deaths, Confirmed, date))
korea21 <- subset(korea21, select=c(Deaths, Confirmed, date))
korea22 <- subset(korea22, select=c(Deaths, Confirmed, date))

korea20 %>% View()
korea21 %>% View()
korea22 %>% View()

#한국 합치기
koreas <- rbind(korea20,korea21,korea22)
tail(koreas,20)
nrow(koreas)

# 마이너스 달기
library(stringr)
subks <- koreas %>% summarise(miDeath = -Deaths, miConfirmed = -Confirmed) 
subks <- cbind(koreas,subks) %>% subset(select=c("date","miDeath","miConfirmed"))
subks
subks <- subks[-884, ]
subks
# rownames(subks) <- NULL
tail(subks,10)
nrow(subks)

# 기준 한국
onekoreas <- koreas 
onekoreas<- koreas[-1, ]
onekoreas
# rownames(onekoreas) <- NULL
nrow(onekoreas)


# subks 와 onekorea 인덱스 넣어주기
for(i in 1:nrow(subks)){
  subks[i,4] <- i
}
head(subks,6)
names(subks) <- c("date","Deaths","Confirmed","index")

for(i in 1:nrow(onekoreas)){
  onekoreas[i,4] <- i
}
head(onekoreas,6)
names(onekoreas) <- c("Deaths","Confirmed","date","index")

# 값 더하기
temps <-NULL
temps <- rbind(subks,onekoreas) #하나의 변수에 담아서!
temps <-aggregate(temps[,c('Deaths','Confirmed')], by=list(temps$index), FUN=sum) #인덱스끼리 묶어서 뺴기
temps %>% View()

#temp에 국가명, 날짜 추가 해주기
as <- onekoreas %>% subset(select=c("date"))
as
koreatotal <- cbind(as,temps) %>% subset(select=c("date","Deaths","Confirmed")) 
names(koreatotal) <- c("date","dayDeaths","dayConfirmed")
koreatotal

# 마이너스 숫자 변형
for(i in 1:nrow(koreatotal)){
  if(koreatotal$dayDeaths[i]<0){
    koreatotal$dayDeaths[i] <- 0
    cat(koreatotal$date[i],"dayDeaths\n")
  }
  if(koreatotal$dayConfirmed[i]<0){
    koreatotal$dayConfirmed[i] <- 0
    cat(koreatotal$date[i],"dayConfirmed\n")
  }
}
koreatotal

# 날짜에 csv 지우기
library(stringr)
for(i in 1:nrow(koreatotal)){
  koreatotal$date[i] <- str_replace(basename(koreatotal$date[i]), '.csv', '') 
}
# rownames(koreatotal) <- NULL

#날짜 형식 변환
koreatotal$date <- as.Date(koreatotal$date, "%m-%d-%Y")
kor <- koreatotal
kor %>% View()
nrow(kor)
kor

#월 단위로 묶기
kor$month <- factor(format(kor$date, "%Y-%m")) ####가져온 데이터에서 연도와 월만 뽑아서 새롭게 월데이터로 저장
kor
kormonth <- aggregate(kor[,c('dayDeaths','dayConfirmed')],by=list(kor$month),FUN = sum) ##월별로 사망자 확진자 합침
names(kormonth) <- c("month","Deaths","Confirmed")

# library(tidyverse)
kormonth
test <- kormonth[grep("2021",kormonth$month),]
test1 <- kormonth[grep("2020",kormonth$month),]
kormonth1 <- rbind(test1,test)

####################################################전처리 종료!
####################################################문제 풀기 시작!
options(scipen=999)  

# 1) 추세선 확인
# X11() #새로운 창에서 시각화화
library(ggplot2)
# 시계열 그래프 그리기
dayConfGrape <- ggplot(data = kor, aes(x = date, y = dayConfirmed))+geom_line(color = "#00AFBB", size = 1)
dayDGrape <- ggplot(data = kor, aes(x = date, y = dayDeaths))+geom_line(color = "#00AFBB", size = 1)
##선형 추세선 추가
dayConfChu <- dayConfGrape+ stat_smooth(color = "#FC4E07", method = "lm") 
dayConfChu
dayDChu <- dayDGrape+ stat_smooth(color = "#FC4E07", method = "lm") 
dayDChu



# 2) 4가지 변동요인 분해
#시계열 요소분해

# install.packages("seasonal")
# install.packages("lubridate")
library(lubridate)
library(seasonal)
library(zoo)
zooCon <- kor$dayConfirmed
zooDea <- kor$dayDeaths

# - time series 변환
tsCon <- as.ts(zooCon) %>% ts(start = decimal_date(as.Date("2020-03-01")), frequency = 365)
tsDea <- as.ts(zooDea) %>% ts(start = decimal_date(as.Date("2020-03-01")), frequency = 365)

#  - 4가지 변동요인 분해
tsCon %>% stl(t.window = 13, s.window = "periodic", robust=T) %>% autoplot()
tsDea %>% stl(t.window = 13, s.window = "periodic", robust=T) %>% autoplot()

kor


#월별 시각화 다채로운 그래프
zooCon1 <- kormonth$Confirmed
zooDea1 <- kormonth$Deaths
kormonth

tsCon1 <- as.ts(zooCon1) %>% ts(start = c(2020,3), frequency = 12)
tsDea1 <- as.ts(zooDea1) %>% ts(start = c(2020,3), frequency = 12)

tsCon1 %>% stl(t.window = 13, s.window = "periodic", robust=T) %>% autoplot()
tsDea1 %>% stl(t.window = 13, s.window = "periodic", robust=T) %>% autoplot()

ggseasonplot(tsCon1, year.labels = TRUE, year.labels.left = TRUE) + ylab("확진자 수") + xlab("월") + ggtitle("계절성 그래프 : 확진자 수")
ggseasonplot(tsDea1, year.labels = TRUE, year.labels.left = TRUE) + ylab("사망자 수") + xlab("월") + ggtitle("계절성 그래프 : 사망자 수")


#2020년, 2021년 그래프 시각화
zooCon2 <- kormonth1$Confirmed
zooDea2 <- kormonth1$Deaths
kormonth
tsCon2 <- as.ts(zooCon2) %>% ts(start = c(2020,3), frequency = 12)
tsDea2 <- as.ts(zooDea2) %>% ts(start = c(2020,3), frequency = 12)

# install.packages("fpp2")
library(fpp2)
plot(tsCon1)
ggseasonplot(tsCon2, year.labels = TRUE, year.labels.left = TRUE) + ylab("확진자 수") + xlab("월") + ggtitle("계절성 그래프 : 확진자 수")
ggseasonplot(tsDea2, year.labels = TRUE, year.labels.left = TRUE) + ylab("사망자 수") + xlab("월") + ggtitle("계절성 그래프 : 사망자 수")


# 3) 시각화
# install.packages("reshape")
# install.packages("cowplot")
library(reshape)
library(cowplot)
A = kor  %>%
  select(-dayDeaths) %>%
  melt(id.vars = c("date")) %>%
  ggplot() +
  geom_point(aes(x = date, y = value, col = variable),
             alpha =  0.5) +
  geom_line(aes(x = date, y = value, col = variable, group = variable),
            alpha = 0.8) +
  xlab("date") + ylab("dayCount") + 
  labs(col = "Type") + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8))

B = kor  %>%
  select(-dayConfirmed) %>%
  melt(id.vars = c("date")) %>%
  ggplot() +
  geom_point(aes(x = date, y = value, col = variable),
             alpha =  0.5) +
  geom_line(aes(x = date, y = value, col = variable, group = variable),
            alpha = 0.8) +
  xlab("date") + ylab("dayCount") + 
  labs(col = "Type") + 
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8))

cowplot::plot_grid(A,B,ncol = 1,labels = c("A","B"))

# 4) 결과 해석
par(mfrow = c(1, 1))
fore <- forecast(tsCon, h = 24) #24개월(2년) 예측
plot(fore, main="24개월 단위 확진자 예측")
fore2 <- forecast(tsCon, h = 6) #6개월 예측
plot(fore2, main="6개월 단위 확진자 예측")
fore3 <- forecast(tsDea, h = 24) #24개월(2년) 예측
plot(fore3, main="24개월 단위 사망자 예측")
fore4 <- forecast(tsDea, h = 6) #6개월 예측
plot(fore4, main="6개월 단위 사망자 예측")














