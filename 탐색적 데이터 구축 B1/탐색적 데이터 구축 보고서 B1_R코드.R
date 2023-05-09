# rm(list())

options(encoding = 'UTF-8')

# options(encoding = 'EUC-KR')

#2021 & 2022 파일 사용

setwd("C:/Rwork/project/2021")
library(dplyr)

## blank data.frame to save all files later
#covid라는 data.fram()형 변수 생성
covid <- data.frame()

## file list
#src_dir에는 파일 위치를, src_file에는 파일 리스트를 담음
src_dir <- c("C:/Rwork/project/2021")
src_file <- list.files(src_dir)
src_file
# src_file에 코로나 csv파일 다 들어가 있음


#for문을 돌려서 모든 파일을 하나의 변수, covid로 만듦(컬럼추가해서 파일명을 넣음)
for (i in 1:length(src_file)){
  # read dataset 1 by 1 sequentially
  #
  covid_temp <- read.csv(
    paste0(src_dir, "/", src_file[i]), 
    sep=",", 
    header=T, 
    stringsAsFactors=F)
  
  # add filename as a new column
  # 새로운 컬럼 date 생성
  covid_temp$date <- src_file[i]
  
  # rbind covid_temp to covid data.frame.. bind_rows?
  covid <- rbind(covid, covid_temp)
  
  print(i) # for progress check
}

#for문 다 돌고 나면, covid 변수에 값 잘 들어간 것 확인 가능!
print(covid)

# 국가명 합치기 : covidAll 변수에 저장!
subset(covid, select=c(Country_Region, Deaths, Confirmed, date))   #국가/사망자수/확인자/파일명명 출력
covidAll <- aggregate(covid[,c('Deaths','Confirmed')], by=list(covid$Country_Region, covid$date), FUN=sum)  # 국가별 사망자수/검사자수
names(covidAll) <- c("Country_Region","date","Deaths","Confirmed")
covidAll %>% arrange(Country_Region) # 오름차순
covidAll





# 문제1.  1년동안 대한민국에서 발생한 코로나 발생자수 및 사망자 수 데이터 대상으로
# 전처리를 실시하시오.


# 한국 값 출력
korea <- covidAll %>% subset(Country_Region == "Korea, South") 
#korea <- korea[-2, ]   # 내 파일에서 2022년 값이 들어가 있어서 제거해 줌@!!!!!!
korea
nrow(korea)

# 마이너스 달기
subk <- korea %>% summarise(miDeath = -Deaths, miConfirmed = -Confirmed) 
subk <- cbind(korea,subk) %>% subset(select=c("Country_Region","date","miDeath","miConfirmed"))
subk
subk <- subk[-153, ]
subk
nrow(subk)

# 기준 한국
onekorea <- korea 
onekorea<- korea[-1, ]
onekorea
nrow(onekorea)


# subk 와 onekorea 인덱스 넣어주기
for(i in 1:nrow(subk)){
  subk[i,5] <- i
}
subk
names(subk) <- c("Country_Region","date","Deaths","Confirmed","index")

for(i in 1:nrow(onekorea)){
  onekorea[i,5] <- i
}
onekorea
names(onekorea) <- c("Country_Region","date","Deaths","Confirmed","index")

# 값 더하기
temp <- rbind(subk,onekorea)
temp <-aggregate(temp[,c('Deaths','Confirmed')], by=list(temp$index), FUN=sum)
temp

#temp에 국가명, 날짜 추가 해주기
a <- onekorea %>% subset(select=c("Country_Region","date"))
a
koreatotal21 <- cbind(a,temp) %>% subset(select=c("Country_Region","date","Deaths","Confirmed")) 
names(koreatotal21) <- c("Country_Region","date","dayDeaths","dayConfirmed")
koreatotal21

# 2021년 한국 일 통계자료 = koreatotal21


##############################2022년
covids <- data.frame()
## file list
#src_dir에는 파일 위치를, src_file에는 파일 리스트를 담음
src_dir <- c("C:/Rwork/project/2022")
src_file <- list.files(src_dir)
src_file
# src_file에 코로나 csv파일 다 들어가 있음


#for문을 돌려서 모든 파일을 하나의 변수, covid로 만듦(컬럼추가해서 파일명을 넣음)
for (i in 1:length(src_file)){
  # read dataset 1 by 1 sequentially
  #
  covid_temps <- read.csv(
    paste0(src_dir, "/", src_file[i]), 
    sep=",", 
    header=T, 
    stringsAsFactors=F)
  
  # add filename as a new column
  # 새로운 컬럼 date 생성
  covid_temps$date <- src_file[i]
  
  # rbind covid_temp to covid data.frame.. bind_rows?
  covids <- rbind(covids, covid_temps)
  
  print(i) # for progress check
}

#for문 다 돌고 나면, covid 변수에 값 잘 들어간 것 확인 가능!
print(covids)

# 국가명 합치기 : covidAll 변수에 저장!
subset(covids, select=c(Country_Region, Deaths, Confirmed, date))   #국가/사망자수/확인자/파일명명 출력
covidAlls <- aggregate(covids[,c('Deaths','Confirmed')], by=list(covids$Country_Region, covids$date), FUN=sum)  # 국가별 사망자수/검사자수
names(covidAlls) <- c("Country_Region","date","Deaths","Confirmed")
covidAlls





# 문제1.  1년동안 대한민국에서 발생한 코로나 발생자수 및 사망자 수 데이터 대상으로
# 전처리를 실시하시오.


# 한국 값 출력
koreas <- covidAlls %>% subset(Country_Region == "Korea, South") 
#korea <- korea[-2, ]   # 내 파일에서 2022년 값이 들어가 있어서 제거해 줌@!!!!!!
koreas
nrow(koreas)

# 마이너스 달기
subks <- koreas %>% summarise(miDeath = -Deaths, miConfirmed = -Confirmed) 
subks <- cbind(koreas,subks) %>% subset(select=c("Country_Region","date","miDeath","miConfirmed"))
subks
subks <- subks[-213, ]
subks
nrow(subks)

# 기준 한국
onekoreas <- koreas 
onekoreas<- koreas[-1, ]
onekoreas
nrow(onekoreas)


# subk 와 onekorea 인덱스 넣어주기
for(i in 1:nrow(subks)){
  subks[i,5] <- i
}
subks
names(subks) <- c("Country_Region","date","Deaths","Confirmed","index")

for(i in 1:nrow(onekoreas)){
  onekoreas[i,5] <- i
}
onekoreas
names(onekoreas) <- c("Country_Region","date","Deaths","Confirmed","index")

# 값 더하기
temps <- rbind(subks,onekoreas)
temps <-aggregate(temps[,c('Deaths','Confirmed')], by=list(temps$index), FUN=sum)
temps

#temp에 국가명, 날짜 추가 해주기
as <- onekoreas %>% subset(select=c("Country_Region","date"))
as
koreatotal22 <- cbind(as,temps) %>% subset(select=c("Country_Region","date","Deaths","Confirmed")) 
names(koreatotal22) <- c("Country_Region","date","dayDeaths","dayConfirmed")
koreatotal22


#############################
# 2021년 데이터와 2022년 데이터 합치기
totalkorea <- rbind(koreatotal21,koreatotal22)
totalkorea

# 날짜에 csv 지우기
library(stringr)
for(i in 1:nrow(totalkorea)){
  totalkorea$date[i] <- str_replace(basename(totalkorea$date[i]), '.csv', '') 
}

# 마이너스 숫자 변형
for(i in 1:nrow(totalkorea)){
  if(totalkorea$dayDeaths[i]<0){
    totalkorea$dayDeaths[i] <- 0
    cat(totalkorea$date[i],"dayDeaths\n")
  }
  if(totalkorea$dayConfirmed[i]<0){
    totalkorea$dayConfirmed[i] <- 0
    cat(totalkorea$date[i],"dayConfirmed\n")
  }
}

tail(totalkorea,250)
totalkorea %>% View()


# 1번 문제.(1)

#결측치 확인
summary(totalkorea$dayDeath )  
summary(totalkorea$dayConfirmed)

nrow(totalkorea)



#  2번 문제.(1). 기술통계량 구하기
# 평균, 중앙값,  최소값, 최대값
summary(totalkorea) 

# 관측수
length(totalkorea$dayDeaths)
length(totalkorea$dayConfirmed)
#합
sum(totalkorea$dayDeaths)
sum(totalkorea$dayConfirmed)
# 최빈값
table(totalkorea$dayDeaths) %>% max(.) 
table(totalkorea$dayConfirmed) %>% max(.) 

# 표준편차
sd(totalkorea$dayDeaths)
 sd(totalkorea$dayConfirmed)

#분산
var(totalkorea$dayDeaths)
var(totalkorea$dayConfirmed)

#범위
diff(range(totalkorea$dayDeaths))
diff(range(totalkorea$dayConfirmed))

library(moments)
#왜도 구하기
skewness(totalkorea$dayDeaths)
skewness(totalkorea$dayConfirmed)
#첨도 구하기
kurtosis(totalkorea$dayDeaths)
kurtosis(totalkorea$dayConfirmed)



# 3번 문제.(1) 1년동안 대한민국에서 발생한 코로나 발생자수 및 사망자 수 데이터 대상으로 box plot.
# par(mfrow = c(1, 1))
options(scipen=999)
boxplot(totalkorea$dayConfirmed, main = "확진자 분포")
boxplot(totalkorea$dayDeaths, main = "사망자 분포")

#  3번 문제.(2) 월별로 코로나 발생자수 및 사망자수의 히스토그램으로 시각화
# 월별 데이터 생성
# 날짜에 일/년도 지우기
library(stringr)
monthkorea <- totalkorea
for(i in 1:365){
  a <- substr(basename(totalkorea$date[i]), '1', '2')
  b <- substr(basename(totalkorea$date[i]), '6', '10')
  monthkorea$date[i] <-paste(b,a)
  monthkorea$date[i] <- str_replace(basename(monthkorea$date[i]), '-', '') 
  monthkorea$date[i] <- str_replace(basename(monthkorea$date[i]), ' ', '-')
}
monthkorea

# 월별 묶기
monthkorea <- aggregate(monthkorea[,c('dayDeaths','dayConfirmed')], by=list(monthkorea$date), FUN=sum)
names(monthkorea) <- c("Month","dayDeaths","dayConfirmed")
monthkorea

# 월별 시각화  
hist(monthkorea$dayDeaths,xlim = c(0, 10000), ylim = c(0, 10), main = "사망자 분포")
hist(monthkorea$dayConfirmed,xlim = c(0, 13000000), ylim = c(0, 10), main = "확진자 분포") 
library(ggplot2)
grape <- ggplot()+ geom_bar(data=monthkorea, aes(x=Month, y=dayDeaths), stat="identity")+ theme(axis.text.x = element_text(angle = 90))+geom_bar(fill = "YELLOW")
grape <- grape + labs(title='한국의 월별 사망자수')+ theme(plot.title = element_text(size=18)) # 제목수정
grape
grape2 <- ggplot()+ geom_bar(data=monthkorea, aes(x=Month, y=dayConfirmed), stat="identity")+ theme(axis.text.x = element_text(angle = 90))+scale_fill_brewer(palette = "Blues")
grape2 <- grape2 + labs(title='한국의 월별 확진자수')+ theme(plot.title = element_text(size=18)) # 제목수정
options(scipen=999)  # 숫자변환
grape2


summary(monthkorea$dayDeaths)
summary(monthkorea$dayConfirmed)









