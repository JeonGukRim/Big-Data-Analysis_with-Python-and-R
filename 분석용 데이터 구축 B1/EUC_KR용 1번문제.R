#######################2021년###############################################
rm(list=ls())
setwd("C:/Rwork/qq/2021")
getwd()
library(dplyr)
options(scipen=999) 
## blank data.frame to save all files later

#covid2021이라는 data.fram()형 변수 생성
covid2021 <- data.frame()
test <- data.frame()

## file list
#src_dir에는 파일 위치를, src_file에는 파일 리스트를 담음
src_dir <- c("C:/Rwork/qq/2021")
src_file <- list.files(src_dir)
src_file

##2021년 정보에 먼저 7월31일 정보담기
covid2021_temp <- read.csv(
  paste0(src_dir, "/", "07-31-2021.csv"), 
  sep=",", 
  header=T, 
  stringsAsFactors=F)  
covid2021_temp

##데이터순서 맞춰주기 위해 국가순으로 배정
covid2021_temp %>% arrange(Country_Region) 

# 새로운 컬럼 date 생성
covid2021_temp$date <- "07-31-2021.csv"
covid2021_temp

# covid2021 데이터프레임에 covid 2021_temp 정보넣기(이 과정을 밑에 반복문에서 차례차례담음) 
covid2021 <- rbind(covid2021, covid2021_temp) 
covid2021

###폴더 첫번째 파일이 07-31-2021.csv이므로 인덱스 1부터 반복
for (i in 2:length(src_file)){
  covid2021_temp <- read.csv(
    paste0(src_dir, "/", src_file[i]), 
    sep=",", 
    header=T, 
    stringsAsFactors=F)
  
  covid2021_temp %>% arrange(Country_Region)
  
  # add filename as a new column
  # 새로운 컬럼 date 생성
  covid2021_temp$date <- src_file[i]
  
  # 2021년 정보를 위의 과정으로 차례차례 담음
  covid2021 <- rbind(covid2021, covid2021_temp) 
  
  print(i)
}
head(covid2021)
tail(covid2021)####데이터 잘들어갔는지 확인

##날짜와 지역이 같은것들끼리 누적 사망자수와 누적확진자수를 합침 
covidAll2021 <- aggregate(covid2021[,c('Deaths','Confirmed')], by=list(covid2021$Country_Region, covid2021$date), FUN=sum) 
#열 이름 지정
names(covidAll2021) <- c("Country_Region","date","Deaths","Confirmed") 
covidAll2021
# View(covid2021)


#일일 정보를 담기위해 새로운 데이터프레임 생성
covidAll2021final <- data.frame() 

for(i in 2:length(src_file)){
  x1 <- covidAll2021 %>% filter(date == src_file[i])  #뒷날짜 지정
  x2 <- covidAll2021 %>% filter(date == src_file[i-1]) #앞날짜 지정
  x3 <- x1$Deaths-x2$Deaths   # 뒷날짜 사망자 - 앞날짜 사망자
  x4 <- x1$Confirmed - x2$Confirmed # 뒷날짜 확진자 - 앞날짜 확진자
  x1$dayDeath <- x3        #x1에 일 사망자 넣기
  x1$dayConfirmed <- x4    #x1에 일 확진자 넣기
  
  # x1에 있는 데이터들을 covidAll2021final로 옮겨줌
  # x1에는 7/31데이터만 있음. covidAll2021final에 모든 데이터 존재
  covidAll2021final <- rbind(covidAll2021final,x1) 
}

#21년일일 확진자와 사망자 음수값 리스트 출력
covidAll2021final[covidAll2021final$dayConfirmed<0,]  %>% nrow()
covidAll2021final[covidAll2021final$dayDeath<0,] %>% nrow()


#21년일일 확진자와 사망자 음수값을 0으로 변경
covidAll2021final$dayDeath <- ifelse(covidAll2021final$dayDeath < 0,0,covidAll2021final$dayDeath)
covidAll2021final$dayConfirmed <- ifelse(covidAll2021final$dayConfirmed < 0,0,covidAll2021final$dayConfirmed)

tail(covidAll2021final) #### 2021년 일별 확진자수 및 사망자수 및 누적값 

##############################################################

# 일일확진자를 전부 더한 07-31에서 12-31까지의 총 사망자및 확진자 수
covidAll2021total <- aggregate(covidAll2021final[,c('dayDeath','dayConfirmed')], by=list(covidAll2021final$Country_Region), FUN=sum)
names(covidAll2021total) <- c("Country_Region","Deaths","Confirmed")
head(covidAll2021total) 





#############################2022년#####################################

#covid라는 data.fram()형 변수 생성
covid2022 <- data.frame()

covid2022_temp <- read.csv("12-31-2021.csv",header = T)
covid2022_temp %>% arrange(Country_Region)

# 새로운 컬럼 date 생성
covid2022_temp$date <- "12-31-2021.csv"
covid2022 <- rbind(covid2022, covid2022_temp)
covid2022

#2021년 12월31일과 2022년 1월1일파일이 다른폴더에있어 setwd하기전에 변수 따로 지정 이후는 위와 과정똑같음
setwd("C:/Rwork/qq/2022")
getwd()

library(dplyr)


## file list
#src_dir에는 파일 위치를, src_file에는 파일 리스트를 담음
src_dir <- c("C:/Rwork/qq/2022")
src_file <- list.files(src_dir)
src_file

###폴더 첫번째 파일이 01-01-2022.csv이므로 인덱스 1부터 반복#####
for (i in 1:length(src_file)){
  covid2022_temp <- read.csv(
    paste0(src_dir, "/", src_file[i]), 
    sep=",", 
    header=T, 
    stringsAsFactors=F)
  
  covid2022_temp %>% arrange(Country_Region)
  
  # add filename as a new column
  # 새로운 컬럼 date 생성
  covid2022_temp$date <- src_file[i]
  
  # rbind covid_temp to covid data.frame.. bind_rows?
  covid2022 <- rbind(covid2022, covid2022_temp)
  
  print(i)
}

head(covid2022)
tail(covid2022)
covid2022
covidAll2022 <- aggregate(covid2022[,c('Deaths','Confirmed')], by=list(covid2022$Country_Region, covid2022$date), FUN=sum)
names(covidAll2022) <- c("Country_Region","date","Deaths","Confirmed")
covidAll2022
tail(covidAll2022)


covidAll2022final <- data.frame()

x1 <- covidAll2022 %>% filter(date == "01-01-2022.csv")
x2 <- covidAll2022 %>% filter(date == "12-31-2021.csv")

x3 <- x1$Deaths - x2$Deaths
x4 <- x1$Confirmed - x2$Confirmed
x3
x1$dayDeath <- x3
x1$dayConfirmed <- x4

covidAll2022final <- rbind(covidAll2022final,x1)

for(i in 2:length(src_file)){
  x1 <- covidAll2022 %>% filter(date == src_file[i])
  x2 <- covidAll2022 %>% filter(date == src_file[i-1])
  
  x3 <- x1$Deaths-x2$Deaths
  x4 <- x1$Confirmed - x2$Confirmed
  x1$dayDeath <- x3
  x1$dayConfirmed <- x4
  
  covidAll2022final <- rbind(covidAll2022final,x1)
}

#22년일일 확진자와 사망자 음수값 리스트 출력
covidAll2022final[covidAll2022final$dayConfirmed<0,] %>% nrow()
covidAll2022final[covidAll2022final$dayDeath<0,]%>% nrow()

#22년일일 확진자와 사망자 음수값을 0으로 변경
covidAll2022final$dayDeath <- ifelse(covidAll2022final$dayDeath < 0,0,covidAll2022final$dayDeath)
covidAll2022final$dayConfirmed <- ifelse(covidAll2022final$dayConfirmed < 0,0,covidAll2022final$dayConfirmed)

View(covidAll2022final) 
tail(covidAll2022final)
####2022년 종합 일별 사망자수 및 확진자수, 누적데이터



# 일일데이터로 구한 01-01에서 07-31까지의 국가별 사망자및 확진자 수
covidAll2022total <- aggregate(covidAll2022final[,c('dayDeath','dayConfirmed')], by=list(covidAll2022final$Country_Region), FUN=sum)
names(covidAll2022total) <- c("Country_Region","Deaths","Confirmed")
head(covidAll2022total) 



######################################################################################

# 문제 (1-1) 일별 데이터 활용 -> 개별 국가 : 사망자/확인자/일별 사망자/일별 확인자
#2021년데이터 2022년 데이터를 합침
totalcovid <- rbind(covidAll2021total,covidAll2022total) ## 국가별 총 사망자, 총 확진자
totalcovid

allR <- totalcovid %>% summarise(allDeath = sum(Deaths), allConfirmed = sum(Confirmed), 
                            dayDeath = sum(Deaths)/365, dayConfirmed = sum(Confirmed)/365)
allR %>% View()

# 문제 (1-1) 일별 데이터 활용 -> 전체 국가 : 사망자/확인자/일별 사망자/일별 확인자
#2021+2022 국가별로 사망자/확진자 각각 더하기
covidtotal <- aggregate(totalcovid[,c('Deaths','Confirmed')], by=list(totalcovid$Country_Region), FUN=sum) 
names(covidtotal) <- c("Country_Region","Deaths","Confirmed")
covidtotal

# 국가별 평균 사망자/평균 확진자
covidtotal$dayDeath <- covidtotal$Deaths/365 ##평균 구하기
covidtotal$dayConfirmed <- covidtotal$Confirmed/365 ##평균 구하기
covidtotal


######################################################################################

# 문제 (1-1) 누적 데이터 활용 -> 전체 국가 : 사망자/확인자/일별 사망자/일별 확인자

#연도별로 변수 분리
covid21 <- covidAll2021 %>% filter(date == "07-31-2021.csv")
covid21
covid22 <- covidAll2022 %>% filter(date == "07-31-2022.csv") 
covid22 <- subset(covid22, select=c(Country_Region, Deaths, Confirmed))
covid22


#21년 데이터에 -값
library(dplyr)
micovid21 <- covid21 %>% summarise(miDeath = -Deaths, miConfirmed = -Confirmed)
micovid21

#21년 데이터에 -한 값을 컬럼 추가해서 붙이기 & -한 값만 남기기
setcovid21 <- cbind(covid21,micovid21)
setcovid21 <- subset(setcovid21, select=c(Country_Region, miDeath, miConfirmed))
names(setcovid21) <- c("Country_Region","Deaths","Confirmed")
setcovid21

length(setcovid21)

# 22년 데이터 - 21년 데이터
library(stringr)
total <- rbind(covid22,setcovid21)
total <- aggregate(total[,c('Deaths','Confirmed')], by=list(total$Country_Region), FUN=sum)  # 국가별 사망자수/검사자수
total


library(dplyr)
global <- total %>% summarise(allDeath = sum(Deaths), allConfirmed = sum(Confirmed), 
                              dayDeath = sum(Deaths)/365, dayConfirmed = sum(Confirmed)/365)
global
global %>% View()

# 문제 (1-1) 누적 데이터 활용 -> 개별별 국가 : 사망자/확인자/일별 사망자/일별 확인자
each <- total %>% mutate(total, dayDeath = round(Deaths/365,2), dayConfirmed = round(Confirmed/365,2))
names(each) <- c("Country_Region","Deaths","Confirmed","dayDeath","dayConfirmed")
each%>% View()


###########################################################################
# 문제 (2) 데이터 전처리 : 0인경우, NA인 경우 확인

deathzero <-covidtotal %>% filter(Deaths == 0)
confirmzero <- covidtotal %>% filter(Confirmed == 0)
deathzero
confirmzero

#데이터 없는 국가 없음
table(is.na(covid2021$Confirmed))
table(is.na(covid2022$Confirmed)) 




####################################################
# 문제 (3) 상위 20개 국가. 각각 내림차순으로
Confirmdesc <- covidtotal %>% arrange(desc(Confirmed))
Deathdesc <- covidtotal %>% arrange(desc(Deaths))
Confirmdaydesc <- covidtotal %>% arrange(desc(dayConfirmed))
Deathdaydesc <- covidtotal %>% arrange(desc(dayDeath)) 

head(Confirmdesc,20) %>% View()
head(Deathdesc,20)%>% View()
head(Confirmdaydesc,20)%>% View()
head(Deathdaydesc,20)%>% View()

