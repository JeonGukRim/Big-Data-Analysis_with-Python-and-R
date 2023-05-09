
############################# 1. 막대차트(가로, 세로) ##########################

# barplot 데이터셋(세로)
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", 
                       "2018 3분기", "2019 3분기", 
                       "2018 4분기", "2019 4분기")
chart_data
# barplot 세로막대그래프
barplot(chart_data, ylim = c(0, 600),
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황",
        col = rainbow(8),
        main = "2018년도 vs 2019년도 매출현항 비교")
legend(7.8, 210, c("2018 1분기", "2018 2분기", "2018 3분기", "2018 4분기",
                   "2019 1분기", "2019 2분기", "2019 3분기", "2019 4분기"),
       cex = 0.7, fill = rainbow(8))
# legend : 범례

# barplot 데이터셋(가로)
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", 
                       "2018 3분기", "2019 3분기", 
                       "2018 4분기", "2019 4분기")
chart_data
# barplot 가로막대그래프
barplot(chart_data, xlim = c(0, 600), horiz = T, 
        ylab = "매출액(단위: 만원)",
        xlab = "년도별 분기 현황", 
        col = rainbow(8), space = 0.5, cex.names = 0.8,
        main = "2018년도 vs 2019년도 매출현항 비교")
# horiz : 수평, 가로 막대 표현 여부 T = 가로, default : 세로
# space속성 : 막대의 굵기와 간격 지정
# cex.names 속성 : 축 이름의 크기 지정

# ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

library(ggplot2)
# ggplot2 데이터셋(세로)
value <- c(305, 450, 320, 460, 330, 480, 380, 520)
quarter <- c("2018 1분기", "2019 1분기",
             "2018 2분기", "2019 2분기", 
             "2018 3분기", "2019 3분기", 
             "2018 4분기", "2019 4분기")
chart_data2 <- data.frame(quarter = quarter, value = value)
# ggplot2 세로막대그래프
ggplot(chart_data2) +
  aes(x = quarter, weight = value, fill = quarter) +
  geom_bar()


# ggplot2 데이터셋(가로)
value <- c(305, 450, 320, 460, 330, 480, 380, 520)
quarter <- c("2018 1분기", "2019 1분기",
             "2018 2분기", "2019 2분기", 
             "2018 3분기", "2019 3분기", 
             "2018 4분기", "2019 4분기")
chart_data2 <- data.frame(quarter = quarter, value = value)
# ggplot2 가로막대그래프
ggplot(chart_data2) +
  aes(x = quarter, weight = value, fill = quarter) +
  geom_bar() +
  coord_flip()

############################### 2. 누적막대차트 ################################

# 데이터 가져오기
data("VADeaths")
VADeaths

# barplot 개별 / 누적 막대그래프
barplot(VADeaths, beside = T, col = rainbow(5),
        main = "미국 버지니아주 하위계층 사망비율")
legend(15, 71, c("50-54", "55-59", "60-64", "65-69", "70-74"),
       cex = 0.8, fill = rainbow(5))

barplot(VADeaths, beside = F, col = rainbow(5))
title(main = "미국 버지니아주 하위계층 사망비율", font.main = 4)
legend(3.8, 200, c("50-54", "55-59", "60-64", "65-69", "70-74"),
       cex = 0.8, fill = rainbow(5))

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# ggplot 누적 막대그래프
VADeaths_df <- as.data.frame.table(VADeaths)
ggplot(VADeaths_df, aes(x=Var2, y=Freq, fill = Var1))+
  geom_bar(stat = 'identity')

############################### 3. 점 차트 #####################################

# dotchart 데이터셋
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", 
                       "2018 3분기", "2019 3분기", 
                       "2018 4분기", "2019 4분기")
# 점 차트
dotchart(chart_data, color = c("blue", "red"),
         lcolor = "black", pch = 1:2,
         labels = names(chart_data),
         xlab = "매출액", 
         main = "분기별 판매현황: 점차트 시각화",
         cex = 1.2)
# col : 레이블과 점 색상 지정
# lcolor : 구분선 색상 지정
# pch : 점 모양
# labels : 점에 대한 레이블 표시
# cex : 확대

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# ggplot2 데이터셋
value <- c(305, 450, 320, 460, 330, 480, 380, 520)
quarter <- c("2018 1분기", "2019 1분기",
             "2018 2분기", "2019 2분기", 
             "2018 3분기", "2019 3분기", 
             "2018 4분기", "2019 4분기")
chart_data2 <- data.frame(quarter = quarter, value = value)
ggplot(chart_data2, aes(value, quarter, colour = value)) +
  geom_point()+
  theme_linedraw()

############################### 4. 원형 차트 ###################################

# pie 데이터셋
chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c("2018 1분기", "2019 1분기",
                       "2018 2분기", "2019 2분기", 
                       "2018 3분기", "2019 3분기", 
                       "2018 4분기", "2019 4분기")

# pie 원형 차트
pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2,
    border = 2, lty = 2)
title("2018~2019년도 분기별 매출현황")
# border : 테두리색
# lty : 점선 종류

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# ggplot2 데이터셋
value <- c(305, 450, 320, 460, 330, 480, 380, 520)
quarter <- c("2018 1분기", "2019 1분기",
             "2018 2분기", "2019 2분기", 
             "2018 3분기", "2019 3분기", 
             "2018 4분기", "2019 4분기")
chart_data2 <- data.frame(quarter = quarter, value = value)

# ggplot2 원형 차트
circle <- ggplot(chart_data2) +
  aes(x='', weight = value, fill = quarter) + 
  geom_bar()
circle + coord_polar('y')

############################## 5. 상자 그래프 ##################################
par(mfrow = c(1, 2))

# boxplot 데이터셋
data("VADeaths")
VADeaths

# boxplot 상자 그래프
boxplot(VADeaths, range = 0)

# notch = TRUE
boxplot(VADeaths, range = 0, notch = T)
abline(h = 37, lty = 3, col = "red")
# h : 선 그을 위치
# notch = T : 중위수 기준 허리선 추가

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# ggplot2 데이터셋
data("VADeaths")
VADeaths

# ggplot2 상자 그래프
VADeaths_df <- as.data.frame.table(VADeaths)
box <- ggplot(VADeaths_df, aes(x = Var2, y = Freq))
box <- box + geom_boxplot(notch = F) +
  geom_abline(intercept = 37, slope = 0, color = 2, linetype = 'dashed')

box

VADeaths_df <- as.data.frame.table(VADeaths)
box <- ggplot(VADeaths_df, aes(x = Var2, y = Freq))
box <- box + geom_boxplot(notch = T) +
  geom_abline(intercept = 37, slope = 0, color = 2, linetype = 'dashed')

box


############################## 6. 히스토그램 ###################################
# hist 데이터셋
data(iris)

# 빈도수에 의해서 히스토그램 그리기
par(mfrow = c(1, 2))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", 
     col = "green", 
     main = "iris 꽃받침 너비 Histogram: 빈도수", xlim = c(2.0, 4.5))

# 확률 밀도에 의해서 히스토그램 그리기
hist(iris$Sepal.Width, xlab = "iris.$Sepal.Width", 
     col = "mistyrose", freq = F, 
     main = "iris 꽃받침 너비 Histogram: 확률 밀도", xlim = c(2.0, 4.5))

# 밀도를 기준으로 line 추가하기
lines(density(iris$Sepal.Width), col = "red")

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#
par(mfrow = c(1, 1))

# ggplot2 데이터셋
data(iris)

# ggplot2 히스토그램
hist2 <- ggplot(data = iris, aes(x = Sepal.Width))
hist2 + geom_histogram(fill = 7, color = 2)
# fill : 막대 색 채우기
# color : 막대 테두리 색

############################## 7. 산점도 #######################################
# xyplot 데이터셋
library(datasets)
data("airquality")

# xyplot 산점도
xyplot(Ozone ~ Wind | Month, data = airquality, layout=c(5,1))
# y축 ~ x축 컬럼 | 조건, 사용할 데이터, 레이아웃

# 2개 변수를 y축에 표시
xyplot(Ozone + Solar.R ~ Wind | factor(Month), 
       data = airquality, 
       col = c("black", "red"),
       layout = c(5, 1))

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# ggplot2 데이터셋
data("airquality")

# ggplot2 산점도
air <- ggplot(data = airquality, aes(x = Wind, y = Ozone, color = Month)) 
air <- air+ geom_point(shape=1, size=4) 
air
# shape : 점 모양
# size : 점 크기

# y축 변수 2개 합
air <- ggplot(data = airquality, aes(x = Wind, y = Ozone + Solar.R, color = Month)) 
air <- air+ geom_point(shape = 1, size = 3)
air

########################### 8. 중첩 자료 시각화 ################################

# 1. galton 데이터셋 가져오기
#install.packages("UsingR", type = 'binary')
#install.packages("latticeExtra")

# plot 데이터셋
library(UsingR)
data(galton)
galtonData <- as.data.frame(table(galton$child, galton$parent))

# plot 중첩 자료 시각화
names(galtonData) = c("child", "parent", "freq")
parent <- as.numeric(galtonData$parent)
child <- as.numeric(galtonData$child)

plot(parent, child, 
     pch = 21, col = "red", bg = "yellow", 
     cex = 0.2 * galtonData$freq, 
     xlab = "parent", ylab = "child")
# col : 도형 테두리 색
# bg : 도형 내부 색

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# ggplot2 데이터셋
data(galton)
galtonData <- as.data.frame(table(galton$child, galton$parent))

# ggplot2 중첩 자료 시각화
names(galtonData) = c("child", "parent", "freq")
galton2 <- ggplot(data = galtonData)
galton2 + aes(x = parent, y = child, colour = freq) + 
  geom_point(mapping = aes(size = 0.3 * freq)) +
  scale_color_gradient() + theme_minimal()

######################## 9. 변수 간의 비교 시각화 ##############################

# pairs 데이터셋
data(iris)

# pairs 변수 간의 비교 시각화
pairs(iris[iris$Species == "virginica", 1:4])
pairs(iris[iris$Species == "setosa", 1:4])
pairs(iris[iris$Species == "versicolor", 1:4])
# pairs() : numeric 컬럼 대상 변수들 사이
# 비교 결과를 행렬구조의 분산된 그래프로 제공

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# ggplot2 데이터셋
library(ggplot2)
data(iris)

# ggplot2 변수 간의 비교 시각화
ggplot() +
  geom_point(mapping = aes(x = Sepal.Length, y = Sepal.Width, 
                           color = Species, shape = Species), data=iris)

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# 3차원 산점도 시각화
#install.packages("scatterplot3d")
library(scatterplot3d)

iris_setosa = iris[iris$Species == 'setosa', ]
iris_versicolor = iris[iris$Species == 'versicolor', ]
iris_virginica = iris[iris$Species == 'virginica', ]

d3 <- scatterplot3d(iris$Petal.Length, 
                    iris$Sepal.Length,
                    iris$Sepal.Width, 
                    type = 'n')

d3$points3d(iris_setosa$Petal.Length,
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, 
            bg = 'orange', pch = 21)
d3$points3d(iris_versicolor$Petal.Length, 
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'blue', pch = 23)
d3$points3d(iris_virginica$Petal.Length, 
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, 
            bg = 'green', pch = 25)


############################## 10. 밀도 그래프 #################################

#install.packages("lattice")
#install.packages("mlmRev")

# densityplot 데이터셋
library(lattice)
library(mlmRev)
data("Chem97")

# densityplot 밀도 그래프
densityplot(~gcsescore | factor(score), data = Chem97,
            groups = gender, plot.Points = T, 
            auto.key = T)

# ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

# ggplot2 데이터셋
library(lattice)
library(mlmRev)
data("Chem97")

# score 기준 데이터 분리
chem97_0<-subset(Chem97, score == 0)
chem97_2<-subset(Chem97, score == 2)
chem97_4<-subset(Chem97, score == 4)
chem97_6<-subset(Chem97, score == 6)
chem97_8<-subset(Chem97, score == 8)
chem97_10<-subset(Chem97, score == 10)

# ggplot2 밀도 그래프(점수 / 성별)
ggplot(data = chem97_0) +  geom_density(mapping = aes(x = gcsescore, colour = gender))
ggplot(data = chem97_2) +  geom_density(mapping = aes(x = gcsescore, colour = gender))
ggplot(data = chem97_4) +  geom_density(mapping = aes(x = gcsescore, colour = gender))
ggplot(data = chem97_6) +  geom_density(mapping = aes(x = gcsescore, colour = gender))
ggplot(data = chem97_8) +  geom_density(mapping = aes(x = gcsescore, colour = gender))
ggplot(data = chem97_10) +  geom_density(mapping = aes(x = gcsescore, colour = gender))
