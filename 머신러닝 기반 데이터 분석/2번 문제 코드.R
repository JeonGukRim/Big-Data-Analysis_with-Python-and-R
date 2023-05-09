# 문제2) mlbench패키지 내 BostonHousing 데이터셋을 대상으로
#        예측기법 2개를 적용하여 기법별 결과를 비교하시오.
#        (종속변수는 MEDV 또는 CMEDV를 사용)

# BostonHousing : 1970년 506명을 대상으로 한 인구 조사에서, 보스턴 지역에 대한 주택 데이터

######################## 적용기법 1) 다중선형회귀 ##############################

## 1. library 호출 ##
#install.packages("mlbench")
library(mlbench)
library(dplyr)

# 데이터 불러오기
data(BostonHousing)
head(BostonHousing)

# BostonHousing 데이터셋 정보 확인
?BostonHousing

# BostonHousing 데이터셋 변수에 담기
bostonData <- BostonHousing

# 기술통계량 확인
summary(BostonHousing)

# 데이터 구성 확인
dim(BostonHousing) # 506명 대상, 14개 수치형 변수

# 14개 변수 설명 
# crim : 도시별 1인당 범죄율
# zn : 25,000 평방 피트 이상의 부지에 대한 주거 토지 의 비율
# indus : 도시당 비소매 업종의 비율
# chas : 찰스강에 대한 더미변수(강의 경계에 위치 1, 아니면 0)
# nox : 10ppm 당 일산화질소 농도
# rm : 주택 1가구당 평균 방의 개수
# age : 1940년 이전에 건축된 소유주택의 비율
# dis : 5개의 보스턴 직업센터까지의 접근성 지수
# rad : 방사형 고속도로 접근성 지수
# tax	: 10,000 달러 당 재산세율
# ptratio : 마을별 학생 / 교사 비율
# b	: 1000(Bk-0.63)^2, Bk : 자치시별 흑인의 비율
# lstat :	모집단의 하위계층의 비율(%)
# medv : 본인 소유의 주택가격(중앙값) / 종속변수 (단위: $1,000)

## 2. 다중회귀분석 실시 및 다중공선성 확인 ##

#install.packages("car")
library(car)

result.lm <- lm(formula = medv~., data = bostonData)
result.lm
summary(result.lm)

vif(result.lm) # 모두 10 이하로 문제 없음

# 다중회귀분석 결과
summary(result.lm)

## 3. 샘플링 ##
set.seed(1234)
idx <- sample(1:nrow(bostonData), nrow(bostonData) * 0.7)
bo_tr <- bostonData[idx, ]
dim(bo_tr) # 506개 * 0.7 = 354개 확인

bo_ts <- bostonData[-idx, ]
dim(bo_ts) # 506개 * 0.3 = 152개 확인

## 4. 다중회귀분석 ##
result.lm <- lm(formula = medv~., data = bo_tr)
result.lm
summary(result.lm) # Adjusted R-squared(수정 결정계수) :  0.7413 
                   # indus, age p-value 0.05보다 큰 것 확인(유의하지않다.)

# 변수 선택(후진 제거법) - 변수가 많을수록 시간적으로 효율적
step <- step(result.lm, direction = 'backward')
formula(step) # AIC가 가장 낮은 indus, age 제거 확인
summary(step) 

# indus, age 변수 제거 후 다시 다중회귀분석 실시
result2.lm <- lm(formula = step, data = bo_tr)
result2.lm
summary(result2.lm) # Adjusted R-squared(수정 결정계수) : 0.7419 / 유의미한 차이없음
# Estimate 확인 시 nox, rm, chas, dis, ptratio가 높은 상관성을 가지는것을 확인할수있다.

## 5. 예측 평가 ##
pred <- predict(result2.lm, bo_ts)
cor(pred, bo_ts$medv) # 예측값과 실제값 상관도 : 0.8349967

## 6. 시각화 ##
library(PerformanceAnalytics)

# 시각화 1) PerformanceAnalytics 사용
# 높은 상관성있는 변수 위주로 설정
sd <- select_at(bo_ts, vars(medv,rm,lstat,dis,ptratio,nox))
chart.Correlation(sd,histogram=T,pch=30)
# -1<= 상관계수 <= 1 에 가까울수록 상관 관계가 높음
# Estimate가 가장 높았던 nox의 상관계수는 -0.43으로 rm, chas의 상관계수 보다 낮아
# 다중회귀분석 결과와 완전히 일치하지는 않지만 높은 상관관계가 있다는 것은 동일한것을 알 수 있다.

# 시각화 2) ggplot 사용
bind <- cbind(pred, bo_ts$medv)
head(bind)
colnames(bind) <- c('pred', 'medv')
df <- as.data.frame(bind)

# 예측치와 실제값에 대한 산점도, 회귀선 시각화
library(ggplot2)
ggplot(df, aes(pred, medv)) + 
      geom_point() + stat_smooth(method = lm)

## 7. 모델 평가 ##
#install.packages("Metrics")
library(Metrics)
# RMSE : 평균 제곱근 오차(낮을수록 오차가 적다.)
rmse(bo_ts$medv, pred) # 5.128862

######################## 적용기법 2) 랜덤포레스트 ##############################

## 1. library 호출 ##
#install.packages("randomForest")
library(randomForest)

## 2. 랜덤포레스트 모델 생성 ##
RFmodel100 <- randomForest(medv ~., data = bo_tr, ntree = 100, proximity=T)
RFmodel100　

RFmodel300 <- randomForest(medv ~., data = bo_tr, ntree = 300, proximity=T)
RFmodel300

RFmodel50 <- randomForest(medv ~., data = bo_tr, ntree = 50, proximity=T)
RFmodel50

RFmodel10 <- randomForest(medv ~., data = bo_tr, ntree = 10, proximity=T)
RFmodel10

## 3. 시각화 ##
plot(RFmodel100, main = "BostonHousing(RandomForest Model)")
# tree 개수가 늘어 날수록 Error 줄어듬을 알 수 있다.
plot(RFmodel300, main = "BostonHousing(RandomForest Model)")

plot(RFmodel50, main = "BostonHousing(RandomForest Model)")

plot(RFmodel10, main = "BostonHousing(RandomForest Model)")

## 4. 중요 변수 확인 ##
importance(RFmodel100)
importance(RFmodel300)
importance(RFmodel50)
importance(RFmodel10)

## 5. 중요한 것 시각화 ##
varImpPlot(RFmodel100)
# Purity(순수도) 확인 시 rm, lstat의 순수도가 높아 높은 상관성을 가지는것을 예측 할 수 있다.
#                        nox, dis, ptratio, crim, indus도 약간의 상관성을 가지는 것을 예측할수있다.

varImpPlot(RFmodel300)
varImpPlot(RFmodel50)
varImpPlot(RFmodel10)

# tree수에 따라 중요 변수가 달라지는 것을 확인할 수 있다.

## 6. 예측 평가 ##
pred100 <- predict(RFmodel100, newdata=bo_ts)
cor(pred100, bo_ts$medv) # 예측값과 실제값 상관도 : 0.94592

pred300 <- predict(RFmodel300, newdata=bo_ts)
cor(pred300, bo_ts$medv) # 예측값과 실제값 상관도 : 0.9457515

pred50 <- predict(RFmodel50, newdata=bo_ts)
cor(pred50, bo_ts$medv) # 예측값과 실제값 상관도 : 0.9479699

pred10 <- predict(RFmodel10, newdata=bo_ts)
cor(pred50, bo_ts$medv) # 예측값과 실제값 상관도 : 0.9419845

# tree수가 달라져도 예측값과 실제값 상관도는 크게 변함은 없다.

## 7. 모델 평가 ##
rmse(bo_ts$medv, pred100) # 3.213019
rmse(bo_ts$medv, pred300) # 3.225541
rmse(bo_ts$medv, pred50) # 3.125153
rmse(bo_ts$medv, pred10) # 3.476369
# tree 수 100 -> 300 으로 변경시 rmse가 높아지고 
#         100 -> 50 으로 변경시 rmse가 낮아짐을 확인했다.
#         하지만 50 -> 10으로 변경시 rmse가 다시 높아지는것으로 보아 적절한 tree수를 찾는게 
#         오차를 줄이는 관건으로 보인다.

### 기법 결과 비교 ###

# rm(방의 개수)[상관계수 0.7, 계수 4.8]
# 방의 개수는 주택 가격과 상관성이 높다는 것을 알 수 있고, 방의 개수가 많을수록 주택 가격 높다는 것을 예측할수있다.

# lstat(하위계층비율)[상관계수 -0.74, 계수 -0.36]
# 하위계층비율과 주택 가격이 상관성이 높다는 것을 알 수 있고, 하위계층비율이 높을수록 주택 가격이 낮다는 것을 예측할수있다.

# dis(직업센터 접근성 지수)[상관계수 0.25, 계수 -1.3] 
# 상관계수가 0.25이므로 주택 가격과는 큰 상관이 없다는 것을 알 수 있다.

# ptratio(학생/교사 비율)[상관계수 -0.51, 계수 -0.85]
# 학생/교사 비율은 주택 가격과 상관성이 있다는 것을 알 수 있고,
# 교사의 수에 비해 학생이 너무 많으면 주택 가격이 낮다는 것을 예측 할 수 있다.

# nox(일산화질소 농도)[상관계수 -0.43, 계수 -18.92]
# 일산화질소 농도와 주택 가격의 상관성이 약간 있지만 계수를 보면 농도가 높을수록
# 주택 가격은 아주 많이 낮아질수 있다는 것을 예측 할 수 있다.

# 두 기법 모두 상관성이 높은 변수를 비슷하게 분류해 내었지만 약간의 차이가 있었고,
# 다중선형회귀 분석 결과 / 상관도 : 0.8349967, RMSE(오차) : 5.128862
# 랜덤포레스트 분석 결과 / 상관도 : 0.9458575, RMSE(오차) : 3.217042
# 랜덤포레스트 기법이 예측값과 실제값의 상관도가 더 높고, 
# RMSE가 더 적은것으로 보아 다중선형회귀 분석보다 향상된 결과를 얻을수 있다고 볼 수 있다.