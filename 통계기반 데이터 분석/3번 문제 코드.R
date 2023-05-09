
# 3. mtcars 데이터에서 엔진(vs)을 종속변수로, 연비(mpg)와 변속기종류(am)를 독립변수로
# 설정하여 로지스틱 회귀분석을 실시하시오

# 엔진(vs)을 종속변수로, 연비(mpg)와 변속기종류(am)를 독립변수로 설정하는 식 = fx
fx <- vs ~ mpg+am

# 데이터 분석
# am : 변속기 종류 : (0=automatic:자동, 1=manual:수동)
# vs :엔진 종류 (0 = V-shaped, 1 = straight)

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

#########################3-1번##################
# rm(list=ls())
# 데이터 가져오기
data(mtcars)
?mtcars
#mtcars_df 담아서 데이터 확인하기
mtcars_df <- mtcars
head(mtcars_df)
summary(mtcars_df)
dim(mtcars_df)  # 차종 32개, 차구성 11개



#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#


#3-2번) 로지스틱 회귀분석 실행하고 회귀모델 확인 ################
mtcars_df
#glm : 로지스틱 회귀분석 실시
# family=binomial로 설정해주면 이항 로지스틱 분류기로 데이터에 적합
mtcars_model <- glm(fx, data = mtcars_df,family = 'binomial', na.action=na.omit)
mtcars_model
summary(mtcars_model)

#mpg(연비)의 회귀계수가 0.6809이기 때문에 mpg가 한 단위 증가하면 vs=1일 오즈가 exp(0.6809) ≒1.98(98%) 증가
# am(변속기)의 회귀계수는 -3.0073임으로 am가 한 단위 증가하면(자동 -> 수동) vs=1일 오즈가 exp(-3.0073) ≒ 0.05, 
#즉 변속기가 수동인 경우 자동에 비해 vs=1인 오즈가 95% 감소.




#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

#3-3번) 로지스틱 회귀모델 요약정보 확인 ###################
# 유의한 변수들만 선택하기

# 1) 회귀식 생성하기
func <- glm(fx, data = mtcars_df,family = 'binomial', na.action=na.omit)
# 2) 후진제거법을 활용한 변수 선택
selectCar <- step(func, direction = "backward")
# am과 mpg의 AIC가 모두 기준 AIC보다 적으므로 두 변수 모두 최적의 예측변수이다.

# 더 나아가 모든 변수가 유의한지 확인하기 위해 이탈도를 확인
anova(func, test="Chisq")
# mpg와 am 모두 Pr(>Chi)가 0.05이하임으로 통계적으로 유의


# predict (object, newdata, interval = c ("none", "confidence", "prediction"))
#type=“response"를 넣어야 우리가 원하는 ‘확률’의 꼴로 반환
# predict() : 새로운 데이터를 input 시켜 새로운 데이터에 대한 예측된 값
pred <- predict(mtcars_model, newdata = mtcars_df, type = "response")
pred


# 회귀분석을 한 데이터 = mtcars_model
# 컷오프를 0.5로 단순 가정하여 분류값을 생성한다. 0.5이상일 경우 1/이하일 경우 0으로 반환한다.
result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred
table(result_pred)


#테이블에서 vs가 올바로 예측된경우는 25개 틀리게 예측된경우는 7개로 
# 약 78.125퍼센트의 신뢰 모델을 가지고있다고 평가할 수 있다. (25/32*100)
table(result_pred, mtcars_df$vs) #예측 성능이 얼마나 좋은지를 확인
#분류 정확도 : 대각석의 합/전체 합
(15+10) / nrow(mtcars_df)








# install.packages("ROCR")
library(car)
library(lmtest)
library(ROCR)
pr <- prediction(pred, mtcars_df$vs)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf )




#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

#3-4번) 로지스틱 회귀식 (회귀 방정식) ###################
# 3-2)mtcars_model에서 구한 절편과 기울기 사용하여 회귀 방정식 만들기
 vs = -12.7051 + (0.6809*mpg) + (-3.0073*am)



