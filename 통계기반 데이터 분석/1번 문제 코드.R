# 다음 사항을 적용하여 다중회귀분석을 실시하시오.
# R의 내장 데이터셋인 state data sets 내 stat.x77 데이터셋 사용
options(scipen=999)
#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

## (1) state 데이터셋을 load하고, state.x77 dataset을 데이터프레임으로 변환하고,
##     Life Exp 변수를 Life.Exp로 HS Grad변수를 HS.Grad로 변경하시오.

# (1)-1 state dataset load
data(state) # -> state : 미국 1970년대 50개 주에 대한 데이터셋
?state  #데이터 셋에 대한 정보를 확인

# (1)-2 state.x77 dataset -> data frame으로 변환 
df_state <- as.data.frame(state.x77)
df_state

# (1)-3 Life Exp 변수 -> Life.Exp 변경
colnames(df_state)[colnames(df_state)=="Life Exp"] <- "Life.Exp"
df_state

# (1)-4 HS Grad 변수 -> HS.Grad 변경
colnames(df_state)[colnames(df_state)=="HS Grad"] <- "HS.Grad"
df_state

# 기술통계량 확인용
summary(df_state)

# 데이터 구성 확인용
dim(df_state) # 50개 주 / 8개 변수



#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#
## (2) Life Expectancy 변수를 종속변수로 설정하고 나머지 변수를 독립변수로 
##     설정하여 회귀분석을 실시하시오. 실시 후 결과에 대해 해석하시오.
# cf. 인구수 내림차순해서 인터넷에서 미국 1970년 인구수와 비교 결과
# Population : 실제 인구수 나누기 1000 되어있음을 알수있음
library(dplyr)
df_state
Populationdesc <- df_state %>% arrange(desc(Population))
Populationdesc


# 독립변수(x), 종속변수(y) 생성
y = df_state$Life.Exp    # 기대수명

x1 = df_state$Population # 미국 주 인구수(1975년 1월)
x2 = df_state$Income     # 인당 소득(1974년)
x3 = df_state$Illiteracy # 문맹률(1970년) 
x4 = df_state$Murder     # 인구 100,000명당 살인율(1976년)
x5 = df_state$HS.Grad    # 고등학교 졸업자 비율(1970년)
x6 = df_state$Frost      # 최저 기온이 영하 미만 평균 일수(1931~1960년)
x7 = df_state$Area       # 평방 마일의 토지 면적


# (2)-1 회귀 분석 실시 / df_state 변수 전부 담기 / 생성한 변수(x1~x7) 이용 안함.
result.lm <- lm(Life.Exp ~., data = df_state)
result.lm

summary(result.lm)



#-- 다중 공선성 문제 확인 --#
#다중공선성(Multicollinearity): 입력(독립)변수들 간의 상관정도가 높은 상태
# 다중공선성 데이터를 <회귀분석>에 적용하는 경우 어떤 문제가 발생할까? 
#   분석 결과인 회귀 계수가 불안정해지는 것이다. 회귀계수가 해당 변수의 종속변수에 미치는 영향력을 올바로 설명하지 못하게 된다. 즉, 다중공선성을 고려하지 않고 회귀분석을 수행한 후 그 결과를 해석하면 잘못된 결론(변수의 중요성을 설명할 때)을 내리게 되는 문제가 발생한다.  
# 
# 다중공선성 데이터를 <결정트리>에 적용하는 경우 어떤 문제가 발생할까?
#   분류에 중요한 영향을 미치는 변수가 결정트리의 분리 조건에 나타나지 않게 되는 문제가 발생한다.
#   또한 중요변수가 사용되지 않음으로 인해서 분류율(정확도)가 낮아지게 되는 문제가 있다.

# 패키지 설치
# install.packages("car")
library(car)

# 분산팽창요인(VIF)
# VIF >= 10 이상인 경우 다중 공선성 문제 의심
vif(result.lm) # 전부 1~4 사이로 문제 없음






# (2)-2 결과 해석
# Intercept (절편): 70.94, 즉 모든 변수가 0일 때의 평균 수명은 70.94살이다.
#Population(인구수) : p-value 값이 유의계수 0.05 보다 커 통계가 유의하지 않다.
#Income(인당 소득) : p-value 값이 유의계수 0.05 보다 커 통계가 유의하지 않다.
#Illiteracy(문맹률) : p-value 값이 0.9269로 유의계수 0.05 보다 커 통계가 유의하지 않다.

#Murder(인구 100,000명당 살인율) : : p-value 값이 0.05 보다 작아 통계가 유의하다.
# -> 살인율의 계수가 -0.3011으로 살인율이 1 증가할때 마다 기대수명이 -0.3 줄어듬을 예측할수있다.

#HS.Grad(고졸 비율) : 0.04893, p-value 값이 0.05 보다 작아 유의하다.
# -> 고졸 비율 계수가 0.048로 고졸비율이 1 증가할때 마다 기대수명이 0.04893 늘어남을 예측할수있다.

#Frost(최저 기온이 영하 미만인 평균 일수) : 0.0752, p-value 값이 0.05 보다 커 유의하지 않다.
#Area(평방 마일의 토지 면적) : p-value 값이 0.05 보다 커 유의하지 않다.





#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

## (3). (2)번 회귀모형에서 Income, Illiteracy, Area 변수를 제외하고 회귀분석을 실시하고 결과 해석.

# (3)-1 회귀 분석 실시 / df_state 변수들 중 Income : x2, Illiteracy : x3, Area : x7 변수를 제외/ 
func3 <- Life.Exp ~Population+Murder+HS.Grad+Frost
result2.lm <- lm(func3, data = df_state)

result2.lm
summary(result2.lm)
vif(result2.lm) # 공선성 1~2 사이로 문제없음


# (3)-2 결과 해석
# Intercept (절편): 71.03, 즉 모든 변수가 0일 때의 평균 수명은 71.03살이다.
#Population(인구수) : p-value 값이 0.05201므로  유의계수 0.05보다 큼 통계가 유의하지 않다.

#Murder(인구 100,000명당 살인율) : : p-value 값이 0.05 보다 많이 작아 유의하다.
# -> 살인율의 계수가 -0.3001으로 살인율이 1 증가할때 마다 기대수명이 -0.3001 줄어듬을 예측할수있다.

#HS.Grad(고졸 비율) : 0.00297, p-value 값이 0.05 보다많이 작아 유의하다.
# -> 고졸 비율 계수가 0.046로 고졸비율이 1 증가할때 마다 기대수명이 0.046 늘어남을 예측할수있다.

#Frost(최저 기온이 영하 미만인 평균 일수) : 0.01802 , p-value 값이 0.05 보다 작아 유의하다.
# -> 영하인 일수가 많을 수록 기대수명이 미세하게 줄어듬을 예측할수있다.


#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#




## (4) Life Expectancy 변수를 종속변수로 설정하고 HS.Grad와 Murder 변수를
##     예측변수(predictor variable)로 설정하여 회귀분석을 실시하시오

# df_state 변수들 중 Murder : x4, HS.Grad : x5 변수 담기
func4 <- Life.Exp ~HS.Grad+Murder
result3.lm <- lm(func4, data = df_state)
result3.lm
vif(result3.lm) # 공선성 1~2 사이로 문제없음

#해석
# Intercept (절편): 70.29708, 즉 모든 변수가 0일 때의 평균 수명은 70.29708살이다.
# HS.Grad (고졸): 0.04389, 고등학교 졸업율이 증가할수록 평균 수명은 0.04389 증가한다.
# Murder (살인 발생률): -0.23709, 살인 발생률이 증가할수록 평균 수명은 -0.23709 감소한다.

#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#

## (5) 전 인구의 55%가 고졸이고 살인비율이 10만명당 8명일 때 Life Expectancy 결과값을 예측하시오.

# result3.lm에서 구한 절편과 기울기 사용하여 회귀 방정식 만들기
# 회귀 방정식 Y = 70.29708 -0.23709*(x4 : Murder) + 0.04389*(x5 : HS.Grad)
# Murder : 살인비율 -> 8.0 대입, HS.Grad : 고졸비율 -> 55.0 대입
Y = 70.29708 + (-0.23709*8.0) + (0.04389*55.0)
Y               # Y = 70.81431


#ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ#
#6-1 (4)번에서 처럼 2개의 독립변수, 1개의 종속변수의 데이터와 fit된 회귀평면(fitted
# regression plane)을 3D 그래프로 시각화하시오.


# 1) 회귀식 생성하기
result7.lm <- lm(Life.Exp ~., data = df_state)
fun <- lm(result7.lm, data = df_state,family = 'binomial', na.action=na.omit)
# 2) 후진제거법을 활용한 변수 선택
select <- step(fun, direction = "backward")


#패키지없으면 설치
# install.packages("scatterplot3d")
library(scatterplot3d)
x <- df_state$HS.Grad
y <- df_state$Murder
z <- df_state$Life.Exp

# 회귀 평면 구하기
fit <- lm(z ~ x + y)

# 3D scatter plot 생성
s3d <- scatterplot3d(x, y, z, type="h", main="3D Scatterplot")

# 회귀 평면 추가
s3d$plane3d(fit)


################# 3d그래프 추가가
# install.packages("plot3D")
# install.packages("plot3Drgl",type="binary")
# install.packages("rgl",type="binary")
# plot3Drgl 패키지 불러오기
library(plot3Drgl)
library(rgl)
library(plot3D)
# 데이터 생성
x1 <- df_state$HS.Grad
x2 <- df_state$Murder
y1 <- df_state$Life.Exp
# 회귀분석 모델 적합
fit <- lm(y1 ~ x1 + x2)
# 회귀분석 평면 시각화
#산포도시각화
rainbowcolor <- rainbow(93) #레인보우 색상 랜덤
plot3d(x = x1, y = x2, z = y1, type = "p",size = 13,  col = rainbowcolor ,xlab = "High", ylab = "Mu", zlab = "LIFE", main = "3D PLOT")
#회귀평면 추가
b0 <- coef(fit)[1]#회귀계수 담기
b1 <- coef(fit)[2]# 고졸
b2 <- coef(fit)[3]#  살인
x1.grid <- seq(min(x1), max(x1), length.out = 10) #x축면의 간격 지정 
x2.grid <- seq(min(x2), max(x2), length.out = 10) #y축면의 간격 지정
y1.grid <- outer(x1.grid, x2.grid, function(x1, x2) b0 + b1*x1 + b2*x2) #z축면 간격 지정
rgl.planes(a = -b1, b = -b2, c = 1, d = -b0, alpha = 0.5, col = "blue") #alpha 투명도
