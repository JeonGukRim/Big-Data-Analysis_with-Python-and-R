rm( list =ls())
getwd()
library(xgboost)

#데이터 생성
wd_df <- read.csv("wdbc_data.csv", header = T)

#종속변수는 diagnosis: Benign(양성), Malignancy(악성)
#양성이면 0 악성이면 1 그외 2 
wd_label <- ifelse(wd_df$diagnosis == "B", 0,
                   ifelse(wd_df$diagnosis == "M", 1, 2))
table(wd_label)
#종속변수 라벨링
wd_df$label <- wd_label
wd_df <- wd_df[-c(1,2)]
#트레이닝, 테스트 데이터 셋 생성 
set.seed(1234)
idx <- sample(nrow(wd_df), 0.7 * nrow(wd_df))
train <- wd_df[idx,]
test <- wd_df[-idx,]


############################ xgboost 분석#######################################
#matrix 객체 변환
train_mat <- as.matrix(train[-c(1, 31)])
dim(train_mat)

train_lab <- train$label
length(train_lab)

#xgb.DMatrix 객체 변환
dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)

#xgb model 생성
xgb_model <- xgboost(data = dtrain, max_depth = 2, eta = 1,
                     nthread = 2, nrounds = 2, num_class = 2,
                     objective = "multi:softmax",
                     verbose = 0)
xgb_model

#test set 생성
test_mat <- as.matrix(test[-c(1, 31)])
dim(test_mat)
test_lab <- test$label
length(test_lab)

#모델 예측치
pred_wd <- predict(xgb_model, test_mat)
pred_wd
table(pred_wd, test_lab)
#정확도 확인
sum(pred_wd == test_lab)/ NROW(pred_wd) # (110+48) /171 =0.92239766
# 정확성 92%

#변수 중요도
importance_matrix <- xgb.importance(colnames(train_mat),
                                    model = xgb_model)
importance_matrix

#중요 변수 시각화
xgb.plot.importance(importance_matrix)
################################################################################


############################로지스틱 분석#######################################
# wd_df <- read.csv("wdbc_data.csv", header = T)
# wd_label <- ifelse(wd_df$diagnosis == "B", 0,
#                    ifelse(wd_df$diagnosis == "M", 1, 2))
# wd_df$label <- wd_label
# wd_df <- wd_df[-c(1,2)]
# #트레이닝, 테스트 데이터 셋 생성 
# set.seed(1234)
# idx <- sample(nrow(wd_df), 0.7 * nrow(wd_df))
# train <- wd_df[idx, ]
# test <- wd_df[-idx, ]
#결측치 확인
table(is.na(wd_df))
str(wd_df)

library(car)
library(lmtest)
wd_model <- glm(label ~ ., data = train)
summary(wd_model)
# p-value 값이 0.05보다 큰 유의않지않는 변수 제거(후진제거법)
wd_model2 <- step(wd_model, direction = "backward")
formula(wd_model2)
summary(wd_model2)
# wd_model2 <- glm(label ~ area_mean+compactness_mean+smoothness_se+concavity_se+
# radius_worst+area_worst, data = train)


pred_wd2 <- predict(wd_model2, newdata =test, type = "response")
pred_wd2
# 컷오프를 0.5로 단순 가정하여 분류값을 생성한다. 0.5이상일 경우 1/이하일 경우 0으로 반환한다.
result_pred <- ifelse(pred_wd2 >= 0.5, 1, 0)
result_pred
table(result_pred)

#모델 평가
table(result_pred, test$label) 
(111+50) / nrow(test) 
# 정확도 측정 결과값 94%
################################################################################

library(car)
library(lmtest)
library(ROCR)
pr <- prediction(pred_wd2, test$label)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf )

################################################################################

