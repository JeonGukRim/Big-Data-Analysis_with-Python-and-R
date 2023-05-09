rm(list = ls())
library(KoNLP)
library(tm)
library(multilinguer)
library(wordcloud2)
library(stringr)
library(tidytext)

getwd()
setwd("c:/Rwork")
#(장문)우크라이나 젤렌스키 대통령의 연설문 파일 읽기
zelensky <- file("Zelensky_address_20220219.txt", encoding = "UTF-8")
zelensky_data <- readLines(zelensky)
zelensky_data


#자체 함수 생성 띄여쓰기 기준으로 단텍스트 추출
#KoNLP패키지내 extractNoun 함수 사용

exNouns <- function(x) {paste(extractNoun(as.character(x)), collapse = " ") }

#함수적용
zelensky_nouns <- sapply(zelensky_data, exNouns)

zelensky_nouns[1]

#추출된 단어를 이용하여 말뭉치(Corpus) 생성

myCorpus <-Corpus(VectorSource(zelensky_nouns))

#한글 불용어 백터
my_stopwords <- c("은", "는", "이", "가", "하다", "것", "들", "그", "되다", "이다", 
                  "보다", "않다", "하다", "되었다", "있다", "같다", "때문", "말하다", 
                  "그러나", "그렇다", "그것", "이렇다", "저렇다", "하지만", "다른", 
                  "어떤", "여러", "싶다", "받다", "모르다", "중", "좀", "잘", "더", "말다", 
                  "그리고", "너무", "아니다", "없다","국가이든","이후","이것","하기","여러분",
                  "때문", "이것", "그것", "들이", "해서",
                  "무엇", "저들", "이번", "우린", "우리")

#데이터 전처리 
#문장부호 제거
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)
#수치 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
#소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
#불용어 제거
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, c(stopwords('english'),my_stopwords))

#결과 확인
inspect(myCorpusPrepro[1:5])

#단어집에서 단어수집
# myCorpusPrepro_term <- NULL

myCorpusPrepro_term <-TermDocumentMatrix(myCorpusPrepro,control = list(wordLengths = c(4,20)))

myCorpusPrepro_term

#matrix 자료구조를 data.frame 자료구조로 변경

myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))

#단어 출현 빈도수 구하기

wordResult <- sort(rowSums(myTerm_df), decreasing = TRUE)

#단어길이 2이상 빈도수와 빈도수 1 예외처리

my_filter <- wordResult[nchar(names(wordResult)) >= 2]


my_filter <- subset(my_filter,my_filter[] != 1)

my_filter

#단어 열 생성
# myName <- names(wordResult)
myName <- names(my_filter)
#단어열에 빈도수 추가한 데이터 프레임 생성성
# word.df <- data.frame(word = myName, freq = wordResult)
word.df <- data.frame(word = myName, freq = my_filter)


# final_word <- subset(word.df, freq != 1)

#데이터 확인 

library(devtools)
library(wordcloud2)

#빈도수 그래프 그리기
wordcloud2(data = word.df, 
           size = 0.9, color = 'random-light', gridSize = 9
           ,backgroundColor="black"
           , maxRotation = 0.8, minRotation = 1,shape = "star")

#파일 생성
write.csv(word.df,"양식1.csv")

########################################################################

#줄 단위 단어 추출
lword <- Map(extractNoun, zelensky_data)

length(lword)

lword <- unique(lword)

length(lword)

#중복 단어 제거와 추출 단어 확인

lword <- sapply(lword, unique)

length(lword)

lword

#연관어 분석을 위한 전처리하기
#단어 필터링 함수 정의
#두글자 이상 4글자 이하 단어 필터 함수 생성
filter1 <- function(x) {
  
  #nchar(x) <= 4 &&
  nchar(x) <= 16 &&nchar(x) >= 2 && is.hangul(x)
  
}

filter2 <- function(x) { Filter(filter1, x) }


#줄 단위로 추출된 단어 전처리

lword <- sapply(lword, filter2)

lword

#트랜잭션 생성하기

library(arules)

wordtran <- as(lword, "transactions")

wordtran

#단어 간 연관규칙 발견하기

library(backports) # to fix errors

#연관규칙 발견
tranrules <-  NULL
tranrules <- apriori(wordtran, parameter = list(supp = 0.05, conf = 0.6))
tranrules
#연관규칙 생성 결과보기

detach(package:tm, unload=TRUE)

inspect(tranrules)

# 연관어 시각화하기
#연관단어 시각화를 위해서 자료구조 변경

rules <- labels(tranrules, ruleSep = " ")

rules
# 문자열로 묶인 연관 단어를 행렬구조로 변경

rules <- sapply(rules, strsplit, " ", USE.NAMES = F)


# 행 단위로 묶어서 matrix로 변환

rulemat <- do.call("rbind", rules)

rulemat

# 연관어 시각화를 위한 igraph 패키지로딩

library(igraph)

#edgelist 보기
# [c(12:59), ]
ruleg <- graph.edgelist(rulemat[c(1:50),], directed = F)

ruleg

# edgelist 시각화

plot.igraph(ruleg, vertex.label = V(ruleg)$name, 
            vertex.label.cex = 1.2, vertext.label.color = 'black',
            vertex.size = 20, vertext.color = 'green',
            vertex.frame.co.or = 'blue')
a

