import pandas as pd
import os
import numpy as np
os.getcwd()
pd.options.display.max_rows =30
pd.options.display.min_rows =20
pd.options.display.max_columns =10
pd.set_option('display.max.colwidth',15)
##검토용 데이터프레임*

#파일 불러오기
#확진자 누적 데이터
covidC = pd.read_csv('time_series_covid19_confirmed_global.csv').drop(['Province/State','Lat','Long'],axis= 1)
#사망자 누적 데이터
covidD = pd.read_csv('time_series_covid19_deaths_global.csv').drop(['Province/State','Lat','Long'],axis= 1)

#국가별 누적합계
sumC = covidC.groupby(['Country/Region']).sum()
sumD = covidD.groupby(['Country/Region']).sum()
#1년간 데이터 추출하기
startdate ='2021-07-31'
enddate='2022-07-31'
sumC_select = sumC.loc[:, startdate:enddate]
sumD_select = sumD.loc[:, startdate:enddate]

#일별데이터 구하기 diff함수 사용
sumC_day = sumC_select.diff(axis=1).drop(['2021-07-31'],axis= 1)
sumD_day = sumD_select.diff(axis=1).drop(['2021-07-31'],axis= 1)
# 음수값을 0으로 변경
sumC_day[sumC_day < 0] = 0
sumD_day[sumD_day < 0] = 0


#1번
# 총 확진자 및 사망자 수 구하기
# 각 나라별 총합 구하기
getTotalC = sumC_day.sum(axis=1)
getTotalD = sumD_day.sum(axis=1)
# 새로운 데이터프레임 생성
total_c = pd.DataFrame({'Country/Region': getTotalC.index, '총확진자수': getTotalC.values})
total_d = pd.DataFrame({'Country/Region': getTotalD.index, '총사망자수': getTotalD.values})
#합병
total_cd = pd.merge(total_c, total_d, on='Country/Region')
#일평균 발생자수
total_cd['일평균확진자'] = total_cd['총확진자수']/365

#일평균 사망자
total_cd['일평균사망자'] = total_cd['총사망자수']/365

#2번
#결측치 확인
na_checkC = sumC.isnull().sum()
na_checkC[na_checkC >= 1]
na_checkD = sumC.isnull().sum()
na_checkD[na_checkD >= 1]
#결측치 없음
#확진자 0 인 국가
total_cd[total_cd['총확진자수'] == 0]

#3번
#확진자 top20
sortC = total_cd.sort_values(by = '총확진자수', ascending=False).head(20)
#일평균 확진자 top20
sortMeanC = total_cd.sort_values(by = '일평균확진자', ascending=False).head(20)
#사망자 top20
sortD = total_cd.sort_values(by = '총사망자수', ascending = False).head(20)
#일평균 사망자 top20
sortMeanD = total_cd.sort_values(by = '일평균사망자', ascending=False).head(20)

#4번
#1년간 코리아 데이터 추출
# korea = total_cd.loc[total_cd['Country/Region'] == 'Korea, South', ['Country/Region','총확진자수', '총사망자수']]
korea_c = sumC_day.loc[sumC_day.index == 'Korea, South']
korea_d = sumD_day.loc[sumD_day.index == 'Korea, South']
#5번
korea_c.T.describe()
korea_d.T.describe()
#이하 통계 결과
# korea_c.T.describe()
# Country/Region   Korea, South
# count              365.000000
# mean             53756.032877
# std              99158.266748
# min                  0.000000
# 25%               2289.000000
# 50%               7210.000000
# 75%              49038.000000
# max             621317.000000
# korea_d.T.describe()
# Country/Region  Korea, South
# count             365.000000
# mean               62.931507
# std                93.262616
# min                 0.000000
# 25%                10.000000
# 50%                23.000000
# 75%                61.000000
# max               470.000000


#6번
#가로 데이터 세로 변경
dfC = korea_c.T
dfD = korea_d.T
dfD
#날짜를 데이트 형태로 변경
dfC.index = pd.to_datetime(dfC.index)
dfD.index = pd.to_datetime(dfD.index)
#resample함수로 월별 함계
monthC = dfC.resample('M').sum()
monthD = dfD.resample('M').sum()
monthD
#형식을 년 월까지만 나오게 수정
monthC.index = monthC.index.strftime('%Y-%m')
monthD.index = monthD.index.strftime('%Y-%m')
monthC = monthC.rename(columns={'Country/Region': '', 'Korea, South': '확진자'})
monthD = monthD.rename(columns={'Country/Region': '', 'Korea, South': '사망자'})
monthD
# totalmonth = pd.concat([monthC, monthD], axis=1)
#시각화
import matplotlib.pyplot as plt

# 월별 발생자수 박스 플롯 그리기
plt.boxplot(monthC.values)
plt.show()
# 월별 발생자수 히스토그램 그리기
monthC.plot(kind='bar', y='확진자')
plt.xlabel('Month')
plt.ylabel('확진자수')
plt.show()

# 월별 사망자수 박스
plt.boxplot(monthD.values)
plt.show()

# 월별 사망자수 히스토그램 그리기
monthD.plot(kind='bar', y='사망자')
plt.show()


