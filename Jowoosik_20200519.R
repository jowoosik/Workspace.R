#조우식 2020.05.19 / 2020.05.19

#문1)

#다음은 직장인 10명의 수입과 교육받은 기간을 조사한 자료이다. 산점도와
#상관계수를 구하고, 수입과 교육기간 사이에 어떤 상관관게가 있는지 설명하시오.

income<- c( 121, 99, 41, 35, 40, 29, 35, 24, 50, 60)
period<- c( 19, 20, 16, 16, 18, 12, 14, 12, 16, 17)

v3 <- data.frame( income, period)
v3

par( mfrow = c( 1, 1) )
plot( income~period, data = v3, main = '교육기간-수입의 상관관계',
      col = 'red', pch = 19)
res <- lm( income~period, data = v3)
abline( res, col = 'blue')
cor ( income, period)

# 답 : 상관계수가 0.79이며 회귀선이 우상향, 양의 값을 가진다.
#      따라서, 교육기간이 많을수록 수입 또한 늘어난다는 것을 알 수 있다.

# 문제2)

score <- c( 77.5, 60, 50, 95, 55, 85, 72.5, 80, 92.5, 87.5)
times <- c( 14, 10, 20, 7, 25, 9, 15, 13, 4, 21)

myds <- data.frame( score, times)
myds

plot( score~times, data = myds, main = '성적-TV시청기간',
      col = 'red', pch = 19)
res <- lm( myds$score~myds$times )
abline ( res, col = 'blue')
cor ( score, times) # 상관계수는 -0.6283671

# 자료를 분석한 결과, 상관게수가 -0.628 이며 회귀선이 우하향,
# 음의 값을 가진다. 따라서, TV시청기간이 늘어 날 수록 성적 또한 내려간다고 할 수 있다.

# 문제3 ) R에서 제공하는 mtcars 데이터셋에서 mpg와 다른 변수들간의 상관계수를 구하시오.

class( mtcars)
str( mtcars)
dim ( mtcars)
head( mtcars)


par( mfrow =  c( 4, 3))
plot( mtcars[ , 1:11], main = '상관계수', col = 'red', pch = 19)
res <-lm( mtcars[ , 1:11])
abline ( res, col = 'blue')
cor( mtcars[ , 1:11])

par( mfrow = c( 1, 1))
#-> 분석결과 wt~mpg의 상관계수가 -0.867로 가장 근접하였다.
#   즉, 이 상관계수를 바탕으로 wt~mpg의 선점도를 구해보면
plot( mtcars$mpg~mtcars$wt, main = '중량-연비 관계성',
      pch = 19, col = 'red',
      xlab = "중량",
      ylab = "연비(Mpg)")
res <- lm( mtcars$wt~mtcars$mpg)
abline( res, col = 'blue')
cor ( mtcars$wt, mtcars$mpg)
# 위와 같은 선형도를 볼 수 있다. 즉, 회귀선 중심으로 근접해있어
# 중량이 무거울 수록 연비가 적다고 할 수 있다. 
# 따라서 연비는 중량과 가장 밀접하다고 할 수 있다.

# 문제4 ) 2015년~26년까지 예상 인구수 추계자료
years <- c( 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)
pop <- c( 51014, 51245, 51446, 51635, 51811, 51973, 52123, 52261, 52388, 52504, 52609, 52704)

myds2 <- data.frame( years, pop)
myds2

plot( years,
      pop,
      main = "15~26 예상 인구",
      type = 'l',
      lty = 1,
      lwd = 3,
      xlab = 'Years',
      ylab = 'Population(천명)',
      col = 'red')

# 문제5 ) 도로교통공단 사고 정보.csv에 대한 EDA를 수행하시오.
setwd( "C:\\WorkspaceR")
df<- ( read.csv( file = 'EDA1.csv'))
df

#문제 정의 

# 1. 변수를 이해 -> 이 데이터에 대해서 어떤 식으로 EDA 수행.         ~           
# 2. EDA를 수행할 때 이 데이터 자체를 이해 -> 상관관계 파악.
# 3. 중앙값, 평균, 절사평균, 사분위수 구하기

# ===================================


# 1. 전국 도시에서 발생한 사고 건수는 몇 건이며, 사건이 가장 많이 발생한 도시 / 군 / 구는?
# 2. 발생건수와 사망자 수간의 상관계수를 구하시오.
# 3. 부상자수 와 중상,경상,부상신고의 선형도와 상관계수를 구하시오.
# 4. 
-------------------------------------------------------
class( df)
str( df)
dim( df)

str( df)
df$시도 <- as.character( df$시도)
df$시군구 <- as.character( df$시군구)
df$월 <- as.character( df$월)

str( df)





vars <- c( df.csv$시도, df.csv$시군구, df.csv$월 )
target <- df.csv[ 1:2745 , vars ]
head( target)

------------
# * 팩터타입일때 다루기가 까다롭다( 가공하기 어렵다.) / 팩터타입이면 나누기가어렵다
# 1. 각 도로 나누고 -> 그 안에서 시/군/구 -> 월 별로,
# 2. 







myds3 <- df.csv[ , c(1:3)]
myds3
