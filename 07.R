#
#
# 1. Review




#
# 2.1 데이터 전처리 이해
#  
# 데이터 전처리( data preprocessing )
#   초기에 확보한 데이터를 정제하고 가공해서
#   분석에 적합한 데이터를 확보하는 과정
#
# 데이터 전처리는 전체 분석 과정 중에서 매우 오랜 시간을
# 차지하기 때문에 이를 효과적으로 처리하는 방법을 알고
# 적용하는 것이 중요.
#

# 데이터 전처리 내룡
#  1. 결측치 처리 - 값이 없는 경우
#  2. 특이값 처리 - 값은 있으나 값이 문제에 맞지 않는 경우
#  3. 데이터 가공 - 원본 data에는 없는 내용을 추가, 변형


#
# 2.2 결측값 처리<- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <-  
is.na( z)               # NA 여부 확인
sum( is.na(z))          # NA 개수 확인
sum( z, na.rm = TRUE )  # NA를 제외한 합계 계산

# NA가 포함되면 산술계산이 x ,          

# 결측치 대체 및 제거
z1 <- c( 1, 2, 3, NA, NA, 8)
z2 <- c( 5, 8, 1, NA, 3, NA, 7)
z1[ is.na( z1 )] <- 0 <- <- <- <- <- <- <- <- <- f , 합계나 평균을 구할 때]
col_na <- function( y ){
    return( sum( is.na( y )))
}

na.count <- apply( x, 2, FUN = col_na) # (  , (1/2 행,열) ,  3번째 적용할 함수)
na.count

# 데이터 프레임 결측치 확인( 관측치에 대한 확인)
#
rowSums( is.na(x))              # 관측치별 NA 개수
sum ( rowSums( is.na(x)) > 0)   # NA가 포함된 관측치 계수
sum ( is.na ( x) )              # dataset 전체에서의 NA 개수
# -> 결측치( 변수, 관측치 )의 체크를 유기적으로 삭제하거나, 대체 하기.!

install.packages( 'mice' )      # 결측치 처리를 위해 사용하는 외부 패키지

library ( mice )

mean ( is.na(x ) )           # 결측치 비율이 낮으면 고려하지 않아도 될 수 있다.
mean ( is.na( iris) )

result <- md.pattern( x )    # 결측치 유형에 대한 표 작성  / mice-> md.pattern() 결측치내용을 패턴으로
result
write.csv( result, 'md_iris.csv', row.names = T ) # 별도의 csv 파일로 만들기.
md.pattern( iris)

# 결측치 탐색을 위한 상관관계 분석
result.cor <- as.data.frame( abs( is.na ( x) )) # x를 데이터 결측치 논리값을 추출해서 
result.cor 
result.cor.final <- result.cor[ which( apply( result.cor, 2, sum) > 0 )]  # which() - index값 알아오기
                                             #[결측치가 있는 인덱스값을 알아내기]
result.cor.final

cor( result.cor.final)

result.cor.full <- cor( result.cor, result.cor.final, use = 'pairwise.complete.obs')
result.cor.full

#
# 데이터프레임의 결측치 제거
#
# complete.cases() : dataset에서 NA를 포함하지 않은 완전한 행을 찾는 함수
#
head( x )
x[ !complete.cases( x ),]
y <- x[ complete.cases( x), ]
head( y)


#
# 결측치가 많은 dataset인 경우 결측치가 포함된 관측치를 모두 제거해버리면
# 실제로 남아 있는 관측치가 별로 없을 수 있으므로 분석이 어려운 경우가 생긴다.
# 위와 같은 경우 만약 결측치가 특정 변수에 모여 있따면 해당 변수만 
# 제거한 후 분석하는 것도 하나의 방법이다.
#
# 결측치가 여러 변수에 흩어져 있는 경우에는 결측치를 적당한 값으로
# 추정하여 대체한 후 분석할 수 있따.
#
# 결측치를 추정값으로 대체하여 분석할 경우 분석의 신뢰도가 떨어질 수 있으나
# 아무런 분석을 못하는 것 보다는 나은 방법이 될 수 있다.
#

# 2.3 특이값 처리
#
# 특이값( outiler, 이상치) : 정상적이라고 생각되는 데이터의 분포 범위 밖에
#               위치하는 값들, 입력오류나 실제 특이값 일 수도 있다.
#               특이값의 성질은 제조 공정의 불량품 선별, 은행 거래 시스템의 사기
#               거래 탐지할 때 사용하기도 한다.
#
# 데이터 분석 시 특이값을 포함한 채 평균 등의 게산을 하면 전체 데이터 양상
# 파악에 왜곡을 가져올 수 있으므로 분석 시 제외하는 경우가 많다.
#               
# Dataset에 특이값이 포함되어 있는지 여부 조사 방법
# 1. 논리적으로 있을 수 없는 값이 있는지 찾는다. 특별한 방법이 없기 때문에
#    분석자가 각 변수의 특성을 이해 한 후 특이값 탐색
# 2. 상식을 벗어난 값이 있는 지 찾는다.
# 3. 상자그래프를 통해 찾는다.
#

# 특이값 찾기
#
# 상자그래프를 이용한 특이값 찾기
st <- data.frame( state.x77 )
boxplot( st$Income)
boxplot.stats( st$Income)$out

#
# 특이값 처리 - 특이값 포함 관측치(행) 제거
#
# 일반적으로 특이값 포함 관측치(행) 제거는 특이값을 NA로
# 바꾸고 NA를 포함한 행을 제거하는 방식으로 진행
#
# %in% : 어떤 벡터에 비교하고자 하는 값이 포함되어 있는지
#        알고 싶을 때 사용 
out.val <- boxplot.stats( st$Income)$out    # 특이값 검출
st$Income[ st$Income %in% out.val ] <- NA   # NA로 대체 
head ( st )

newdata <- st[ complete.cases( st ), ] 
head( newdata )

#
# 2.4 데이터 가공
#
# 데이터 가공( processing ) : 수집한 데이;터에 대하여 분석을 용이하게 하기 위한
#                             정렬, 집계, 병합 등과 관련한 직업
#
# 1. 정렬 ( sort ) : 데이터를 주어진 기준에 따라 크기순으로
#                    재배열하는 과정 , 데이터 분석 시 빈번히 수행하는 과정
#----------------------> Vector 타입에서 주로!!
#
# order() : 주어진 열의 값들에 대한 순서를 붙이는 함수
#           값의 크기를 기준으로 작은 값부터 시작해서 번호부여
#-----------------------> data.frame, matrix
v1 <- c( 1, 7, 6, 8, 4, 2, 3)
order( v1 )
v1 <- sort ( v1 )
v1
v2 <- sort( v1, decreasing = T )
v2

# 매트릭스와 데이터 프레임 정렬
# : 특정 열의 값을 기준으로 행들을 재배열하는 형태로 정렬
# = 
head( iris)
order ( iris$Sepal.Length)
iris[ order( iris$Sepal.Length), ]
iris[ order( iris$Sepal.Length, decreasing = T) , ]
iris.new <- iris( order( iris$Sepal.Length ), )
head( iris.new)
iris[ order( iris$Sepal.Length, decreasing = T, iris$Petal.Length), ]

#
# 2. 데이터 분리와 선택
#
# split() : 하나의 dataset을 열의 값을 기준으로 여러 개의 dataset으로 분리
# subset() : dataset으로부터 조건에 맞는 행들을 ㅜ출
#
# 데이터 분리
sp <- split( iris, iris$Species)
sp
summary ( sp )
sp$Septosa


# 데이터 선택
subset( iris, Species == 'setosa')
subset( iris, Sepal.Length > 7.5 )
subset( iris, Sepal.Length > 5.1 & Sepal.Width > 3.9)
subset( iris, Sepal.Length > 7.6, select = c( Petal.Length, Petal.Width)) # 해당 관측치와 원하는 변수만.

# 3. 데이터 샘플링과 조합
#
# 데이터 샘플링( Sampling ) : 통계용어로, 주어진 값들이 있을 때 그중에서 임의의 개수의
#                             값들을 추출하는 작업
#
#    (1)비복원 추출 : 한 번 추출된 값은 다시 추출하지 않도록 하는 추출 방식
#    (2)복원 추출 : 추출한 값을 확인한 후 다시 데이터에 합친 후 새로 추출하느 방식
#      * 데이터 분석에서는 비복원 추출을 사용
#
# 샘플링이 필요한 경우는 dataset의 크기가 너무 커서 데이터 분석에 시간이 많이
# 걸릴 때 일부의 데이터만 샘플링하여 대략의 결과를 미리 확인하고 할 때 사용
#
# 숫자를 임의로 추추
x <- 1:45
y <- sample( x, size = 6, replace = TRUE)
            # size : 추출할 값, replace = FALSE 비복원 추출
y


# 행을 임의로 추출
idx <- sample( 1:nrow( iris), size = 50, replace = FALSE)
idx.50 <- iris[ idx, ]
dim ( idx.50)
head( idx.50)

sample( 1:20, size = 5)
sample( 1:20, size = 5)
sample( 1:20, size = 5)

set.seed( 100 )               # 인수가 같을 경우 set.seed를 통해 같은 값을 추출할 수 있다.
sample( 1:20, size = 5)
set.seed( 100 )
sample( 1:20, size = 5)
set.seed( 100)
sample( 1:20, size = 5)


#
# 4. 데이터 조합( combination) : 주어진 데이터 값들 중에서 몇 개씩 짝을 지어
#                                추출하는 작업
# combn() : 데이터 조합 시 사용, 결과에서 각 열이 하나의 조합을 의미
combn( 1:5, 3)

x = c( 'red', 'blue', 'green', 
       'black', 'white')
com <- combn( x, 2)
com

for ( i in 1:ncol( com) ){
    cat( com[ , i ], "\n")
}

#
# 5. 데이터 집계와 병합
#
# 데이터 집계( aggregation ) : 매트릭스와 데이터프레임과 같은 2차원 데이터는
#                              데이터 그룹에 대해서 합계나 평균을 계산해야
#                              하는 경우가 많은데 이 작업을 의미한다.
# aggregate() : 데이터 집계함수
#                dataset     ,   집계기준(범주형) ,          집계작업내용(apply)
agg <- aggregate(iris[ , -5 ], by = list( iris$Species), mean)
agg

agg <- aggregate( iris[ , -5], by = list( 품종 = iris$Species), mean)
agg

agg <- aggregate( iris[ , -5], by = list( 표준편차 = iris$Species), sd )
agg


#
# 데이터 병합( merge) : 데이터 분석을 위해 자료를 모으다 보면 연관된 정보가
#                       여러 파일에 흩어져 있는 경우가 있는데 이를 합치는
#                       작업을 의미한다.
#
x <- data.frame( name = c( 'a', 'b','c'), math = c( 90, 80, 40))
y <- data.frame( name = c( 'a', 'b', 'd'), korean = c( 75, 60, 90))
x
y
#                 병합기준
z <- merge( x, y, by = c( 'name'))
z
              # 병합기준이 없을 때, 같은 변수만 가지고 합침.
z2 <- merge( x, y )
z2
               # all.(dataset) , 
merge( x, y, all.x = T)
merge( x, y, all.y = T )
merge( x , y, all = T)

# 병합 기준이 되는 열의 이름이 다른 경우에 대한 병합
x <- data.frame( name = c( 'a', 'b', 'c') , math = c( 90, 80, 40))
y <- data.frame( sname = c( 'a', 'b', 'c' ), korean = c( 75, 60, 90))
x
y
#  변수의 내용이 다르지만,
#  x데이터의 name과 y데이터의 sname 값이 같으면 병합해라. 
               
merge( x, y, by.x = c( 'name'), by.y = c( 'sname'))
