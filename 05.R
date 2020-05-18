#
# 5일차
#
# 0. OPen
# 1. Review
# 2. Topic
#  2.1 tx/excel 파일 읽기
#  2.2 자료의 종류
#  2.3 단일 변수 범주형 자료 탐색
#  2.4 단일 변수 연속형 자료 탐색
# 3. Q & A 
# 4. Next
# 5. Close
# 
# 1. Review
#
# Text file 읽기
setwd( "C:\\WorkspaceR" ) # 파일 저장 경로 설정
df <- read.table( file = "airquality.txt", header = T )
df

class ( df )

#
# Excel 파일 읽기
#
# Excel 파일 읽기용 패키지 설치
install.packages( "xlsx" )      # 엑셀파일 읽을 때 쓰는 패키지.
install.packages( "rJava" )     # 자바를 실행할 때 쓰는 패키지.

# 기본 패키지 외에 외부에서 설치된 패키지 사용 ( library Load )
library( rJava ) 
library( xlsx )

setwd( "C:\\WorkspaceR" ) # 파일 저장 경로 설정
df.xlsx <- read.xlsx( file = "airquality.xlsx",
                       sheetIndex = 1,
                       encoding = "UTF-8")

df.xlsx
class ( df.xlsx )
str ( df.xlsx)
head( df.xlsx)
tail( df.xlsx)


#
# which () 인덱스를 알고자 할 때 , which ( 조건 변수 )
# 데이터 전처리 과정에서 유용하게 사용!
score <- c( 35, 24, 37, 74, 29, 15, 22, 65, 48, 83 )
which( score == 37 ) # 조건에 만족하는 위치의 index
which( score >= 65 )
max ( score )
which.max( score )    # 최고값의 index
min( score )
which.min ( score )   # 최저값의 index

idx <- which( score >= 50 ) 
score [ idx ] <- 51
score

idx <- which(df.xlsx[ , 1:2 ] == "NA", arr.ind = TRUE )  # arr.ind : 몇행 몇열.
idx









