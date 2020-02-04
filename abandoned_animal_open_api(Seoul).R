library(XML)
library(tidyverse)


#open api 인증키
service_key <-'15PKRVa0527Ap7dvKbQOvC979eprNyZLi5a%2F4RNdtTOGAHUxQ0IqE%2FkfGLgMAG7%2FDPBnpINkhvWKtsX%2Fbkxc%2Fg%3D%3D'

#open api url
url <- 'http://openapi.animal.go.kr/openapi/service/rest/abandonmentPublicSrvc/abandonmentPublic?'

#검색 시작일
bgnde = '20140101'

#축종코드 417000 = 개, 422400 = 묘
upkind = c('417000','422400')

#시도코드 6110000 = 서울특별시 
uprcd = '6110000'

#시군구 코드 ex)322000 = 강남구
orgcd = c('3220000','3240000','3080000'
          ,'3150000','3200000','3040000'
          ,'3160000','3170000','3100000'
          ,'3090000','3050000','3190000'
          ,'3130000','3120000','3210000'
          ,'3030000','3070000','3230000'
          ,'3140000','3180000','3020000'
          ,'3110000','3000000','3010000','3060000')

#한번에 출력되는 값의 갯수
num_rows <- 5000

#xml을 담기 위한 리스트 생성
urlist <- list()

cnt <- 0

#for문을 이용하여 urlist에 xml 담기
for(i in 1:length(orgcd)){
  for (j in 1:length(upkind)){
    #cnt를 이용하여 인덱스 설정
    cnt = cnt + 1 
    #xml을 urlist에 각 인덱스마다 복사
    urlist[cnt] <- paste0(url,'bgnde=',bgnde,
                          '&upkind=', upkind[j],
                          '&upr_cd=', uprcd,
                          '&org_cd=',  orgcd[i],
                          '&numOfRows=', num_rows,
                          '&ServiceKey=', service_key)
                          
  }
}
urlist
#urlist에 있는 xml을 파싱하기 위해 빈 리스트 생성
raw.data <- list()
rootnode <- list()
for(i in 1:length(urlist)){
  #xml 파싱
  raw.data[[i]] <- xmlTreeParse(urlist[i], useInternalNodes = TRUE,encoding = "utf-8") # 생성한 URL 대로 XML 을 요청한다
  #xml을 정보를 불러 올 수 있는 형태로 변환
  rootnode[[i]] <- xmlRoot(raw.data[[i]])
}

#접수일, 품종, 발견장소,공고번호 데이터를 담기 위해 빈 벡터 생성
a <- c()
b <- c()
c <- c()
d <- c()

#벡터에 접수일, 품종, 발견장소, 공고번호 삽입
#union_all을 하지 않으면 전에 삽입했던 값이 사리지고 union을사용하면 중복된 값이 삭제 됨
for(i in 1: length(rootnode)){
   a <- union_all(a,xpathSApply(rootnode[[i]],"//happenDt",xmlValue))
   b <- union_all(b,xpathSApply(rootnode[[i]],"//kindCd",xmlValue))
   c <- union_all(c,xpathSApply(rootnode[[i]],"//happenPlace",xmlValue))
   d <- union_all(d,xpathSApply(rootnode[[i]],"//orgNm",xmlValue))
   
}

#벡터를 이용하여 유기견 데이터프레임 생성
df <- data.frame(접수일= a,
                    품종=b,
                    발견장소=c,
                    관활기간=d)
view(df)


view(df$발견장소) 

#CSV파일로 export
write.csv(df,'C:\\Users\\LG\\Documents\\Basic_R_Learning_Spoons\\project\\csv\\abandoned_animal.csv',row.names = FALSE)
       