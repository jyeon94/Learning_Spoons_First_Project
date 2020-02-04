library(tidyverse)
#전처리 필요한 파일 load
df <- read.csv('csv/abandoned_animal_national.csv')
#'개' 전처리를 위해 필터사용
df2 <- df %>%
  filter(종류=='개')

#factor를 character로 변경
df2$품종 <- as.character(df2$품종)
#사이사이 
df2$품종 <- gsub("\\s+","",df2$품종)

#믹스견으로 바꾸기 위한 백터 생성
mix <- c('믹스','혼','mix','교잡종','유사견','\\+','잡종','발바리','몽그렐','폼피츠','발발이','비숑푸들','또는','시바블랙탄','Mix')
#제거할 값들 백터 생성
remove_list <- c('추정','새끼', '추정', '\\?', '비슷', '보임','고슴도치', '닭', '염소')
#기타로 처리할 값들 백터 생성
etc <- c('갈색','검은','돼지','들개','모르겠음','미상','번호없음','부리탄','사냥','샤파이어','시잉프랑세즈','올드잉글리쉬','울프',
         '한국','없음','불가','중','푸말','빠삐용,장모치와와')

#백터의 값들을 |로 연결하여 값들 처리하기 용이하게 변형
mixed <- paste0( "(", paste(mix, collapse = "|"), ")" ) 
removed<- paste0( "(", paste(remove_list, collapse = "|"), ")" )
etc_changed<-paste0( "(", paste(etc, collapse = "|"), ")" )

#믹스견,제거할 값들, 기타값들 및 품종마다 str_detect를 활용하여 처리
# Ex) ^골든(골든으로 시작하는 값들), 리버$(리버로 끝나는 값들)
df2[str_detect(df2$품종, mixed), "품종"] <- "믹스견"
df2[str_detect(df2$품종, removed), "품종"] <- ""
df2[str_detect(df2$품종,etc_changed),"품종"]<-"기타"
df2[str_detect(df2$품종,"^골든|리버$|리바$"),"품종"]<-"리트리버"
df2[str_detect(df2$품종,"덴$|데인$"),"품종"]<-"그레이트데인"
df2[str_detect(df2$품종,"^꼬똥"),"품종"]<-"꼬똥드툴레아"
df2[str_detect(df2$품종,"블랙탄$"),"품종"]<-"블랙탄"
df2[str_detect(df2$품종,"^동경|댕견|동견"),"품종"]<-"동경견"
df2[str_detect(df2$품종,"도사"),"품종"]<-"도사견"
df2[df2$품종=='개|테리어|하운드|크로렌탈|찡','품종']<-'기타'
df2[str_detect(df2$품종,"닥스훈드$"),"품종"] <- '닥스훈트'
df2[str_detect(df2$품종,"치와와$"),"품종"] <- '치와와'
df2[str_detect(df2$품종,"노이즈$|네이즈$"),"품종"] <- '말리리노이즈'
df2[str_detect(df2$품종,"숑"),"품종"] <- '비숑프리제'
df2[str_detect(df2$품종,"^버니즈"),"품종"]<-"버니즈마운틴독"
df2[str_detect(df2$품종,"^베드링|^베들린|^베들링|^베를링|^베링턴"),"품종"]<-"베들링턴테리어"
df2[str_detect(df2$품종,"^베어|라이카$"),"품종"]<-"라이카"
df2[str_detect(df2$품종,"^벨기에|벨지언"),"품종"]<-"벨지안쉽독"
df2[str_detect(df2$품종,"^볼|^뿔"),"품종"]<-"불테리어"
df2[str_detect(df2$품종,"불독|불도그"),"품종"]<-"불독"
df2[df2$품종=='브레타니|브리타니|프렌치브리타니','품종']<-'브리트니'
df2[str_detect(df2$품종,"블랙테리어"),"품종"]<-"블랙러시안테리어"
df2[str_detect(df2$품종,"진도|호피|칡"),"품종"]<-"진돗개" 
df2[str_detect(df2$품종,"빠삐"),"품종"]<-"파피용"
df2[str_detect(df2$품종,"삽"),"품종"]<-"삽살개"

view(table(df2$품종))

