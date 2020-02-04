install.packages('zoo')
install.packages('lubridate')
Sys.setlocale("LC_ALL", "Korean")

library(lubridate)
library(tidyverse)


df <- read.csv("./data/abandoned_animal_national.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

df_animal <- df

# 년도 데이터로 변환
df_animal$접수일<-ymd(df_animal$접수일)

# 고양이
df_cat <- df_animal %>%
    filter(종류=='고양이')

df_cat$품종 <- as.character(df_cat$품종)
df_cat$품종 <- trimws(df_cat$품종)
df_cat$품종 <- gsub("\\s+","",df_cat$품종)

df_cat[str_detect(df_cat$품종,"\\+"),"품종"]<- "믹스"
df_cat[str_detect(df_cat$품종,"콧숏"),"품종"]<- "한국고양이"
df_cat[str_detect(df_cat$품종,"폐"),"품종"]<- "페르시안"
df_cat[df_cat$품종=="","품종"] <- "기타"

cat1 <- c( '고양이','한국고양이','코숏','\\s+','길고양이','쇼콧','쇼컷','코숏믹스추정','미상','야생고양이','코솟','KSH','치즈','치즈태비','코리안숏헤어','코쇽','터앙+셀커크렉스','턱시도코숏' )
cat2 <- c( '하일랜드폴드' )
cat3 <- c( '페르시안','페르시안익스트림' )
cat4 <- c('통키니즈')
cat5 <- c('터키시앙고라','앙고라')
cat6 <- c('엑조틱','엑조틱숏헤어','엑죠틱','이그조틱숏헤어','엑조틱고양이','엔죠틱')
cat7 <- c('기타', '새끼고양이', '품종묘', '검은색무늬', '드고양이', '모름', '애기고양이', '이집션마우?')
cat8 <- c('레그돌-라가머핀', '라가머핀', '렉돌')
cat9 <- c('혼합', '페르시안믹스', '믹스묘', '잡종', '터키시앙고라믹스', '노르웨이숲믹스추정', '러시안블루믹스', '품종묘믹스추정', '노르웨이숲믹스추정.', '노르웨이숲믹스', '놀숲혼종', '러블믹스', '러시안블루믹스', '러시안블루+코숏', '방갈믹스', '벵갈혼종', '브리티쉬숏헤어믹스', '브리티시숏헤어추정', '브리티시숏헤어믹스', '샴믹스', '샴+쿤믹스추정', '스핑크스믹스', '아메리칸컬믹스', '아메숏믹스', '아숏믹스', '앙고라믹스', '엑조틱믹스', '친칠라믹스', '터키쉬믹스추정', '터키쉬앙고라믹스', '터키쉬앙고라혼합종', '터키쉬앙골라믹스', '턱시도고숏믹스', '페르시아믹스', '페르시안혼합', '페르시안(혼종)', '혼종고양이', '혼합(페르시아추정)', '혼합종(장모종)')
cat10 <- c('재패니즈밥테일')
cat11 <- c('스코티시폴드스트레이트', '스코티쉬스트레이트', '스코디티쉬스트레이트', '스코디쉬스트레이트', '스트레이트폴드', '폴드', '스코티시폴드')
cat12 <- c('실버태비')

str1 <- paste0( "(", paste(cat1, collapse = "|"), ")" ) 
str2 <- paste0( "(", paste(cat2, collapse = "|"), ")" )
str3 <- paste0( "(", paste(cat3, collapse = "|"), ")" )
str4 <- paste0( "(", paste(cat4, collapse = "|"), ")" )
str5 <- paste0( "(", paste(cat5, collapse = "|"), ")" )
str6 <- paste0( "(", paste(cat6, collapse = "|"), ")" )
str7 <- paste0( "(", paste(cat7, collapse = "|"), ")" )
str8 <- paste0( "(", paste(cat8, collapse = "|"), ")" )
str9 <- paste0( "(", paste(cat9, collapse = "|"), ")" )
str10 <- paste0( "(", paste(cat10, collapse = "|"), ")" )
str11 <- paste0( "(", paste(cat11, collapse = "|"), ")" )
str12 <- paste0( "(", paste(cat12, collapse = "|"), ")" )

df_cat[str_detect(df_cat$품종, str1), "품종"] <- "한국고양이"
df_cat[str_detect(df_cat$품종, str2), "품종"] <- "하일랜드폴드"
df_cat[str_detect(df_cat$품종, str3), "품종"] <- "하바나브라운"
df_cat[str_detect(df_cat$품종, str4), "품종"] <- "페르시안"
df_cat[str_detect(df_cat$품종, str5), "품종"] <- "터키시앙고라"
df_cat[str_detect(df_cat$품종, str6), "품종"] <- "엑조틱"
df_cat[str_detect(df_cat$품종, str7), "품종"] <- "기타"
df_cat[str_detect(df_cat$품종, str8), "품종"] <- "랙돌"
df_cat[str_detect(df_cat$품종, str9), "품종"] <- "믹스"
df_cat[str_detect(df_cat$품종, str10), "품종"] <- "밥테일"
df_cat[str_detect(df_cat$품종, str11), "품종"] <- "스코티시폴드"
df_cat[str_detect(df_cat$품종, str12), "품종"] <- "아메리칸컬"

view(df_cat)


# 개
df_dog <- df %>%
    filter(종류=='개')

df_dog$품종 <- as.character(df_dog$품종)
df_dog$품종 <- trimws(df_dog$품종)
df_dog$품종 <- gsub("\\s+","",df_dog$품종)

# 개 분류 대전제
mix <- c('믹스','혼','mix','교잡종','유사견','\\+','잡종','발바리','몽그렐','폼피츠','발발이','비숑푸들','또는','시바블랙탄','Mix')
remove_list <- c('추정','새끼', '추정', '\\?', '비슷', '보임','고슴도치', '닭', '염소')
etc <- c('갈색','검은','돼지','들개','모르겠음','미상','번호없음','부리탄','사냥','샤파이어','시잉프랑세즈','올드잉글리쉬','울프', '한국','없음','불가','중','푸말','빠삐용,장모치와와')

mixed <- paste0( "(", paste(mix, collapse = "|"), ")" ) 
removed<- paste0( "(", paste(remove_list, collapse = "|"), ")" )
etc_changed<-paste0( "(", paste(etc, collapse = "|"), ")" )

# 개 세부 분류
dog1 <- c('올드잉글리쉬쉽독', '올드잉글리시시프독', '올드잉그리쉬쉽독', '올드잉글리쉬쉽동', '올드잉글리쉬쉽독', '올드잉글리시쉽독', '올드잉글리시십독', '잉글랜드쉽독', '잉글리쉬쉽독', '잉글리쉬쉽독', '잉글리시쉽독')
dog2 <- c('요키')
dog3 <- c('웰시코기가디언','웰시코기펨브로크')
dog4 <- c('허스키')
dog5 <- c('휘펫','휘핏그레이하운드')
dog6 <- c('황구')
dog7 <- c('피플', '피플테리어', '핀불', '핏볼테리어', '핏불', '핏불테리어', '핏블', '핏블테리어', '핏풀', '핏플', '핏플테리어', '아메리칸핏불테리어', '팻불테리어')
dog8 <- c('그레이트피레니즈', '피레니안마운틴독')
dog9 <- c('플롯하운드')
dog10 <- c('플레사카나리오', '프레사까나리오', '카나리오')
dog11 <- c('미텔 스피츠', '재패니즈 스피츠')
dog12 <- c('풍산','풍산견','풍산기')
dog13 <- c('포인타','잉글리쉬포인터','저먼포인터','저먼 와이어헤어드포인터')
dog14 <- c('토이폭스테리어', '와이어폭스테리어', '폭스테리어','토이폭스테리어')
dog15 <- c('티베탄마스티프', '티벳탄마스티프', '티벳탄마스티프', '짱오' )
dog16 <- c('토이맨체스터테리어')
dog17 <- c('테뷰런', '티브론')
dog18 <- c('코카푸')
dog19 <- c('케인코르소')
dog20 <- c('케니스펜더')
dog21 <- c('카이훗', '카이훗 하운드')
dog22 <- c('카레리안베어독')
dog23 <- c('체코스로바키아울푸독')
dog24 <- c('잭러셀', '잭넉셀테리어', '잭러셀테리어', '잭러셀테리어', '젝너셀테리어', '젝러셀테리어')
dog25 <- c('재패니즈친', '재패니즈칭', '저패니즈', '제패니즈친', '제페니스친', '제페니스친', '제페니즈찡','제페니즈 친', '제페니즈찡','제페니즈친', '제페니즈칭')
dog26 <- c('새끼발바리')
dog27 <- c('세틀랜드시프독', '셀티', '셔틀랜드쉽독', '셔틀랜드쉽독', '셔틀렌드쉽독', '셰틀랜드쉽독', '쉐트랜드쉽독', '쉽독', '쉽득')
dog28 <- c('센트럴아시안오브차카', '미들아시안오브차카', '오브차카 코카시안', '코카시안오브차카')
dog29 <- c('셰터', '잉글리쉬세타', '잉글리쉬세터', '잉글리쉬셰터', '아이리쉬세터', '아이리쉬쉐터', '세터브리트니', '셋타', '잉글리쉬세터', '잉글리시세타')
dog30 <- c('저먼셰퍼드', '저먼셰퍼드독', '오스트랄리안셰퍼드독')
dog31 <- c('스코티쉬테리어')
dog32 <- c('스탠다드푸들', '미니어쳐푸들', '미디움푸들', '토이푸들', '토이푸들', '팬텀푸들')
dog33 <- c('스테포드셔불테리어', '스텐포드셔테리어')
dog34 <- c('브레타니스파니엘', '브리타니스파니엘', '브리티쉬스파니엘', '블레타니스파니엘', '스파니엘', '스프링거스파니엘', '아메리카코카스파니엘', '아메리칸코카스파니엘', '아메리칸코카', '잉글리시스프링어스패니얼', '잉글리쉬스프링거스파니엘', '잉글리쉬코카스파니엘', '프렌치스파니엘', '코카스파니엘', '코카스파니엘', '킹찰스스파니엘', '킹찰스스패니얼', '킹 찰스스페니엘', '킹찰스스파니엘', '킹찰스스파니엘', '킹찰스코카스파니엘', '킹찰스파니엘', '킹챨스', '캐벌리어킹찰스스파니엘')
dog35 <- c('시바', '시바이누')
dog36 <- c('시츄')
dog37 <- c('실키테리어')
dog38 <- c('아끼다', '아키다', '아키타')
dog39 <- c('아나톨리안셰퍼드', '아나톨리안셰퍼드도그(캉갈)', '캉갈')
dog40 <- c('아메리칸에스키모')
dog41 <- c('화이트테리어', '화이트테리어')
dog42 <- c('아메리칸블리')
dog43 <- c('아메리칸스테포드셔테리어')
dog44 <- c('미니어쳐슈나우저', '화이트슈나우저', '슈나우져', '자이언트슈나우져', '자이언트슈나우져')
dog45 <- c('아이리쉬울프하운드', '잉글리쉬올프하운드')
dog46 <- c('아이리쉬테리어')
dog47 <- c('아이리쉬소프트코튼휘튼테리어')
dog48 <- c('아츠간하운드, 아프간하운드, 아프칸하운드')
dog49 <- c('알라스칸말라뮤트')
dog50 <- c('미텔스피츠, 재패니즈스피츠')

str21 <- paste0( "(", paste(dog1, collapse = "|"), ")" ) 
str22 <- paste0( "(", paste(dog2, collapse = "|"), ")" )
str23 <- paste0( "(", paste(dog3, collapse = "|"), ")" )
str24 <- paste0( "(", paste(dog4, collapse = "|"), ")" )
str25 <- paste0( "(", paste(dog5, collapse = "|"), ")" )
str26 <- paste0( "(", paste(dog6, collapse = "|"), ")" )
str27 <- paste0( "(", paste(dog7, collapse = "|"), ")" )
str28 <- paste0( "(", paste(dog8, collapse = "|"), ")" )
str29 <- paste0( "(", paste(dog9, collapse = "|"), ")" )
str30 <- paste0( "(", paste(dog10, collapse = "|"), ")" )
str31 <- paste0( "(", paste(dog11, collapse = "|"), ")" )
str32 <- paste0( "(", paste(dog12, collapse = "|"), ")" )
str33 <- paste0( "(", paste(dog13, collapse = "|"), ")" ) 
str34 <- paste0( "(", paste(dog14, collapse = "|"), ")" )
str35 <- paste0( "(", paste(dog15, collapse = "|"), ")" )
str36 <- paste0( "(", paste(dog16, collapse = "|"), ")" )
str37 <- paste0( "(", paste(dog17, collapse = "|"), ")" )
str38 <- paste0( "(", paste(dog18, collapse = "|"), ")" )
str39 <- paste0( "(", paste(dog19, collapse = "|"), ")" )
str40 <- paste0( "(", paste(dog20, collapse = "|"), ")" )
str41 <- paste0( "(", paste(dog21, collapse = "|"), ")" )
str42 <- paste0( "(", paste(dog22, collapse = "|"), ")" )
str43 <- paste0( "(", paste(dog23, collapse = "|"), ")" )
str44 <- paste0( "(", paste(dog24, collapse = "|"), ")" )
str45 <- paste0( "(", paste(dog25, collapse = "|"), ")" )
str46 <- paste0( "(", paste(dog26, collapse = "|"), ")" ) 
str47 <- paste0( "(", paste(dog27, collapse = "|"), ")" ) 
str48 <- paste0( "(", paste(dog28, collapse = "|"), ")" ) 
str49 <- paste0( "(", paste(dog29, collapse = "|"), ")" ) 
str50 <- paste0( "(", paste(dog30, collapse = "|"), ")" ) 
str51 <- paste0( "(", paste(dog31, collapse = "|"), ")" ) 
str52 <- paste0( "(", paste(dog32, collapse = "|"), ")" ) 
str53 <- paste0( "(", paste(dog33, collapse = "|"), ")" ) 
str54 <- paste0( "(", paste(dog34, collapse = "|"), ")" ) 
str55 <- paste0( "(", paste(dog35, collapse = "|"), ")" ) 
str56 <- paste0( "(", paste(dog36, collapse = "|"), ")" ) 
str57 <- paste0( "(", paste(dog37, collapse = "|"), ")" ) 
str58 <- paste0( "(", paste(dog38, collapse = "|"), ")" ) 
str59 <- paste0( "(", paste(dog39, collapse = "|"), ")" ) 
str60 <- paste0( "(", paste(dog40, collapse = "|"), ")" ) 
str61 <- paste0( "(", paste(dog41, collapse = "|"), ")" ) 
str62 <- paste0( "(", paste(dog42, collapse = "|"), ")" ) 
str63 <- paste0( "(", paste(dog43, collapse = "|"), ")" ) 
str64 <- paste0( "(", paste(dog44, collapse = "|"), ")" ) 
str65 <- paste0( "(", paste(dog45, collapse = "|"), ")" ) 
str66 <- paste0( "(", paste(dog46, collapse = "|"), ")" ) 
str67 <- paste0( "(", paste(dog47, collapse = "|"), ")" ) 
str68 <- paste0( "(", paste(dog48, collapse = "|"), ")" ) 
str69 <- paste0( "(", paste(dog49, collapse = "|"), ")" ) 
str70 <- paste0( "(", paste(dog50, collapse = "|"), ")" ) 

df_dog[str_detect(df_dog$품종, mixed), "품종"] <- "믹스견"
df_dog[str_detect(df_dog$품종, removed), "품종"] <- ""
df_dog[str_detect(df_dog$품종,etc_changed),"품종"]<-"기타"
df_dog[df_dog$품종=='개',"품종"] <- '기타'
df_dog[df_dog$품종=="","품종"] <- "기타"
df_dog[df_dog$품종=='개|테리어|하운드|크로렌탈|찡','품종']<-'기타'
df_dog[str_detect(df_dog$품종,"라도"),"품종"]<- "리트리버"
df_dog[str_detect(df_dog$품종,"보더콜리"),"품종"]<- "보더콜리"
df_dog[str_detect(df_dog$품종,"^골든|리버$|리바$"),"품종"]<-"리트리버"
df_dog[str_detect(df_dog$품종,"덴$|데인$"),"품종"]<-"그레이트데인"
df_dog[str_detect(df_dog$품종,"^꼬똥"),"품종"]<-"꼬똥드툴레아"
df_dog[str_detect(df_dog$품종,"블랙탄$"),"품종"]<-"블랙탄"
df_dog[str_detect(df_dog$품종,"^동경|댕견|동견"),"품종"]<-"동경견"
df_dog[str_detect(df_dog$품종,"도사"),"품종"]<-"도사견"
df_dog[str_detect(df_dog$품종,"닥스훈드$"),"품종"] <- '닥스훈트'
df_dog[str_detect(df_dog$품종,"치와와$"),"품종"] <- '치와와'
df_dog[str_detect(df_dog$품종,"노이즈$|네이즈$"),"품종"] <- '말리리노이즈'
df_dog[str_detect(df_dog$품종,"숑"),"품종"] <- '비숑프리제'
df_dog[str_detect(df_dog$품종,"^버니즈"),"품종"]<-"버니즈마운틴독"
df_dog[str_detect(df_dog$품종,"^베드링|^베들린|^베들링|^베를링|^베링턴"),"품종"]<-"베들링턴테리어"
df_dog[str_detect(df_dog$품종,"^베어|라이카$"),"품종"]<-"라이카"
df_dog[str_detect(df_dog$품종,"^벨기에|벨지언"),"품종"]<-"벨지안쉽독"
df_dog[str_detect(df_dog$품종,"^볼|^뿔"),"품종"]<-"불테리어"
df_dog[str_detect(df_dog$품종,"불독|불도그"),"품종"]<-"불독"
df_dog[df_dog$품종=='브레타니|브리타니|프렌치브리타니','품종']<-'브리트니'
df_dog[str_detect(df_dog$품종,"블랙테리어"),"품종"]<-"블랙러시안테리어"
df_dog[str_detect(df_dog$품종,"진도|호피|칡"),"품종"]<-"진돗개" 
df_dog[str_detect(df_dog$품종,"빠삐"),"품종"]<-"파피용"
df_dog[str_detect(df_dog$품종,"삽"),"품종"]<-"삽살개"

df_dog[str_detect(df_dog$품종, str21), "품종"] <- "올드잉글리쉬쉽독"
df_dog[str_detect(df_dog$품종, str22), "품종"] <- "요크셔테리어"
df_dog[str_detect(df_dog$품종, str23), "품종"] <- "웰시코기"
df_dog[str_detect(df_dog$품종, str24), "품종"] <- "시베리안허스키"
df_dog[str_detect(df_dog$품종, str25), "품종"] <- "휘핏"
df_dog[str_detect(df_dog$품종, str26), "품종"] <- "누렁이"
df_dog[str_detect(df_dog$품종, str27), "품종"] <- "핏불테리어"
df_dog[str_detect(df_dog$품종, str28), "품종"] <- "피레니즈"
df_dog[str_detect(df_dog$품종, str29), "품종"] <- "플롯하운드"
df_dog[str_detect(df_dog$품종, str30), "품종"] <- "페로드프레사카나리오"
df_dog[str_detect(df_dog$품종, str31), "품종"] <- "스피츠"
df_dog[str_detect(df_dog$품종, str32), "품종"] <- "풍산개"
df_dog[str_detect(df_dog$품종, str33), "품종"] <- "포인터"
df_dog[str_detect(df_dog$품종, str34), "품종"] <- "폭스테리어"
df_dog[str_detect(df_dog$품종, str35), "품종"] <- "티베탄마스티프"
df_dog[str_detect(df_dog$품종, str36), "품종"] <- "맨체스터테리어"
df_dog[str_detect(df_dog$품종, str37), "품종"] <- "벨기에테뷰런"
df_dog[str_detect(df_dog$품종, str38), "품종"] <- "코카푸"
df_dog[str_detect(df_dog$품종, str39), "품종"] <- "카네코르소"
df_dog[str_detect(df_dog$품종, str40), "품종"] <- "케니스펜더"
df_dog[str_detect(df_dog$품종, str41), "품종"] <- "아메리칸스테그하운드"
df_dog[str_detect(df_dog$품종, str42), "품종"] <- "카렐리안베어도그"
df_dog[str_detect(df_dog$품종, str43), "품종"] <- "체코슬로바키아늑대개"
df_dog[str_detect(df_dog$품종, str44), "품종"] <- "잭러셀테리어"
df_dog[str_detect(df_dog$품종, str45), "품종"] <- "재패니즈친"
df_dog[str_detect(df_dog$품종, str46), "품종"] <- "발바리"
df_dog[str_detect(df_dog$품종, str47), "품종"] <- "셰틀랜드십도그"
df_dog[str_detect(df_dog$품종, str48), "품종"] <- "오브차카"
df_dog[str_detect(df_dog$품종, str49), "품종"] <- "세터"
df_dog[str_detect(df_dog$품종, str50), "품종"] <- "셰퍼드"
df_dog[str_detect(df_dog$품종, str51), "품종"] <- "스코티시테리어"
df_dog[str_detect(df_dog$품종, str52), "품종"] <- "푸들"
df_dog[str_detect(df_dog$품종, str53), "품종"] <- "스타포드셔불테리어"
df_dog[str_detect(df_dog$품종, str54), "품종"] <- "스패니얼"
df_dog[str_detect(df_dog$품종, str55), "품종"] <- "시바견"
df_dog[str_detect(df_dog$품종, str56), "품종"] <- "시추"
df_dog[str_detect(df_dog$품종, str57), "품종"] <- "오스트레일리안실키테리어"
df_dog[str_detect(df_dog$품종, str58), "품종"] <- "아키타견"
df_dog[str_detect(df_dog$품종, str59), "품종"] <- "캉갈도그"
df_dog[str_detect(df_dog$품종, str60), "품종"] <- "아메리칸에스키모도그"
df_dog[str_detect(df_dog$품종, str61), "품종"] <- "웨스트하일랜드화이트테리어"
df_dog[str_detect(df_dog$품종, str62), "품종"] <- "아메리칸불리"
df_dog[str_detect(df_dog$품종, str63), "품종"] <- "아메리칸스태퍼드셔테리어"
df_dog[str_detect(df_dog$품종, str64), "품종"] <- "슈나우저"
df_dog[str_detect(df_dog$품종, str65), "품종"] <- "울프하운드"
df_dog[str_detect(df_dog$품종, str66), "품종"] <- "아이리시테리어"
df_dog[str_detect(df_dog$품종, str67), "품종"] <- "아이리쉬소프트코티드휘튼테리어"
df_dog[str_detect(df_dog$품종, str68), "품종"] <- "아프간하운드"
df_dog[str_detect(df_dog$품종, str69), "품종"] <- "알래스칸맬러뮤트"
df_dog[str_detect(df_dog$품종, str70), "품종"] <- "스피츠"

view(df_dog)
