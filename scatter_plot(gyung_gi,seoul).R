library(tidyverse)
library(quantmod)
library(scales)
library(gridExtra)
df <- read.csv('./csv/abandoned_animal(All)_v3.csv',fileEncoding = 'UTF-8')
df_pops <- read.csv('./csv/세대구성별_가구_및_가구원__시군구_20200210152518.csv')

#경기도 1인가구/유기동물 수

###접수일 컬럼 date format으로 변형 
df$접수일 <- ymd(df$접수일)

df$년도 <- year(df$접수일)

df <- df[df$년도 < 2019,]

view(df)

df_gyung_gi <- df %>%
  filter(시도=='경기도') %>%
  group_by(년도)

view(df_gyung_gi)
df_gyung_gi <- df_gyung_gi %>%
  rename("관할기관" = 관활기간)

df_gyung_gi$관할기관 <- as.character(df_gyung_gi$관할기관)

df_gyung_gi$시군구<- sapply(str_split(df_gyung_gi$관할기관," "),tail,1)

view(df_gyung_gi)
df_gyung_gi <- df_gyung_gi[,-c(1,2,3,4,5)]

sort(unique(df_gyung_gi$시군구))

view(df_gyung_gi)

df_gyung_gi_sm<-df_gyung_gi %>%
  group_by(년도,시군구) %>%
  summarise(count=n())%>%
  view()

###wide format을 long format으로 변환
df_pops<- df_pops[-1,]

df_pops<-df_pops[df_pops$행정구역별.시군구.!='경기도',]

view(df_pops)

df_pops_all <- df_pops %>%
  gather(key="years", value="pops", "X2015", "X2016", "X2017", "X2018") %>%
  subset(select=-2:-5)

view(df_pops_all)
df_pops_all$years <- df_pops_all$years %>%
  substr(2,5)

df_pops_one <- df_pops %>%
  gather(key="years",value="one_pops","X2015.1", "X2016.1", "X2017.1", "X2018.1") %>%
  subset(select=-2:-5)

df_pops_one$years <- df_pops_one$years %>%
  substr(2,5)

df_pops_all<- cbind(df_pops_all,df_pops_one$one_pops)

view(df_pops_one)

view(df_pops_all)
names(df_pops_all)[names(df_pops_all) == "df_pops_one$one_pops"] <- "one_house"

df_merged <- merge(df_gyung_gi_sm,df_pops_all,by.x=c("시군구","년도"),by.y=c("행정구역별.시군구.","years"))

df_merged<-df_merged[-c(5)]

names(df_merged)[names(df_merged)=="count"] <-"유기동물수"

names(df_merged)
view(df_merged)

df_merged$one_house.1 <- 
  df_merged$one_house.1 %>%
  sapply(as.character) %>%
  sapply(as.numeric)

###상관관계 분석 및 시각화
cor(df_merged$one_house,df_merged$유기동물수)

gyungi_cor<-ggplot(df_merged,aes(one_house.1,유기동물수)) +
  geom_point(colour = 'black') + 
  ggtitle(label = '경기도1인가구/유기동물수')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('1인가구') +
  xlim(0,100000)+
  stat_smooth(method=lm,col='red',geom='smooth',se=FALSE) +
    scale_x_continuous(labels = comma) 

gyungi_cor

#서울 1인가구 /유기동물 수
view(df)


df_seoul <- df %>%
  filter(시도=='서울특별시')%>%
  group_by(년도)

view(df_seoul)
  
df_seoul <- df_seoul %>%
  rename("관할기관" = 관활기간)

df_seoul$구<- sapply(str_split(df_seoul$관할기관," "),tail,1)

df_seoul <- df_seoul[,-c(1,2,3,4,5)]

df_seoul_sm<-df_seoul %>%
  group_by(년도,구) %>%
  summarise(count=n())

view(df_seoul_sm)

###wide format을 long format으로 변환
df_pops_seoul <- read.csv('csv/세대구성별_가구_및_가구원__시군구_20200211201308.csv')
df_pops_seoul<-df_pops_seoul[-1,]

df_pops_seoul_all <- df_pops_seoul %>%
  gather(key="years", value="pops", "X2015", "X2016", "X2017", "X2018")

df_pops_seoul_all <- subset(df_pops_seoul_all,select= -2:-5)

view(df_pops_seoul_all)

df_pops_seoul_all$years <- df_pops_seoul_all$years %>%
  substr(2,5)

df_pops_seoul_one <- df_pops_seoul %>%
  gather(key="years",value="one_pops","X2015.1", "X2016.1", "X2017.1", "X2018.1") 

df_pops_seoul_one <- subset(df_pops_seoul_one,select= -2:-5)

df_pops_seoul_one$years <- df_pops_seoul_one$years %>%
  substr(2,5)

df_pops_seoul_all<- cbind(df_pops_seoul_all,df_pops_seoul_one$one_pops)

names(df_pops_seoul_all)[names(df_pops_seoul_all) == "df_pops_seoul_one$one_pops"] <- "one_house"

df_pops_seoul_all <- df_pops_seoul_all[-1,]

view(df_pops_seoul_all)

df_seoul_merged <- merge(df_seoul_sm,df_pops_seoul_all,by.x=c("구","년도"),by.y=c("행정구역별.시군구.","years"))

view(df_seoul_merged)

df_seoul_merged$one_house<- 
  df_seoul_merged$one_house %>%
  sapply(as.character) %>%
  sapply(as.numeric)

names(df_seoul_merged)[names(df_seoul_merged)=="count"] <-"유기동물수"
###상관관계 분석 및 시각화
cor(df_seoul_merged$one_house,df_seoul_merged$유기동물수)

seoul_cor<-ggplot(df_seoul_merged,aes(one_house,유기동물수)) +
  geom_point(colour = 'black') + 
  ggtitle(label = '서울1인가구/유기동물수')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('1인가구') +
  xlim(0,100000)+
  stat_smooth(method=lm,col='red',geom='smooth',se=FALSE) +
  scale_x_continuous(labels = comma) 


grid.arrange(gyungi_cor,seoul_cor,nrow=1)
