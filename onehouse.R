house<- read.csv("./data/family_sido.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

# 전국, 도별 전체 가구 수
house2 <- house %>%
    filter(house$가구주의.연령=='합계') %>%
    gather(key="years", value="family", "X2015", "X2016", "X2017", "X2018")

house2 <- subset(house2, select = -2:-6)

house2$years <- substr(house2$years,2,5)

house2$family <- as.numeric(house2$family)

# 전국, 도별 1인 가구 수
one.house2 <- house %>%
    filter(house$가구주의.연령=='합계') %>%
    gather(key="years", value="one.family", "X2015.1", "X2016.1", "X2017.1", "X2018.1")

one.house2 <- subset(one.house2, select = -2:-6)

one.house2$years<-substr(one.house2$years,2,5)

one.house2$one.family <- as.numeric(one.house2$one.family)

# 전국, 도별 전체 가구 수 및 1인 가구 수

house3 <- cbind(house2, one.house2$one.family)

names(house3)[4] <- c("one.family")

house4 <- house3

# 가구 수와 인구 수 합치기(pops 먼저 처리해야함)

total1 <- cbind(house4, t.pops3$pops)
names(total1)[5] <- c("pops")

# 비율 열 추가

n.house <- sapply(house4$family,as.numeric)
n.onehouse <- sapply(house4$one.family,as.numeric)
a<-(n.onehouse/n.house)*100

percent <- NA
percent <- round(a,2)

total1 <-cbind(total1, percent)

# 확인

view(total1)
str(total1)

#이 부분부터가 1인가구 증가율 계산하는 부분입니다
names(house4)[4]
house4 <- house4 %>%
    rename(행정구역별 = 행정구역별.시군구. ,
                one_person = names(house4)[4]      
    )

view(house4)

house5 <- house4 %>%
    arrange(행정구역별,years) %>%
    group_by(행정구역별) %>%
    mutate(lag(one_person))


class(house5$one_person)

house5[4:5] <-
    house5[4:5]%>%
    sapply(as.character) %>%
    sapply(as.numeric)

house5$increase <- house5$one_person / house5$`lag(one_person)`-1
house5$increase <- round(house5$increase, 2)

view(house5)

# 연도 별로 보기

total1 %>%
    filter(total1$years=="2015")

# 지역 별로 보기

total1 %>%
    filter(total1$행정구역별.시군구.=='서울특별시')

# 시각화

library(tidyverse)
library(ggplot2)     
library(gridExtra)   
library(ggthemes)    
library(scales)
library(viridis)
library(extrafont)

total2 <- total1 %>% filter(total1$행정구역별.시군구. != "전국")

# 1인 가구 비율
gg1 <- ggplot(total2, aes(x=total2$행정구역별.시군구., y=years, fill=percent))
gg1 <- gg1 + geom_tile(color="white", size=0.1)
gg1 <- gg1 + scale_fill_viridis(name="비율(%)", label=comma)
gg1 <- gg1 + coord_equal()
gg1 <- gg1 + labs(x=NULL, y=NULL, title="1인 가구 비율")
gg1 <- gg1 + theme_tufte(base_family="NanumBarunGothic")
gg1 <- gg1 + theme(plot.title=element_text(hjust=0))
gg1 <- gg1 + theme(axis.ticks=element_blank())
gg1 <- gg1 + theme(axis.text.x=element_text(size=7, angle = 90))
gg1 <- gg1 + theme(legend.title=element_text(size=7))
gg1 <- gg1 + theme(legend.text=element_text(size=6))
gg1

# 1인 가구 수

gg2 <- ggplot(total2, aes(x=total2$행정구역별.시군구., y=years, fill=total2$one.family)) 
gg2 <- gg2 + geom_tile(color="white", size=0.1)
gg2 <- gg2 + scale_fill_viridis(name="1인 가구 수", label=comma)
gg2 <- gg2 + coord_equal()
gg2 <- gg2 + labs(x=NULL, y=NULL, title="1인 가구 수")
gg2 <- gg2 + theme_tufte(base_family="NanumBarunGothic")
gg2 <- gg2 + theme(plot.title=element_text(hjust=0))
gg2 <- gg2 + theme(axis.ticks=element_blank())
gg2 <- gg2 + theme(axis.text.x=element_text(size=7, angle = 90))
gg2 <- gg2 + theme(legend.title=element_text(size=7))
gg2 <- gg2 + theme(legend.text=element_text(size=6))
gg2

# 1인 가구 증가율

str(house5)
total3 <- house5 %>% filter(house5$years=="2015")
total3 <- house5[house5$years!=2015,]

gg3 <- ggplot(total3, aes(x=total3$행정구역별, y=years, fill=increase)) 
gg3 <- gg3 + geom_tile(color="white", size=0.1)
gg3 <- gg3 + scale_fill_viridis(name="증가율(%)", label=comma)
gg3 <- gg3 + coord_equal()
gg3 <- gg3 + labs(x=NULL, y=NULL, title="1인 가구 증가 추세")
gg3 <- gg3 + theme_tufte(base_family="NanumBarunGothic")
gg3 <- gg3 + theme(plot.title=element_text(hjust=0))
gg3 <- gg3 + theme(axis.ticks=element_blank())
gg3 <- gg3 + theme(axis.text.x=element_text(size=7, angle = 90))
gg3 <- gg3 + theme(legend.title=element_text(size=7))
gg3 <- gg3 + theme(legend.text=element_text(size=6))
gg3

# 합치기
grid.arrange(gg1, gg2, gg3)
