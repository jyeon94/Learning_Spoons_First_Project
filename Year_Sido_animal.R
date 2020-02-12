library("lubridate")
library("tidyverse")
library("ggplot2")
library('scales')
library("gganimate")
library('gifski')
library('reprex')

## 기초 전처리

df_animal <- read.csv("C:/Users/seung/Desktop/2020년도/NanoD/github/Learning_Spoons_First_Project/csv/abandoned_animal(All).csv",fileEncoding = "UTF-8")

df_animal$접수일 <- ymd(df_animal$접수일)

df_animal <- df_animal %>%
  drop_na()

## 년도별 시도 유기동물 전체 수

Year_animal <- df_animal %>%
  group_by(year(df_animal$접수일) , df_animal$시도) %>%
  summarise(count = n())

Year_animal <- Year_animal %>%
  rename("year" = `year(df_animal$접수일)`)

Year_animal <- Year_animal %>%
  rename("sido" = 'df_animal$시도')

Year_animal <- Year_animal[-c(86:102),]

## 연도별 기준 시도 전체 동물 수 :2015~2019

gap1 <- Year_animal %>%
  group_by(year) %>%
  mutate(rank = min_rank(-count) * 1,
         count_rel = count/count[rank==1],
         count_lbl = paste0(" ",count)) %>%
  filter(rank !=1) %>%
  ungroup()

p1 <- ggplot(gap1, aes(rank, group = gap1$sido, 
                       fill = as.factor(gap1$sido), 
                       color = as.factor(gap1$sido))) 
p1 <- p1 + geom_tile(aes(y = gap1$count/2, height = gap1$count, width = 0.9), 
                     alpha = 0.8, color = NA)
p1 <- p1 + theme_tufte(base_family="NanumBarunGothic") 

# 축 서식
p1 <- p1 + geom_text(aes(y = 0, label = paste(gap1$sido, " "), 
                         family="NanumBarunGothic"), vjust = 0.2, hjust = 1)
p1 <- p1 + geom_text(aes(y=gap1$count,label = count_lbl, hjust=0))

p1

# 범레 지우기
p1 <- p1 + guides(color = FALSE, fill = FALSE)
p1 <- p1 + labs(title='연도별 기준 시도 전체 동물 수 : {closest_state}', 
                x = NULL, y = NULL, caption = "Sources: 통계청", 
                family="NanumBarunGothic")
# 가로로 변경
p1 <- p1 + coord_flip(clip = "off", expand = FALSE) 

# 랭크 순서 변경
p1 <- p1 + scale_x_reverse()

# 테마 꾸미기
p1 <- p1 + theme(plot.title = element_text(hjust = 0, size = 18))
p1 <- p1 + theme(axis.ticks.y = element_blank())
p1 <- p1 + theme(axis.text.y  = element_blank())
p1 <- p1 + theme(plot.margin = margin(1,2.5,1,2.5, "cm"))

p1

# 애니메이션으로 만들기
p1 <- p1 + transition_states(year, transition_length = 8, state_length = 1) +
  ease_aes('cubic-in-out') 

p1

anim_save("연도별 기준 시도 전체 유기동물 수.gif",animation=last_animation())
