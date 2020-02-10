### 2. 연도별 유기견, 유기묘 선그래프 화면 분할

library("lubridate")
library("tidyverse")
library("ggplot2")
library("scales")
library("gridExtra")
library("grid")
library(ggthemes)
library(viridis)

##------------------------------------ 기초 전처리

df_animal <- read.csv("C:/Users/seung/Desktop/2020년도/NanoD/github/Learning_Spoons_First_Project/csv/abandoned_animal(All).csv",fileEncoding = "UTF-8")

df_animal$접수일 <- ymd(df_animal$접수일)

df_animal <- df_animal %>%
  drop_na()

## --------------------------------------------------------연도별 유기견 수 geom_line 적용

E_dog <- df_animal %>%
  group_by(year(df_animal$접수일),종류) %>%
  filter(종류=="개") %>%
  summarise(count = n())

E_dog <- E_dog[-c(6),]

"년도" <- E_dog$`year(df_animal$접수일)`

ggplot(data=E_dog,aes(년도,count)) +
  geom_line(color="red") +
  scale_y_continuous(labels = comma) +
  ggtitle("연도별 유기견 수") +
  theme(plot.title = element_text( face = "bold", color="red", size=18))

E_dog

### -------------------------------------------------------- 연도별 유기견 다른 코드

ggplot(data=E_dog,aes(년도,count)) +
  geom_line(color="red") +
  geom_text(aes(label = paste(count,""), 
                family="NanumBarunGothic"), vjust = -0.2, hjust = 0.5, size=3) +
  theme(plot.title = element_text(family = "NanumBarunGothic")) + 
  scale_y_continuous(labels = comma) +
  theme(legend.position = "none")


 
## --------------------------------------------------------------------------년도별 유기묘 수 

E_cat <- df_animal %>%
  group_by(year(df_animal$접수일),종류) %>%
  filter(종류=="고양이") %>%
  summarise(count = n())

E_cat <- E_cat[-c(6),]

ggplot(data=E_cat,aes(년도,count)) +
  geom_line(color="blue") +
  scale_y_continuous(labels = comma) +
  ggtitle("년도별 유기묘 수") +
  theme(plot.title = element_text( face = "bold", color="blue", size=18))

## -----------------------------------------------------------------------------유기견, 유기묘 통합

E_dog_visualize <- ggplot(data=E_dog,aes(년도,count)) +
  geom_line(color="red") +
  scale_y_continuous(labels = comma) +
  ggtitle("연도별 유기견 수") +
  theme(plot.title = element_text( face = "bold", color="red", size=18))

E_cat_visualize <- ggplot(data=E_cat,aes(년도,count)) +
  geom_line(color = 'blue') + 
  scale_y_continuous(labels = comma) +
  geom_point(aes(2018,26869),pch = 1,colour = 'red',size=8,) +
  ggtitle("연도별 유기묘 수") +
  theme(plot.title = element_text( face = "bold", color="blue", size=18))


### ------------------------------------------------------2. 년도별 유기묘, 유기견 - 화면 분할 그래프 
grid.arrange(E_dog_visualize,E_cat_visualize,nrow=1,newpage = TRUE)

