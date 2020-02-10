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

## 년도별 유기동물 전체 수

E_animal <- df_animal %>%
  group_by(year(df_animal$접수일)) %>%
  summarise(count = n())
  
E_animal <- E_animal[-c(6),]

"년도" <- E_animal$`year(df_animal$접수일)`

## 1. 유기동물 년도별 전체 bar 그래프   -> $animation 그려야함$

animal_E <- ggplot(E_animal, aes(년도, count, fill = count)) +
  geom_col() +
  ggtitle("연도별 유기동물 전체 수") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )

animal_E + theme(plot.title = element_text( face = "bold", color="red", size=18)) +
  transition_states(년도, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()

## 애니메이션 저장

anim_save("연도별 전체 유기동물 수.gif",animation=last_animation())
