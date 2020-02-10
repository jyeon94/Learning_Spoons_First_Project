library("lubridate")
library("tidyverse")
library("ggplot2")
library('scales')
library("gganimate")
library('gifski')
library('reprex')

## ���� ��ó��

df_animal <- read.csv("C:/Users/seung/Desktop/2020�⵵/NanoD/github/Learning_Spoons_First_Project/csv/abandoned_animal(All).csv",fileEncoding = "UTF-8")

df_animal$������ <- ymd(df_animal$������)

df_animal <- df_animal %>%
  drop_na()

## �⵵�� ���⵿�� ��ü ��

E_animal <- df_animal %>%
  group_by(year(df_animal$������)) %>%
  summarise(count = n())
  
E_animal <- E_animal[-c(6),]

"�⵵" <- E_animal$`year(df_animal$������)`

## 1. ���⵿�� �⵵�� ��ü bar �׷���   -> $animation �׷�����$

animal_E <- ggplot(E_animal, aes(�⵵, count, fill = count)) +
  geom_col() +
  ggtitle("������ ���⵿�� ��ü ��") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )

animal_E + theme(plot.title = element_text( face = "bold", color="red", size=18)) +
  transition_states(�⵵, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()

## �ִϸ��̼� ����

anim_save("������ ��ü ���⵿�� ��.gif",animation=last_animation())
