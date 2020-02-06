library(dplyr)
library(lubridate)
library(XML)
library(tidyverse)

## 기초 전처리

df_animal <- read.csv("C:/Users/seung/Desktop/2020년도/NanoD/github/Learning_Spoons_First_Project/csv/abandoned_animal(All).csv",fileEncoding = "UTF-8")

df_animal$접수일 <- ymd(df_animal$접수일)

df_animal <- df_animal %>%
  drop_na()

## 년도별 유기동물 전체 수

E_animal <- df_animal %>%
  group_by(year(df_animal$접수일)) %>%
  summarise(count = n())

## 년도별 유기견 수 파악

E_dog <- df_animal %>%
  group_by(year(df_animal$접수일),종류) %>%
  filter(종류=="개") %>%
  summarise(count = n())

## 년도별 유기묘 수 파악

E_cat <- df_animal %>%
  group_by(year(df_animal$접수일),종류) %>%
  filter(종류=="고양이") %>%
  summarise(count = n())

## 유기견 품종 top 10 파악

Seq_dog <- df_animal %>%
  filter(종류 == "개") %>%
  group_by(품종) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)

## 유기묘 품종 top 10 파악

Seq_cat <- df_animal %>%
  filter(종류 == "고양이") %>%
  group_by(품종) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)


            