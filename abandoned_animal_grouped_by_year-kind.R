install.packages("tidyverse")
library(tidyverse)
library(lubridate)

df <- read.csv("C:\\Users\\seung\\Desktop\\2020년도\\NanoD\\github\\Learning_Spoons_First_Project\\csv\\abandoned_animal(All).csv",fileEncoding='UTF-8')
df2<-df %>%
  filter(종류=='개') %>%
  group_by(year(접수일),종류) %>%
  summarise(count=n())

df3<-df %>%
  filter(종류=='고양이')%>%
  group_by(year(접수일),종류) %>%
  summarise(count=n())

