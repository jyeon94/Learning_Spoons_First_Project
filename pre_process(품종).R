install.packages('zoo')
install.packages('lubridate')
Sys.setlocale("LC_ALL", "Korean")
library(lubridate)
library(tidyverse)
df <- read.csv('./csv/abandoned_animal_national.csv')
df2 <- df

#종류 컬럼을 공백으로 나눈후 첫 번째 값을 종류로 지정
df2$종류<-sapply(str_split(df2$품종," "),head,1)

#종류에 있는 [개],[고양이]에 있는 대괄호 제거
df2$종류 <- gsub('\\[|\\]',"",df2$종류)

#품종에 있는 [개],[고양이] 제거
df2$품종 <- gsub('\\[.*\\]',"",df2$품종)


view(df2)

write.csv(df2,'./csv/abandoned_animal_national.csv',row.names = FALSE)
