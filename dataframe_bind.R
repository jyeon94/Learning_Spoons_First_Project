library(tidyverse)
df_dog <- read.csv('csv/품종(개).csv')
df_cat <- read.csv('csv/품종(고양이).csv')

#rbind를 활용하여 df_dog,df_cat 합치기
df_all <- rbind(df_dog,df_cat)

#첫번째 컬럼인 x(인덱스와 똑같다)를 제외한 나머지를 df_all로 저장
df_all<-df_all[-1]

#값들이 dataframe에 제대로 들어있는지 확인
view(df_all[-1])

#csv 파일로 저장
write_csv(df_all,'csv/abandoned_animal(All)')

