library(tidyverse)
df_dog <- read.csv('csv/품종(개).csv',fileEncoding='UTF-8')
df_cat <- read.csv('csv/품종(고양이).csv',fileEncoding='UTF-8')

#rbind를 활용하여 df_dog,df_cat 합치기
df_all <- rbind(df_dog,df_cat)
view(df_all)
#첫번째 컬럼인 x(인덱스와 똑같다)를 제외한 나머지를 df_all로 저장
#합쳐진 파일을 확인하고 x컬럼이 있으면 이 코드를 실행하고 아니면 실행하지 말 것
df_all<-df_all[-1]

#값들이 dataframe에 제대로 들어있는지 확인
view(df_all[-1])

#csv 파일로 저장
write_csv(df_all,'csv/abandoned_animal(All)')

