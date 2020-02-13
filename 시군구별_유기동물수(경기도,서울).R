#주로 pipe 및 dplyr을 쓰기 위한 tidyverse
library(tidyverse)
#date format으로 바꿔주기 위한 lubridate
library(lubridate)
#interactive한 데이터를 plot하기 위한 plotly
library(plotly)

#유기동물 데이터를 data frame으로 불러오기 위한 read.csv 
#fileEncoding을 지정 안할시 데이터가 안불러져 올 수도 있습니다.
df <- read.csv('./csv/abandoned_animal(All)_v3.csv',fileEncoding = 'UTF-8')

#데이터를 제대로 불러왔는지 확인하기 위에 head 사용
head(df)

#관할기간 컬럼명에 오타가 있기 때문에 바꿔주기 위해 rename 사용
df <- df %>%
  rename("관할기관" = 관활기간)

#컬럼명이 제대로 바뀌었는지 확인하기 위해 names 활용
names(df)

#년도 컬럼을 만들기위해 접수일을 ymd를 활용하여 date format으로 바꾸고
#year을 사용하여 년도만 추출
df$년도 <- df$접수일%>%
  ymd()%>%
  year()

#시군구 컬럼을 만들기 위해 관할기관을 str_split으로 공백으로 나누고 
#tail을 활용하여 두번째 값을 추출하고 sapply로 전체 적용
df$시군구<-sapply(str_split(df$관할기관," "),tail,1)

#컬럼이 제대로 만들어졌는지 확인하기 위해 view 사용
view(df)

#2020년은 아직 데이터가 많이 없기때문에 제외하기 위한 코드
df <- df[df$년도!=2020,]

#경기도 유기견 수 추출을 위해 filter 사용 
#유기견수 집계를 위해 group_by를 활용하여 년도,시군구로 묶고 summarise로 카운트 생성
df_count <- df %>% 
  filter(시도=='경기도') %>%
  group_by(년도,시군구) %>%
  summarise(count=n())

#그래프를 2019년 기준으로 정렬하기 위해 2019년으로 필터 및 count순으로 오름차순
df_count_2019 <- df_count %>%
  filter(년도==2019) %>%
  arrange(count)

#시군구를 활용하여 yaxis를 정렬하기 위해 gyug_gi_order 변수에 저장
gyung_gi_order<- df_count_2019$시군구

gyung_gi<-df_count%>%
  plot_ly(y=~시군구,x=~count,type='bar',
          text=~count,
          textposition='outside',
          marker = list(color = 'rgb(255,112,102)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.5)),
          transforms = list(
            list(
              type='filter',
              target= ~년도,
              operation = "=",
              value = unique(df_count$년도[1]))
          )
  ) %>%
  layout(
    title = "<b>경기도 시군별 유기동물 수<br> </b>",
    font = list(size = 10),
    xaxis = list(title='<b?유기동물 수</b>'),
    yaxis = list(title = '<b>시군<br> </b>',
                 categoryarray=gyung_gi_order,
                 size = 10),
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 1,
        x = -0.1,
        buttons = list(
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_count$년도)[1]),
               label = 2015),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_count$년도)[2]),
               label = 2016),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_count$년도)[3]),
               label = 2017),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_count$년도)[4]),
               label = 2018),
          list(method = "restlye",
               args = list('transforms[0].value',unique(df_count$년도)[5]),
               label = 2019)
        )
      )
    )
  )
gyung_gi

#위 코드를 서울특별시 유기견 수 추출을 위해 동일하게 적용
df_seoul_count <- df %>% 
  filter(시도=='서울특별시') %>%
  group_by(년도,시군구) %>%
  summarise(count=n())

#그래프를 2019년 기준으로 정렬하기 위해 2019년으로 필터 및 count순으로 오름차순
df_seoul_count_2019 <- df_seoul_count %>%
  filter(년도==2019) %>%
  arrange(count)

#구 순서대로 yaxis를 정렬하기 위해 seoul_order 변수에 저장
seoul_order<- df_seoul_count_2019$시군구

seoul<-df_seoul_count%>%
  plot_ly(y=~시군구,x=~count,type='bar',
          text=~count,
          textposition='outside',
          marker = list(color = 'rgb(255,112,102)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.0)),
          transforms = list(
            list(
              type='filter',
              target= ~년도,
              operation = "=",
              value = unique(df_seoul_count$년도[1]))
          )
  ) %>%
  layout(
    title = "<b>서울특별시 구별 유기동물 수<br> </b>",
    font = list(size = 10),
    xaxis = list(title='<b>유기동물수</b?'),
    yaxis = list(title = '<b>구<br> </b>',
                 categoryarray=seoul_order,
                 size = 10),
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 1,
        x = -0.1,
        buttons = list(
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_seoul_count$년도)[1]),
               label = 2015),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_seoul_count$년도)[2]),
               label = 2016),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_seoul_count$년도)[3]),
               label = 2017),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_seoul_count$년도)[4]),
               label = 2018),
          list(method = "restlye",
               args = list('transforms[0].value',unique(df_seoul_count$년도)[5]),
               label = 2019)
        )
      )
    )
  )

seoul


