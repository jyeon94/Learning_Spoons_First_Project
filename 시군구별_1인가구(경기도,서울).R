library(tidyverse)
library(scales)
df_gyung_gi_pops <- read.csv('./csv/세대구성별_가구_및_가구원__시군구_20200210152518.csv')

df_gyung_gi_pops<- df_gyung_gi_pops[-1,]

df_gyung_gi_pops<-df_gyung_gi_pops[df_gyung_gi_pops$행정구역별.시군구.!='경기도',]

df_gyung_gi_pops_all <- df_gyung_gi_pops %>%
  gather(key="years", value="pops", "X2015", "X2016", "X2017", "X2018") %>%
  subset(select=-2:-5)

view(df_gyung_gi_pops_all)
df_gyung_gi_pops_all$years <- df_gyung_gi_pops_all$years %>%
  substr(2,5)

df_gyung_gi_pops_one <- df_gyung_gi_pops %>%
  gather(key="years",value="one_pops","X2015.1", "X2016.1", "X2017.1", "X2018.1") %>%
  subset(select=-2:-5)

df_gyung_gi_pops_one$years <- df_gyung_gi_pops_one$years %>%
  substr(2,5)

df_gyung_gi_pops_all<- cbind(df_gyung_gi_pops_all,df_gyung_gi_pops_one$one_pops)


names(df_gyung_gi_pops_all)[names(df_gyung_gi_pops_all) == "df_gyung_gi_pops_one$one_pops"] <- "one_house"

df_gyung_gi_pops_all$one_house <- df_gyung_gi_pops_all$one_house%>%
  sapply(as.character)%>%
  sapply(as.numeric)

view(df_gyung_gi_pops_all)
names(df_gyung_gi_pops_all)

df_gyung_gi_pops_all_2018 <-
  df_gyung_gi_pops_all %>%
  filter(years==2018) %>%
  arrange(one_house)


gyung_gi_order_lst <- df_gyung_gi_pops_all_2018$행정구역별.시군구.

gyung_gi_one_house<-df_gyung_gi_pops_all%>%
  plot_ly(y=~행정구역별.시군구.,x=~one_house,type='bar',
          text=~one_house,
          textposition='outside',
          marker = list(color = 'rgb(255,112,102)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.5)),
          transforms = list(
            list(
              type='filter',
              target= ~years,
              operation = "=",
              value = unique(df_gyung_gi_pops_all$years[1]))
          )
  ) %>%
  layout(
    title = "<b>경기도 시군별 1인가구 수<br> </b>",
    font = list(size = 10),
    xaxis = list(title='<b>1인가구 수</b>'),
    yaxis = list(title = '<b>시군<br> </b>',
                 categoryarray=gyung_gi_order_lst,
                 size = 10),
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 1,
        x = -0.1,
        buttons = list(
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_gyung_gi_pops_all$years)[1]),
               label = 2015),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_gyung_gi_pops_all$years)[2]),
               label = 2016),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_gyung_gi_pops_all$years)[3]),
               label = 2017),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_gyung_gi_pops_all$years)[4]),
               label = 2018)
        )
      )
    )
  )

gyung_gi_one_house

###서울시 구별 1인가구 수
df_pops_seoul <- read.csv('csv/세대구성별_가구_및_가구원__시군구_20200211201308.csv')
df_pops_seoul<-df_pops_seoul[-1,]
df_pops_seoul <- df_pops_seoul[df_pops_seoul$행정구역별.시군구.!='서울특별시',]
df_pops_seoul_all <- df_pops_seoul %>%
  gather(key="years", value="pops", "X2015", "X2016", "X2017", "X2018")

df_pops_seoul_all <- subset(df_pops_seoul_all,select= -2:-5)

view(df_pops_seoul_all)

df_pops_seoul_all$years <- df_pops_seoul_all$years %>%
  substr(2,5)

df_pops_seoul_one <- df_pops_seoul %>%
  gather(key="years",value="one_pops","X2015.1", "X2016.1", "X2017.1", "X2018.1") 

df_pops_seoul_one <- subset(df_pops_seoul_one,select= -2:-5)

df_pops_seoul_one$years <- df_pops_seoul_one$years %>%
  substr(2,5)

df_pops_seoul_all<- cbind(df_pops_seoul_all,df_pops_seoul_one$one_pops)

df_pops_seoul_all$one_house <- df_pops_seoul_all$one_house%>%
  sapply(as.character)%>%
  sapply(as.numeric)

names(df_pops_seoul_all)[names(df_pops_seoul_all) == "df_pops_seoul_one$one_pops"] <- "one_house"

df_seoul_2018 <- df_pops_seoul_all %>%
  filter(years==2018) %>%
  arrange(one_house)

seoul_order_lst<- df_seoul_2018$행정구역별.시군구.

seoul_one_house<-df_pops_seoul_all%>%
  plot_ly(y=~행정구역별.시군구.,x=~one_house,type='bar',
          text=~one_house,
          textposition='outside',
          marker = list(color = 'rgb(255,112,102)',
                        line = list(color = 'rgb(8,48,107)',
                                    width = 1.5)),
          transforms = list(
            list(
              type='filter',
              target= ~years,
              operation = "=",
              value = unique(df_pops_seoul_all$years[1]))
          )
  ) %>%
  layout(
    title = "<b>서울특별시 구별 1인가구 수<br> </b>",
    font = list(size = 10),
    xaxis = list(title='<b>1인가구 수</b>'),
    yaxis = list(title = '<b>시군<br> </b>',
                 categoryarray=seoul_order_lst,
                 size = 10),
    updatemenus = list(
      list(
        type = 'dropdown',
        active = 1,
        x = -0.1,
        buttons = list(
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_pops_seoul_all$years)[1]),
               label = 2015),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_pops_seoul_all$years)[2]),
               label = 2016),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_pops_seoul_all$years)[3]),
               label = 2017),
          list(method = "restyle",
               args = list('transforms[0].value',unique(df_pops_seoul_all$years)[4]),
               label = 2018)
        )
      )
    )
  )

seoul_one_house

