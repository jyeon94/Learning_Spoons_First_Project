library(extrafont)
font_import()
t.pops <- read.csv("./data/total_pop.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

str(t.pops)

# 1행 제거
t.pops2 <-t.pops[-1,]

# 롱 포맷으로 변경
t.pops3 <- t.pops2 %>%
    gather(key="years", value="pops", "X2015", "X2016", "X2017", "X2018")

# X 제거
t.pops3$years <- substr(t.pops3$years,2,5)

t.pops3$pops <- as.numeric(t.pops3$pops)

str(t.pops3)

# 연도 별로 보기

t.pops4 <- t.pops3 %>%
    filter(t.pops3$years=="2018")

# 지역 별로 보기

t.pops5 <- t.pops3 %>%
    filter(t.pops3$행정구역별.읍면동.=="서울특별시")

# 시각화
library(tidyverse)
library(gganimate)
library(gapminder)
library(extrafont)
font_import()
theme_set(theme_gray(base_family='NanumBarunGothic'))
theme_set(theme_classic())

gap1 <- t.pops3 %>%
    group_by(years) %>%
    mutate(rank = min_rank(-pops) * 1,
           pops_rel = pops/pops[rank==1],
           pops_lbl = paste0(" ",pops)) %>%
    filter(rank !=1) %>%
    ungroup()

p1 <- ggplot(gap1, aes(rank, group = 행정구역별.읍면동., 
                       fill = as.factor(행정구역별.읍면동.), 
                       color = as.factor(행정구역별.읍면동.))) 
p1 <- p1 + geom_tile(aes(y = pops/2, height = pops, width = 0.9), 
                     alpha = 0.8, color = NA) 
p1 <- p1 + theme_tufte(base_family="NanumBarunGothic") 

# 축 서식
p1 <- p1 + geom_text(aes(y = 0, label = paste(행정구역별.읍면동., " "), 
                         family="NanumBarunGothic"), vjust = 0.2, hjust = 1)
p1 <- p1 + geom_text(aes(y=pops,label = pops_lbl, hjust=0))

# 범레 지우기
p1 <- p1 + guides(color = FALSE, fill = FALSE)
p1 <- p1 + labs(title='도별 인구 수 : {closest_state}', 
                x = NULL, y = NULL, caption = "Sources: 통계청", 
                family="NanumBarunGothic")

# 가로로 변경
p1 <- p1 + coord_flip(clip = "off", expand = FALSE) 

# 랭크 순서 변경
p1 <- p1 + scale_x_reverse()

# 테마 꾸미기
p1 <- p1 + theme(plot.title = element_text(hjust = 0, size = 18))
p1 <- p1 + theme(axis.ticks.y = element_blank())
p1 <- p1 + theme(axis.text.y  = element_blank())
p1 <- p1 + theme(plot.margin = margin(1,2.5,1,2.5, "cm"))

# 애니메이션으로 만들기
p1 <- p1 + transition_states(years, transition_length = 8, state_length = 1) +
    ease_aes('cubic-in-out') 

p1
