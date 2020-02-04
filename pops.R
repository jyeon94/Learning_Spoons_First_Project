t.pops <- read.csv("./data/total_pop.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

str(t.pops)

t.pops2 <-t.pops[-1,]

# 롱 포맷으로 변경
t.pops3 <- t.pops2 %>%
    gather(key="years", value="pops", "X2015", "X2016", "X2017", "X2018")

# X 제거
t.pops3$years <- substr(t.pops3$years,2,5)

# 연도 별로 보기

t.pops4 <- t.pops3 %>%
    filter(t.pops3$years=="2015")

# 지역 별로 보기

t.pops5 <- t.pops3 %>%
    filter(t.pops3$행정구역별.읍면동.=="서울특별시")
