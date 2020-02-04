pops <- read.csv("./data/pops_sigungu.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

str(pops)

# 전국, 도별 전체 가구 수
pops2 <- pops %>%
    filter(pops$가구주의.연령=='합계') %>%
    gather(key="years", value="pops", "X2015", "X2016", "X2017", "X2018")

pops2 <- subset(pops2, select = -2:-6)

pops2$years<-substr(pops2$years,2,5)

pops2$years

# 전국, 도별 1인 가구 수
one.pops2 <- pops %>%
    filter(pops$가구주의.연령=='합계') %>%
    gather(key="years", value="one.pops", "X2015.1", "X2016.1", "X2017.1", "X2018.1")

one.pops2 <- subset(one.pops2, select = -2:-6)

one.pops2$years<-substr(one.pops2$years,2,5)

one.pops2$years

# 전국, 도별 전체 가구 수 및 1인 가구 수

pops3 <- cbind(pops2, one.pops2$one.pops)

pops4 <- pops3
str(pops4)

# 비율 열 추가

n.pop <- sapply(pops4$pops,as.numeric)
n.onepop <- sapply(pops4$`one.pops2$one.pops`,as.numeric)
a<-(n.onepop/n.pop)*100

percent <- NA
percent <- round(a,5)

pops5 <-cbind(pops4, percent)

# 확인
view(pops5)

# 연도 별로 보기

pops6 <- pops5 %>%
    filter(pops4$years=="2015")

# 지역 별로 보기

pops7 <- pops5 %>%
    filter(pops4$행정구역별.시군구.=='서울특별시')
