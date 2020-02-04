library(tidyverse)
df <- read.csv('./csv/abandoned_animal_national.csv')

df2 <- df

a <- regmatches(df2$상태,gregexpr("\\(.*?\\)",df2$상태))


df2$상태<-sapply(df2$상태, function(a) gsub("[\\(\\)]","",a))

for(i in 1:length(df2$상태)){
  if(substring(df2$상태[i],0,2) == "종료"){
    df2$상태<-gsub(substring(df2$상태[i],0,2),"",df2$상태)
  }
}

view(df2$상태)
