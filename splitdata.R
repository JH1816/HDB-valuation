library(tidyverse)
library(ggplot2)
set.seed(43)
df = read.csv("final.csv")
df1 = df %>%
  mutate(storey_index = as.numeric(substr(storey_range, 7, 8))/3, years=years.x) %>%
  select(-storey_range, -dist_kinder, -years.x, -years.y, -X)
df2 = df1 %>%
  group_by(years) %>%
  summarize(n=n())
df3 = df1 %>%
  group_by(years)
ggplot(data=df2, aes(x=years, y=n)) + geom_line()
ggplot(data=df1, aes(x=years, y=index)) + geom_line()
ggsave("numberofresalebymonth.png")
year_range <- function(start, end) {
  temp <- df1 %>%
  filter(year>=start & year<=end)
  return (temp)
}
k_fold <- function(m,k) {
  n = nrow(m)
  remainder = n%%k
  split1 = ceiling(n/k)
  split2 = floor(n/k)
  x1 = rep(split1, remainder)
  x2 = rep(split2, k-remainder)
  x = c(x1,x2)
  m$k_fold <- sample(rep(1:k, x))
  return(m)
}
df_2001s <- k_fold(year_range(2001, 2021), 10)
write.csv(df_2001s, file="df2001.csv")
df_2003s <- k_fold(year_range(2003, 2021), 10)
write.csv(df_2003s, file="df2003.csv")
df_2005s <- k_fold(year_range(2005, 2021), 10)
write.csv(df_2005s, file="df2005.csv")
df_2007s <- k_fold(year_range(2007, 2021), 10)
write.csv(df_2007s, file="df2007.csv")
df_2009s <- k_fold(year_range(2009, 2021), 10)
write.csv(df_2009s, file="df2009.csv")
df_2011s <- k_fold(year_range(2011, 2021), 10)
write.csv(df_2011s, file="df2011.csv")
df_2013s <- k_fold(year_range(2013, 2021), 10)
write.csv(df_2013s, file="df2013.csv")
df_2015s <- k_fold(year_range(2015, 2021), 10)
write.csv(df_2015s, file="df2015.csv")
df_2017s <- k_fold(year_range(2017, 2021), 10)
write.csv(df_2017s, file="df2017.csv")
df_2019s <- k_fold(year_range(2019, 2021), 10)
write.csv(df_2019s, file="df2019.csv")
df_2020s <- k_fold(year_range(2020, 2021), 10)
write.csv(df_2020s, file="df2020.csv")
df_2021s <- k_fold(year_range(2021, 2021), 10)
write.csv(df_2021s, file="df2021.csv")
s1 <- k_fold(year_range(2022, 2022),2)

#write.csv(s1, file="s1.csv")
df_2017s <- read.csv("df2017.csv")
df_2020s <- read.csv("df2020.csv")
s1 <- read.csv("s1.csv")
s1.1<- s1 %>%
  filter(k_fold == 1)
s1.2<- s1 %>%
  filter(k_fold == 2)
df_2017s1 <- bind_rows(df_2017s, s1.1)
df_2017n <- k_fold(df_2017s1, 10)
df_2020s1 <- bind_rows(df_2020s, s1.1)
df_2020n <- k_fold(df_2020s1, 10)
#write.csv(df_2017n, file="df2017n.csv")
#write.csv(s1.2, file="s1_2.csv")
#write.csv(df_2020n, file="df2020n.csv")
