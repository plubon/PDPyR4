library('dplyr')
files <- list.files('data')
len <- 0
len13 <- 0
sum13 <- 0
sum <- 0
count <- 0
count13 <- 0
for(file in files)
{
  data <- read.csv(file.path('data', file))
  subset <- data %>% filter(DayofMonth !=13 | DayOfWeek != 5)
  subset13 <- data %>% filter(DayofMonth ==13 & DayOfWeek ==5)
  len <- len + nrow(subset)
  len13 <- len13 + nrow(subset13)
  sum13 <-  sum13 + subset13 %>% select(ArrDelay) %>% sum(na.rm = TRUE)
  sum <-  sum + subset %>% select(ArrDelay) %>% sum(na.rm = TRUE)
  count <- count + subset %>% filter(ArrDelay >0) %>% nrow()
  count13 <- count13 + subset13 %>% filter(ArrDelay >0) %>% nrow()
}
print(sum13/len13)
print(sum/len)
print(count13/len13)
print(count/len)