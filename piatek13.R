library('dplyr')
files <- list.files('data')
files <- c('1987.csv')
results <- sapply(files, function(x)
{
  data <- read.csv(file.path('data', x))
  year <- tools::file_path_sans_ext(x)
  subset_lucky <- data %>% filter(DayofMonth !=13 | DayOfWeek != 5)
  subset_unlucky <- data %>% filter(DayofMonth ==13 & DayOfWeek ==5)
  subset_lucky_spa <- data %>% filter(DayofMonth !=13 | DayOfWeek != 2)
  subset_unlucky_spa <- data %>% filter(DayofMonth ==13 & DayOfWeek == 2)
  subset_lucky_ita <- data %>% filter(DayofMonth !=13 | DayOfWeek != 2)
  subset_unlucky_ita <- data %>% filter(DayofMonth ==13 & DayOfWeek == 2)
  subsets <- list(subset_lucky, subset_unlucky, subset_lucky_spa, 
               subset_unlucky_spa, subset_lucky_ita, subset_unlucky_ita)
  year <- tools::file_path_sans_ext(x)
  len_all <- nrow(data)
  arr_delay <- mean(data$ArrDelay, na.rm = TRUE)
  dep_delay <- mean(data$DepDelay,, na.rm = TRUE)
  lens <- sapply(subsets, nrow)
  by_carrier <- data %>% group_by(UniqueCarrier) %>% 
    summarise(dep=mean(DepDelay, na.rm = TRUE), arr =mean(ArrDelay, na.rm = TRUE), count = n())
  by_origin_airport <- data %>% group_by(Origin) %>% 
    summarise(dep=mean(DepDelay, na.rm = TRUE), arr =mean(ArrDelay, na.rm = TRUE), count = n())
  by_dest_airport <- data %>% group_by(Dest) %>% 
    summarise(dep=mean(DepDelay, na.rm = TRUE), arr =mean(ArrDelay, na.rm = TRUE), count = n())
  sums_arr <- lapply(subsets, function(y)
  {
    return(y %>% select(ArrDelay) %>% sum(na.rm = TRUE))
  })
  sums_dep <- lapply(subsets, function(y)
  {
    return(y %>% select(DepDelay) %>% sum(na.rm = TRUE))
  })
  count_arr <- lapply(subsets, function(y)
    {
    return(y %>% filter(ArrDelay >0) %>% nrow())
  })
  count_dep <- lapply(subsets, function(y)
  {
    return(y %>% filter(DepDelay >0) %>% nrow())
  })
  count_div <- lapply(subsets, function(y)
    {
    return(y %>% filter(Diverted == 1) %>% nrow())
  })
  count_cancelled <- lapply(subsets, function(y)
  {
    return(y %>% filter(Cancelled == 1) %>% nrow())
  })
  return(list(year, len_all, arr_delay, dep_delay,by_carrier, by_origin_airport, by_dest_airport, sums_arr, sums_dep, 
              count_arr, count_dep, count_div, count_cancelled))
})