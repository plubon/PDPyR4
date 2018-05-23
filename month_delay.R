library('dplyr')

options(stringsAsFactors = FALSE)
files <- list.files('/home/kamil/R4/data')


for(file in files){
  data <- read.csv(file.path('/home/kamil/R4/data', file))
  m <- data %>% filter(!is.na(DepDelay)) %>%group_by(Month) %>% summarise(DepDelayAvg = sum(DepDelay)/n()) %>% select(Month, DepDelayAvg)
  write.csv(m, paste('/home/kamil/R4/results_months/', basename(file)))
  s <- data %>% filter(!is.na(DepDelay))  %>% summarise(DepDelayAvg = sum(DepDelay)/n()) %>% select(DepDelayAvg)
  write.csv(s, paste('/home/kamil/R4/results_sum/', basename(file)))
  print(s)
}

files_m <- list.files('/home/kamil/R4/results_months')

data <-  read.csv(file.path('/home/kamil/R4/results_months', files_m[2]))

for(i in 3:length(files_m)){
  data_temp <- read.csv(file.path('/home/kamil/R4/results_months', files_m[i]))
  data <- data + data_temp
}

data['DepDelayAvg'] <- data['DepDelayAvg']/(length(files_m)-2)

data['DepDelayAvg']
plot.default(data['DepDelayAvg'])

data_temp <- read.csv(file.path('/home/kamil/R4/results_months', files_m[2]))
plot.default(data_temp['DepDelayAvg'])

for(i in 2:length(files_m)){
  data_temp <- read.csv(file.path('/home/kamil/R4/results_months', files_m[i]))
  print(which.max(data_temp[,'DepDelayAvg']))
}
