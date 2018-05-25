library('dplyr')
loty_w_latach <- function(data)
{
  rok <- as.numeric(unname(unlist(agg_data[1, ])))
  liczba_lotow <- as.numeric(unname(unlist(agg_data[2,])))
  arr_delay <- unname(unlist(agg_data[3,]))
  dep_delay <- unname(unlist(agg_data[4,]))
  return(data.frame(rok=rok, liczba_lotow=liczba_lotow, arr_delay=arr_delay, dep_delay=dep_delay))
}

delay_by_carrier <- function(data)
{
  m <- apply(data, 2, function(x)
  {
    frame <- x[[5]]
    frame$year <- as.numeric(x[[1]])
    return(frame)
  })
  m <- do.call('rbind',m)
  dd <- sapply(unique(m$UniqueCarrier), function(y)
    {
    return(data.frame(ret <- m %>% filter(UniqueCarrier == y) %>% arrange(year)))
  })
  return(apply(dd,2, function(x){return(data.frame(x))}))
}

delay_by_orign <- function(data)
{
  m <- apply(data, 2, function(x)
  {
    frame <- x[[6]]
    frame$year <- as.numeric(x[[1]])
    return(frame)
  })
  m <- do.call('rbind',m)
  sapply(unique(m$Origin), function(y)
  {
    return(ret <- m %>% filter(Origin == y) %>% arrange(year))
  })
}

delay_by_dest <-  function(data)
{
  m <- apply(data, 2, function(x)
  {
    frame <- x[[7]]
    frame$year <- as.numeric(x[[1]])
    return(frame)
  })
  m <- do.call('rbind',m)
  sapply(unique(m$Dest), function(y)
  {
    return(ret <- m %>% filter(Dest == y) %>% arrange(year))
  })
}