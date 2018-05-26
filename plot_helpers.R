library('dplyr')
loty_w_latach <- function(agg_data)
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

avg_delay_unlucky <- function(data)
{
  lens <- rowSums(data.frame(data[8,])) 
  results <- sapply(9:nrow(data), function(x, arg1, arg2)
    {
    arr <- lapply(arg1[x,], unlist)
    arr <- Reduce("+", arr)
    res <- arr/lens
    return(res)
  }, arg1 =data, arg2=lens)
  colnames(results) <-c('Srednie opoznienie przylotu', 'Srednie opoznienie odlotu', 'Odsetek opóźnionych przylotów',
                        'Odsetek opóźnionych odlotów', 'Odsetek przekierowanych lotów', 'Odsetek odwołanych lotów')
  rownames(results) <- c('Inne niż piątek 13', 'Piatek 13', 'Inne niż wtorek 13', 'Wtorek 13', 'Inne niż piątek 17',
                         'Piatek 17')
  return(results)
}

get_mean_test_results <- function(data)
{
  lens <- t(data.frame(data[8,])) 
  results <- sapply(9:10, function(x, arg1, arg2)
  {
    arr <- do.call(rbind,lapply(arg1[x,],unlist))/lens
    p_values <- sapply(seq(1,5,2), function(y, arr)
    {
       return(t.test(arr[,y], arr[,y+1])$p.value)
    }, arr)
    return(p_values)
  }, arg1 =data, arg2=lens)
  colnames(results) <- c('Srednie opoznienie przylotu', 'Srednie opoznienie odlotu')
  return(results)
}

get_prop_test_results <- function(unlucky_data, lens)
{
  unlucky_subset <- unlucky_data[,3:ncol(unlucky_data)]
  lens <- rowSums(data.frame(lens))
  lens[c(1,3,5)] <- 1
  props <- lens * unlucky_subset
  ret <-apply(props, 2, function(x, lens)
  {
    t <- sapply(seq(1,6,2), function(y, lens, x)
    {
      return(prop.test(x[y+1], lens[y+1], x[y])$p.value)
    }, lens, x)
    return(t)
  },lens)
  return(ret)
}

get_test_results<- function(data, unlucky_data) 
{
  ret <- cbind(get_mean_test_results(data), get_prop_test_results(unlucky_data, data[8,]))
  rownames(ret) <- rep('p.value', 3)
  return(ret)
}