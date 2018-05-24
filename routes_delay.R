library('dplyr')
library('rworldmap')
library('rworldxtra')
library('ggplot2')
library('ggmap')
library('ggrepel')

options(stringsAsFactors = FALSE)
files <- list.files('/home/kamil/R4/data')
# for(file in files)
# {
#   data <- read.csv(file.path('/home/kamil/R4/data', file))
#   print(head(data))
# }
airports <- read.csv('/home/kamil/R4/airports.dat', header = FALSE)
colnames(airports) <- c("ID", "name", "city", "country", "IATA", "ICAO", "lat", "lon", "altitude", "timezone", 
                        "DST", "tz database time zone", "type", "source")
# take_lat_lon <- function(name)
# {
#   sapply(name, function(n) {
#     a<- airports[airports$IATA==n,]
#     c(a$lat, a$lon )
#   })
# }

take_lat_lon <- function(name)
{
    a<- airports[airports$IATA==name,]
    c(a$lat, a$lon )
}

create_routes_table <- function(flights)
{
  rownumber <- 1
 m <- apply(flights, 1, function(r){
    df[rownumber, 'from'] <- r['Origin']
    from_lat_lon <- take_lat_lon(r['Origin'])
    df[rownumber, 'from.lat'] <- from_lat_lon[1]
    df[rownumber, 'from.lon'] <- from_lat_lon[2]
    
    df[rownumber, 'to'] <- r['Dest']
    to_lat_lon <- take_lat_lon(r['Dest'])
    df[rownumber, 'to.lat'] <- to_lat_lon[1]
    df[rownumber, 'to.lon'] <- to_lat_lon[2]

    df[rownumber, 'from'] <- flights[1,'Origin']
    rownumber = rownumber + 1
    c(r['Origin'], from_lat_lon[1], from_lat_lon[2], r['Dest'], to_lat_lon[1], to_lat_lon[2])
  })
 df <- data.frame(t(m))
 
 colnames(df) <- c('from', 'from.lat', 'from.lon', 'to', 'to.lat', 'to.lon')
 df[,'from.lat'] <- as.double(df[,'from.lat'])
 df[,'from.lon'] <- as.double(df[,'from.lon'])
 df[,'to.lat'] <- as.double(df[,'to.lat'])
 df[,'to.lon'] <- as.double(df[,'to.lon'])
 df
}

plot_routes <- function(routes)
{
  worldmap <- borders("world", colour="#efede1", fill="#efede1") # create a layer of borders
  ggplot() + worldmap + 
    geom_curve(data=routes, aes(x = from.lon, y = from.lat, xend = to.lon, yend = to.lat), col = "#b29e7d", size = 1, curvature = .2) + 
    #geom_point(data=airports, aes(x = lon, y = lat), col = "#970027") + 
    #geom_text_repel(data=routes, aes(x = from.lon, y = from.lat, label = from), col = "black", size = 2, segment.color = NA) + 
    theme(panel.background = element_rect(fill="white"), 
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
    )
}

col_from_per <- function(p)
{
  ifelse(p <= 10, 'green', ifelse(p <= 15, 'purple', ifelse(p <= 20, 'blue', ifelse(p <= 25, 'orange', 'red'))))
}

plot_routes_percent <- function(routes)
{
  worldmap <- borders("world", colour="#efede1", fill="#efede1") # create a layer of borders
  ggplot() + worldmap + 
    geom_curve(data=routes, aes(x = from.lon, y = from.lat, xend = to.lon, yend = to.lat), col = col_from_per(routes$percent), size = 0.2, curvature = .2) + 
    #geom_point(data=airports, aes(x = lon, y = lat), col = "#970027") + 
    #geom_text_repel(data=routes, aes(x = from.lon, y = from.lat, label = from), col = "black", size = 2, segment.color = NA) + 
    theme(panel.background = element_rect(fill="white"), 
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
    )
}

routes_dep_delay_month <- function(df, airport, month)
{
  den <- df %>% filter(Origin == airport, Month == month) %>% select_all()
  den_group_dest <- den %>% group_by(Origin, Dest)  %>% summarise( n = n()) %>% select(Origin, Dest, n )
  den_group_dest_delay <- den  %>% filter(DepDelay > 15)  %>% group_by(Origin, Dest)  %>% summarise( n = n()) %>% select(Origin, Dest, n )
  den_dep_percent <- den_group_dest %>% left_join(den_group_dest_delay, by=c("Origin", "Dest")) %>% mutate(percent = n.y/n.x*100) %>%select_all()
  den_dep_percent <- data.frame(den_dep_percent)
  r <- create_routes_table(den_dep_percent)
  r['percent'] <- den_dep_percent$percent
  r <- r[complete.cases(r), ]
  plot_routes_percent(r)
}

routes_dep_delay <- function(df, airport)
{
  den <- df %>% filter(Origin == airport) %>% select_all()
  den_group_dest <- den %>% group_by(Origin, Dest)  %>% summarise( n = n()) %>% select(Origin, Dest, n )
  den_group_dest_delay <- den  %>% filter(DepDelay > 15)  %>% group_by(Origin, Dest)  %>% summarise( n = n()) %>% select(Origin, Dest, n )
  den_dep_percent <- den_group_dest %>% left_join(den_group_dest_delay, by=c("Origin", "Dest")) %>% mutate(percent = n.y/n.x*100) %>%select_all()
  den_dep_percent <- data.frame(den_dep_percent)
  r <- create_routes_table(den_dep_percent)
  r['percent'] <- den_dep_percent$percent
  r <- r[complete.cases(r), ]
  plot_routes_percent(r)
}






file <- files[length(files)]
data_r <- read.csv(file.path('/home/kamil/R4/data', file))
head(data_r)
routes_dep_delay_month(data_r, 'IND', 9)

# for(file in files)
# {
#   data <- read.csv(file.path('/home/kamil/R4/data', file))
#   subset <- data %>% filter(DayofMonth !=13 | DayOfWeek != 5)
#   subset13 <- data %>% filter(DayofMonth ==13 & DayOfWeek ==5)
#   len <- len + nrow(subset)
#   len13 <- len13 + nrow(subset13)
#   sum13 <-  sum13 + subset13 %>% select(ArrDelay) %>% sum(na.rm = TRUE)
#   sum <-  sum + subset %>% select(ArrDelay) %>% sum(na.rm = TRUE)
#   count <- count + subset %>% filter(ArrDelay >0) %>% nrow()
#   count13 <- count13 + subset13 %>% filter(ArrDelay >0) %>% nrow()
# }

# len <- 0
# len13 <- 0
# sum13 <- 0
# sum <- 0
# count <- 0
# count13 <- 0
# for(file in files)
# {
#   data <- read.csv(file.path('/home/kamil/R4/data', file))
#   subset <- data %>% filter(DayofMonth !=13 | DayOfWeek != 5)
#   subset13 <- data %>% filter(DayofMonth ==13 & DayOfWeek ==5)
#   len <- len + nrow(subset)
#   len13 <- len13 + nrow(subset13)
#   sum13 <-  sum13 + subset13 %>% select(ArrDelay) %>% sum(na.rm = TRUE)
#   sum <-  sum + subset %>% select(ArrDelay) %>% sum(na.rm = TRUE)
#   count <- count + subset %>% filter(ArrDelay >0) %>% nrow()
#   count13 <- count13 + subset13 %>% filter(ArrDelay >0) %>% nrow()
# }
# print(sum13/len13)
# print(sum/len)
# print(count13/len13)
# print(count/len)