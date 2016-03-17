#FINAL EXAM
#NAME: JINGNI LI
#ID: 1505021
install.packages("nycflights13")
library(nycflights13)
library(dplyr)
install.packages("RSQLite")
library(RSQLite)
library(ggplot2)

# load data and create data frames
db <- src_sqlite("db.sqlite3", create = T)
flights_sqlite <- copy_to(
  db, flights, temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", 
    "tailnum")
)

airlines_sqlite <- copy_to(
  db, airlines, temporary = FALSE, 
  indexes = list("carrier")
)

airports_sqlite <- copy_to(
  db, airports, temporary = FALSE, 
  indexes = list("faa")
)

planes_sqlite <- copy_to(
  db, planes, temporary = FALSE, 
  indexes = list("tailnum")
)

weather_sqlite <- copy_to(
  db, weather, temporary = FALSE, 
  indexes = list(
    c("year", "month","day","hour"),
    "origin")
)

######################################################################################
df.flight<-tbl(db, sql("SELECT * FROM flights")) %>% 
  collect() %>%
  mutate(canceled=is.na(arr_time))
df.plane<-tbl(db, sql("SELECT * FROM planes")) %>% 
  collect()
df.airport<-tbl(db,sql("SELECT * FROM airports"))%>% 
  collect()
df.airport<-airports %>% mutate(dest = faa)
df.weather<-tbl(db,sql("SELECT * FROM weather")) %>%
  collect()
######################################################################################
df.fp<- tbl(db, sql("SELECT *
                    FROM flights join planes 
                    ON flights.tailnum = planes.tailnum")
) %>% 
  collect()
# panel data flights and airport
df.fa<-inner_join(
  df.flight, df.airport,by="dest")
# panel data flights and plane
df.fp<-inner_join(
  df.flight,df.plane,by="tailnum")
# panel data flights and weather
df.fw<-inner_join(
  df.flight,df.weather)
# panel data flights and departure time
df.ft<-df.flight
df.ft$time_of_day<-NA
df.ft$time_of_day<-df.ft$hour+(df.ft$minute/60)
df.ft$date<-paste(df.ft$year,df.ft$month,df.ft$day)
df.ft$date<-as.Date(df.ft$date,"%Y%m%d")
df.ft$weekdays<-weekdays(df.ft$date)

df.fa$canceled<-ifelse(df.fa$canceled==FALSE,0,1)
df.fp$canceled<-ifelse(df.fp$canceled==FALSE,0,1)
df.fw$canceled<-ifelse(df.fw$canceled==FALSE,0,1)
df.ft$canceled<-ifelse(df.ft$canceled==FALSE,0,1)


#1
l1<-lm(dep_delay~wind_speed+humid+pressure+temp,data = df.fw)
summary(lm1)
g1<-glm(canceled~wind_speed+humid+pressure+temp,data = df.fw)
summary(g1)

p<-ggplot(df.fw,aes(temp,dep_delay))+geom_point()
p


#2
l2<-lm(dep_delay~df.ft$time_of_day,data = df.ft)
summary(l2)
g2<-glm(canceled~df.ft$time_of_day,data = df.ft)
summary(g2)

l2.1<-lm(dep_delay~df.ft$date,data = df.ft)
g2.1<-glm(canceled~df.ft$date,data = df.ft)
summary(g2.1)

l2.2<-lm(dep_delay~df.ft$weekdays,data=df.ft)
g2.2<-glm(canceled~df.ft$weekdays,data=df.ft)
summary(g2.2)
p0<-ggplot(df.ft,aes(df.ft$time_of_day,canceled))+geom_bar(stat="identity", fill = "indianred")
p0
p1<-ggplot(df.ft,aes(df.ft$weekdays,canceled))+geom_bar(stat="identity", fill = "indianred")
p1
p2<-ggplot(df.ft,aes(df.ft$date,canceled))+geom_bar(stat="identity", fill = "indianred")
p2

#3
l3<-lm(dep_delay~dest,data=df.fa)
g3<-glm(canceled~dest,data=df.fa)
summary(g3)
p3<-ggplot(df.fa,aes(dest,canceled))+geom_bar(stat="identity", fill = "indianred")
p3

#4
l4<-lm(dep_delay~engines+seats,data = df.fp)
summary(l4)

g4<-glm(canceled~engines+seats,data = df.fp)
summary(g4)

p4<-ggplot(df.fp,aes(engines,canceled))+geom_bar(stat="identity", fill = "indianred")
p4
p5<-ggplot(df.fp,aes(seats,canceled))+geom_bar(stat="identity", fill = "indianred")
p5
