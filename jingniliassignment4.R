#Econ 294 Assignment4
print("JINGNI LI")
print(1505021)
print("jli239@ucsc.edu")

#problem 1
library(dplyr)
library(foreign)
flights <-read.csv("/Users/tina93120/Desktop/econ294_2015-master/data/flights.csv", stringsAsFactors = FALSE)
airports <-read.csv("/Users/tina93120/Desktop/econ294_2015-master/data/airports.csv", stringsAsFactors = FALSE)
weather <-read.csv("/Users/tina93120/Desktop/econ294_2015-master/data/weather.csv", stringsAsFactors = FALSE) 
planes   <-read.csv("/Users/tina93120/Desktop/econ294_2015-master/data/planes.csv", stringsAsFactors = FALSE) 

summary(flights)
summary(weather)
summary(planes)
#the planes datasets has no data type date
flightdate<-mutate(flights,date=as.Date(date))
weatherdate<-mutate(weather,date=as.Date(date))

#problem2
flights.2a<-subset(flights,dest=="SFO"|dest=="OAK")
nrow(flights.2a)

flights.2b<-subset(flights,dep_delay>=60)
nrow(flights.2b)

flights.2c<-subset(flights,arr_delay>=2*dep_delay)
nrow(flights.2c)


#problem 4
require(dplyr)
flights4adelay1<-flights%>%dplyr::select(dep_delay,arr_delay)
flights4adelay2<-select(flights,ends_with("delay"))
flights4adelay3<-select(flights,contains("delay"))


#problem 5
flights5a<-flights%>%select(dep_delay)%>%arrange(desc(dep_delay))%>%head(5)
flights5b<-df.flights%>%mutate(catchuptime=(dep_delay-arr_delay))%>%arrange(desc(catchuptime))%>%head(5)

#problem 6
flights<-mutate(df.flights,
                mph=(time*dist)/60,
                delta=dep_delay-arr_delay)
View(flights)

flights6a<-flights%>%select(mph)%>%arrange(desc(mph))%>%head(5)     
flights6b<-flights%>%select(delta)%>%arrange(desc(delta))%>%head(5)
flights6c<-flights%>%select(time)%>%arrange(desc(time))%>%head(1)

#problem 7
flights7a<- flights %>% group_by(carrier) %>%
  summarise (
    cancelled = sum(cancelled),
    total_flights = n(),
    percent_cancelled= (cancelled/total_flights),
    min = min(delta, na.rm = T),
    quantile1 = quantile(delta, .25, na.rm = T),
    quantile2 = quantile(delta, .75, na.rm = T),
    mean = mean(delta, na.rm = T),
    median = median(delta, na.rm = T),
    quantile90 = quantile(delta, .90, na.rm = T),
    max = max(delta, na.rm = T)
  )
flights7a1<-print(lights.7a%>%arrange(desc(percent_cancelled)))
day_delay <- dplyr::filter(summarize(group_by(dplyr::filter(flights,!is.na(dep_delay)),date),delay = mean(dep_delay),n = n()),n > 10)


day_delay1<- dplyr::filter(flights,!is.na(dep_delay))%>% group_by(date)%>%
  summarise(
    delay = mean(dep_delay),
    n = n(),
    n>10)

#problem 8
flights8<- mutate(day_delay, difference = delay-lag(delay)) %>%arrange(desc(difference))

#problem 9

dest_delay<-flights %>% group_by(dest) %>%
  summarise (
    mean = mean(arr_delay, na.rm = T),
    number_flights=n()
  )

airports<-select(airports,
                 dest = iata, name = airport , city,state, lat, long)

dest_delay %>% tbl_df
df9a<- airports%>%left_join(dest_delay,by="dest")
df9b<- airports%>%inner_join(dest_delay,by="dest")
df9c<- airports%>%right_join(dest_delay,by="dest")

hourly_delay<- filter(flights,!is.na(dep_delay))%>% group_by(date,hour)%>%
  summarise(
    delay = mean(dep_delay),
    n = n(),
    n>10)
weather %>% tbl_df

df.10<- hourly_delay%>%full_join(weather)
df.10<-df.10%>%group_by(conditions)%>%summarise(a=max(delay,na.rm=T))#worst weather

#11
#a
library(tidyr)
library(dplyr)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
df1>-df %>% gather(subject, value, -treatment) %>% 
  mutate(subject = subject %>% substr(8,9)) %>% select(subject, treatment, value)

#b

df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df2<-df %>% mutate(subject=ifelse(subject==1,"subject1","subject2"))%>%spread(subject,value)

#c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df
df3>-df %>% separate(demo, into = c('sex','age','state') , sep = '_')

#d
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
  
df

df4<- df %>% unite("demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df




