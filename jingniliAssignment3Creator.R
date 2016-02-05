##Assignment3##
##Q0
firstName<- "JINGNI"
lastName <- "LI"
print(
  paste(firstName,
        lastName
  )
)

studentID<- "1505021"
print(studentID)
print("jli239@ucsc.edu")

##  Q1
##loding data
library(foreign)
df.ex<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
View(df.ex)

##Q2
##filter
install.packages("dplyr")
library(dplyr)
df.ex.lastyear2013<- dplyr::filter(df.ex,year == 2013 & month == 12)
print(nrow(df.ex.lastyear2013))

july <- sum(with(df.ex,year==2013&month==7))
august <- sum(with(df.ex,year==2013&month==8))
sept <-sum(with(df.ex,year==2013&month==9))
summer2013<-sum(july+august+sept)
print(summer2013)

##Q3
install.packages("dplyr")
library(foreign)
df.ex.3a <-arrange(df.ex, year, month)
View(df.ex.3a)

##Q4
df.ex.4a<-select(df.ex,year,age)
View(df.ex.4a) 
df.ex.4b <- select(df.ex, year,month,starts_with("i"))
state<-distinct(select(df.ex, state))
View(state)

##Q5
stndz <- function(x){(x - mean(x, na.rm = T))  /  sd(x, na.rm = T)}
nrmlz <-function(x) {(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm=T))}
df.ex.5a <- dplyr::mutate(df.ex, rw.stndz = stndz(rw), rw_nrmlz = nrmlz(rw)) %>%
  select(rw.stndz, rw_nrmlz)

df.ex.5b <- df.ex %>% group_by(year,month) %>% 
  mutate(rw.stndz = stndz(rw), rw_nrmlz = nrmlz(rw),count = n()) %>% 
  select(rw.stndz, rw_nrmlz,count)

##Q6
df.ex.6 <- 
  dplyr::group_by(df.ex,year, month, state) %>%
  dplyr::summarise(
    rw.min = min(rw, na.rm = T),
    rw.1stq = quantile(rw, 0.25, na.rm = T),
    rw.mean = mean(rw, na.rm = T),
    rw.median = median(rw, na.rm = T),
    rw.3rdq = quantile(rw, 0.75, na.rm = T),
    rw.max = max(rw, na.rm = T),
    count = n()
  )

print(nrow(df.ex.6))

View(df.ex.6)