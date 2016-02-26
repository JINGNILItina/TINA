
print("jingni li")
print("jli239@ucsc.edu")
StudentID <-1505021
print(StudentID)
#Q1
#a

install.packages("ggplot2")
library(ggplot2)

data=diamonds
p<-ggplot(data=diamonds,aes(x=x*y*z,y=price,color=clarity))
p<- p + geom_point(aes(color = clarity))+geom_point(aes(size = carat))+scale_x_log10()+scale_y_log10()
p
#b
p2 <- ggplot(diamonds, aes(x=carat))
p2<-p2 + geom_histogram(aes(fill=factor(clarity),y=..density..))+facet_grid(cut~.)
p2

#c
p3<-ggplot(diamonds,aes(cut,price))
p3<-p3+geom_violin(alpha=1,width=1)+geom_jitter(alpha=0.02)
p3


#Q2
#a
library(foreign)
require(dplyr)
data<-read.dta("/Users/tina93120/Desktop/org_example.dta")

d1<- data %>% dplyr::group_by(year,month)%>%
  dplyr::summarise(
    q_one = quantile(rw, .1, na.rm = T),
    q_nine = quantile(rw, .9, na.rm = T),
    q_first = quantile(rw, .25, na.rm = T),
    q_third = quantile(rw, .75, na.rm = T),
    median.rw = median(rw, na.rm = T),
    count = n())
d1<-d1 %>% mutate(date=paste(year,month,"01", sep="-"),
                            date=as.Date(date,format="%Y-%m-%d"))
p4 <- ggplot(d1, aes(x=date, y=median.rw))
p4<-p4 + geom_ribbon(aes(ymin=q_one , ymax=q_nine),alpha=0.6) + geom_ribbon(aes(ymin=q_first , ymax=q_third),alpha=0.2) + geom_line(aes(y=median.rw))+lims(y=c(0,50))
p4


#b
d2 <- data %>% dplyr::group_by(year,month,educ)%>%
  dplyr::summarise(
   median.rw = median(rw, na.rm = T),
    count = n())

d2<-d2 %>% mutate(date=paste(year,month,"01", sep="-"),
                              date=as.Date(date,format="%Y-%m-%d"))

p5 <- ggplot(d2, aes(x=date, y=median.rw,group=educ))
p5<-p5 + geom_line(aes(color=educ))
p5
