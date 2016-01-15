#assignment1-294B
#name: jingni li
#ID: 1505021

#1
firstName <- "JINGNI"
lastName  <- "LI"
print(paste(firstName,lastName))
studentID <- "1505021"
print(studentID)

#2 load data
library(foreign)
df.dta<-read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
View(df.dta)

df.csv<-url.show("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td<-url.show("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
df.rdata<-url.show("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData")
df.rdata<-load("/Users/tina93120/Desktop/NHIS_2007_RData.rdata")
View(df.rdata)
#3
#Rdata:45.3 KB,dta:188 KB,txt:139 KB,csv:139 KB
#The Rdata is the smallest
#reason for the variability: because they have different coding ways

#4
typeof(df.rdata)
class(df.rdata)
length(df.rdata)
dim(df.rdata)
nrow(df.rdata)
ncol(df.rdata)
summary(df.rdata)
#5
df.dta<-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(df.dta)
View(df.dta)
#The data set has 1119754 obs and 30 variables

summary(df.dta$rw)
#the min,median,mean,and max are 1.8, 15.9 19.8 and 354.8
#the 1st and 3rd quatitle are 10.7 and 24.4
#there are 521279 NA obs

#6
v<-c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)
length(v)
summary(v)

#7
x <- matrix( c(1, 4, 7, 2, 5, 8, 3, 6, 9), nrow=3, ncol=3) 

#the transpose of the matrix(x)
t(x)
# Find the eigenvalues and eigenvectors of x
eigen(x)

y <- matrix( c(1, 2, 3, 3, 2, 1, 2, 3, 0), nrow=3, ncol=3) 
z<-solve(y)
y%%z
#The new matrix is identity matrix

#7
carat = c(5, 2, 0.5, 1.5, 5, NA, 3) 
cut = c("fair", "good", "very good", "good", "fair", "ideal", "fair") 
clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", "NA" )
price = c(850, 450, 450, 0, 750, 980, 420)
diamonds <- data.frame(carat, cut, clarity, price)
print(diamonds)
#a
summary(diamonds$price)
#mean:557.1

#b
diamonds1<-subset(diamonds,(cut=="fair"))
summary(diamonds1$price)
#mean:673.3

#c
diamonds2<-subset(diamonds,(cut=="good"|cut=="very good"|cut=="ideal"))
summary(diamonds2$price)
#mean:470
#d
diamonds3<-subset(diamonds,carat>"2")
diamonds4<-subset(diamonds3,cut=="very good"|cut=="Ideal")
summary(diamonds4$price)
#so there is no null


