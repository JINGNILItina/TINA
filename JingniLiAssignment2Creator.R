

jingniliAssignment2 <- list(
  firstName = "jingni",
  lastName  = "li",
  email     = "jli239@ucsc.edu",
  studentID = 1505021
)

#1
install.packages("RCurl")
library(RCurl)
diamonds<-getURL("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
diamonds<- read.csv(text = diamonds)
jingniliAssignment2$s1a <- nrow(diamonds)#1a=7
jingniliAssignment2$s1b <- ncol(diamonds)
jingniliAssignment2$s1c <- names(diamonds)
jingniliAssignment2$s1d <- summary(diamonds$price)

#2
install.packages("repmis")
library("repmis")
tsv2007<-source_data("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
nrow(tsv2007)


jingniliAssignment2$s2a <- nrow(tsv2007)#1a=7
jingniliAssignment2$s2b <- ncol(tsv2007)
jingniliAssignment2$s2c <- names(tsv2007)
jingniliAssignment2$s2d <- mean(tsv2007$weightna,na.rm=TRUE)
jingniliAssignment2$s2d
jingniliAssignment2$s2e <- median(tsv2007$weight)
tsv2007$weightna <- ifelse(test=tsv2007$weight > 999 | tsv2007$weight < 996, 
                             tsv2007$weight, NA)
#排除999 996以外的
hist(tsv2007$weight) 
table(tsv2007$weight)
jingniliAssignment2$s2f <- mean(tsv2007$weightna)
View(jingniliAssignment2$s2f)
jingniliAssignment2$s2g <- median(tsv2007$weightna)
tsv2007male<-subset(tsv2007,(SEX==1))
tsv2007female<-subset(tsv2007,(SEX==2))
jingniliAssignment2$s2h <- summary(tsv2007male$weight)
jingniliAssignment2$s2i <- summary(tsv2007female$weight)

#3
vec<-c(letters,LETTERS)
jingniliAssignment2$s3a<-paste(vec[c(1:26*2)])
jingniliAssignment2$s3b<-paste(vec[c(36,21,14)])
arr<-array(c(letters,LETTERS),dim=c(3,3,3))
View(arr)
arr1<-arr[,,1]
arr2<-arr[,,2]
arr3<-arr[,,3]
jingniliAssignment2$s3c<-arr2[,1]
jingniliAssignment2$s3d<-c(arr1[,2],arr2[,2],arr3[,3])
jingniliAssignment2$s3e<-paste(c(arr[1,1,2],arr[2,2,2],arr[3,1,2]))


