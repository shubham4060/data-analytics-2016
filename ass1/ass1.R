# READING TABLE1
table1 <- read.csv("/home/aniket/Desktop/DACourse/ass1/Data1.csv",header = T,sep = ",")
View(table1)

# READING TABLE2
table2 <- read.csv("/home/aniket/Desktop/DACourse/ass1/Data2.csv",header = T,sep = ",")
View(table2)

table1[,2]


#  SUBJECT_WISE MEAN  VARIANCE   STANDARD DEVIATION
mean1 <- colMeans(table1[,-1],na.rm = T,dims = 1)
mean2 <- colMeans(table2[,-1],na.rm = T,dims = 1)

var1 <- vector(mode = "numeric")
var2 <- vector(mode = "numeric")

sd1 <- vector(mode = "numeric")
sd2 <- vector(mode = "numeric")

for(i in 2:ncol(table1)){
  m <- var(table1[,i],na.rm = T)
  var1 <- c(var1,m)
  
  m <- sd(table1[,i],na.rm = T)
  sd1 <- c(sd1,m)

  m <- var(table2[,i],na.rm = T)
  var2 <- c(var2,m)
  
  m <- sd(table2[,i],na.rm = T)
  sd2 <- c(sd2,m)
}

#  MEDIAN of MEAN VALUES SUBJECT_WISE
median1 <- median(mean1,na.rm = T)
median2 <- median(mean2,na.rm = T)


#  STUDENT_WISE MEAN  VARIANCE   STANDARD DEVIATION
mea1 <- rowMeans(table1,na.rm = T,dims = 1)
mea2 <- rowMeans(table2,na.rm = T,dims = 1)

va1 <- vector(mode = "numeric")
va2 <- vector(mode = "numeric")

s1 <- vector(mode = "numeric")
s2 <- vector(mode = "numeric")

i <- 1
for(i in 1:25){
  
  vec <- as.numeric(table1[i,-1])
  m <- var(vec,na.rm = T)
  print(m)
    va1 <- c(va1,m)
  
  m <- sd(vec,na.rm = T)
  s1 <- c(s1,m)
  
  vec <- as.numeric(table2[i,-1])
  m <- var(vec,na.rm = T)
  print(m)
  va2 <- c(va2,m)
  
  m <- sd(vec,na.rm = T)
  s2 <- c(s2,m)
}

