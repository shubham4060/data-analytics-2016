#setting the parameters for drawing samples using poison's distribution
sample=1000
n=20
lambda = 50

#generating a matrix where each row corresponds to one replication/sample of size n.
mat <- matrix(rpois(n, lambda=lambda),ncol=n)

#filling up of above matrix
for (i in 2:1000) {
  mat1 <- matrix(rpois(n, lambda=lambda),ncol=n) 
  mat<-rbind(mat, mat1)
}

#view the above formed matrix
View(mat)

#calculating row wise means and variances for mat
means <- rowMeans(mat)
variances <- apply(mat, 1, var)

#histogram for Ploting the sampling distributions of means and variances
hist(means, xlab="Means of Samples",border="blue",col="green",main="Mean Sampling Distribution", breaks=sample/10)

hist(variances, xlab="Variances of Samples",border="blue",col="green",main="Variance Sampling Distribution", breaks=sample/10)