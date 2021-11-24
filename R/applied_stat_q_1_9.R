#1.9:

x = c(12, 4, 5, 3, 7, 5, 6)
mean.x = mean(x)
#print(mean.x)

lambda.seq = seq(0.1, 20, by = 0.1)
#print(length(lambda.seq))
l = numeric()

for(i in 1:length(lambda.seq)){
  l[i] = sum(log(dpois(x, lambda = lambda.seq[i])))
}

#par(mar = c(5,5,5,5), oma = c(5,5,5,5)) #Changing the plot size
plot(lambda.seq,l,xlab = 'lambda', ylab = 'log-likelihood', type = 'p', main = 'MLE')
abline(v = mean.x, col = 2)