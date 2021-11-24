## Arun Agarwal
## Applied Statistics and Data Science
## Problem Set 1
## 10/25/2021

#1.9: Plotting the log-likelihood l(lambda) in R for the specific sample of 7 articles (12, 4, 5, 3, 7, 5, 6):

#The given specific sample being stored as well as its mean
x = c(12, 4, 5, 3, 7, 5, 6)
mean.x = mean(x)
#print(mean.x)

lambda.seq = seq(0.1, 20, by = 0.1)
#print(length(lambda.seq))
l = numeric()
#print(l)

for(i in 1:length(lambda.seq))
{
  l[i] = sum(log(dpois(x, lambda = lambda.seq[i])))
}

#par(mar = c(5,5,5,5), oma = c(5,5,5,5)) #Changing the plot size

#Plotting the log-likelihood
plot(lambda.seq,l,xlab = 'lambda', ylab = 'log-likelihood', type = 'p', main = 'MLE')
abline(v = mean.x, col = 2)

#2.10: Plotting the log-likelihood l(lambda) in R for the specific sample of 5 articles:

x = c(17, 9, 18, 12, 11)
y = c(1730,947,1830,1210,1100)

#Our Maximum Likelihood estimator
v.hat = 53/6.817 

v.seq = seq(0.1, 20, by=0.1)
l = numeric()

for(i in 1:length(v.seq))
{
    l[i] = sum(log(dpois(x, lambda = v.seq[i]*y/1000)))
}

#Changing the Plot Size
#par(mfrow = c(1,1), mar = c(5,5,5,5), oma = c(5,5,5,5))

plot(v.seq,l,xlab = 'v', ylab = 'log-likelihood', type = 'p', main = 'MLE')
abline(v = v.hat, col = 2)

#3.3: R Simulation

#Givens:
n = 1000
pi = 0.3
lambda_p = 30
theta_o = 0.02


z = rbinom(n = n, size = 1, prob = pi)
x = numeric()

for(i in 1:n)
{
    if (z[i] == 1)
    {
        x[i] = rpois(1,lambda_p)
    }
    else if (z[i] == 0)
    {
        x[i] = rbinom(1, size = 1000, prob = theta_o)
    }
}

#Poisson Distribution Histogram
hg1 <- hist(x[z==1], breaks = 20, plot = FALSE)

#Binomial Distribution Histogram
hg0 <- hist(x[z==0], breaks = 20, plot = FALSE)

hg1$counts = hg1$density
hg0$counts = hg0$density

c0 = rgb(150,220,230, max = 255, alpha = 80, names = 'lt.blue')
c1 <- rgb(100,200,100, max = 255, alpha = 80, names = 'lt.green')

print("Note: these two histograms have the same design, as desired/constructed")

#Adjusting the size of the plot:
#par(mfrow = c(1,1), mar = c(5,5,5,5), oma = c(5,5,5,5))

#Plotting 1st Histogram (Binomial Distribution)
plot(hg0, col = c0, xlim = c(0,50), main = 'Histograms for Q 3.3', xlab = 'x', ylab = 'Density')

#Plotting 2nd Histogram (Poisson Distribution)
plot(hg1, col = c1, add = TRUE)

legend('topright', legend = c('Binomial', 'Poisson'), pch = c(15,15), col = c(c0,c1))

#YOU CAN IGNORE THIS BLOCK! IT WAS JUST FOR CALCULATION!
#3.7 R Simulation

#Givens:
n = 8
pi = 0.3
lambda_p = 6.25
theta_o = 0.78125


z = rbinom(n = n, size = 1, prob = pi)
x = numeric()

for(i in 1:n)
{
    if (z[i] == 1)
    {
        x[i] = rpois(1,lambda_p)
        print(x[i])
    }
    else if (z[i] == 0)
    {
        x[i] = rbinom(1, size = 8, prob = theta_o)
        print(x[i])
    }
}


