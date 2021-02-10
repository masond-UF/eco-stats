# HW 3—Eco stats
# 10 Feb 2021—David S. Mason
# Q3 ####
x <- seq(-5, 15, 0.1)

plot(x, dnorm(x, mean = 2, sd = 1.5), type = "l",
     ylim = c(0, 0.4), ylab = "", lwd = 2, col = "red")
lines(x, dnorm(x, mean = 4, sd = 1.5), col = "blue", lty = 1, lwd = 2)
lines(x, dnorm(x, mean = 6, sd = 1.5), col = "green", lty = 1, lwd = 2)

Mean <- 21.37
Sd <- 4.55

pnorm(30, Mean, Sd) - pnorm(20, Mean, Sd) 
# 0.5893969 or 58.94% of ACT test scores between 20-30
# Q4 ####
samp <- c(5,10,50,100,1000)

for(j in 1:length(samp)){
	mat <- matrix(ncol = samp[j], nrow = 500)
	means <- vector()
			for(i in 1:500){
				mat[i,] <- rnorm(n = samp[j], mean = 100, sd = 30)
				means[i] <- mean(mat[i,]) # a
			}
	hist(means) # b
}
# c = larger numbers of trials get closer to expected mean
# d = the theoretical mean of the sampling mean is the population mean?
# d = PDF of sampling distribution approaches normal distribution at large sample sizes


