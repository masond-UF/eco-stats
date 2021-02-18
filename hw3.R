# HW 3—Eco stats
# 10 Feb 2021—David S. Mason
library(ggplot2)
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

# Q6b ####
pop <- c(200, 183, 171, 154, 140, 128, 113, 97, 87, 84, 71,
			 66, 61, 58, 53, 48, 46, 43, 40, 35, 33, 31, 28, 26)
p <- seq(0,1,0.01)
ln.ll.p <- vector("list", length(pop))

for(i in 1:length(pop)){
	if(pop[i] == 200){
	ln.ll.p[[i]] <- dbinom(pop[i], pop[i], p, log = TRUE)
	}
	else{
	ln.ll.p[[i]] <- dbinom(pop[i], pop[i-1], p, log = TRUE)
	}
}

joint.probability <- as.vector(ln.ll.p[[1]]+ln.ll.p[[2]]+ln.ll.p[[3]]+
		ln.ll.p[[4]]+ln.ll.p[[5]]+ln.ll.p[[6]]+ln.ll.p[[7]]+
		ln.ll.p[[8]]+ln.ll.p[[9]]+ln.ll.p[[10]]+ln.ll.p[[11]]+
		ln.ll.p[[12]]+ln.ll.p[[13]]+ln.ll.p[[14]]+ln.ll.p[[15]]+
		ln.ll.p[[16]]+ln.ll.p[[17]]+ln.ll.p[[18]]+ln.ll.p[[19]]+
		ln.ll.p[[20]]+ln.ll.p[[21]]+ln.ll.p[[22]]+ln.ll.p[[23]]+ln.ll.p[[24]])

joint.probability <- as.data.frame(joint.probability)

joint.probability <- cbind(joint.probability, p)

ggplot(d = joint.probability, aes(x = p, y = joint.probability))+
			 	geom_line(color = "red")+
				geom_vline(xintercept = p[which.max(joint.probability$joint.probability)], 
									 color="black",size=1)+
				xlab("Joint probability of survival")+
				ylab("-Log Likelihood")+
				scale_x_continuous(breaks=seq(0, 1, 0.1))+
				scale_y_continuous(breaks=seq(-8000,0, 1000))+
				theme_classic()

return(joint.probability)

# Q6c ####

output <- list()
pop <- 200
p <- 0.90

for(j in 1:2000){
	d <- vector()
	pop <- 200
		for(i in 1:24){
		pop <- rbinom(1,pop,p)
		d[i] <- pop
		}
	output[[j]] <- d
}

# Q6d ####

mle.output <- vector()
p <- seq(0,1,0.01)

for(i in 1:2000){
 	current.vec <- output[[i]]
 	ln.ll.p <- vector("list", length = 24)
			for(j in 1:24){
				if(current.vec[j] == max(current.vec)){
				ln.ll.p[[j]] <- dbinom(current.vec[j],
														 current.vec[j], p, log = TRUE)
					}
				else{
				ln.ll.p[[j]] <- dbinom(current.vec[j], current.vec[j-1], p, log = TRUE)
				}
			}
	  joint.probability <- as.vector(ln.ll.p[[1]]+ln.ll.p[[2]]+ln.ll.p[[3]]+
												ln.ll.p[[4]]+ln.ll.p[[5]]+ln.ll.p[[6]]+ln.ll.p[[7]]+
												ln.ll.p[[8]]+ln.ll.p[[9]]+ln.ll.p[[10]]+ln.ll.p[[11]]+
												ln.ll.p[[12]]+ln.ll.p[[13]]+ln.ll.p[[14]]+ln.ll.p[[15]]+
												ln.ll.p[[16]]+ln.ll.p[[17]]+ln.ll.p[[18]]+ln.ll.p[[19]]+
												ln.ll.p[[20]]+ln.ll.p[[21]]+ln.ll.p[[22]]+ln.ll.p[[23]]+ln.ll.p[[24]])
		
		joint.probability <- as.data.frame(joint.probability)
		joint.probability <- cbind(joint.probability, p)
		
		mle.p <- joint.probability$p[joint.probability$joint.probability == max(joint.probability$joint.probability)] 
 		mle.output[i] <- mle.p
}

mle.output <- as.data.frame(mle.output)
mle.output$MLE <- mle.output$mle.output

ggplot(d = mle.output, aes(x = MLE))+
				geom_histogram(bins = 10)+
				ylab("Frequency")+
				theme_classic()

# Q7 ####
input <- list(seq(0:8), seq(0:12), seq(0:25),
					 seq(0:50), seq(0:100), seq(0:200))
size <- c(50, 100, 200, 400, 800, 1600)
p <- 0.0588
m <- c(12, 25, 50, 100, 200, 400)
n <- c(200, 400, 800, 1600, 3200, 6400)
k <- c(50, 100, 200, 400, 800, 1600)

for(i in 1:6){ # SCROLL BACK SIX PLOTS TO SEE THE BEGINNING
	x <- input[[i]]
	par(mfrow=c(1,2))
	plot(dbinom(x = x, size = size[i], p = p), main = "Binomial") 
	plot(dhyper(x = x, m = m[i], n = n[i], k = k[i]), main = "Hypergeometric") 
}

# Q10 ####
d <- c(74, 72, 51, 6, 6, 6)
goat.mat <- matrix(d, nrow = 3, ncol = 2)
unmarked <- goat.mat[,1] - goat.mat[,2]
d <- c(74, 72, 51, 6, 6, 6, 68, 66, 45)
goat.mat <- matrix(d, nrow = 3, ncol = 3)
rownames(goat.mat) <- c('Flight 1', 'Flight 2', 'Flight 3')
colnames(goat.mat) <- c('Total goats', 'Marked goats', 'Unmarked goats')
print(goat.mat)


t <- seq(75,400,1) # total animals N
m <- 13 # marked animals (t)
n <- goat.mat[1,1] # k
x <- goat.mat[1,2] # r

ln.ll.h <- (dhyper(goat.mat[1,2], m, t-m, goat.mat[1,1]))*
					 (dhyper(goat.mat[2,2], m, t-m, goat.mat[2,1]))*
					 (dhyper(goat.mat[3,2], m, t-m, goat.mat[3,1]))
table <- as.data.frame(cbind(ln.ll.h, t))

ggplot(d = table, aes(x = t, y = ln.ll.h))+
				geom_line(color = "red")+
				geom_vline(xintercept = t[which.max(table$ln.ll.h)], 
									 color="black",size=1)+
				xlab("Total population")+
				ylab("Likelihood")+
				scale_x_continuous(breaks=seq(0, 400, 50))+
				theme_classic()
	