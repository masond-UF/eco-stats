# HW 4—David Mason—Ecological Statistics
# Question 1 ####
d.q1 <- matrix(c(1, 51.85, 2, 2, 48.50, 2, 3, 50.20, 1, 4, 52.53,
								 1, 5, 65.37, 4, 6, 70.12, 5), ncol = 3, byrow = TRUE)
colnames(d.q1) <- c("day", "Total effort", "Total steeleheads caught")
d.q1 <- as.data.frame(d.q1)

x = d.q1[,3] 

negll.poisson <- function(y, t){
	lambda <- y/t
	llike <- log(((exp(1)^(lambda))*((lambda)^(x)))/factorial(x))
	negll <- -sum(llike)
	return(negll)
}

my.guess <- mean(d.q1$`Total steeleheads caught`/d.q1$`Total effort`)


negll.poisson(y = d.q1[,3], t = d.q1[,2], x = d.q1[,3])
ml.estim <- optim(par = my.guess, fn = negll.poisson, y = d.q1[,3],
									t = d.q1[,2], hessian = TRUE)

# Question 1 NEW ####
lambda.guess <- sum(d.q1$`Total steeleheads caught`)/6
t <- sum(d.q1$`Total effort`)/6
x = d.q1[,3] 

negll.poisson <- function(lambda.guess){
	lambda <- lambda.guess
	llike <- log(((exp(1)^(lambda*t))*((lambda*t)^(x)))/factorial(x))
	negll <- -sum(llike)
	return(negll)
}

ml.estim <- optim(par = lambda.guess, fn = negll.poisson)

# Question 3 ####

# 3d
samp.1 <- c(2, 7, 3, 2, 4, 6)
samp.2 <- c(1, 1, 1, 2, 2, 0, 1, 1)
samp.3 <- c(2, 4, 3, 0, 2, 4, 1)

# Test 1
samp.1.2.3. <- c(samp.1,samp.2,samp.3)
mu.hat <- sum(samp.1.2.3.)/length(samp.1.2.3.)
mu1.hat <- sum(samp.1)/length(samp.1)
mu2.hat <- sum(samp.2)/length(samp.2)
mu3.hat <- sum(samp.3)/length(samp.3)
lnL0 <- log(prod(((exp(1)^mu.hat)*(mu.hat^samp.1.2.3.))/factorial(samp.1.2.3.)))
lnL1 <- log((prod(((exp(1)^mu1.hat)*(mu1.hat^samp.1))/factorial(samp.1)))*
				(prod(((exp(1)^mu2.hat)*(mu2.hat^samp.2))/factorial(samp.2)))*
				(prod(((exp(1)^mu3.hat)*(mu3.hat^samp.3))/factorial(samp.3))))

qchisq(p=.0125, df=2, lower.tail=FALSE) # 8.764053
t1.g2 <- -2*(lnL0 - lnL1)  # 12.08075 
# G2 obs > G2 critical

# Test 2
samp.1.2.3. <- c(samp.1,samp.2,samp.3)
samp.1.2. <- c(samp.1,samp.2)
mu.hat.1.2 <- sum(samp.1.2.)/length(samp.1.2.)

lnL0 <- log(prod(((exp(1)^mu.hat)*(mu.hat^samp.1.2.3.))/factorial(samp.1.2.3.)))
lnL2 <- log((prod(((exp(1)^mu.hat.1.2)*(mu.hat.1.2^samp.1.2.))/factorial(mu.hat.1.2)))*
				(prod(((exp(1)^mu.hat.3)*(mu.hat.3^samp.3))/factorial(samp.3))))

qchisq(p=.0125, df=1, lower.tail=FALSE) #6.238533
t2.g2 <- -2*(lnL0 - lnL2) # 16.40134 
# G2 obs > G2 critical

# Test 3
samp.2.3. <- c(samp.2, samp.3)
mu.hat.2.3 <- sum(samp.2.3.)/length(samp.2.3.)

lnL0 <- log(prod(((exp(1)^mu.hat)*(mu.hat^samp.1.2.3.))/factorial(samp.1.2.3.)))
lnL3 <- log((prod(((exp(1)^mu1.hat)*(mu1.hat^samp.1))/factorial(samp.1)))*
				(prod(((exp(1)^mu.hat.2.3)*(mu.hat.2.3^samp.2.3.))/factorial(samp.2.3.))))

qchisq(p=.0125, df=1, lower.tail=FALSE) #6.238533
t3.g2 <- -2*(lnL0 - lnL3) # 9.04822
# G2 obs > G2 critical

# Test 4
samp.1.3. <- c(samp.1,samp.3)
mu.hat.1.3 <- sum(samp.1.3.)/ length(samp.1.3.)

lnL0 <- log(prod(((exp(1)^mu.hat)*(mu.hat^samp.1.2.3.))/factorial(samp.1.2.3.)))
lnL4 <- log((prod(((exp(1)^mu2.hat)*(mu2.hat^samp.2))/factorial(samp.2)))*
				(prod(((exp(1)^mu.hat.1.3)*(mu.hat.1.3^samp.1.3.))/factorial(samp.1.3.))))

qchisq(p=.0125, df=1, lower.tail=FALSE) #6.238533
t4.g2 <- -2*(lnL0 - lnL4) # 8.999312
# G2 obs > G2 critical

# 3e
# Step 1
theta.0 <- c(mu.hat, mu.hat, mu.hat)
theta.1 <- c(mu1.hat, mu2.hat, mu3.hat)

lnL0 <- function(theta.0){
	lnL0.val <-	prod(((exp(1)^theta.0[1])*(theta.0[1]^samp.1))/factorial(samp.1))*
							prod(((exp(1)^theta.0[2])*(theta.0[2]^samp.2))/factorial(samp.2))*
							prod(((exp(1)^theta.0[3])*(theta.0[3]^samp.3))/factorial(samp.3))
	return(lnL0.val)
}
	
lnL1 <- function(theta.1){
	lnL1.val <-	prod(((exp(1)^theta.1[1])*(theta.1[1]^samp.1))/factorial(samp.1))*
							prod(((exp(1)^theta.1[2])*(theta.1[2]^samp.2))/factorial(samp.2))*
							prod(((exp(1)^theta.1[3])*(theta.1[3]^samp.3))/factorial(samp.3))
	return(lnL1.val)
}

# Step 2
lambda.obs <- (-2)*log(lnL0(theta.0 = theta.0)/lnL1(theta.1 = theta.1))

# Step 3
null.func <- function(n1,n2,n3){
	null.func.sim <- list(rpois(n1, lambda = mu.hat),
								rpois(n2, lambda = mu.hat),
								rpois(n3, lambda = mu.hat))
	return(null.func.sim)
}

alt.func <- function(n1,n2,n3){
	alt.func.sim <- list(rpois(n1, lambda = mu1.hat),
								rpois(n2, lambda = mu2.hat),
								rpois(n3, lambda = mu3.hat))
	return(alt.func.sim)
}

sim.null <- list()
sim.alt <- list()

for (i in 1:2000){
	sim.null[[i]] <- null.func(6,8,7)
	sim.alt[[i]] <- alt.func(6,8,7)
	
	sim.null
}

# Step 4

sim.null.theta <- list()
sim.alt.theta <- list()

for(i in 1:2000){

# null theta
sim.null.theta.rd <- c(do.call(sum, sim.null[[i]])/sum(do.call(length, sim.null[[i]][1]),
																do.call(length, sim.null[[i]][2]),
																do.call(length, sim.null[[i]][3])),
												 do.call(sum, sim.null[[i]])/sum(do.call(length, sim.null[[i]][1]),
																do.call(length, sim.null[[i]][2]),
																do.call(length, sim.null[[i]][3])),
												 do.call(sum, sim.null[[i]])/sum(do.call(length, sim.null[[i]][1]),
																do.call(length, sim.null[[i]][2]),
																do.call(length, sim.null[[i]][3])))

sim.null.theta[[1]] <- sim.null.theta.rd

# alt theta
sim.alt.theta.rd <- c(do.call(sum, sim.null[[i]][1])/do.call(length, sim.null[[i]][1]),
										do.call(sum, sim.null[[i]][2])/do.call(length, sim.null[[i]][2]),
										do.call(sum, sim.null[[i]][3])/do.call(length, sim.null[[i]][3]))

sim.alt.theta[[i]] <- sim.alt.theta.rd
}

# negative log likelihood ratio

sim.null <- as.numeric(sim.null)

negloglik.rat <- vector(length = 2000)
L0.boot <- vector(length=2000)
L1.boot <- vector(length=2000)



for(i in 1:2000){

L0.boot <- vector()
L1.boot <- vector()	
	
L0.boot <-	prod(((exp(1)^sim.null.theta[[i]][1])*(sim.null.theta[[i]][1]^unlist(sim.null[[i]][1]))/factorial(unlist(sim.null[[i]][1]))))*
						prod(((exp(1)^sim.null.theta[[i]][2])*(sim.null.theta[[i]][2]^unlist(sim.null[[i]][2]))/factorial(unlist(sim.null[[i]][2]))))*
						prod(((exp(1)^sim.null.theta[[i]][3])*(sim.null.theta[[i]][3]^unlist(sim.null[[i]][3]))/factorial(unlist(sim.null[[i]][3]))))

L1.boot <-	prod(((exp(1)^sim.alt.theta[[i]][1])*(sim.alt.theta[[i]][1]^unlist(sim.alt[[i]][1]))/factorial(unlist(sim.alt[[i]][1]))))*
						prod(((exp(1)^sim.alt.theta[[i]][2])*(sim.alt.theta[[i]][2]^unlist(sim.alt[[i]][2]))/factorial(unlist(sim.alt[[i]][2]))))*
						prod(((exp(1)^sim.alt.theta[[i]][3])*(sim.alt.theta[[i]][3]^unlist(sim.alt[[i]][3]))/factorial(unlist(sim.alt[[i]][3]))))

negloglik.rat[i] <- (-2)*log(L0.boot/L1.boot)

}

