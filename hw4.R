# HW 4—David Mason—Ecological Statistics
# Question 1 ####
d.q1 <- matrix(c(1, 51.85, 2, 2, 48.50, 50.20, 3, 50.20, 1, 4, 52.53,
								 1, 5, 65.37, 4, 6, 70.12, 5), ncol = 3, byrow = TRUE)
colnames(d.q1) <- c("day", "Total effort", "Total steeleheads caught")
d.q1 <- as.data.frame(d.q1)
d.q1$fish_hr <- (d.q1$`Total steeleheads caught`)/(d.q1$`Total effort`)
# x = observations, s = per units


my.guess <- sum(d.q1$`Total steeleheads caught`)/sum(d.q1$`Total effort`)

x <- seq(0,30 , 1)

negll.poisson <- function(guess, obs.vec, xvals){
	lambda <- guess[1]
	p.x <- ((exp(1)^(lambda*t))*((lambda*t)^(x)))/factorial(x)
	llike <- dpoison(x=obs.vec, size = 1, prob = p.x, log = TRUE)
	negll <- -sum(llike)
	return(negll)
}



my.guess <- sum(d.q1$`Total steeleheads caught`)/sum(d.q1$`Total effort`)
negll.poisson(guess = my.guess,
						 obs.vec = d.q1[,2], xvals = d.q1[,1])

ml.estim <- optim(par = my.guess, fn =negll.poisson, 
									method = "Nelder-Mead", obs.vec =  d.q1[,2],
									xvals = d.q1[,1], hessian = TRUE)

# Question 2 ####
