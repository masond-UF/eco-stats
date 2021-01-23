# 23 January 2021
# Exercise 1 ####
# 1. Create a sequence of all odd numbers from 1 to 11.
odd.seq <- seq(1,11,2)
# 2. Create a sequence of even numbers from 2 to 1000.
even.seq <- seq(2,1000,2) 
# 3. Declare a vector with the city names Seattle, Amherst, Bozeman, and Gainesville
city.seq <- c("Seattle", "Amherst", "Bozeman", "Gainesville")
# Exercise 2 ####
# 1. In the matrix x replace all values greater than 20 with -1.
y<- c(18,32,15,-7,12,19)
x<-matrix(data=y,nrow=2,ncol=3)
a <- which(x>20,arr.ind=TRUE)
x[a] <- -1 
# 2. Calculate x*x (elementwise multiplication).
x*x
# 3. Type in the command window t(x)%*%x and compare the results to the results of x*x. Why are they different?
# The t command transposed the matrix, and the %*% rotated the matrix.
# 4. Type in the command window x[1,]. What is the output? How would you extract the first column of x?
# This command returned the first row. to return the first column:
x[,1]
# Exercise 3 ####
# 1. Interpret the following input and output
as.numeric(F)
# This means that zero is the numerical representation of false.
as.numeric(T)
# This means that one is the numerical representation of true
# 2. Try the following lines and explain the differences in output and why it
# is important to keep track of parentheses.
((1+4)^2) + 2/4
((1+4)^2 + 2)/4
# The parenthesis govern order of operations.
# 3. Interpret the following input and output
!F
# The opposite (!) of false is true.
T|F
# The union of true and false is true.
T&F
# The intersection of true and false is true.
# Exercise 4 ####
# 1. Look up the help file for the variance function var by typing ?var. 
# What is the x argument? What do the other arguments mean?
?var
# x is the input, which can be a vector or dataframe
# adding another matrix or vector (y) can teturn corr and cov
# method can be used to dictate the method of correlation
# na.rm can be used to remove NAs
# Exercise 5 ####
# 1. For the the vector x defined above calculate the coefficent of 
# variation (the standard deviation over the mean).
x <- c(8,12,5,4,19,2,1)
cv <- sd(x)/mean(x)
# 2. Modify the quantile statement above to return the 25th, 50th and 75th percentiles. 
quantile(x, probs=c(0.25, 0.5, 0.75))
# 3. Try replacing the cbind command in the variance calculation above with rbind. Ex-
quantile(x, probs=c(0.25, 0.5, 0.75))

# plain the difference in output.
# Exercise 6 ####
# 1. Try the following
set.seed(6)
rnorm(5,0,1)
set.seed(6)
rnorm(5,0,1)
# now try
set.seed(6)
rnorm(5,0,1)
rnorm(5,0,1)
# Explain the different outputs.
# 2. What happens if you change the seed in the fourth line?
# Exercise 7 ####
# 1. Try to read a file that you create into R. Some hints: 
# If using Excel, save file as ”Tab Delimited Text (.csv)”. 
# If you include row names in your file you need to include 
# append=T as an argument in read.table
# Exercise 8 ####
# 1. Write a while loop that finds the numerical limit of R. 
# This means you need to find the power x of 10x that R treats as infinite. 
# You can use the function is.finite().
# 2. Write a for loop that will print out ith iteration where i goes from 1 to 100.
# 3. Use a for loop to write a simple yearly population growth model.
# Let the initial population size be 10 and assume that each 
# individual has 2 offspring, then dies.
# Bonus 1 ####
# 1. Construct a function that calculates a one or two sided t-test, for an 
# arbitrary t-statistic and sample size. You can use the pt function.
# Bonus 2 ####
# 1. Modify the exp.growth function to make the variable a a random variable.
# Use the normal distribution random number generator rnorm. 