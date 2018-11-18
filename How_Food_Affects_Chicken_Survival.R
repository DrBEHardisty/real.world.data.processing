## Benjamin E. Hardisty c2011
## How to Do Maximum Likelihood Analysis in R

library(lattice)
library(latticeExtra)
library(latticedl)

## Here is a practice optimization problem:
## No necessary relation between the variable in the function
## and the variable you GIVE to the function to get an output:
func <- function(y) {  2*y - 3*y*y  }
x <- seq(0, 20, 0.02)
xyplot(func(x)~x, type="l")

## Graphing it on a smaller interval allows us to see that the
## maximum value of the function is at 1/3:
x <- seq(0, 2, 0.02)
xyplot(func(x)~x, type="l")

## Next we can optimize the value of the function to see
## if we have indeed found the maximum of (2y-3y^2)
fit <- optimize(func, interval=c(0,1), maximum=T)

## str(fit) will tell us the straight line fit
str(fit)

plt <- xyplot(func(x)~x, type="l")
plt <- plt + layer(panel.abline(v=fit$maximum,col="gray"))
plt <- update(plt, main=list("Finding Maxima", cex=2))
plt

## To do a log-likelihood test, we first must build a model
## and then derive the model we're testing against it.
## H_0 < H_1; we will estimate lambda at 0, and lambda at 1
## We devise a function for each model, L_0 and L_1,
## plug the likelihood values into each model to derive it's value
## Find Max(L_0) and Max(L-1).
## So, we have L_0, lambda_0 and L_1, and lambda_1.
## How many constraints seperate the Hypothesis 0 & 1? K constraints for each model
## Each Kth constraint is one equation.
## For a+bx, for example, a=0 would mean there is K=1 constraints
## Z=-2ln(L_1/L_0)
## Actually Z is a chi-squared random variable with 
## K degrees of freedom (dof).
## We interpret it in the standard manner, rejecting
## H_1 if P < alpha

## Get the data loaded and ready for use:
setwd('/Users/Authenticated User/Desktop/')
Chicks <- read.table("survival.txt", header=T)
Chicks

## Make a function you want to find the log likelihood of.
## We are estimating the parameter "a", b/c we want to see how 
## food affects chick survival:
ChicksL <- function(a) {
	with(Chicks,
	sum(log(y*sp(x,a) + (1-y)*(1-sp(x, a)))))
	} sp <- function(x,a) {
   (1 - exp(-a*x))^4
}


## What does a basic plot tell us?:
xyplot(ChicksL(x)~x)
a <- seq(0,10,0.02)
A <- sapply(a, ChicksL)
xyplot(A~a, type="l")
## It looks like Max(ChicksL) is {1, 3}

## Now we can optimize the MLE function:
FitChicks <- optimize(ChicksL, interval=c(0,10), maximum=T)
FitChicks

Chickies <- xyplot((A)~a, type="l")
Chickies <- Chickies + layer(panel.abline(v=FitChicks$maximum,col="red"))
Chickies <- update(Chickies, main=list("Finding Maxima", cex=2))
Chickies

LogL1 <- FitChicks$objective
LogL0 <- ChicksL(1.5)
Z <- -2*(LogL0 - LogL1)

Pval <- 1- pchisq(Z,1)
Pval
