## Benjamin E. Hardisty 2011

## The setup: there are male and female crabs. The crabs are blue and orange.
## The question: Do crab carapace lengths differ signficantly due to sexual selection
## within phenotypes? We will use a few different methods to see if the hypothesis
## is confirmed or disconfirmed.

## Make a bwplot:
spsex <- with(crabs, interaction(sp,sex))
spsex <- ordered(spsex, levels=c("O.M","O.F","B.M","B.F"))
bwplot(CL ~ spsex, data=crabs)

## Some pairs appear to differ, according to the above plotting method:
## O.M/O.F: O.F > O.M
## B.M/B.F: B.M > B.F
## O.F/B.F: O.F > B.F
## O.M/B.M: O.M = B.M

## We can compare qq plots to get a quick idea of whether carapace length is Gaussian:
QQCrabs <- qqmath( ~ CL | spsex, data=crabs, xlab = "Unit Normal Quantile", ylab="Crab Carapace Length", col="purple")

QQCrabs <- QQCrabs + layer(panel.grid())
QQCrabs <- QQCrabs + layer(panel.qqmathline(...))
QQCrabs

## Interpretation of the q-q plots:
## O.M/O.F: differ only at lowest and highest quantiles
## B.M/B.F: differ only at lowest and highest quantiles
## O.F/B.F: differ only at lowest and highest quantiles
## O.M/B.M: differ only at lowest and highest quantiles

## Now let's test for pairwise differences between male and female crabs of each color,
## with mediantst:
OM <- with(crabs, subset(CL, sp=="O" & sex=="M"))
OF <- with(crabs, subset(CL, sp=="O" & sex=="F"))
BM <- with(crabs, subset(CL, sp=="B" & sex=="M"))
BF <- with(crabs, subset(CL, sp=="B" & sex=="F"))

## We use permutation resampling to do so:
mediantst <- function(x, y, nreps=10000) {
	d.obs <- abs(median(x) - median(y))
   	nx <- length(x)
   	ny <- length(y)
   	tail.prob <- 0

	for(i in 1:nreps) {
      	xy <- sample(c(x,y)) 
		x <- xy[1:nx]
		y <- xy[seq(nx+1,nx+ny)]
		d.sim <- abs(median(x) - median(y))
		if(d.sim >= d.obs)
			tail.prob <- tail.prob + 1
   	}
	tail.prob <- tail.prob / nreps
   	return(tail.prob)
}

## Mediantst gives the following p-values with 10,000 reps
## This tells us that only 2 pairs have significant differences in 
## the values of their medians.
## OM vs BM p = 0.779
## OF vs BF p = 0.0
## OM vs OF p = 0.4585
## BM vs BF p = 0.0338

## But then you might ask: is the absolute difference between quantiles significant?
## Here is a modified version of the above permutator to get quantiles instead of medians:
Quantst <- function(x, y, pr=0.5, nreps=10000) {
	qx <- quantile(x, probs=pr, na.rm=T)
   	qy <- quantile(y, probs=pr, na.rm=T)
	d.obs <- sum(abs(qx - qy))

	nx <- length(x)
	ny <- length(y)
      tail.prob <- 0

	for(i in 1:nreps) {
      	xy <- sample(c(x,y)) 
		x <- xy[1:nx]
		y <- xy[seq(nx+1,nx+ny)]
		qx <- quantile(x, probs=pr, na.rm=T)
		qy <- quantile(y, probs=pr, na.rm=T)
		d.sim <- sum(abs(qx - qy))
		if(d.sim >= d.obs)
			tail.prob <- tail.prob + 1
   	}
	tail.prob <- tail.prob / nreps
   	return(tail.prob)
}

## Use "OM", "BM" etc as inputs for Quantst and get some results:
## OM, OF = 0.4619
## BM, BF = 0.0365
## OM, BM = 0.7743
## OF, BF = 0.0 

## We should also use a t-test to see if there are pairwise differences:
with(subset(crabs, sp=="O"), t.test(CL ~ sex))
with(subset(crabs, sp=="B"), t.test(CL ~ sex))
with(subset(crabs, sex=="M"), t.test(CL~ sp))
with(subset(crabs, sex=="F"), t.test(CL ~ sp))

## According to the t-tests, there is not a significant difference between 
## the OM and OF (p=0.494); OM & BM (p=0.267) and there is a significant
## difference between BM and BF (p=0.0041) & OF & BF (p=0.0000002522)

## Results:
            Comparing Data Analysis Methods

              OM/OF     BM/BF     OM/BM     OF/BF
boxplots        1         1         0          1
=====================================================
qqplots         0         1         0          0
=====================================================
median test     0         1         0          1
=====================================================
quantile test   0         1         0          1 
=====================================================
t-test          0         1         0          1           
=====================================================

## The box plot detected 3 significant differences in the pairs of observations
## However, the quantile-quantile plots appeared to me to do even worse, with all 4 
## plots looking basically the same.  Each of the 4 seemed to me to be virtually 
## indistinguishable from one another.  BM and BF looked slightly different
## but it's hard to say if there was any significance there.  I would thus rate qq plotting 
## as the least sensitive method, followed by box plotting.  It was interesting that 
## the blue males and blue females were significantly different according to 
## the 3 last tests, but the orange males and blue males were not significantly 
## different according to any test.
## In species with strong sexual selection on males we often see such differences
## (according to Dr. Cashdan anyhow).
## It would be nice to know more about our crabs to see what sort of
## ecological differences might account for the size difference observed there,
## and the heterogeneity of the orange males & females. 
## That t-tests and bootstrap sampling both agreed 
## might mean that the simple t-test is as powerful as we think,
## or, on the contrary, it might just mean that using permutation tests of medians,
## or quantiles, are simply not very powerful when compared with the t-tests.
## It would be interesting to see if another type of analysis would give us the 
## same results compared with the final 3 in my table.