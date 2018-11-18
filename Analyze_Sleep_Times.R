## Project 7
library(lattice)
library(latticeExtra)
source("http://www.anthro.utah.edu/~rogers/datanal/R/quanttst.r")
source("http://www.anthro.utah.edu/~rogers/datanal/R/scatboot.r")

data(msleep, package="ggplot2")
Omnivore <- subset(msleep, vore=="omni")
Herbivore <- subset(msleep, vore=="herbi")
TSleep <- subset(msleep, sleep_total=="TSleep")

## Make a quantile-quantile plot:
Vores <- subset(msleep, vore=="omni" | vore=="herbi")
Plt <- qq(vore ~ sleep_total, data=Vores)

## Do a t-test:
with(subset(msleep,vore=="omni" | vore=="herbi"), t.test(sleep_total ~ vore))

## Do a quantile test with quanttst.  Nreps=100000.:
## Who sleeps longer, omnivores or herbivores?:

function(x, y, pr=c(0.1, 0.25, 0.5, 0.75, 0.9), nreps=100000) {
    qx <- quantile(x, probs=pr, na.rm=T)
    qy <- quantile(y, probs=pr, na.rm=T)
    d.obs <- sum(abs(qx-qy))
    nx <- length(x)
    ny <- length(y)
    tail.prob <- 0
    
    for(i in 1:nreps) {

        xy <- sample(c(x,y))           
        x <- xy[1:nx]                 
        y <- xy[seq(nx+1,nx+ny)]       
        qx <- quantile(x, probs=pr, na.rm=T)
        qy <- quantile(y, probs=pr, na.rm=T)
        d.sim <- sum(abs(qx-qy))
        if(d.sim >= d.obs)
             tail.prob <- tail.prob + 1
     }
     tail.prob <- tail.prob / nreps
     return(tail.prob)
}

## We want to see if the sleep_total of omnivores is different than the sleep_total of herbivores
OVores <- with(Vores, sleep_total[vore=="omni"])
HVores <- with(Vores, sleep_total[vore=="herbi"])
quanttst(OVores, HVores)
## Value from 100,000 reps when quanttst(OVores, HVores) is run = .77.
## Value from quanttst(OVores, sleep_total) = 0.78, quanttst(HVores, sleep_total) = 0.98. 

## It appears that the sleep times ARE different between omnivores and herbivores.
## The quantile-quantile plot shows a significant difference between the quantiles
## of how long omnivores sleep and how long herbivores sleep.
## However, the t-test finds NO significant difference between omnivore total sleep time
## and herbivore total sleep time, when testing to see if the difference
## between means of the two types of organisms are significantly different.
## Finally, the quantile test ("quanttst") also shows that the differences in sleep times
## are not significantly different.

## Subset the data for use in the following:
S <- subset(sleep_total, vore=="omni", msleep)
R <- subset(sleep_rem, vore=="omni", msleep)

## Now plot everything:
Sleepy <- xyplot(S ~ R, main="Total Sleep (S) vs REM Sleep (R) in Omnivores", data=msleep)
Sleepy <- Sleepy + layer(panel.loess(x, y, degree = 2, span=1, col="red"))

## Given the plots, this definitely looks like a linear relationship:
## when sleep_total goes up, sleep_rem goes up.
## So far we have this xyplot and our quantile-quantile plot that both imply
## a relationship,
## but the 2 other methods do not imply a relationship (quanttst & t-test).

## Finally, what do common tests of correlation tells us?:
cor.test(S, R, method="pearson")
## Pearson's correlation = 0.4925817

cor.test(S, R, method="spearman")
## Spearman's correlation = 0.1378241

cor.test(S, R, method="kendall")
## Kendall's correlation = 0.0802716
