## Benjamin E. Hardisty 2011

## Load lattice, latticedl, and latticeExtra
library(lattice)
library(latticedl)
library(latticeExtra)

## First I load my brhist data. I saved "brhist.txt" as "Brainsize":
setwd('C:/Documents and Settings/Ben/My Documents')
Brainsize <- read.table("brhist.txt", header=T)

## Preliminary data viz: a scatter plot of "brvol" (brain volume) against "age":
xyplot(brvol ~ age, data=Brainsize, xlim=c(max(Brainsize$age)+0.1, min(Brainsize$age)-0.1)) 

## Model the data with a linear model and graph the model fit w/the original data:
BrainScatter <- xyplot(brvol ~ age, data=Brainsize)
BrainScatter <- BrainScatter + layer(panel.smoother(y ~ x, method="lm"))
BrainScatter

## Plot the residuals against "age w/a residual dependence plot:
Brainmodel <- lm(brvol ~ age, data=Brainsize)
resBrain <- residuals(Brainmodel)

getlim <- function(x) {
    r <- range(x)
    d <- diff(r)/20
    return( c( r[1]-d, r[2]+d ))
}
resBrainlim <- getlim(resBrain)

Brainresdep <- function(resBrain, x, ...) {
    Brainplt <- xyplot(resBrain ~ age, ylab="Residuals", ...)
    Brainplt <- Brainplt + layer(panel.loess(x, y, degree=1, span=0.75))
    Brainplt <- Brainplt + layer(panel.abline(h=0, col="grey"))
    return(Brainplt)
}

Brainplt <- xyplot(resBrain ~ age, data=Brainsize)
Brainplt <- update(Brainplt, ylab="Residuals", xlab="Age")
Brainplt <- Brainplt + layer(panel.loess(x, y, degree=2, span=0.75))
Brainplt <- Brainplt + layer(panel.abline(h=0, col="red"))
Brainplt
 
## Remarks: The residual fit isn't great, the line broadly tracks the data
## but there are so many points above, below and beyond the beginning
## and end points of our line that I scrapped that model.
## Also, there is a huge number of data points that fall both above
## and below our line at y=0, which tells us that most of the variation
## in the residuals is not explained by the model.
## The errors are huge for the linear regression model
## and a significant number of data points are not accounted for by it.
## Perhaps it's time to move on to some more sophisticated models?
## Ecologists seem to love linear models, perhaps because they're so easy to fit?
## I agree with Lev Ginzburg that how we model our data
## can change what the data say to us, sometimes in misleading ways.
## However, I hasten to add that here, the data hasn't told me anything yet.

## So, plot the data and fit a quadratic line to it instead:
Brainmodel2 <- xyplot(brvol ~ age, data = Brainsize)
Brainmodel2 <- Brainmodel2 + layer(panel.smoother(y ~ x+I(x^2), method="lm"))
Brainmodel2

## Find the residuals and make a residual dependence plot,
## along with a line at y=0:
BrainLM <- lm(brvol ~ age + I(age^2), data=Brainsize)
owfitBrain <- owfit$residuals
resBrain2 <- residuals(BrainLM)

resdep <- function(resBrain2, x, ...) {
    Brainplt2 <- xyplot(resBrain2 ~ age + I(age^2), ylab="Residuals", ...)
    Brainplt2 <- Brainplt2 + layer(panel.loess(x, y, degree=2, span=0.75))
    Brainplt2 <- Brainplt2 + layer(panel.abline(h=0, col="grey"))
    return(Brainplt2)
}
BrainResiduals <- resdep(residuals(BrainLM), data=Brainsize)

getlim <- function(x) {
    r <- range(x)
    d <- diff(r)/20
    return( c( r[1]-d, r[2]+d ))
}
Brainlim2 <- getlim(resBrain2)

Brainplt2 <- xyplot(resBrain2 ~ age + I(age^2), data=Brainsize)
Brainplt2 <- update(Brainplt2, ylab="Residuals", xlab="Age")
Brainplt2 <- Brainplt2 + layer(panel.loess(x, y, degree=2, span=0.75))
Brainplt2 <- Brainplt2 + layer(panel.abline(h=0, col="red"))
Brainplt2

## The quadratic fit is more accurate model of the data,
## it really takes care of the extreme values that spread righward along the x axis.
## Is this model great? Not by any means, but it makes predictions that
## are slightly more accurate than the straight line fit made.
## I think though that it is time to move on to a, hopefully,
## better model than these first 2.

## Let's try a loess model with span=0.75:
BrainLoess <- xyplot(brvol ~ age, data=Brainsize, aspect="1.0")
BrainLoess <- BrainLoess + layer(panel.smoother(x, y, span=0.75))
BrainLoess <- update(BrainLoess, col="green", main="Evolution of Hominin Brain Capacity")
BrainLoess <- update(BrainLoess, xlab= "Specimen Age (millions of years)", ylab="Brain Volume (cc)")
BrainLoess


LBrainmodel <- loess(brvol ~ age, data=Brainsize)
resBrainL <- residuals(LBrainmodel)

getlim <- function(x) {
    r <- range(x)
    d <- diff(r)/20
    return( c( r[1]-d, r[2]+d ))
}
resBrainLlim <- getlim(resBrainL)

Brainresdep <- function(resBrainL, x, ...) {
    BrainLplt <- xyplot(resBrainL ~ age, ylab="Residuals", ...)
    BrainLplt <- BrainLplt + layer(panel.loess(x, y, aspect=1.0, span=0.75))
    BrainLplt <- BrainLplt + layer(panel.abline(h=0, col="grey"))
    return(BrainLplt)
}

BrainLplt <- xyplot(resBrainL ~ age, data=Brainsize)
BrainLplt <- BrainLplt + layer(panel.loess(x, y, degree=1.0, span=0.75))
BrainLplt <- BrainLplt + layer(panel.abline(h=0, col="red"))
BrainLplt <- update(BrainLplt, main="Residual Dependence Plot")
BrainLplt <- update(BrainLplt, xlab="Age of Skull Specimen (millions of years)", ylab="Pooled Residuals (brain volume, cc)")
BrainLplt

## Now let's make a spread-location plot using the linear model:
SLBrain <- lm(brvol~age+I(age^2), data = Brainsize)

owfitSLBrain <- owfit$residuals
resSLBrain <- residuals(SLBrain)

SLBrainresdep <- function(resSLBrain, x, ...) {
    BrainLplt <- xyplot(resSLBrain ~ age, ylab="Residuals", ...)
    BrainLplt <- BrainLplt + layer(panel.loess(x, y, aspect=1.0, span=2))
    BrainLplt <- BrainLplt + layer(panel.abline(h=0, col="grey"))
    return(SLBrain)
}

SLBrainplt <- xyplot(sqrt(abs(residuals(SLBrain))) ~ fitted(SLBrain))
SLBrainplt <- SLBrainplt + layer(panel.loess(x,y, span=2))
SLBrainplt <- update(SLBrainplt, aspect=1.0,
              main=list("Spread-Location (SL) Plot", cex=2),
              xlab = "Fitted Brain Volume (cc)",
              ylab = "Square Root Absolute Residual Brain Volume")
SLBrainplt

## How does a log fit change the model's accuracy?
## I chose to use log base 10 (log10) for brain volume:

BrainScatter <- xyplot(log10(brvol) ~ age, data=Brainsize)
BrainScatter <- BrainScatter + layer(panel.smoother(y ~ x, method="lm"))

Brainmodel <- lm(log10(brvol) ~ age, data=Brainsize)
resBrain <- residuals(Brainmodel)

getlim <- function(x) {
    r <- range(x)
    d <- diff(r)/20
    return( c( r[1]-d, r[2]+d ))
}
resBrainlim <- getlim(resBrain)

Brainresdep <- function(resBrain, x, ...) {
    Brainplt <- xyplot(resBrain ~ age, ylab="Residuals", ...)
    Brainplt <- Brainplt + layer(panel.loess(x, y, degree=1, span=0.75))
    Brainplt <- Brainplt + layer(panel.abline(h=0, col="grey"))
    return(Brainplt)
}

Brainplt <- xyplot(resBrain ~ age, data=Brainsize)
Brainplt <- update(Brainplt, ylab="Residuals", xlab="Age")
Brainplt <- Brainplt + layer(panel.loess(x, y, degree=2, span=0.75))
Brainplt <- Brainplt + layer(panel.abline(h=0, col="red"))
Brainplt

Brainmodel2 <- xyplot(brvol ~ age, data = Brainsize)
Brainmodel2 <- Brainmodel2 + layer(panel.smoother(y ~ x+I(x^2), method="lm"))
Brainmodel2

BrainLM <- lm(log10(brvol) ~ age + I(age^2), data=Brainsize)
owfitBrain <- owfit$residuals
resBrain2 <- residuals(BrainLM)

resdep <- function(resBrain2, x, ...) {
    Brainplt2 <- xyplot(resBrain2 ~ age + I(age^2), ylab="Residuals", ...)
    Brainplt2 <- Brainplt2 + layer(panel.loess(x, y, degree=2, span=0.75))
    Brainplt2 <- Brainplt2 + layer(panel.abline(h=0, col="grey"))
    return(Brainplt2)
}
BrainResiduals <- resdep(residuals(BrainLM), data=Brainsize)

getlim <- function(x) {
    r <- range(x)
    d <- diff(r)/20
    return( c( r[1]-d, r[2]+d ))
}
Brainlim2 <- getlim(resBrain2)

Brainplt2 <- xyplot(resBrain2 ~ age + I(age^2), data=Brainsize)
Brainplt2 <- update(Brainplt2, ylab="Residuals", xlab="Age")
Brainplt2 <- Brainplt2 + layer(panel.loess(x, y, degree=1, span=0.75))
Brainplt2 <- Brainplt2 + layer(panel.abline(h=0, col="red"))
Brainplt2

## 4 Redone with base 10 log of brvol
BrainLoess <- xyplot(log10(brvol) ~ age, data=Brainsize, aspect="1.0")
BrainLoess <- BrainLoess + layer(panel.smoother(x, y, span=0.75))
BrainLoess <- update(BrainLoess, col="green", main="Evolution of Hominin Brain Capacity")
BrainLoess <- update(BrainLoess, xlab= "Specimen Age (millions of years)", ylab="Brain Volume (cc)")
BrainLoess

LBrainmodel <- loess(log10(brvol) ~ age, data=Brainsize)
resBrainL <- residuals(LBrainmodel)

getlim <- function(x) {
    r <- range(x)
    d <- diff(r)/20
    return( c( r[1]-d, r[2]+d ))
}
resBrainLlim <- getlim(resBrainL)

Brainresdep <- function(resBrainL, x, ...) {
    BrainLplt <- xyplot(resBrainL ~ age, ylab="Residuals", ...)
    BrainLplt <- BrainLplt + layer(panel.loess(x, y, aspect=1.0, span=0.75))
    BrainLplt <- BrainLplt + layer(panel.abline(h=0, col="grey"))
    return(BrainLplt)
}

BrainLplt <- xyplot(resBrainL ~ age, data=Brainsize)
BrainLplt <- BrainLplt + layer(panel.loess(x, y, degree=1.0, span=0.75))
BrainLplt <- BrainLplt + layer(panel.abline(h=0, col="red"))
BrainLplt <- update(BrainLplt, main="Residual Dependence Plot")
BrainLplt <- update(BrainLplt, xlab="Age of Skull Specimen (millions of years)", ylab="Pooled Residuals (brain volume, cc)")
BrainLplt

## Now plot all 3 together to see which model is better
## If your computer's monitor is as small as mine you'll probably
## want to view the following by expanding your R graphics window.:
plot(Brainplt, split=c(1,1,3,1), more=T)
plot(Brainplt2, split=c(2,1,3,1),  more=T)
plot(BrainLplt, split=c(3,1,3,1), more=F)


# Now use the loess model to make an S-L plot
BrainLogLoess <- xyplot(log10(brvol) ~ age, data=Brainsize, aspect="1.0")
BrainLogLoess <- BrainLogLoess + layer(panel.loess(x, y, aspect=1, span=0.75))
BrainLogLoess <- update(BrainLogLoess, col="green", main="Evolution of Hominin Brain Capacity")
BrainLogLoess <- update(BrainLogLoess, xlab= "Specimen Age (millions of years)", ylab="Brain Volume (cc)")
BrainLogLoess

BrainLL <- lm(log10(brvol)~age+I(age^2), data = Brainsize)

owfitBrainLL <- owfit$residuals
resBrainLL <- residuals(BrainLL)

BrainLLresdep <- function(resBrainLL, x, ...) {
    BrainLplt <- xyplot(resBrainLL ~ age, ylab="Residuals", ...)
    BrainLplt <- BrainLplt + layer(panel.loess(x, y, aspect=1.0, span=2))
    BrainLplt <- BrainLplt + layer(panel.abline(h=0, col="grey"))
    return(BrainLL)
}
BrainLLplt <- xyplot(sqrt(abs(residuals(BrainLL))) ~ fitted(BrainLL))
BrainLLplt <- SLBrainplt + layer(panel.loess(x,y, span=2))
BrainLLplt <- update(BrainLLplt, aspect=1.0,
              main=list("Spread-Location (SL) Plot", cex=2),
              xlab = "Fitted Brain Volume (cc)",
              ylab = "Square Root Absolute Residual Brain Volume")
BrainLLplt

## The spread in the Log10(brvol) Loess Model appears to be uniform, with 
## the spread of y (the square root absolute value residual brvol) 
## increasing as the fitted values of brain volume increase.  This is good for
## our model because it means that spread of your plotted y versus plotted
## x values does not increase in a non-uniform manner.

## Finally, make an r-f plot to see how well our model actually explains the variance
## in the original data
BrainLoess <- xyplot(log10(brvol) ~ age, data=Brainsize, aspect="1.0")
BrainLoess <- BrainLoess + layer(panel.loess(x, y, span = 0.75))
BrainLoess <- update(BrainLoess, col="green", main="Evolution of Hominin Brain Capacity")
BrainLoess <- update(BrainLoess, xlab= "Specimen Age (millions of years)", ylab="Brain Volume (cc)")
BrainLoess

Loessowfit <- oneway(log10(brvol) ~ age, data = Brainsize)
LoessPlt<- LoessPlt + layer(panel.loess(x, y, span = 0.75))
LoessPlt <- rfs(Loessowfit, aspect=1, main = list("Residual-Fit Spread Plot", cex=2), xlab = "Age (Mya)", ylab = "Log10(Brain Volume) (cc)")
LoessPlt

## TAKE AWAY: We often fit a model, find that it doesn't explain much,
## and then try fitting another type of model to our data.
## In addition, using residuals to make r-f plots and s-l plots
## can be a powerful tool to determine fairly easily how much varation your model explains.
## Contention over the speed of changes and the type of changes that occurred in hominin brain
## volume is certainly partly explained by the choice of model to fit to the data.
## Finally, this all also reinforces the notion that "off the shelf" statistics
## are not always the best way to model data.
