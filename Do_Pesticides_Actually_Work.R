## Benjamin E. Hardisty
## Insect sprays, do they do anything?

library(lattice)
InsectSprays

## Here are the qq plots on how each pair of pesticides stacks up against one another.
## I couldn't figure out hot to automate all of this so I did it the old-fashioned way.
## c2011.
 
QQI1 <- qq(spray ~ count, data=InsectSprays, subset=spray=="A" | spray=="B", col="red", main="Pesticide Effectiveness on Insect Count")
QQI2 <- qq(spray ~ count, data=InsectSprays, subset=spray=="A" | spray=="C", col="red")
QQI3 <- qq(spray ~ count, data=InsectSprays, subset=spray=="A" | spray=="D", col="red")
QQI4 <- qq(spray ~ count, data=InsectSprays, subset=spray=="A" | spray=="E", col="red")
QQI5 <- qq(spray ~ count, data=InsectSprays, subset=spray=="A" | spray=="F", col="red")
QQI6 <- qq(spray ~ count, data=InsectSprays, subset=spray=="B" | spray=="C", col="red")
QQI7 <- qq(spray ~ count, data=InsectSprays, subset=spray=="B" | spray=="D", col="red")
QQI8 <- qq(spray ~ count, data=InsectSprays, subset=spray=="B" | spray=="E", col="red")
QQI9 <- qq(spray ~ count, data=InsectSprays, subset=spray=="B" | spray=="F", col="red")
QQI10 <- qq(spray ~ count, data=InsectSprays, subset=spray=="C" | spray=="D", col="red")
QQI11 <- qq(spray ~ count, data=InsectSprays, subset=spray=="C" | spray=="E", col="red")
QQI12 <- qq(spray ~ count, data=InsectSprays, subset=spray=="C" | spray=="F", col="red")
QQI13 <- qq(spray ~ count, data=InsectSprays, subset=spray=="D" | spray=="E", col="red")
QQI14 <- qq(spray ~ count, data=InsectSprays, subset=spray=="D" | spray=="F", col="red")
QQI15 <- qq(spray ~ count, data=InsectSprays, subset=spray=="E" | spray=="F", col="red")

## Now make TMD plots of the above QQ plots and see if they tell us anything new:
TMD1 <- tmd(QQI1, aspect=1, data=InsectSprays)
TMD2 <- tmd(QQI2, aspect=1, data=InsectSprays)
TMD3 <- tmd(QQI3, aspect=1, data=InsectSprays)
TMD4 <- tmd(QQI4, aspect=1, data=InsectSprays)
TMD5 <- tmd(QQI5, aspect=1, data=InsectSprays)
TMD6 <- tmd(QQI6, aspect=1, data=InsectSprays)
TMD7 <- tmd(QQI7, aspect=1, data=InsectSprays)
TMD8 <- tmd(QQI8, aspect=1, data=InsectSprays)
TMD9 <- tmd(QQI9, aspect=1, data=InsectSprays)
TMD10 <- tmd(QQI10, aspect=1, data=InsectSprays)
TMD11 <- tmd(QQI11, aspect=1, data=InsectSprays)
TMD12 <- tmd(QQI12, aspect=1, data=InsectSprays)
TMD13 <- tmd(QQI13, aspect=1, data=InsectSprays)
TMD14 <- tmd(QQI14, aspect=1, data=InsectSprays)
TMD15 <- tmd(QQI15, aspect=1, data=InsectSprays)

## How about them box plots?:
Bb1 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="A" | spray=="B")
Bb2 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="A" | spray=="C")
Bb3 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="A" | spray=="D")
Bb4 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="A" | spray=="E")
Bb5 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="A" | spray=="F")
Bb6 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="B" | spray=="C")
Bb7 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="B" | spray=="D")
Bb8 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="B" | spray=="E")
Bb9 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="B" | spray=="F")
Bb10 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="C" | spray=="D")
Bb11 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="C" | spray=="E")
Bb12 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="C" | spray=="F")
Bb13 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="D" | spray=="E")
Bb14 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="D" | spray=="F")
Bb15 <- boxplot(count ~ spray, data=InsectSprays, subset=spray=="E" | spray=="F")

## You can also make one big box plot:
Plt1 <- bwplot(count ~ spray,
              data=InsectSprays,
              aspect=1,
              main=list("Box Plots", cex=2),
              sub = list("Pesticides in Competition with Each Other",cex=1.5),
              xlab="Spray")
Plt1

## TMD plots and QQ plots are different in that q-q plots 
## show how each of the quantiles of one variable are related to those of 
## another variable.
## TMD plots however, show the influence of one set of means on
## another variable.
## If most of the plotted values on a TMD plot are above or below the mean,
## then that variable which has more points on its side of the plot is
## affecting the independent variable more.
## Of course, TMD plots do not show you the strength of any significant difference,
## only that there is a significant difference.
## QQ plots tell you how much one variable is pulling on another,
## but do not tell you how the influence can be broken down.
## According to the QQ plots, A, B & F are the most effective when
## compared with all of the other Insect Sprays (including compared with each other).
## Each of A, B & F beat 3 other competitors in pair-wise competition.
## It appears that E was the absolute worst performer, only having beaten 1 other pesticide 
## in all the trials conducted.
## The TMD plots tell us that there appear to be 3 combinations of pesticides
## that are equally effective.
## This is the same as the QQ plot findings, where 3 pairs were also found to be equally effective.

## The box plots seem to tell a slightly different story.
## According to them, in one trial A was equally as effective as F,
## and in the rest, there was always one insect spray that was more effective
## than another in every pair-wise comparison.
## Overall, it appears that pesticide B was the most effective and A second,
## with F coming in third.
## This is interesting when we remember that in the other two types of analysis,
## A, B, and F were basically equally effective (each winning 3 of the 'trials'/tests).
## The combined box plot also tells the same story: A, B & F are more effective
## than C, D & E.
## Though in the combined box plot it appears that F is the most effective.

# Let's calculate the mean for each type of pesticide:
meanvals <- with(InsectSprays, tapply(count,spray,mean))
meanvals

## Now show the mean insects killed by each pesticide using a dot plot:
Pesticide <- dotplot(meanvals,aspect=1,
	 	ylab="Type of Pesticide", xlab="Mean Insects Killed", col="red")
Pesticide

## Let's fit a one-way ANOVA model:
owfit <- oneway(count ~ spray, data=InsectSprays, spread = 1)

## Get residulas from the ANOVA model, i.e. take a looky at the deviations from the mean:
res <- owfit$residuals

## Now plot the residuals for each type of pesticide using box-and-whisker plots.
Pests <- boxplot(count ~ res, data=InsectSprays,aspect=0.75,
              ylab = "Insects Killed", xlab="Residuals of Spray Effectiveness")

## So, one might naturally ask, what happens when we pool the residuals?:
Peskyness <- qqmath(~ res | count,
              distribution = PestsResPooled,
              aspect=1, data=InsectSprays,
              layout=c(2,4))
Peskyness
## Add a 45 degree line: it looks like most of the points are far from the line.
## Justifying our earlier decision not to pool the residuals.
Mplt <- update(Peskyness, panel.abline(0,1, col="grey"))
Mplt

## What does the residual spread plot tell us about the data and the model?
ResPests <- rfs(owfit,aspect=1,
           main = list("Residual-Fit Spread Plot", cex=2),
           sub = list("How Many Insects Can We Kill?",cex=1.5))
ResPests

## Because both of the curves are pretty similar,
## we explain roughly the same amount of variance in our data
## with our residuals by themselves, or our points plotted by
## subtracting the mean from each fitted value.
## What this means is that our model is no better than looking at the residuals.
## That is, we would like our model to explain more of the data than what we
## started with but that didn't happen. The f-values are high, but they 
## are basically the same as for our residuals,
## which means that the model doesn't explain more than our plotted residuals,
## but doesn't explain less either.

## Classical statistics often makes the mistake of assuming equal variance when
## there are in fact tests of equality of variance that should be run
## before making any further analyses.
## For this dataset, the tests probably would have been
## very useful to perform beforehand.
## The variances for different pair-wise comparisons should have been tested first
## to determine whether or not parametric methods of analysis were appropriate.

## TAKE AWAY: pesticides A, B & F are the most effective.
## Spray E seemed to flat-out be the worst. Perhaps it was the control spray? Of water?
## Further analysis of the data using the Tukey "Honest Significant Difference" test
## may have made some sense.
## The Tukey Mean-Difference plots only reinforced what the qq plots implied.
