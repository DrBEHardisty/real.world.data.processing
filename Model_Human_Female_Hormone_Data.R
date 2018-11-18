## Benjamin E. Hardisty c2011

## Load packages
library("latticeExtra")
library("MASS")

## Used: some human hormone Data provided by Dr. Elisabeth Cashdan.

## The following, when plotted as scatter plots, revealed NO apparent
## statistically significant relationships:
## t & dheas; t & e; a & dheas; a & e; dheas & e; freet & e, freet & cort; dheas & cort.

## The following pairs of hormones DID appear to have statistically 
## significant effects on each other and thus warranted further study:
## t & a, t & freet; a & cort; dheas & freet

## t ~ a
## Add the family="symmetric" argument to downweight the outliers:
TA <- xyplot(t ~ a, data=CHormone, col="red")
TA <- TA + layer(panel.loess(x,y, span=1, col="green", family="symmetric"))

## Remove the most striking outliers by setting the ylim to a new value:
TA <- update(TA, ylim=c(0:100))
## Now the fit looks pretty good.

## t ~ freet:
TFreet <- xyplot(t ~ freet, data=CHormone)
## Not terribly surprising that testosterone and free testosterone levels are correlated.

## Try loess smoothing and see if that helps:
TFreet <- TFreet + layer(panel.loess(x,y, span=1, col="green", family="symmetric"))

## It's an okay fit but perhaps we should remove some more outliers:
TFreet <- update(TFreet, ylim=c(0:80))
## That looks much better.  In real life, freet is typically only about 1-4%
## of our total t count so I guess we shouldn't expect a perfect fit.

## a ~ cort:
ACort <- xyplot(a ~ cort, data=CHormone, col="green")

## Perhaps a robust linear regression is in order:
ACort <- ACort + layer(panel.smoother(y~x, method="rlm", psi=psi.bisquare, init="lts"))
## The fit isn't great, there are a lot of points far above the line,
## a lot of variation, even though the line describes the data OK.

## Next, try a robust loess to see if we get a better fit to the data:
ACort <- ACort + layer(panel.loess(x,y, span=2.5, family="symmetric", col="purple"))
## This seems quite a bit better than the bisquare weighted regression fit.

## This relationship had an OK correlation, 0.464:
var(a)
var(cort)
var(a, cort)
cor(a, cort)

## dheas & freet:
DFreet <- xyplot(dheas ~ freet, data=CHormone)

## I'll get rid of the blatant outlier that lies above y=8 by changing ylim to 8:
DFreet <- update(DFreet, ylim=c(0:8))

## Now I'll try psi.bisquare again:
DFreet <- DFreet + layer(panel.smoother(y~x, method="rlm", psi=psi.bisquare, init="lts"))
## That looks pretty good, I would have guessed that the correlation
## between levels of dheas and cort in the blood isn't very strong, but is real nonetheless.

## I'll try a loess regression though and see if the fit is better:
DFreet <- DFreet + layer(panel.loess(x,y, family="symmetric", col="blue"))
## Indeed, it is a bit better.
## A correlation test here revealed why I needed to do so much fiddling:
## because the correlation coefficient was only = 0.0595;
## thus the data are even more weakly correlated than I suspected.
var(dheas)
var(cort)
var(dheas, cort)
cor(dheas, cort)

#############################################################################
## The following pairs of hormones appeared to have statistically significant
## correlations with each other which were not obviously linear.
## Several of them appeared to my eyes to show
## possible exponential and logarithmic response curves. 
## These pairs thus warrant further study via techniques such as 
## logarithmic, square root and other types of data transformation:
## t & cort; a & freet; e & cort

## Does t ~ cort?:
TCort <- xyplot(t ~ cort, data=CHormone)
TCort <- update(TCort, ylim=c(0:80))
TCort <- TCort + layer(panel.loess(x,y, span=0.75, col="red"))

## It initially looks like something interesting is going on,
## but can we improve the fit even more?
## A quadratic fit but accomplished nothing,
## and didn't look any better than Loess smoothing.
## A log fit which also accomplished nothing.
## How about the weighted bisquares method?:
TCort <- xyplot(t ~ cort, data=CHormone)
TCort <- update(TCort, ylim=c(0:80))
TCort <- TCort + layer(panel.smoother(y~x, method="rlm", psi=psi.bisquare, init="lts"))
## Finally, we get a decent fit to the data,
## but I suspect that it's a weak relationship because there is so much spread.

## Does a ~ freet?
AFreet <- xyplot(a ~ freet, data=CHormone, col="red")
AFreet <- AFreet + layer(panel.loess(x,y, span=0.50))

## While the loess smoothed data above looked OK,
## the log transformed data look like there is a relationship,
## when one increases the other does too.
## I debated over whether to use degree 2 or degree 1,
## but degree 1 seems to provide the better fit to the data.
AFreet <- xyplot(log10(a) ~ freet, data=CHormone, col="blue")
AFreet <- AFreet + layer(panel.loess(x,y, degree=1, span=0.95, family="symmetric"))

## A correlation test confirmed my hunch that a & freet are weakly correlated,
## with a correlation coefficient = 0.549.
## So low I didn't waste anymore time on it.
## The amount of predictability we want in applications can vary,
## but with such a weak correlation coefficient,
## I wouldn't want to do much of anything with this relationship.
xyplot(a ~ freet, data=CHormone)
var(a)
var(freet)
var(a, freet)
cor(a, freet)

## Does e ~ cort?:
## When I remove outliers there appears to be a nice correlation between the two:
ECort <- xyplot(e~cort, data=CHormone, col="red", ylim=c(0:70), xlim=c(0:30))

## Now try a loess fit. We find that there is a linear relationship between e & cort:
ECort <- ECort + layer(panel.loess(x,y, span=1, degree=1, col="purple"))

## A correlation test here also revealed an even smaller correlation, = 0.111:
var(e)
var(cort)
var(e, cort)
cor(e, cort)

##################################
Part 2

## Perhaps a Principal Components Analysis might reveal some new and exciting patterns 
## I note that in PCA, the scaling of the variables
## can make estimates of the impact of different variables
## on the various Principal Components unreliable.
## This should not be a problem here since the variables of interest are measurements
## of follicular hormone levels, and thus should all be in the same units.
## First, we limit the data to the 6 variables we were analyzing in depth above:
PCAHormone <- CHormone[,6:11]

## Now we use our new, smaller data.frame to run our PCA:
BigModel <- prcomp(PCAHormone, scale=TRUE)
summary(BigModel)

## The scree and biplots tell us what factors are contributing most to PCs 1 & 2:
plot(BigModel, main="")
biplot(BigModel)

## This tells us that we should plot some of the strongest contributors to PC1 & PC2
## against potential explanatory variables, such as time.
## Time of day (time) wasn't correlated with PC1 or PC2I.
## For PC2, the correlation with day after onset of menstruation was interesting,
## as the effect of day on PC2 stayed above zero throughout and was nearly constant.
## Month had a moderate effect on PC2 and were clustered for PC1.
## Testosterone had a strong negative effect on PC1,
## implying that when considering our other 5 hormones in the aggregate,
## when testosterone goes up, the amount of the other hormones in the body go down.
## The effect of testosterone on PC2 was constant and linear.
## The effects were virtually the same for levels of Androstenidione (a) as well.
## DHEAS had no effect on PC1 or PC2.
## Estradiol (e) also had no effect on PC1 or PC2.
## Cortisone (cort) had no effect on PC1 but was linearly correlated with PC2
## (with the exception of one outlier). 
## Unsurprisingly, free testosterone (freet) was negatively correlated with PC1 
## and positively correlated with PC2.


## TAKE AWAY: In summary, though our correlation tests above,
## and graphical data transformation methods, only revealed weak correlations
## between various pairs of hormones, my Principal Components Analysis revealed that
## testosterone level (t) appears to be a strong predictor of levels of all the other hormones,
## in a way that none of the other hormones are,
## as well as the number of days a woman (at least the women in the study)
## was recently menstruating for, and the time of day at which hormone measurements were taken.

## I found it enlightening that some pairs of hormones looked like they would 
## have strong correlation and didn't (e.g., a & cort; dheas & cort; dheas & freet), 
## while some of the pairs of hormones I thought did not have a significant correlation 
## with each other actually did (e.g., a & freet).

## I learned that graphical data transformation methods may be appropriate
## when you want to predict data values,
## but they may not be appropriate if you want strong predictions 
## (that is, predicted values that would fit very well with the original data).
## And thus, simulation methods and multivariate analysis can also both
## be quite useful for model building and hypothesis testing.
