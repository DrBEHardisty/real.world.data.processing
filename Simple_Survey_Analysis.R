## Here are some handy means to begin taking a precursory look at your data...
## What the total "big" data set reveals of course will differ from the look you get here,
## starting with one set or whatever. Thought it might be handy for you though

## First, open up R package "survey"
library(survey)

##### Then, import your data
## choose names wisely!
## you called the 2 T1 and T2 if I recollect
## You typically need stringsAsFactors set to FALSE
## so R doesn't treat all the text in your data file as categorical variables right off the bat:
T1sample <- read.table(T1.csv,stringsAsFactors=FALSE,colClasses=)
## Or, just use the "Import Dataset" tab but you have more control doing it the old fashioned way

## You may want to convert some variables to factors right off the bat:
## I forget which, but it seemed like you definitely had some variables that could be factors
T1sample$variable1 = factor(T1sample$variable1)
T1sample$variable2 = factor(T1sample$variable2)

## Also you may need to do something with the ID numbers of respondents,
## since those aren't really variables? This is where colClasses comes in
## This is cool because it would allow you specify for each of your columns within your dataframe
## what sort of numbers you're dealing with:
## Here's an example corresponding to a dataframe with 10 columns total
## This vector you made would go above into your read.csv line of code
colClasses = c("character", "character", "complex", 
               "factor", "integer", "integer",
               "numeric", "character", "Date", "integer")

## str summarizes your data set 
## tells you whether observations are integers or characters for each variable
str(T1sample)

## table counts how many responses are in 
## whatever category you want to look at
## the use of the $ just says that for your data frame,
## you only want to know what's going on in one particular variable
table(T1sample$whatevervariable)

## you can also use something like this
## to see that the variables you converted to factors
## are in fact the ones you intended to convert to factors
## Etc etc
summary(T1sample)

## Some actual analysis:
## srvydesignfunction tells the package what sort of survey design you used
## "newdataframe" gives your data to the actual package for analysis and plotting etc
## You had 12 data sets, so I think that means 12 samples, right?
## id = your sample unit, which I think were actual people? So it would be that variable
## your survey respondent IDs
## data = your full data frame (raw data that is)
## If the sampling was simple random, which it sounded like, then strate=NULL,
## but if you used stratified random sampling, then strata would have some value
## ids = ~1 means there is no clustering
T1svy <- svydesign(ids=~1,strata=NULL,data=T1sample)

## summary then shows you what svydesign is working with from here on out
## It should show you all of your variables and some summary statistics for each
summary(T1svy)

## Now you can finally calculate things!
## For example, mean, totals, even mean by subgroups within each particular survey
svymean(~somevariable,data=TIsvy)
svytotal(~somevariable, data=T1svy)

## look at some interesting variable broken up by a category
## FUN is just whatever function you want to calculate
svyby(~somevariable, by=~someinterestingbroadcategory, design=T1svy, FUN = svymean)

## the svydesign package also makes it super easy to calculate confidence intervals
## here this is done for the mean, but you could imagine
## using other summary statistics beyond just the mean
confint(svymean(~somevariable, T1svy))

## You'll prolly want to use your data to see if some things predict other things?
## If so then the package's built in lin regression function is handy:
## the general form in R of course being: something is predicted by some other things response ~ var + var + var...
linear.reg = svyglm(somevariable ~ variable + anothervariable + anothervariable, design = T1svy)
summary(linear.reg)
plot(linear.reg)
## Note that the plots R Studio throws up include Cook's Distance, which shows specific data points
## that may be outliers

## For some data, you may need to turn responses into binary variables and the like
## because that particular variable may be predicted well by some linear model.
## Then, you could run a logistic regression using those new transformed responses
## The as.numeric argument takes your non-numeric variable with whatever responses survey respondents could give,
## say "Yes" or "No" for example, and turns those into 0s and 1s for example
logistic.reg = svyglm(as.numeric(whatevernonnumericvariable=="Yes") ~ somevariable + anothervariable, design = T1svy, family=quasibinomial())
summary(logistic.reg)
plot(logistic.reg)

## You might make some boxplots too
## To see how some variable breaks down by categories
## I.e., how much of some total does each meaningful category account for?
## ylim will depend upon how low and how high your variable of interest goes
svyboxplot(variable~groupsofinteresttoyou, T1svy, main="Cool Title for Your Plot", ylim=c(,))
