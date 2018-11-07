## See what's up in the land of dead dogs:
# Comprehensive autopsies were performed on 387 Portuguese water dogs who died of natural causes.

## Load S. Worthington's "ipak" function: load multiple R packages at the same time ###
# Checks to see if packages are installed. 
# Installs them if they are not, then loads them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


## Enter names of packages you're going to use in the R session:
packages <- c("skimr","funModeling","infotheo","corrplot","xray","funModeling","magrittr","dplyr","tidyverse","leaps","boot")

## Run it
ipak(packages)

## Load some data:
load("Dog_Data.Rdata")

## Check the dimensions, and how many NAs are present in each column.
dim(pwd.autopsy.data)
colSums(is.na(pwd.autopsy.data))

## Look at the structure, "str", summary to see if any factors were misread by R
## as numbers or integers.
str(pwd.autopsy.data)
unique(pwd.autopsy.data)

## It appears that out of 240 total variables, most should be factors.
## Use "mutate_if" to set all variables to factors:
new_dogs <- pwd.autopsy.data %>% mutate_if(is.numeric,as.factor)

## Now set the small number of numeric variables back to numeric:
## Make numeric data numeric:
shouldBeNumeric <- c("Body.weight.lbm","trunk.length.cm","trunk.circumfrance.cm","Heart.wt.g","left.ventrical.thickness.mm",
                     "right.ventrical.thickness.mm","pancreas.wt.g","gluteus.medius","triceps","quadraceps","gastronemus","temporalis","sex","AOD")

for(v in shouldBeNumeric) {
  new_dogs[[v]] <- as.numeric(new_dogs[[v]])
}

## Make sure everything was converted:
str(new_dogs)

## What are all of the variables called?
names(pwd.autopsy.data)

## Spaces in variable names are always a problem for R.
## Fix this problem by using the "make.names" function.
names(pwd.autopsy.data) <- make.names(names(pwd.autopsy.data),unique = TRUE)

## Verify that the variable names have been fixed:
names(pwd.autopsy.data)

## Take a look at the overall structure of the data set.
profiling_num(pwd.autopsy.data)

## What's the data look like as far as missing values, zeros, etc.?:
dogs <- df_status(pwd.autopsy.data)

## Variables with lots of zeros should be imputed or removed.
## Make a note of all the variables with more than 40% zeros.
vars_to_remove <- filter(dogs, p_zeros > 60)  %>% .$variable
vars_to_remove

## Now remove them and make a reduced data set:
reduced_dogs <- select(pwd.autopsy.data, -one_of(vars_to_remove))
dim(reduced_dogs)

## Now look at the reduced data set:
describe(reduced_dogs)

## Numerical profiling of the reduced data set.
## Select indicators of interest.
my_dogs <- profiling_num(reduced_dogs, print_results = FALSE) %>%
  select(variable, mean, p_01, p_99, range_98, iqr,kurtosis,skewness)

## Inspect the output of the previous operation.
my_dogs

## Variables with high kurtosis:
## right.ventrical.thickness.mm,FIBROSIS.all.22,LYMPHOCYTIC..INFILTRATION.all.17

## Variables  with sparse data (defined as iqr > 10):
#Body.weight.lbm, heart.wt.g, pancreas.wt.g, gluteus.medius,triceps,quadraceps,gastronemus,temporalis

## Variables with a high degree of skew (defined as skewness > 10)
## None. Excellent!

## Now run "anomalies" through R package "xray" to further look for suspicious variables.
## Here, "anomalies" means variables that contain > 80% zeros, NAs, infinities or some combination thereof.
anomalies(my_dogs)
## None found! Also excellent.

## SUMMARY: High kurtosis means there are probably outliers in a variable.
## High IQR tells us that there is sparsity in a variable.
## Positive skew indicates a tail on the right,
## negative skew indicates a tail on the left. Skew = 0 is a good thing.

## Make frequency plots of categorical data:
freq(reduced_dogs,input=c("INFLAMMATORY.BOWEL.W.JEJUNUM","INFLAMMATORY.BOWEL.intestine","INFLAMMATION.intestine","INFLAMMATION.all.24","INFLAMMATORY.BOWEL.all.2",
                          "INFLAMMATION.kidney","INFLAMMATION.I.KIDNEY.SAMPLE.CROSS.SECTION","INFLAMMATORY.BOWEL.V.DEODENUM","INFLAMMATION.V.DEODENUM"))

freq(reduced_dogs,input=c("AUTOLYSIS.gall","AUTOLYSIS.liver","AUTOLYSIS.intestine","AUTOLYSIS.pancreas","AUTOLYSIS.colon","AUTOLYSIS.all.20",
                          "AUTOLYSIS.F.LIVER.PROXIMAL..LEFT.LATERAL","AUTOLYSIS.K.PANCREAS.TAIL","AUTOLYSIS.G.LIVER.PROXIMAL.RIGHT.LATERAL",
                          "AUTOLYSIS.AH.ASCENDING.COLON","AUTOLYSIS.AI.TRANSVERSE.COLON","AUTOLYSIS.AJ.DESCENDING.COLON","AUTOLYSIS.U.STOMACH.BENEATH.ESOPHAGUS",
                          "AUTOLYSIS.W.JEJUNUM","AUTOLYSIS.V.DEODENUM","AUTOLYSIS.X.ILEUM","AUTOLYSIS.J.PANCREAS.HEAD"))

freq(reduced_dogs,input=c("FIBROSIS.pancreas","FIBROSIS.all.22","FIBROSIS.J.PANCREAS.HEAD","FIBROSIS.K.PANCREAS.TAIL"))

freq(reduced_dogs,input=c("HEMOSIDEROSIS.spleen","HEMOSIDEROSIS.liver","HEMOSIDEROSIS.all.11","HEMOSIDEROSIS.E.SPLEEN.MID.CRANIAL.SEGMENT",
                          "HEMOSIDEROSIS.F.LIVER.PROXIMAL..LEFT.LATERAL","HEMOSIDEROSIS.G.LIVER.PROXIMAL.RIGHT.LATERAL","HEMOSIDEROSIS.AK.CERVICAL.LYMPH.NODE"))

freq(reduced_dogs,input=c("CONGESTION.spleen","CONGESTION.lung","CONGESTION.liver","CONGESTION.L.LUNG.MID.RIGHT.CAUDAL","CONGESTION.E.SPLEEN.MID.CRANIAL.SEGMENT",
                          "CONGESTION.F.LIVER.PROXIMAL..LEFT.LATERAL","CONGESTION.G.LIVER.PROXIMAL.RIGHT.LATERAL","CONGESTION.all.14"))

freq(reduced_dogs,input=c("VACUOLIZATION.liver","VACUOLIZATION.F.LIVER.PROXIMAL..LEFT.LATERAL","VACUOLIZATION.G.LIVER.PROXIMAL.RIGHT.LATERAL","VACUOLIZATION.all.7"))

freq(reduced_dogs,input=c("ATROPHY.thyroid","ATROPHY.AE.THYROID.GLAND.AND.PARA","ATROPHY.all.18"))

freq(reduced_dogs,input=c("PLASMACYTIC.intestine","PLASMACYTIC.V.DEODENUM","PLASMACYTIC.all.15"))

freq(reduced_dogs,input=c("sex","HYPERPLASIA.all.13","LYMPHOCYTIC..INFILTRATION.all.17","GLOMERULOSCLEROSIS.I.KIDNEY.SAMPLE.CROSS.SECTION",
                          "FOLLICULAR.DYSPLASIA.A.SKIN..ANKLE..BACK.AND.NOSE.","ENDOCARDIOSIS.P.HEART.ATRIOVENTRICULAR.VALVES"))

## Plote the distributions of the numerical variables:
plot_num(reduced_dogs[2:15])

## Make a correlation table to see what variables are correlated
## with Age of Death (of the dog) ("AOD")
correlation_table(data=reduced_dogs, target="AOD")

## We can also use package "xray" to find anomolies in the reduced dataset.
xray::anomalies(reduced_dogs)
# Possible anomalous variable: HEMOSIDEROSIS.AK.CERVICAL.LYMPH.NODE

## Now take the subset of variables with the highest correlation,
## discretize them, and plot them using an information theory approach.
reduced_dogs.2 <-pwd.autopsy.data %>% select("INFLAMMATION.intestine","INFLAMMATION.kidney","AUTOLYSIS.gall","AUTOLYSIS.liver",
                                             "AUTOLYSIS.intestine","AUTOLYSIS.pancreas","AUTOLYSIS.colon","FIBROSIS.pancreas",
                                             "HEMOSIDEROSIS.spleen","HEMOSIDEROSIS.liver","CONGESTION.spleen","CONGESTION.lung",
                                             "CONGESTION.liver","VACUOLIZATION.liver","ATROPHY.thyroid",
                                             "PLASMACYTIC.intestine","sex","HYPERPLASIA.all.13","LYMPHOCYTIC..INFILTRATION.all.17",
                                             "GLOMERULOSCLEROSIS.I.KIDNEY.SAMPLE.CROSS.SECTION",
                                             "FOLLICULAR.DYSPLASIA.A.SKIN..ANKLE..BACK.AND.NOSE.",
                                             "ENDOCARDIOSIS.P.HEART.ATRIOVENTRICULAR.VALVES","Body.weight.lbm","trunk.length.cm",
                                             "trunk.circumfrance.cm","Heart.wt.g","left.ventrical.thickness.mm",
                                             "right.ventrical.thickness.mm","pancreas.wt.g","gluteus.medius", "triceps",
                                             "quadraceps","gastronemus","temporalis","sex")

# discretizing every variable
reduced_dogs_disc <- discretize(reduced_dogs.2) 

# calculating "correlation" based on mutual information
reduced_dogs_info <- mutinformation(reduced_dogs_disc, method= "emp")

# hack to visualize the maximum value of the scale excluding the diagonal (var against itself)
diag(reduced_dogs_info) <- 0
## Makes all the data discrete by binning it.

# Correlation plot with color and correlation Mutual Information from Infotheo package.
corrplot(reduced_dogs_info, method="color",type="lower", number.cex=0.6,
         addCoef.col = "black", tl.col="red", tl.srt=90, tl.cex = 0.55, diag=FALSE, is.corr = F)

## Which variables are best to model the dogs' age of death (AOD)?:
variable_importance <- var_rank_info(reduced_dogs, "AOD")

## View outputs:
variable_importance

## Plot the results:
ggplot(variable_importance, 
       aes(x = reorder(var, gr), 
           y = gr, fill = var)
) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_bw() + 
  xlab("") + 
  ylab("Variable Importance 
       (based on Information Gain)"
  ) + 
  guides(fill = FALSE)

reduced_dogs.3 <- reduced_dogs %>% select(-AOD)

## Use regular subsets regression to find the best model to predict dog age of death ("AOD")
regsubsets_dead_dogs <- regsubsets(AOD ~ sex + Body.weight.lbm + trunk.length.cm +
                    trunk.circumfrance.cm + Heart.wt.g + left.ventrical.thickness.mm +
                    right.ventrical.thickness.mm + pancreas.wt.g + gluteus.medius + triceps +
                    quadraceps + gastronemus + temporalis + INFLAMMATION.intestine +
                    INFLAMMATION.kidney + AUTOLYSIS.gall + AUTOLYSIS.liver + 
                    AUTOLYSIS.intestine + AUTOLYSIS.pancreas + AUTOLYSIS.colon + FIBROSIS.pancreas +
                    HEMOSIDEROSIS.spleen + HEMOSIDEROSIS.liver + CONGESTION.spleen + CONGESTION.lung +
                    CONGESTION.liver + VACUOLIZATION.liver + ATROPHY.thyroid + 
                    PLASMACYTIC.intestine + HYPERPLASIA.all.13 + LYMPHOCYTIC..INFILTRATION.all.17+ 
                    GLOMERULOSCLEROSIS.I.KIDNEY.SAMPLE.CROSS.SECTION + 
                    FOLLICULAR.DYSPLASIA.A.SKIN..ANKLE..BACK.AND.NOSE. +
                    ENDOCARDIOSIS.P.HEART.ATRIOVENTRICULAR.VALVES,
                   data=reduced_dogs,method = "backward")

summary(regsubsets_dead_dogs,matrix.logical=TRUE)

## Plot a Bayesian Information Criterion (BIC) calcualtion made from each of the possible the possible models.
## THe best model is that one with the lowest BIC.
plot(regsubsets_dead_dogs)

## According to the plot, the best model is the linear regression model represented by the equation:
## AOD = gluteus.medius + HEMOSIDEROSIS.spleen + VACUOLIZATION.liver +  PLASMACYTIC.intestine
## + HYPERPLASIA.all.13 + GLOMERULOSCLEROSIS.I.KIDNEY.SAMPLE.CROSS.SECTION


## Finally, how reliable are our estimates of model coefficients, however?
## Use the Bootstrap re-sampling method to find out.
set.seed(123)

## Fit the final model given us by regsubsets:
(fit <- lm(AOD ~ gluteus.medius + HEMOSIDEROSIS.spleen + VACUOLIZATION.liver +  PLASMACYTIC.intestine + HYPERPLASIA.all.13 +
             GLOMERULOSCLEROSIS.I.KIDNEY.SAMPLE.CROSS.SECTION, data=reduced_dogs))

## View output of the final model fit:
summary(fit)

sqrt(diag(vcov(fit)))

## CIs of the fit:
confint(fit)

## Function to find estimates using bootstrapping:
getRegr <- function(dat, idx) {
  bsFit <- lm(AOD ~ gluteus.medius + HEMOSIDEROSIS.spleen + VACUOLIZATION.liver +  PLASMACYTIC.intestine + HYPERPLASIA.all.13 +
                GLOMERULOSCLEROSIS.I.KIDNEY.SAMPLE.CROSS.SECTION, subset=idx, data=dat)
  coef(bsFit)
}


nR <- 2500 ## Take 1000 resaples
(bsRegr <- boot(reduced_dogs, statistic=getRegr, R=nR))

## There are 7 total terms in the model.
## So there are 7 terms (including the intercept) whose coefficient estimates have been bootstrapped.
boot.ci(bsRegr, conf=0.95, type="bca", index=1)$bca
boot.ci(bsRegr, conf=0.95, type="bca", index=2)$bca
boot.ci(bsRegr, conf=0.95, type="bca", index=3)$bca
boot.ci(bsRegr, conf=0.95, type="bca", index=4)$bca
boot.ci(bsRegr, conf=0.95, type="bca", index=5)$bca
boot.ci(bsRegr, conf=0.95, type="bca", index=6)$bca
boot.ci(bsRegr, conf=0.95, type="bca", index=7)$bca

## Plot final results. Do they look normal?:
plot(bsRegr,index=1)
title("Bootstrapped Estimate of Model Intercept")

plot(bsRegr,index=2)
title("Bootstrapped Estimate of Model Coefficient 1")

plot(bsRegr,index=3)
title("Bootstrapped Estimate of Model Coefficient 2")

plot(bsRegr,index=4)
title("Bootstrapped Estimate of Model Coefficient 3")

plot(bsRegr,index=5)
title("Bootstrapped Estimate of Model Coefficient 4")

plot(bsRegr,index=6)
title("Bootstrapped Estimate of Model Coefficient 5")

plot(bsRegr,index=7)
title("Bootstrapped Estimate of Model Coefficient 6")
## Yes they do!
