## Some schemes for detecting outliers
## and looking at your data with and without them.

## Load "ipak": load multiple R packages at the same time ###
# Checks to see if packages are installed. 
# Installs them if they are not, then loads them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

## Enter names of packages you're going to use in the R session:
packages <- c("tidyverse", "dplyr", "stringr", "readxl", "magrittr", "forcats", "utf8", "cli", "purrr","VIM", "car", "ggplot2",
              "reshape2","funModeling","Hmisc","gdata","xray","assertr","readxl","skimr")

## Run it
ipak(packages)

## Visualize all of the NAs in your data to start with:
ggplot_missing <- function(x){
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

# Make a plot of all the missing values in your data:
ggplot_missing(your_data)

## The outlierKD function looks for outliers
## and provides easy plotting.
## Also uses Tukey's method because it is nonparametric.:
outlierKD <- function(dt, var) {
 var_name <- eval(substitute(var),eval(dt))
 na1 <- sum(is.na(var_name))
 m1 <- mean(var_name, na.rm = T)
par(mfrow=c(2, 2), oma=c(0,0,3,0))
boxplot(var_name, main="With outliers")
hist(var_name, main="With outliers", xlab=NA, ylab=NA)
outlier <- boxplot.stats(var_name)$out
mo <- mean(outlier)
var_name <- ifelse(var_name %in% outlier, NA, var_name)
boxplot(var_name, main="Without outliers")
hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
title("Outlier Check", outer=TRUE)
na2 <- sum(is.na(var_name))
cat("Outliers identified:", na2 - na1, "n")
cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

## See how many outliers there are for numerical variables of interest:
## Run it:
outlierKD(your_data,name_of_the_variables)

## Make a boxplot of the output
## $out is the actual output from running outlierKD:
boxplot.stats(your_data$your_varaiables)$out

### Make density plots of each of your numeric variables

## Note that "is.numeric" extracts all of your numeric variables for you.
## "gather" puts all of those variables into one new data frame.
## "geom_density" tells ggplot2 what kind of plot you want to plot.
your_data %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()

## Exploratory Analysis
## Run this to get a smaller summary.
## "df_status" tells you the quantity and percentage of zeros (q_zeros and p_zeros, respectively).
## Also gives you the quantity and percentage of zeros (q_zeros and p_zeros respectively) for NAs and/or infinite values (q_inf/p_inf).
## The last two columns indicates data type and the quantity of unique values in each variable.
df_status(your_data)

## To get a more comprehensive summary.
## "profiling_num" Performs a univariate analysis of every variable
profiling_num(your_data)

## Plot your numerica variables to see the distribution of each:
plot_num(your_data)
# NOTE: you can add the "bins =" option in your plot_num command
# The default setting is: bins = 10.

## Make frequency plots of factors:
freq(your_data,input=c("Some_Independent_Variable_1","Some_Independent_Variable_2","Some_Independent_Variable_3"))

## Use xtabs to construct contingency tables.
## If large numbers of zeros appear in the xtables, then further action must be taken before modeling.
## object with ~ in front of it is the variable you want to predict.
## object on right hand side of the addition sign is a variable you think might
## make a good predictor.
xtabs(~Dependent_Variable + Some_Independent_Variable_1, data = your_data)
xtabs(~Dependent_Variable + Some_Independent_Variable_2, data = your_data)

## Now it's time to model your data! Woohoo!


################
## First argument is the data.frame
## Second is the list of variables you're interested in the effect on group.var of
## Third is the variable you're testing against:

