## A simple function that characterizes the variation in each numeric variable of interest:
summary.stats.fun <- function(x) {
  c(min = min(x,na.rm=TRUE), mean = mean(x,na.rm=TRUE), median = median(x,na.rm=TRUE),
    max = max(x,na.rm=TRUE),std.dev = sd(x,na.rm=TRUE))
}

## Use lapply to run all of your data through the function:
five_num_summary <- (lapply(your_numeric_data,summary.stats.fun))

## Inspect output:
five_num_summary

## Cast five_num_summary into a data frame:
your_data_table <- as.data.frame(five_num_summary)

## Add rownames:
rownames(your_data_table) <- c("Min", "Mean","Std_Dev","Median","Max")

## View table:
your_data_table

## Save table:
write.csv(your_data_table,"Your_Numeric_Data_Summary.csv")
