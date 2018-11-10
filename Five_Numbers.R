## Function "ipak": install and load multiple R packages
# Check to see if packages are installed and install them if not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage:
packages <- c("tidyverse", "dplyr", "stringr", "readxl", "magrittr", "utf8", "cli", "tableone", "anytime", "data.table")
ipak(packages)

## Function that turns empty cells into NAs:
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

## Function that characterizes the variation in each numeric variable of interest:
summary.stats.fun <- function(x) {
  c(min = min(x,na.rm=TRUE), mean = mean(x,na.rm=TRUE), std.dev = sd(x,na.rm=TRUE), median = median(x,na.rm=TRUE), max = max(x,na.rm=TRUE))
}

## Read in the Preventice event monitor data:
Event_1 <- read_excel("Utah_2009.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_2 <- read_excel("Utah_2010.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_3 <- read_excel("Utah_2011.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_4 <- read_excel("Utah_2012.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_5 <- read_excel("Utah_2013.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_6 <- read_excel("Utah_2014.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_7 <- read_excel("Utah_2015.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_8 <- read_excel("Utah_2016.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_9 <- read_excel("Utah_2017.xlsx", na = "", col_names = TRUE, col_types = NULL)
Event_10 <- read_excel("Utah_2018_thru_March.xlsx", na = "", col_names = TRUE, col_types = NULL)

# Use dplyr's "bind_rows" command to put together each dataset:
Event_master <- bind_rows(Event_1, Event_2, Event_3, Event_4, Event_5, Event_6, Event_7, Event_8, Event_9, Event_10)

## We can drop X__1:
Event_master <- Event_master %>% select(-X__1)

# Save full data set:
write.csv(Event_master,"EVENT_DATA_FULL.csv")

## Read in the combined event monitor data:
events <- read.csv("EVENT_DATA_FULL.csv")

## Remove useless column:
events <- events %>% select(-X)

## Inspect the structure of the event data:
str(events)

## Make numeric data numeric:
shouldBeNumeric <- c("ST_Rate","EN_Rate","ST_PR","EN_PR","ST_QRS","EN_QRS","ST_QT","EN_QT",
                     "Strip1.Length","Strip2.Length","Strip3.Length","Strip4.Length","Strip5.Length",
                     "Strip6.Length","Strip1.msPerSample","Strip2.msPerSample","Strip3.msPerSample","Strip4.msPerSample","Strip5.msPerSample",
                     "Strip6.msPerSample","Study.Duration")
for(v in shouldBeNumeric) {
  events[[v]] <- as.numeric(events[[v]])
}

## Make character data character data:
shouldBeCharacter <- c("Location.Name","Last.Name","First.Name","Device.Name","DX1","DX2","DX3","Comment","Tach_Symp","TriagePVCCount",
                       "Strip1.Comment","Strip2.Comment","Strip3.Comment","Strip4.Comment","Strip5.Comment","Strip6.Comment","Ordering.MD","Confirming.MD")
for(v in shouldBeCharacter) {
  events[[v]] <- as.character(events[[v]])
}

## Convert factors with incorrect levels auto-parsed by R into factors with the levels we want:
# Specify levels of "Gender":
gender_levels <- c("M", "F")
# Put in the levels, everything else gets a value of "NA":
events$Gender <- factor(events$Gender, levels = gender_levels)

# Specify levels of "Event.Report.Published.":
event_levels <- c("No", "Yes")
# Put in the levels, everything else gets a value of "NA":
events$Event.Report.Published. <- factor(events$Event.Report.Published., levels = event_levels)

## Use "anytime" to parse out date-time data:
events$HeartRateTimeStamp <- anytime(events$HeartRateTimeStamp)
events$DOB <- anytime(events$DOB)

## Make all other dates dates with R package "anytime"
# Note: "anytime" moves cell by cell and searches all the date-time formats in its memory.
events <- events %>% mutate_at(vars(contains("Date")), (anytime))

## Take out numeric variables we want to take a closer look at:
events.summary <- events %>%
  select('ST_Rate', 'EN_Rate', 'ST_PR', 'EN_PR', 'ST_QRS', 'EN_QRS', 'ST_QT', 'EN_QT', 'Study.Duration')

five_num_summary <- (lapply(events.summary,summary.stats.fun))

# Inspect output:
five_num_summary

# Cast five_num_summary into a data frame:
event.monitors.table <- as.data.frame(five_num_summary)

# Add rownames:
rownames(event.monitors.table) <- c("Min", "Mean","Std_Dev","Median","Max")

# View table:
event.monitors.table

# Save table:
write.csv(event.monitors.table,"Event_Monitor_Data_Summary.csv")

# Plot the numeric variables in a single frame:
events %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density() 


#events %>%
#  group_by(MRN) %>%
#  summarise(n=n_distinct(DX1)) %>%
#  ggplot(., aes(x = y, y = n)) + geom_bar(stat = 'identity')

                     


barplot(with(events, tapply(MRN, comment, function(v) length(unique(v)))))

#ggplot(data = events, aes(x = DX1, group = type,fill = type)) +
#  geom_histogram(position = "dodge", binwidth = 0.25) + theme_bw()
  
#hist(unique(events$DX1))

#events$DX1 <- as.factor(events$DX1)
#events$DX2 <- as.factor(events$DX2)
#events$DX3 <- as.factor(events$DX3)

plot(1:nrow(events$DX1), DX1, yaxt = "n"); axis(2, 1:3, levels(events$DX1))

#qplot(seq_along(events$DX1), events$DX1)
