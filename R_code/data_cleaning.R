# Author: Sven Jacobs
# Content: Code for cleaning the original data provided by the authors
# OSE data science, Prof. Dr. Philipp Eisenhauer | Summer 2021, M.Sc. Economics, Bonn University

#----

# The two datasets final4.dta and final5.dta are cleaned in accordance with the
# provided do-files from Angrist's data archive:

# 1) The empirical analysis is restricted to schools with more than 5 pupils
#    reported enrolled in the relevant grade and to classes with less than 45 pupils.
# 2) There were data entry errors for the average scores.These scores are too large by 100 points.
# 3) Classes with no students that took a test have average scores
#    that are not coded as missing value.
# 4) Classes missing both average scores are removed.

library(tidyverse)
library(haven) # Enables R to read dta files

clean_data <- function(datafile, filename) {
    data <- read_dta(datafile)
    
    data <- data %>%
        rename(enrollment = c_size, classsize = classize, pct_disadv = tipuach,
               readsize = verbsize, avgread = avgverb, schltype = c_pik) %>%
        filter(enrollment > 5 & classsize < 45) %>%
        select(classsize, enrollment, pct_disadv,
               readsize, mathsize, avgread, avgmath, schltype, schlcode)
    
    avgread_error <- which(data$avgread > 100)
    data$avgread[avgread_error] <- data$avgread[avgread_error] - 100
    avgmath_error <- which(data$avgmath > 100)
    data$avgmath[avgmath_error] <- data$avgmath[avgmath_error] - 100
    
    data$avgread[data$readsize == 0] <- NA
    data$avgmath[data$mathsize == 0] <- NA
    
    data <- data[!is.na(data$avgread) | !is.na(data$avgmath), ]
    
    write_csv(data, file = filename)
}

clean_data("data/final4.dta", "final4_cleaned.csv")
clean_data("data/final5.dta", "final5_cleaned.csv")