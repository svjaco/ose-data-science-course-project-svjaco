# Author: Sven Jacobs
# Content: Code generating functions that are repeatedly used in the other R scripts
# OSE data science, Prof. Dr. Philipp Eisenhauer | Summer 2021, M.Sc. Economics, Bonn University

#----

# Class-size function induced by Maimonides' rule 
maimonides_rule <- function(x) {
    x / ( floor((x - 1)/40) + 1 )
}

#----

# +/- 5 discontinuity sample (enrollment 36-45, 76-85, 116-125)
sub_sample_5 <- function(full_sample) {
    filter(full_sample, 
           enrollment >= 36 & enrollment <= 45 |
           enrollment >= 76 & enrollment <= 85 |
           enrollment >= 116 & enrollment <= 125)
}

# +/- 3 discontinuity sample (enrollment 38-43, 78-83, 118-123)
sub_sample_3 <- function(full_sample) {
    filter(full_sample, 
           enrollment >= 38 & enrollment <= 43 |
           enrollment >= 78 & enrollment <= 83 |
           enrollment >= 118 & enrollment <= 123)
}

grade4_sub_sample_5 <- sub_sample_5(grade4)
grade5_sub_sample_5 <- sub_sample_5(grade5)

grade4_sub_sample_3 <- sub_sample_3(grade4)
grade5_sub_sample_3 <- sub_sample_3(grade5)

#----

# Piecewise linear trend
trend_fun <- function(x) {
    trend <- c()
    
    for (i in 1:length(x)) {
        if (x[i] >= 0 & x[i] <= 40) trend[i] <- x[i]
        else if (x[i] >= 41 & x[i] <= 80) trend[i] <- 20 + x[i]/2
        else if (x[i] >= 81 & x[i] <= 120) trend[i] <- 100/3 + x[i]/3
        else if (x[i] >= 121 & x[i] <= 160) trend[i] <- 130/3 + x[i]/4
    }
    
    return(trend)
}