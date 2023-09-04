# Author: Sven Jacobs
# Content: Code generating the replication tables
# OSE data science, Prof. Dr. Philipp Eisenhauer | Summer 2021, M.Sc. Economics, Bonn University

#----

# Table I from Angrist and Lavy (1999)
table1 <- function(data) {
    num_classes <- nrow(data)
    num_schools <- length(unique(data$schlcode))
    
    summary_stat <- function(data) {
        df_summary_stat <- data.frame(
            mean   = sapply(data, mean, na.rm = TRUE),
            sd     = sapply(data, sd, na.rm = TRUE),
            p10    = sapply(data, quantile, probs = 0.10, na.rm = TRUE),
            p25    = sapply(data, quantile, probs = 0.25, na.rm = TRUE),
            median = sapply(data, median, na.rm = TRUE),
            p75    = sapply(data, quantile, probs = 0.75, na.rm = TRUE),
            p90    = sapply(data, quantile, probs = 0.90, na.rm = TRUE)
        )
        
        return(df_summary_stat[!(row.names(df_summary_stat) %in% c("schltype", "schlcode")), ]) 
    }
    
    df_summary_stat <- summary_stat(data)
    
    rownames(df_summary_stat) <- c("Class Size", "Enrollment", "Percent disadvantaged",
                                   "Reading size", "Math size", "Average verbal", "Average math")
    
    ifelse(identical(data, grade5) | identical(data, grade5_sub_sample_5), grade <- "5th", grade <- "4th")
    
    output <- capture_output(
        
        stargazer(df_summary_stat,
                  type = "html",
                  title = paste(grade, "grade:", num_classes, "classes,",
                          num_schools, "schools, tested in 1991"),
                  digits = 1,
                  covariate.labels = c("Variable", "Mean", "S.D.", 
                                       "0.10", "0.25", "0.50", "0.75", "0.90"),
                  summary = FALSE)

    )
    
    display_html(output)   
}

#----

# The package stargazer (used to produce HTML code for well-formatted tables)
# currently does not support model outputs from the package estimatr 
# (e. g., lm_robust and iv_robust). To circumvent this, we replace the
# uncorrected standard errors from stargazer with the cluster-robust ones 
# provided by estimatr. For linear regression this can be achieved automatically
# by the starprep function. For 2SLS estimation, however, one has to do
# the replacement manually.

#----

# Table II from Angrist and Lavy (1999)
table2 <- function(data) {
    table2_col1 <- lm(avgread ~ classsize, data = data)
    table2_col2 <- lm(avgread ~ classsize + pct_disadv, data = data)
    table2_col3 <- lm(avgread ~ classsize + pct_disadv + enrollment, data = data)
    table2_col4 <- lm(avgmath ~ classsize, data = data)
    table2_col5 <- lm(avgmath ~ classsize + pct_disadv, data = data)
    table2_col6 <- lm(avgmath ~ classsize + pct_disadv + enrollment, data = data)
    table2_col1_to_6 <- list(table2_col1, table2_col2, table2_col3, table2_col4, table2_col5, table2_col6)    
    
    ifelse(identical(data, grade5_reg), caption <- "<center>5th Grade</center>", caption <- "<center>4th Grade</center>")
    
    output <- capture_output(
        
        stargazer(table2_col1_to_6,
                  se = starprep(table2_col1_to_6, clusters = data$schlcode, se_type = "CR0"),
                  dep.var.caption = caption, dep.var.labels = c("<center>Reading comprehension</center>", "<center>Math</center>"),
                  column.separate = c(3, 3),
                  covariate.labels = c("Class size", "Percent disadvantaged", "Enrollment"),
                  omit = "Constant",
                  keep.stat = "rsq",
                  add.lines = list(c("Root MSE", round(sapply(table2_col1_to_6, RMSE), 2)),
                                   c("N", NA, format(length(data$avgread), big.mark = ","), NA, NA, format(length(data$avgmath), big.mark = ","))),
                  digits = 3, initial.zero = FALSE,
                  no.space = TRUE, 
                  notes = "Standard errors (in parantheses) were corrected for within-school correlation between classes.",
                  notes.append = FALSE, notes.label = "", notes.align = "l",
                  report = "vcs", table.layout = "=ld#-t-sa=n",
                  type = "html")
        
    )
    
    display_html(output)   
}

#----

# Table III from Angrist and Lavy (1999)
table3 <- function(data) {
    f_sc <- maimonides_rule(data$enrollment)
    
    table3_col1 <- lm(classsize ~ f_sc + pct_disadv, data = data)
    table3_col2 <- lm(classsize ~ f_sc + pct_disadv + enrollment, data = data)
    table3_col3 <- lm(avgread ~ f_sc + pct_disadv, data = data)
    table3_col4 <- lm(avgread ~ f_sc + pct_disadv + enrollment, data = data)
    table3_col5 <- lm(avgmath ~ f_sc + pct_disadv, data = data)
    table3_col6 <- lm(avgmath ~ f_sc + pct_disadv + enrollment, data = data)
    table3_col1_to_6 <- list(table3_col1, table3_col2, table3_col3, table3_col4, table3_col5, table3_col6)    
    
    ifelse(identical(data, grade5_reg) | identical(data, grade5_sub_sample_5),
           caption <- "<center>5th Graders</center>", caption <- "<center>4th Graders</center>")
    
    output <- capture_output(
        
        stargazer(table3_col1_to_6,
                  se = starprep(table3_col1_to_6, clusters = data$schlcode, se_type = "CR0"),
                  dep.var.caption = caption, dep.var.labels = c("<center>Class size</center>", "<center>Reading comprehension</center>", "<center>Math</center>"),
                  column.separate = c(2, 2, 2),
                  covariate.labels = c("f<sub>sc</sub>", "Percent disadvantaged", "Enrollment"),
                  omit = "Constant",
                  keep.stat = "rsq",
                  add.lines = list(c("Root MSE", round(sapply(table3_col1_to_6, RMSE), 2)),
                                   c("N", rep(format(nrow(data), big.mark = ","), 6))),
                  digits = 3, initial.zero = FALSE,
                  no.space = TRUE, 
                  notes = "Standard errors (in parantheses) were corrected for within-school correlation between classes.",
                  notes.append = FALSE, notes.label = "", notes.align = "l",
                  report = "vcs", table.layout = "=ld#-t-sa=n",
                  type = "html")
        
    )
    
    display_html(output)   
}

#----

# Table IV and V from Angrist and Lavy (1999)
table4and5 <- function(data, test) {
    f_sc <- maimonides_rule(data$enrollment)
    enrollment_2 <- (data$enrollment^2)/100
    trend <- trend_fun(data$enrollment)
    
    ifelse(test == "reading", dep_var <- data$avgread, dep_var <- data$avgmath)
    
    table4and5_col1 <- ivreg(dep_var ~ classsize + pct_disadv |
                             f_sc + pct_disadv, data = data)
    table4and5_col2 <- ivreg(dep_var ~ classsize + pct_disadv + enrollment |
                             f_sc + pct_disadv + enrollment, data = data)
    table4and5_col3 <- ivreg(dep_var ~ classsize + pct_disadv + enrollment + enrollment_2 |
                             f_sc + pct_disadv + enrollment + enrollment_2, data = data)
    table4and5_col4 <- ivreg(dep_var ~ classsize + trend |
                             f_sc + trend, data = data, subset = enrollment <= 160)
    
    table4and5_col1_se <- iv_robust(dep_var ~ classsize + pct_disadv | f_sc + pct_disadv,
                                    clusters = schlcode, se_type = "CR0", data = data)$std.error[-1]
    table4and5_col2_se <- iv_robust(dep_var ~ classsize + pct_disadv + enrollment| f_sc + pct_disadv + enrollment,
                                    clusters = schlcode, se_type = "CR0", data = data)$std.error[-1]
    table4and5_col3_se <- iv_robust(dep_var ~ classsize + pct_disadv + enrollment + enrollment_2 | f_sc + pct_disadv + enrollment + enrollment_2,
                                    clusters = schlcode, se_type = "CR0", data = data)$std.error[-1]
    table4and5_col4_se <- iv_robust(dep_var ~ classsize + trend | f_sc + trend,
                                    clusters = schlcode, se_type = "CR0", data = data, subset = enrollment <= 160)$std.error[-1]
    
    ifelse(identical(data, grade5_reg), disc_sample_5 <- grade5_sub_sample_5, disc_sample_5 <- grade4_sub_sample_5)
    f_sc <- maimonides_rule(disc_sample_5$enrollment)
    
    ifelse(test == "reading", dep_var <- disc_sample_5$avgread, dep_var <- disc_sample_5$avgmath)
    
    table4and5_col5 <- ivreg(dep_var ~ classsize + pct_disadv |
                             f_sc + pct_disadv, data = disc_sample_5)
    table4and5_col6 <- ivreg(dep_var ~ classsize + pct_disadv + enrollment |
                             f_sc + pct_disadv + enrollment, data = disc_sample_5)
    
    table4and5_col5_se <- iv_robust(dep_var ~ classsize + pct_disadv | f_sc + pct_disadv,
                                    clusters = schlcode, se_type = "CR0", data = disc_sample_5)$std.error[-1]
    table4and5_col6_se <- iv_robust(dep_var ~ classsize + pct_disadv + enrollment| f_sc + pct_disadv + enrollment,
                                    clusters = schlcode, se_type = "CR0", data = disc_sample_5)$std.error[-1]
    
    ifelse(identical(data, grade5_reg), disc_sample_3 <- grade5_sub_sample_3, disc_sample_3 <- grade4_sub_sample_3)
    f_sc <- maimonides_rule(disc_sample_3$enrollment)
    
    ifelse(test == "reading", dep_var <- disc_sample_3$avgread, dep_var <- disc_sample_3$avgmath)
    
    table4and5_col7 <- ivreg(dep_var ~ classsize + pct_disadv |
                             f_sc + pct_disadv, data = disc_sample_3)
    table4and5_col8 <- ivreg(dep_var ~ classsize + pct_disadv + enrollment |
                             f_sc + pct_disadv + enrollment, data = disc_sample_3)
    
    table4and5_col7_se <- iv_robust(dep_var ~ classsize + pct_disadv | f_sc + pct_disadv,
                                    clusters = schlcode, se_type = "CR0", data = disc_sample_3)$std.error[-1]
    table4and5_col8_se <- iv_robust(dep_var ~ classsize + pct_disadv + enrollment| f_sc + pct_disadv + enrollment,
                                    clusters = schlcode, se_type = "CR0", data = disc_sample_3)$std.error[-1]
    
    table4and5_col1_to_8 <- list(table4and5_col1, table4and5_col2, table4and5_col3, table4and5_col4,
                                 table4and5_col5, table4and5_col6, table4and5_col7, table4and5_col8)
    table4and5_col1_to_8_se <- list(table4and5_col1_se, table4and5_col2_se, table4and5_col3_se, table4and5_col4_se,
                                    table4and5_col5_se, table4and5_col6_se, table4and5_col7_se, table4and5_col8_se)
    
    ifelse(test == "reading", caption <- "<center>Reading comprehension</center>", caption <- "<center>Math</center>")
    
    output <- capture_output(
        
        stargazer(table4and5_col1_to_8,
                  se = table4and5_col1_to_8_se,
                  dep.var.caption = caption,
                  column.labels = c("<center>Full sample</center>", "<center>+/- 5 Discontinuity sample</center>", "<center>+/- 3 Discontinuity sample</center>"),
                  column.separate = c(4, 2, 2),
                  covariate.labels = c("Class size", "Percent disadvantaged", "Enrollment", "Enrollment squared/100", "Piecewise linear trend"),
                  omit = "Constant",
                  add.lines = list(c("Root MSE", round(sapply(table4and5_col1_to_8, RMSE), 2)),
                                   c("N", rep(nrow(data), 3), sum(!is.na(trend)), rep(nrow(disc_sample_5), 2), rep(nrow(disc_sample_3), 2))),
                  digits = 3, initial.zero = FALSE,
                  no.space = TRUE, 
                  notes = paste("Standard errors (in parantheses) were corrected for within-school correlation between classes.",
                                "All estimates use f<sub>sc</sub> as an instrument for class size."),
                  notes.append = FALSE, notes.label = "", notes.align = "l",
                  report = "vcs", table.layout = "=lc#-t-a=n",
                  type = "html")
        
    )
    
    display_html(output)   
}

#----

# Table VII from Angrist and Lavy (1999)
table7 <- function(data_4th, data_5th) {
    grade4 <- data_4th
    grade5_reg <- data_5th
    
    f_sc_grade5_reg <- maimonides_rule(grade5_reg$enrollment)
    f_sc_grade4 <- maimonides_rule(grade4$enrollment)
    instr2_grade5_reg <- f_sc_grade5_reg*grade5_reg$pct_disadv
    instr2_grade4 <- f_sc_grade4*grade4$pct_disadv
    
    table7_col1 <- ivreg(avgread ~ classsize + classsize*pct_disadv + pct_disadv + enrollment |
                         f_sc_grade5_reg + instr2_grade5_reg + pct_disadv + enrollment,
                         data = grade5_reg)
    table7_col2 <- ivreg(avgmath ~ classsize + classsize*pct_disadv + pct_disadv + enrollment |
                         f_sc_grade5_reg + instr2_grade5_reg + pct_disadv + enrollment,
                         data = grade5_reg)
    table7_col3 <- ivreg(avgread ~ classsize + classsize*pct_disadv + pct_disadv + enrollment |
                         f_sc_grade4 + instr2_grade4 + pct_disadv + enrollment,
                         data = grade4)
    table7_col4 <- ivreg(avgmath ~ classsize + classsize*pct_disadv + pct_disadv + enrollment |
                         f_sc_grade4 + instr2_grade4 + pct_disadv + enrollment,
                         data = grade4)
    
    table7_col1_se <- iv_robust(avgread ~ classsize + classsize*pct_disadv + pct_disadv + enrollment |
                                f_sc_grade5_reg + instr2_grade5_reg + pct_disadv + enrollment,
                                clusters = schlcode, se_type = "CR0", data = grade5_reg)$std.error[-1]
    table7_col2_se <- iv_robust(avgmath ~ classsize + classsize*pct_disadv + pct_disadv + enrollment |
                                f_sc_grade5_reg + instr2_grade5_reg + pct_disadv + enrollment,
                                clusters = schlcode, se_type = "CR0", data = grade5_reg)$std.error[-1]
    table7_col3_se <- iv_robust(avgread ~ classsize + classsize*pct_disadv + pct_disadv + enrollment |
                                f_sc_grade4 + instr2_grade4 + pct_disadv + enrollment,
                                clusters = schlcode, se_type = "CR0", data = grade4)$std.error[-1]
    table7_col4_se <- iv_robust(avgmath ~ classsize + classsize*pct_disadv + pct_disadv + enrollment |
                                f_sc_grade4 + instr2_grade4 + pct_disadv + enrollment,
                                clusters = schlcode, se_type = "CR0", data = grade4)$std.error[-1]
    
    data_pooled <- rbind(grade4, grade5_reg)
    f_sc_pooled <- append(f_sc_grade4, f_sc_grade5_reg)
    instr2_pooled <- append(instr2_grade4, instr2_grade5_reg)  
    dummy_grade4 <- c(rep(1, nrow(grade4)), rep(0, nrow(grade5_reg)))
    
    table7_col5 <- ivreg(avgread ~ classsize + pct_disadv + dummy_grade4 + enrollment |
                         f_sc_pooled + pct_disadv + dummy_grade4 + enrollment,
                         data = data_pooled)
    table7_col6 <- ivreg(avgread ~ classsize + classsize*pct_disadv + pct_disadv + dummy_grade4 + enrollment |
                         f_sc_pooled + instr2_pooled + pct_disadv + dummy_grade4 + enrollment,
                         data = data_pooled)
    table7_col7 <- ivreg(avgmath ~ classsize + pct_disadv + dummy_grade4 + enrollment |
                         f_sc_pooled + pct_disadv + dummy_grade4 + enrollment,
                         data = data_pooled)
    table7_col8 <- ivreg(avgmath ~ classsize + classsize*pct_disadv + pct_disadv + dummy_grade4 + enrollment |
                         f_sc_pooled + instr2_pooled + pct_disadv + dummy_grade4 + enrollment,
                         data = data_pooled)
    
    table7_col5_se <- iv_robust(avgread ~ classsize + pct_disadv + dummy_grade4 + enrollment |
                                f_sc_pooled + pct_disadv + dummy_grade4 + enrollment,
                                clusters = schlcode, se_type = "CR0", data = data_pooled)$std.error[-1] 
    table7_col6_se <- iv_robust(avgread ~ classsize + classsize*pct_disadv + pct_disadv + dummy_grade4 + enrollment |
                                f_sc_pooled + instr2_pooled + pct_disadv + dummy_grade4 + enrollment,
                                clusters = schlcode, se_type = "CR0", data = data_pooled)$std.error[-1]
    table7_col7_se <- iv_robust(avgmath ~ classsize + pct_disadv + dummy_grade4 + enrollment |
                                f_sc_pooled + pct_disadv + dummy_grade4 + enrollment,
                                clusters = schlcode, se_type = "CR0", data = data_pooled)$std.error[-1]
    table7_col8_se <- iv_robust(avgmath ~ classsize + classsize*pct_disadv + pct_disadv + dummy_grade4 + enrollment |
                                f_sc_pooled + instr2_pooled + pct_disadv + dummy_grade4 + enrollment,
                                clusters = schlcode, se_type = "CR0", data = data_pooled)$std.error[-1]
    
    table7_col1_to_8 <- list(table7_col1, table7_col2, table7_col3, table7_col4, table7_col5, table7_col6, table7_col7, table7_col8)
    table7_col1_to_8_se <- list(table7_col1_se, table7_col2_se, table7_col3_se, table7_col4_se, table7_col5_se, table7_col6_se, table7_col7_se, table7_col8_se)
    
    output <- capture_output(
        
        stargazer(table7_col1_to_8,
                  se = table7_col1_to_8_se,
                  dep.var.labels = c(rep(c("Reading", "Math"), 2), "<center>Reading</center>", "<center>Math</center>"),
                  column.labels = c("<center>5th grade</center>", "<center>4th grade</center>", "<center>Pooled estimates</center>"), column.separate = c(2, 2, 4),
                  covariate.labels = c("Class size", "Percent disadvantaged", "Grade 4", "Enrollment", "Class size*PD"),
                  omit = "Constant",
                  add.lines = list(c("Root MSE", round(sapply(table7_col1_to_8, RMSE), 2)),
                                   c("N", rep(nrow(grade5_reg), 2), rep(nrow(grade4), 2), rep(nrow(data_pooled), 4))),
                  digits = 3, initial.zero = FALSE,
                  no.space = TRUE, 
                  notes = paste("Standard errors (in parantheses) were corrected for within-school correlation between classes.",
                                "All estimates use f<sub>sc</sub> and f<sub>sc</sub>*PD as instruments for class size and class size*PD."),
                  notes.append = FALSE, notes.label = "", notes.align = "l",
                  report = "vcs", table.layout = "=cd#-t-a=n",
                  type = "html")
        
    )
    
    display_html(output)   
}

#----

# Table 3 from Angrist et al. (2019)
table3_MRR <- function(data_4th, data_5th) {
    grade4 <- data_4th
    grade5 <- data_5th
    
    grade4_schllev <- grade4 %>% group_by(schlcode) %>% slice(1)
    grade5_schllev <- grade5 %>% group_by(schlcode) %>% slice(1)
    
    f_sc_schllev <- maimonides_rule(grade5_schllev$enrollment)
    enrollment_2_schllev <- (grade5_schllev$enrollment^2)/100
    trend_schllev <- trend_fun(grade5_schllev$enrollment)
    
    table3_MRR_col1 <- lm(pct_disadv ~ f_sc_schllev + enrollment + schltype, data = grade5_schllev)
    table3_MRR_col2 <- lm(pct_disadv ~ f_sc_schllev + enrollment + enrollment_2_schllev + schltype, data = grade5_schllev)
    table3_MRR_col3 <- lm(pct_disadv ~ f_sc_schllev + trend_schllev + schltype, data = grade5_schllev)
    
    f_sc_schllev <- maimonides_rule(grade4_schllev$enrollment)
    enrollment_2_schllev <- (grade4_schllev$enrollment^2)/100
    trend_schllev <- trend_fun(grade4_schllev$enrollment)
    
    table3_MRR_col4 <- lm(pct_disadv ~ f_sc_schllev + enrollment + schltype, data = grade4_schllev)
    table3_MRR_col5 <- lm(pct_disadv ~ f_sc_schllev + enrollment + enrollment_2_schllev + schltype, data = grade4_schllev)
    table3_MRR_col6 <- lm(pct_disadv ~ f_sc_schllev + trend_schllev + schltype, data = grade4_schllev)
    
    table3_MRR_col1_to_6 <- list(table3_MRR_col1, table3_MRR_col2, table3_MRR_col3,
                                 table3_MRR_col4, table3_MRR_col5, table3_MRR_col6)
    
    output <- capture_output(
        
        stargazer(table3_MRR_col1_to_6,
                  dep.var.caption = "<center>Percent disadvantaged</center>",
                  column.labels = c("<center>Fifth grade</center>", "<center>Fourth grade</center>"), column.separate = c(3, 3),
                  covariate.labels = c("f<sub>sc</sub>", "Enrollment", "Enrollment squared/100", "Piecewise linear trend"),
                  omit = c("Constant", "schltype"),
                  keep.stat = "n",
                  digits = 4,
                  no.space = TRUE, 
                  notes = "This table reports OLS estimates. The unit of analysis is the school.",
                  notes.append = FALSE, notes.label = "", notes.align = "l",
                  report = "vcs", table.layout = "=lc#-t-s=n",
                  type = "html")
        
    )
    
    display_html(output)   
}

#----

# Table A6 from Angrist et al. (2019)
tableA6_MRR <- function(data) {
    enrollment_2 <- (data$enrollment^2)/100
    f_sc <- maimonides_rule(data$enrollment)
    data_extended <- add_column(data, enrollment_2, f_sc)
    
    donut_1 <- data_extended %>% filter(enrollment < 39 | enrollment > 41)
    donut_2 <- data_extended %>% filter(enrollment < 38 | enrollment > 42)
    donut_3 <- data_extended %>% filter(enrollment < 37 | enrollment > 43)
    
    cell11 <- iv_robust(avgread ~ classsize + pct_disadv + enrollment | f_sc + pct_disadv + enrollment,
                        clusters = schlcode, se_type = "CR0", data = donut_1)
    cell21 <- iv_robust(avgread ~ classsize + pct_disadv + enrollment | f_sc + pct_disadv + enrollment,
                        clusters = schlcode, se_type = "CR0", data = donut_2)
    cell31 <- iv_robust(avgread ~ classsize + pct_disadv + enrollment | f_sc + pct_disadv + enrollment,
                        clusters = schlcode, se_type = "CR0", data = donut_3)
    
    cell12 <- iv_robust(avgread ~ classsize + pct_disadv + enrollment + enrollment_2 | f_sc + pct_disadv + enrollment + enrollment_2,
                        clusters = schlcode, se_type = "CR0", data = donut_1)
    cell22 <- iv_robust(avgread ~ classsize + pct_disadv + enrollment + enrollment_2 | f_sc + pct_disadv + enrollment + enrollment_2,
                        clusters = schlcode, se_type = "CR0", data = donut_2)
    cell32 <- iv_robust(avgread ~ classsize + pct_disadv + enrollment + enrollment_2 | f_sc + pct_disadv + enrollment + enrollment_2,
                        clusters = schlcode, se_type = "CR0", data = donut_3)
    
    cell13 <- iv_robust(avgmath ~ classsize + pct_disadv + enrollment | f_sc + pct_disadv + enrollment,
                        clusters = schlcode, se_type = "CR0", data = donut_1)
    cell23 <- iv_robust(avgmath ~ classsize + pct_disadv + enrollment | f_sc + pct_disadv + enrollment,
                        clusters = schlcode, se_type = "CR0", data = donut_2)
    cell33 <- iv_robust(avgmath ~ classsize + pct_disadv + enrollment | f_sc + pct_disadv + enrollment,
                        clusters = schlcode, se_type = "CR0", data = donut_3)
    
    cell14 <- iv_robust(avgmath ~ classsize + pct_disadv + enrollment + enrollment_2 | f_sc + pct_disadv + enrollment + enrollment_2,
                        clusters = schlcode, se_type = "CR0", data = donut_1)
    cell24 <- iv_robust(avgmath ~ classsize + pct_disadv + enrollment + enrollment_2 | f_sc + pct_disadv + enrollment + enrollment_2,
                        clusters = schlcode, se_type = "CR0", data = donut_2)
    cell34 <- iv_robust(avgmath ~ classsize + pct_disadv + enrollment + enrollment_2 | f_sc + pct_disadv + enrollment + enrollment_2,
                        clusters = schlcode, se_type = "CR0", data = donut_3)
    
    col1_coef <- c(cell11$coefficients[2], cell21$coefficients[2], cell31$coefficients[2])
    col2_coef <- c(cell12$coefficients[2], cell22$coefficients[2], cell32$coefficients[2])
    col3_coef <- c(cell13$coefficients[2], cell23$coefficients[2], cell33$coefficients[2])
    col4_coef <- c(cell14$coefficients[2], cell24$coefficients[2], cell34$coefficients[2])
    
    col1_se <- c(cell11$std.error[2], cell21$std.error[2], cell31$std.error[2])
    col2_se <- c(cell12$std.error[2], cell22$std.error[2], cell32$std.error[2])
    col3_se <- c(cell13$std.error[2], cell23$std.error[2], cell33$std.error[2])
    col4_se <- c(cell14$std.error[2], cell24$std.error[2], cell34$std.error[2])
    
    row_names <- c("Donut: [39, 41]", "Donut: [38, 42]", "Donut: [37, 43]")
    df_coef <- round(data.frame(col1_coef, col2_coef, col3_coef, col4_coef, row.names = row_names), 4)
    df_se <- round(data.frame(col1_se, col2_se, col3_se, col4_se, row.names = row_names), 4)
    
    return(list("coef" = df_coef, "se" = df_se))
}