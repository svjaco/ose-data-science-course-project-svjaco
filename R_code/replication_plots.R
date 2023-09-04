# Author: Sven Jacobs
# Content: Code generating the replication plots
# OSE data science, Prof. Dr. Philipp Eisenhauer | Summer 2021, M.Sc. Economics, Bonn University

#----

# Panels for Figure I from Angrist and Lavy (1999) 
figure1_panel <- function(data) {
    avg_csize <- aggregate(data$classsize,
                           by  = list(data$enrollment),
                           FUN = mean)
    colnames(avg_csize) <- c("enrollment", "avg_classsize")
    
    ifelse(identical(data, grade5), title <- "a. Fifth Grade", title <- "b. Fourth Grade")
    
    ggplot(data = avg_csize) + aes(x = enrollment) +
        geom_line(aes(y = avg_classsize, color = "Actual class size", linetype = "Actual class size")) +
        stat_function(fun = maimonides_rule, aes(color = "Maimonides Rule", linetype = "Maimonides Rule")) +
        scale_x_continuous(breaks = seq(0, 220, 20)) +
        scale_y_continuous(breaks = seq(5, 40, 5)) +
        labs(x = "Enrollment count", y = "Class size", title = title) +
        theme_light() +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_manual("", values = c("darkorange", "dodgerblue")) +
        scale_linetype_manual("", values = c("solid", "longdash"))
}

figure1_panelA <- figure1_panel(data = grade5)
figure1_panelB <- figure1_panel(data = grade4)

# Figure I from Angrist and Lavy (1999)
figure1 <- function(panelA = figure1_panelA, panelB = figure1_panelB) {
    ggarrange(panelA, panelB, ncol = 1, common.legend = TRUE, legend = "bottom")
}

#----

# Panels for Figure II from Angrist and Lavy (1999)
figure2_panel <- function(data, avgread_ticks) {
    data_truncated <- data %>% filter(enrollment > 9 & enrollment < 190)
   
    intervals <- cut(data_truncated$enrollment, breaks = c(seq(1, 170, 10), 190), right = FALSE)
    int_midpoints <- seq(5, 170, 10)
    if(is.element(10, data_truncated$enrollment) == FALSE) int_midpoints <- int_midpoints[-1]
    
    avgread_int <- aggregate(data_truncated$avgread,
                             by = list(intervals),
                             FUN = mean)
    colnames(avgread_int) <- c("interval", "score")
   
    classsize_pred <- maimonides_rule(data_truncated$enrollment)
    avg_classsize_pred <- aggregate(classsize_pred,
                                    by  = list(intervals),
                                    FUN = mean)
    colnames(avg_classsize_pred) <- c("interval", "classsize")
    
    par(mar = c(4.5, 4.5, 4.5, 4.5)) 
    
    plot(int_midpoints, avgread_int$score,
         type = "l",
         xaxt = "n", yaxt = "n",
         xlim = c(5, 165), ylim = c(min(avgread_ticks), max(avgread_ticks)),
         xlab = "Enrollment count", ylab = "Average reading score",
         main = ifelse(identical(data, grade5), "a. Fifth Grade", "b. Fourth Grade"))
    axis(side = 1, at = seq(5, 165, 20))
    axis(side = 2, at = avgread_ticks, las = 1)
    axis(side = 2, at = seq(min(avgread_ticks), max(avgread_ticks), 0.2), tck = -0.01, labels = FALSE)
    
    par(new = TRUE)
    
    plot(int_midpoints, avg_classsize_pred$classsize,
         type = "l", lty = "longdash",
         axes = FALSE, xlab = "", ylab = "",
         xlim = c(5, 165), ylim = c(5, 40))
    axis(side = 4, at = seq(5, 40, 5), las = 1)
    mtext("Average size function", side = 4, line = 3)
}

# Figure II from Angrist and Lavy (1999)
figure2 <- function() {
    layout(matrix(c(1, 2), 2))
    
    figure2_panel(data = grade5, 70:80)
    legend("bottomright", legend = c("Average test score", "Predicted class size"),
           lty = c("solid", "longdash"), bty = "n", cex = 0.8)
    
    figure2_panel(data = grade4, 68:78)
    legend("bottomright", legend = c("Average test score", "Predicted class size"),
           lty = c("solid", "longdash"), bty = "n", cex = 0.8)
}

#----

# Panels for Figure III from Angrist and Lavy (1999)
figure3_panel <- function(data, title, test) {
    data_truncated <- data %>% filter(enrollment > 9 & enrollment < 190)
    
    intervals <- cut(data_truncated$enrollment, breaks = c(seq(1, 170, 10), 190), right = FALSE)
    int_midpoints <- seq(5, 170, 10)
    if(is.element(10, data_truncated$enrollment) == FALSE) int_midpoints <- int_midpoints[-1]
    
    ifelse(test == "reading", data_truncated_test <- data_truncated$avgread, data_truncated_test <- data_truncated$avgmath)
    avgtest_int <- aggregate(data_truncated_test, 
                             by = list(intervals),
                             FUN = mean, na.rm = TRUE)
    colnames(avgtest_int) <- c("interval", "score")
    
    classsize_pred <- maimonides_rule(data_truncated$enrollment)
    avg_classsize_pred <- aggregate(classsize_pred,
                                    by  = list(intervals),
                                    FUN = mean)
    colnames(avg_classsize_pred) <- c("interval", "classsize")
    
    avg_enrollment_int <- aggregate(data_truncated$enrollment,
                                    by = list(intervals),
                                    FUN = mean)
    colnames(avg_enrollment_int) <- c("interval", "enrollment")
    
    avg_pct_disadv_int <- aggregate(data_truncated$pct_disadv,
                                    by = list(intervals),
                                    FUN = mean)
    colnames(avg_pct_disadv_int) <- c("interval", "pd")
    
    df <- data.frame(avgtest_int$score, avg_classsize_pred$classsize,
                     avg_enrollment_int$enrollment, avg_pct_disadv_int$pd) 
    colnames(df) <- c("score", "classsize", "enrollment", "pd")
    
    reg_score <- lm(score ~ enrollment + pd, data = df)
    reg_classsize <- lm(classsize ~ enrollment + pd, data = df)
    
    par(mar = c(4.5, 4.5, 4.5, 4.5)) 
    
    plot(int_midpoints, reg_score$residuals,
         type = "l",
         xaxt = "n", yaxt = "n",
         xlim = c(5, 165), ylim = c(-5, 5),
         xlab = "Enrollment count", ylab = ifelse(test == "reading", "Reading score residual", "Math score residual"),
         main = title)
    axis(side = 1, at = seq(5, 165, 20))
    axis(side = 2, at = -5:5, las = 1)
    axis(side = 2, at = seq(-5, 5, 0.2), tck = -0.01, labels = FALSE)
    
    par(new = TRUE)
    
    plot(int_midpoints, reg_classsize$residuals,
         type = "l", lty = "longdash",
         axes = FALSE, xlab = "", ylab = "",
         xlim = c(5, 165), ylim = c(-15, 15))
    axis(side = 4, at = seq(-15, 15, 5), las = 1)
    mtext("Size-function residual", side = 4, line = 3,
          cex = ifelse(title == "Fourth Grade (Math)", 1, 0.66))
}

# Figure III from Angrist and Lavy (1999)
figure3 <- function() {
    layout(matrix(1:3, 3))
    
    figure3_panel(data = grade5, "a. Fifth Grade (Reading)", "reading")
    legend("bottomright", legend = c("Average test score", "Predicted class size"),
           lty = c("solid", "longdash"), bty = "n", cex = 0.8)
    
    figure3_panel(data = grade4, "b. Fourth Grade (Reading)", "reading")
    legend("bottomright", legend = c("Average test score", "Predicted class size"),
           lty = c("solid", "longdash"), bty = "n", cex = 0.8)
    
    figure3_panel(data = grade5, "c. Fifth Grade (Math)", "math")
    legend("bottomright", legend = c("Average test score", "Predicted class size"),
           lty = c("solid", "longdash"), bty = "n", cex = 0.8)
}

#----

# Figure A6 Panel A from Angrist et al. (2019)
figureA6_MRR_hist <- function(data) {
    data_schllev <- data %>% filter(enrollment < 200) %>% group_by(schlcode) %>% slice(1)
    
    ggplot(data_schllev, aes(x = enrollment)) +
        geom_histogram(binwidth = 1, fill = "grey") + 
        geom_vline(xintercept = seq(40, 120, 40), color = "red3", size = 0.25) +
        scale_x_continuous(breaks = seq(20, 120, 20)) +
        scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) + 
        labs(x = "Enrollment", y = "Frequency",
             title = ifelse(identical(data, grade5), "5th Grade Enrollment", "4th Grade Enrollment")) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(angle = 90, hjust = 0.5))
}

figureA6_panelA_MRR <- function() {
    hist_5th <- figureA6_MRR_hist(data = grade5)
    hist_4th <- figureA6_MRR_hist(data = grade4)
    
    figure <- ggarrange(hist_5th, hist_4th, nrow = 1)
    
    annotate_figure(figure, top = text_grob("A. Histograms", face = "bold", size = 14))
}