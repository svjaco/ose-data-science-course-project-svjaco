# Author: Sven Jacobs
# Content: Code generating the extension plots
# OSE data science, Prof. Dr. Philipp Eisenhauer | Summer 2021, M.Sc. Economics, Bonn University

#----

# Figure EXT_1
figure1_EXT <- function() {
    df_grade4 <- setNames(data.frame(matrix(nrow = 2*nrow(grade4), ncol = 3)), c("test", "grade", "avg_score"))
    df_grade5 <- setNames(data.frame(matrix(nrow = 2*nrow(grade5), ncol = 3)), c("test", "grade", "avg_score"))
    
    df_grade4$grade <- "4th grade"
    df_grade4$test[1:nrow(grade4)] <- "reading"
    df_grade4$avg_score[1:nrow(grade4)] <- grade4$avgread
    df_grade4$test[(nrow(grade4) + 1):nrow(df_grade4)] <- "math"
    df_grade4$avg_score[(nrow(grade4) + 1):nrow(df_grade4)] <- grade4$avgmath
    
    df_grade5$grade <- "5th grade"
    df_grade5$test[1:nrow(grade5)] <- "reading"
    df_grade5$avg_score[1:nrow(grade5)] <- grade5$avgread
    df_grade5$test[(nrow(grade5) + 1):nrow(df_grade5)] <- "math"
    df_grade5$avg_score[(nrow(grade5) + 1):nrow(df_grade5)] <- grade5$avgmath
    
    df_comb <- rbind(df_grade4, df_grade5)
    
    ggplot(df_comb, aes(x = test, y = avg_score, fill = grade)) + 
        geom_boxplot() +
        labs(x = "Test", y = "Average score") +
        scale_y_continuous(limits = c(20, 100), breaks = seq(20, 100, 10)) +
        theme_classic() + theme(legend.title = element_blank())
}

#----

# Figure EXT_3
figure3_EXT_panel <- function(data) {
    data_schllev <- data %>% group_by(schlcode) %>% slice(1)
    
    ggplot(data_schllev, aes(x = enrollment, y = pct_disadv)) + 
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        stat_cor(method = "pearson", cor.coef.name = "rho", aes(label = ..r.label..), label.x = 150, label.y = 75) +
        ylim(0, 80) + 
        labs(x = "Enrollment", y = "Percent disadvantaged",
             title = ifelse(identical(data, grade5), "Fifth Grade", "Fourth Grade")) +
        theme_classic() + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.x = element_text(margin = margin(t = 10)),
              axis.title.y = element_text(margin = margin(r = 10)))
}

figure3_EXT <- function() {
    panelA <- figure3_EXT_panel(data = grade5)
    panelB <- figure3_EXT_panel(data = grade4)
    
    ggarrange(panelA, panelB, nrow = 1)
}

#----

# Figure EXT_4
figure4_EXT_densities <- function(data) {
    data_schllev <- data %>% filter(enrollment < 200) %>% group_by(schlcode) %>% slice(1)
    
    rdd <- rddensity(data_schllev$enrollment, c = 41)
    
    ggplot <- rdplotdensity(rdd, X = data_schllev$enrollment,
                            plotRange = c(min(data_schllev$enrollment), 80), plotN = c(50, 50),
                            histBreaks = seq(min(data_schllev$enrollment) - 0.5, 80.5, 1),
                            xlabel = "Enrollment",
                            title = ifelse(identical(data, grade5), "5th Grade", "4th Grade"))
    
    ggplot$Estplot + 
        scale_y_continuous(limits = c(0, 0.02), expand = c(0, 0)) +
        theme_classic() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
}

figure4_EXT <- function() {
    density_5th <- figure4_EXT_densities(data = grade5)
    density_4th <- figure4_EXT_densities(data = grade4)
    
    ggarrange(density_5th, density_4th, nrow = 1)
}

#----

# Panels for Figure EXT_5
figure5_EXT_panel <- function(data) {
    data_truncated <- data %>% filter(enrollment > 9 & enrollment < 190) %>% group_by(schlcode) %>% slice(1)
    
    intervals <- cut(data_truncated$enrollment, breaks = c(seq(1, 170, 10), 190), right = FALSE)
    int_midpoints <- seq(5, 170, 10)
    if(is.element(10, data_truncated$enrollment) == FALSE) int_midpoints <- int_midpoints[-1]
    
    pct_disadv_int <- aggregate(data_truncated$pct_disadv, 
                                by = list(intervals),
                                FUN = mean)
    colnames(pct_disadv_int) <- c("interval", "pd")
    
    classsize_pred <- maimonides_rule(data_truncated$enrollment)
    avg_classsize_pred <- aggregate(classsize_pred,
                                    by  = list(intervals),
                                    FUN = mean)
    colnames(avg_classsize_pred) <- c("interval", "classsize")
    
    avg_enrollment_int <- aggregate(data_truncated$enrollment,
                                    by = list(intervals),
                                    FUN = mean)
    colnames(avg_enrollment_int) <- c("interval", "enrollment")
    
    schltype_int <- aggregate(data_truncated$schltype,
                              by = list(intervals),
                              FUN = mean)
    colnames(schltype_int) <- c("interval", "schltype")
    
    df <- data.frame(pct_disadv_int$pd, avg_classsize_pred$classsize,
                     avg_enrollment_int$enrollment, schltype_int$schltype) 
    colnames(df) <- c("pd", "classsize", "enrollment", "schltype")
    
    reg_pd <- lm(pd ~ enrollment + schltype, data = df)
    reg_classsize <- lm(classsize ~ enrollment, data = df)
    
    par(mar = c(4.5, 4.5, 4.5, 4.5))
    
    plot(int_midpoints, reg_pd$residuals,
         type = "l",
         xaxt = "n", yaxt = "n",
         xlim = c(5, 165), ylim = c(-5, 5),
         xlab = "Enrollment count", ylab = "Percentage disadvantaged residual",
         main = ifelse(identical(data, grade5), "a. Fifth Grade", "b. Fourth Grade"))
    axis(side = 1, at = seq(5, 165, 20))
    axis(side = 2, at = -5:5, las = 1)
    axis(side = 2, at = seq(-5, 5, 0.2), tck = -0.01, labels = FALSE)
    
    par(new = TRUE)
    
    plot(int_midpoints, reg_classsize$residuals,
         type = "l", lty = "longdash",
         axes = FALSE, xlab = "", ylab = "",
         xlim = c(5, 165), ylim = c(-15, 15))
    axis(side = 4, at = seq(-15, 15, 5), las = 1)
    mtext("Size-function residual", side = 4, line = 3)
}

# Figure EXT_5
figure5_EXT <- function() {
    layout(matrix(1:2, 2))
    
    figure5_EXT_panel(data = grade5)
    legend("bottomright", legend = c("Percentage disadvantaged", "Predicted class size"),
           lty = c("solid", "longdash"), bty = "n", cex = 0.8)
    
    figure5_EXT_panel(data = grade4)
    legend("bottomright", legend = c("Percentage disadvantaged", "Predicted class size"),
           lty = c("solid", "longdash"), bty = "n", cex = 0.8)
}

#----

# Figure EXT_A1
figureA1_EXT <- function() {
    avg_score_reading_mean_3rd <- 86.3
    avg_score_reading_mean_4th <- mean(grade4$avgread)
    avg_score_reading_mean_5th <- mean(grade5$avgread)
    
    avg_score_math_mean_3rd <- 84.1
    avg_score_math_mean_4th <- mean(grade4$avgmath)
    avg_score_math_mean_5th <- mean(grade5$avgmath, na.rm = TRUE)
    
    df_bar_plot <- setNames(data.frame(matrix(nrow = 6, ncol = 3)), c("test", "grade", "avg_score_mean"))
    df_bar_plot$test <- rep(c("reading", "math"), 3)
    df_bar_plot$grade <- c(rep("3rd grade", 2), rep("4th grade", 2), rep("5th grade", 2))
    df_bar_plot$avg_score_mean <- c(avg_score_reading_mean_3rd, avg_score_math_mean_3rd,
                                    avg_score_reading_mean_4th, avg_score_math_mean_4th,
                                    avg_score_reading_mean_5th, avg_score_math_mean_5th)
    
    ggplot(df_bar_plot, aes(x = test, y = avg_score_mean, fill = grade)) + 
        geom_bar(position = "dodge", stat = "identity") +
        geom_text(aes(label = round(avg_score_mean, 1)), position = position_dodge(width = 0.9), vjust = -1) +
        labs(x = "Test", y = "Average score - Mean") +
        scale_y_continuous(breaks = seq(0, 80, 20), expand = expansion(mult = c(0, 0.05))) +
        scale_fill_brewer(palette = "Set1") +
        theme_classic() +
        theme(legend.title = element_blank(), axis.title.y = element_text(margin = margin(r = 10)))
}

#----

# Figure EXT_A2
figureA2_EXT <- function() {
    figure3_panel(data = grade4, "Fourth Grade (Math)", "math")
    legend("bottomright", legend = c("Average test score", "Predicted class size"),
           lty = c("solid", "longdash"), bty = "n", cex = 0.8)
}