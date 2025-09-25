################################################################
########## Load Packages and a final analytic dataset ########## 
################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lme4)
library(clubSandwich)
library(interactions)
library(ggplot2)
library(ggpubr)

df <- read_csv("./cleaned_data/final_analysis_dataset.csv")

###########################################################################################################################
###### Standardize three explanatory variables (i.e., student ability, course difficulty, and procrastination index) ######
###########################################################################################################################

# procra_rank: procrastination index
# diff: course difficulty
# abilities: student ability

final_df <- df %>%
  mutate(across(all_of(c('procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))


#######################################################
###### Hierarchical linear models (HLM) analysis ###### 
#######################################################

# Model1 (courseGrade ~ procrastination + (1 | student))
model1 <- lmer(grade ~ procra_rank + (1 | student_id), data = final_df)
summary(model1)
sjPlot::tab_model(model1, digits = 3)
performance::icc(model1)

# Model 2 (courseGrade ~ procrastination + courseDifficulty + (1 | student))
model2 <- lmer(grade ~ procra_rank + diff + (1 | student_id), data = final_df)
summary(model2)
sjPlot::tab_model(model2, digits = 3)
performance::icc(model2)

# Model 3 (courseGrade ~ procrastination + courseDifficulty + studentAbility + (1 | student))
model3 <- lmer(grade ~ procra_rank + diff + abilities + (1 | student_id), data = final_df)
summary(model3)
sjPlot::tab_model(model3, digits = 3)
performance::icc(model3)

# Model 4 (courseGrade ~ procrastination*courseDifficulty + studentAbility + (1 | student))
model4 <- lmer(grade ~ procra_rank*diff + abilities + (1 | student_id), data = final_df)
summary(model4)
sjPlot::tab_model(model4, digits = 3)
performance::icc(model4)

# model5 (courseGrade ~ procrastination*courseDifficulty + procrastination*studentAbility +  (1 | student))
model5 <- lmer(grade ~ procra_rank*diff + procra_rank*abilities +  (1 | student_id), data = final_df)
summary(model5)
sjPlot::tab_model(model5, digits = 3)
performance::icc(model5)

# model6 (courseGrade ~ procrastination*courseDifficulty*studentAbility + (1 | student))
model6 <- lmer(grade ~ procra_rank*diff*abilities + (1 | student_id), data = final_df)
summary(model6)
sjPlot::tab_model(model6, digits = 3)
performance::icc(model6)


### Checking distributional and modeling assumptions via visual inspection of standard diagnostic plots
# Q-Q Plot
resid_vals_m6 <- resid(model6)
qqnorm(resid_vals_m6)
qqline(resid_vals_m6)

# Residuals vs. fitted values plot
fhat_m6  <- fitted(model6)  
plot(fhat_m6, resid_vals_m6,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted") +
  abline(h = 0, lty = 2)

# Scale–Location: sqrt(|standardized residuals|)
y_sl <- sqrt(abs(resid_vals_m6))

plot(fhat_m6, y_sl,
     xlab = "Fitted values",
     ylab = "Sqrt(|standardized residuals|)",
     main = "Scale–Location (Spread–Location)",
     pch = 16, cex = 0.6, col = "gray30")

order <- order(fhat_m6)
sl_fit <- loess(y_sl ~ fhat_m6, span = 0.6)
lines(fhat_m6[order], predict(sl_fit)[order], col = "red", lwd = 2)

# residuals vs. leverage plots
check_model_outliers <- performance::check_model(model6, check = "outliers")
plot(check_model_outliers)


### Calculating Robust Standard Error (All reported results are based on robust standard errors)
vcov1 <- vcovCR(model1, type='CR2')
coef_test(model1, vcov=vcov1)

vcov2 <- vcovCR(model2, type='CR2')
coef_test(model2, vcov=vcov2)

vcov3 <- vcovCR(model3, type='CR2')
coef_test(model3, vcov=vcov3)

vcov4 <- vcovCR(model4, type='CR2')
coef_test(model4, vcov=vcov4)

vcov5 <- vcovCR(model5, type='CR2')
coef_test(model5, vcov=vcov5)

vcov6 <- vcovCR(model6, type='CR2')
coef_test(model6, vcov=vcov6)


### Comparing the six HLM models ###
anova(model1, model2, model3, model4, model5, model6)


### Generating Fig. 1 - three-way interaction effects of procrastination, latent student ability, and course difficulty on course performance
interaction_plot <- interact_plot(model6, pred = procra_rank, modx = abilities, mod2 = diff,
                                  interval = TRUE, colors = "blue",
                                  x.label = "Procrastination Index",
                                  y.label = "Course Grade",
                                  legend.main = "Student Ability",
                                  modx.labels = c("-1 SD", "Mean", "+1 SD"),
                                  mod2.labels = c("Mean of Course Difficulty -1 SD",
                                                  "Mean of Course Difficulty",
                                                  "Mean of Course Difficulty +1 SD"),) +
  scale_y_continuous(limits = c(2, 4),
                     breaks = seq(2, 4, by = 0.5))


ggsave("./figure/three_way_interaction_plot.png", plot = interaction_plot, width = 10, height = 6)


### Generating Fig. 2 and Fig. 3
plot_df <- final_df %>%
  mutate(
    procra_grp  = if_else(procra_rank >= median(procra_rank, na.rm = TRUE),
                          "High procrastination", "Low procrastination"),
    ability_grp = if_else(abilities   >= median(abilities, na.rm = TRUE),
                          "High ability", "Low ability"),
    course_grp = if_else(diff >= median(diff, na.rm=TRUE),
                         "Hard courses", "Easy courses"),
    term_desc = factor(term_desc,
                       levels = c("Fall 2019", "Winter 2020", "Spring 2020",
                                  "Fall 2020", "Winter 2021", "Spring 2021",
                                  "Fall 2021", "Winter 2022", "Spring 2022",
                                  "Fall 2022", "Winter 2023", "Spring 2023",
                                  "Fall 2023", "Winter 2024", "Spring 2024")),
    ability_grp = factor(ability_grp, levels = c("High ability","Low ability")),
    course_grp  = factor(course_grp,  levels = c("Hard courses","Easy courses")),
    facet_grp = paste(ability_grp, "&", course_grp),
    facet_grp = factor(facet_grp,
                       levels = c("High ability & Hard courses",
                                  "High ability & Easy courses",
                                  "Low ability & Hard courses",
                                  "Low ability & Easy courses"))
  )


### Fig. 2
p2_data <- plot_df %>%
  group_by(term_desc, procra_grp) %>%
  summarise(avg_grade = mean(grade, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(p2_data, aes(term_desc, avg_grade, color = procra_grp, group = procra_grp)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_discrete() +
  labs(x = "Semester", y = "Average Course Grades",
       color = "Procrastination") +
  scale_y_continuous(limits = c(2.5, 4),
                     breaks = seq(2.5, 4, by = 0.5)) +
  ggpubr::theme_pubr() + 
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))

p2_diff_data <- p2_data %>%
  pivot_wider(names_from = procra_grp, values_from = avg_grade) %>%
  mutate(diff = `Low procrastination` - `High procrastination`) %>%
  select(term_desc, diff)


p2_diff <- ggplot(p2_diff_data, aes(term_desc, diff, group = 1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  labs(x = "Semester", y = "Difference in Course Grades") +
  ggpubr::theme_pubr() + 
  theme(legend.position = "none",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

p2_combined <- ggarrange(p2, p2_diff, ncol = 1, heights = c(1.5, 1), align = "v")

ggsave("./figure/plot2_combined.png", plot = p2_combined, width = 10, height = 8)


### Fig. 3
p_data3 <- plot_df %>%
  group_by(term_desc, facet_grp, procra_grp) %>%
  summarise(avg_grade = mean(grade, na.rm = TRUE), .groups = "drop")

p3_diff_data <- p_data3 %>%
  pivot_wider(names_from = procra_grp, values_from = avg_grade) %>%
  mutate(diff = `Low procrastination` - `High procrastination`) %>%
  select(term_desc, facet_grp, diff)

p3_diff <- ggplot(p3_diff_data, aes(term_desc, diff, group = 1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 2, color = "black") +
  facet_wrap(~ facet_grp, ncol = 2) +
  labs(x = "Semester", y = "Difference in Course Grades") +
  ggpubr::theme_pubr() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        strip.text   = element_text(size = 12))

ggsave("./figure/plot3_diff_2X2.png", plot = p3_diff, width = 10, height = 5)



