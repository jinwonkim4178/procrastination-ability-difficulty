################################################################
########## Load Packages and a final analytic dataset ########## 
################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lme4)
library(clubSandwich)


df <- read_csv("./cleaned_data/final_analysis_dataset.csv")

###################################################################################################################################################
# =================================================================================================================================================
# Standardize the outcome variable and all predictors (i.e., course grade, academic performance propensity, course difficulty, and submission delay)

# procra_rank: procrastination index
# avg_late_hours: alternative measures for procrastination (the time difference between submission and deadline)
# diff: course difficulty
# abilities: academic performance propensity
# grade: course grade
# =================================================================================================================================================
###################################################################################################################################################

final_df <- df %>% 
  filter(!is.na(avg_late_hours)) %>% 
  mutate(across(all_of(c('grade', 'avg_late_hours', 'procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))


##########################################################
# ========================================================
# Robustness Check: Using a submission-to-deadline measure
# ========================================================
##########################################################

# Model 1 (courseGrade ~ submissionDelay + (1 | student))
model1_alt <- lmer(grade ~ avg_late_hours + (1 | student_id), data = final_df)
summary(model1_alt)
sjPlot::tab_model(model1_alt, show.se = TRUE, digits = 3)
performance::icc(model1_alt)

# Model 2 (courseGrade ~ submissionDelay + courseDifficulty + (1 | student))
model2_alt <- lmer(grade ~ avg_late_hours + diff + (1 | student_id), data = final_df)
summary(model2_alt)
sjPlot::tab_model(model2_alt, show.se = TRUE, digits = 3)
performance::icc(model2_alt)

# model3 (courseGrade ~ submissionDelay + performancePropensity + courseDifficulty +(1 | student))
model3_alt <- lmer(grade ~ avg_late_hours + diff + abilities + (1 | student_id), data = final_df)
summary(model3_alt)
sjPlot::tab_model(model3_alt, show.se = TRUE, digits = 3)
performance::icc(model3_alt)

# model4 (courseGrade ~ submissionDelay*courseDifficulty + performancePropensity + (1 | student))
model4_alt <- lmer(grade ~ avg_late_hours*diff + abilities + (1 | student_id), data = final_df)
summary(model4_alt)
sjPlot::tab_model(model4_alt, show.se = TRUE, digits = 3)
performance::icc(model4_alt)

# model5 (courseGrade ~ submissionDelay*courseDifficulty + submission delay*performancePropensity + (1 | student))
model5_alt <- lmer(grade ~ avg_late_hours*diff + avg_late_hours*abilities + (1 | student_id), data = final_df)
summary(model5_alt)
sjPlot::tab_model(model5_alt, show.se = TRUE, digits = 3)
performance::icc(model5_alt)

# model6 (courseGrade ~ submissionDelay*courseDifficulty + submissionDelay*performancePropensity + courseDifficulty*performancePropensity + (1 | student))
model6_alt <- lmer(grade ~ avg_late_hours*diff + avg_late_hours*abilities + diff*abilities + (1 | student_id), data = final_df)
summary(model6_alt)
sjPlot::tab_model(model6_alt, show.se = TRUE, digits = 3)
performance::icc(model6_alt)

# model7 (courseGrade ~ submissionDelay*courseDifficulty*performancePropensity + (1 | student))
model7_alt <- lmer(grade ~ avg_late_hours*diff*abilities + (1 | student_id), data = final_df)
summary(model7_alt)
sjPlot::tab_model(model7_alt, show.se = TRUE, digits = 3)
performance::icc(model7_alt)


# Calculating Robust Standard Error via cluster-robust variance estimators 
vcov1_alt <- vcovCR(model1_alt, type='CR2')
coef_test(model1_alt, vcov=vcov1_alt)

vcov2_alt <- vcovCR(model2_alt, type='CR2')
coef_test(model2_alt, vcov=vcov2_alt)

vcov3_alt <- vcovCR(model3_alt, type='CR2')
coef_test(model3_alt, vcov=vcov3_alt)

vcov4_alt <- vcovCR(model4_alt, type='CR2')
coef_test(model4_alt, vcov=vcov4_alt)

vcov5_alt <- vcovCR(model5_alt, type='CR2')
coef_test(model5_alt, vcov=vcov5_alt)

vcov6_alt <- vcovCR(model6_alt, type='CR2')
coef_test(model6_alt, vcov=vcov6_alt)

vcov7_alt <- vcovCR(model7_alt, type='CR2')
coef_test(model7_alt, vcov=vcov7_alt)



########################################################################################
# ======================================================================================
# Robustness Check: Replicate the main model specification after excluding non-submission
# ======================================================================================
########################################################################################

# Model 1 (courseGrade ~ procrastination + (1 | student))
model1_replic <- lmer(grade ~ procra_rank + (1 | student_id), data = final_df)
summary(model1_replic)
sjPlot::tab_model(model1_replic, show.se = TRUE, digits = 3)
performance::icc(model1_replic)

# Model 2 (courseGrade ~ procrastination + courseDifficulty + (1 | student))
model2_replic <- lmer(grade ~ procra_rank + diff + (1 | student_id), data = final_df)
summary(model2_replic)
sjPlot::tab_model(model2_replic, show.se = TRUE, digits = 3)
performance::icc(model2_replic)

# model3 (courseGrade ~ procrastination + performancePropensity + courseDifficulty +(1 | student))
model3_replic <- lmer(grade ~ procra_rank + diff + abilities + (1 | student_id), data = final_df)
summary(model3_replic)
sjPlot::tab_model(model3_replic, show.se = TRUE, digits = 3)
performance::icc(model3_replic)

# model4 (courseGrade ~ procrastination*courseDifficulty + performancePropensity + (1 | student))
model4_replic <- lmer(grade ~ procra_rank*diff + abilities + (1 | student_id), data = final_df)
summary(model4_replic)
sjPlot::tab_model(model4_replic, show.se = TRUE, digits = 3)
performance::icc(model4_replic)

# model5 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + (1 | student))
model5_replic <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + (1 | student_id), data = final_df)
summary(model5_replic)
sjPlot::tab_model(model5_replic, show.se = TRUE, digits = 3)
performance::icc(model5_replic)

# model6 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + courseDifficulty*performancePropensity + (1 | student))
model6_replic <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + diff*abilities + (1 | student_id), data = final_df)
summary(model6_replic)
sjPlot::tab_model(model6_replic, show.se = TRUE, digits = 3)
performance::icc(model6_replic)

# model7 (courseGrade ~ procrastination*courseDifficulty*performancePropensity + (1 | student))
model7_replic <- lmer(grade ~ procra_rank*diff*abilities + (1 | student_id), data = final_df)
summary(model7_replic)
sjPlot::tab_model(model7_replic, show.se = TRUE, digits = 3)
performance::icc(model7_replic)


# Calculating Robust Standard Error via cluster-robust variance estimators 
vcov1_replic <- vcovCR(model1_replic, type='CR2')
coef_test(model1_replic, vcov=vcov1_replic)

vcov2_replic <- vcovCR(model2_replic, type='CR2')
coef_test(model2_replic, vcov=vcov2_replic)

vcov3_replic <- vcovCR(model3_replic, type='CR2')
coef_test(model3_replic, vcov=vcov3_replic)

vcov4_replic <- vcovCR(model4_replic, type='CR2')
coef_test(model4_replic, vcov=vcov4_replic)

vcov5_replic <- vcovCR(model5_replic, type='CR2')
coef_test(model5_replic, vcov=vcov5_replic)

vcov6_replic <- vcovCR(model6_replic, type='CR2')
coef_test(model6_replic, vcov=vcov6_replic)

vcov7_replic <- vcovCR(model7_replic, type='CR2')
coef_test(model7_replic, vcov=vcov7_replic)



########################################################################################
# ======================================================================================
# Robustness Check: Using a submission-to-deadline measure (Excluding outliers)
# ======================================================================================
########################################################################################

final_df_remove_outliers <- df %>% 
  filter(!is.na(avg_late_hours)) %>% 
  mutate(avg_late_hours_z = as.numeric(scale(avg_late_hours))) %>%
  filter(between(avg_late_hours_z, -3, 3)) %>%
  select(-avg_late_hours_z) %>% 
  mutate(across(all_of(c('grade', 'avg_late_hours', 'procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))


# Model 1 (courseGrade ~ submissionDelay + (1 | student))
model1_remove <- lmer(grade ~ avg_late_hours + (1 | student_id), data = final_df_remove_outliers)
summary(model1_remove)
sjPlot::tab_model(model1_remove, show.se = TRUE, digits = 3)
performance::icc(model1_remove)

# Model 2 (courseGrade ~ submissionDelay + courseDifficulty + (1 | student))
model2_remove <- lmer(grade ~ avg_late_hours + diff + (1 | student_id), data = final_df_remove_outliers)
summary(model2_remove)
sjPlot::tab_model(model2_remove, show.se = TRUE, digits = 3)
performance::icc(model2_remove)

# model3 (courseGrade ~ submissionDelay + performancePropensity + courseDifficulty +(1 | student))
model3_remove <- lmer(grade ~ avg_late_hours + diff + abilities + (1 | student_id), data = final_df_remove_outliers)
summary(model3_remove)
sjPlot::tab_model(model3_remove, show.se = TRUE, digits = 3)
performance::icc(model3_remove)

# model4 (courseGrade ~ submissionDelay*courseDifficulty + performancePropensity + (1 | student))
model4_remove <- lmer(grade ~ avg_late_hours*diff + abilities + (1 | student_id), data = final_df_remove_outliers)
summary(model4_remove)
sjPlot::tab_model(model4_remove, show.se = TRUE, digits = 3)
performance::icc(model4_remove)

# model5 (courseGrade ~ submissionDelay*courseDifficulty + submission delay*performancePropensity + (1 | student))
model5_remove <- lmer(grade ~ avg_late_hours*diff + avg_late_hours*abilities + (1 | student_id), data = final_df_remove_outliers)
summary(model5_remove)
sjPlot::tab_model(model5_remove, show.se = TRUE, digits = 3)
performance::icc(model5_remove)

# model6 (courseGrade ~ submissionDelay*courseDifficulty + submissionDelay*performancePropensity + courseDifficulty*performancePropensity + (1 | student))
model6_remove <- lmer(grade ~ avg_late_hours*diff + avg_late_hours*abilities + diff*abilities + (1 | student_id), data = final_df_remove_outliers)
summary(model6_remove)
sjPlot::tab_model(model6_remove, show.se = TRUE, digits = 3)
performance::icc(model6_remove)

# model7 (courseGrade ~ submissionDelay*courseDifficulty*performancePropensity + (1 | student))
model7_remove <- lmer(grade ~ avg_late_hours*diff*abilities + (1 | student_id), data = final_df_remove_outliers)
summary(model7_remove)
sjPlot::tab_model(model7_remove, show.se = TRUE, digits = 3)
performance::icc(model7_remove)


# Calculating Robust Standard Error via cluster-robust variance estimators 
vcov1_remove <- vcovCR(model1_remove, type='CR2')
coef_test(model1_remove, vcov=vcov1_remove)

vcov2_remove <- vcovCR(model2_remove, type='CR2')
coef_test(model2_remove, vcov=vcov2_remove)

vcov3_remove <- vcovCR(model3_remove, type='CR2')
coef_test(model3_remove, vcov=vcov3_remove)

vcov4_remove <- vcovCR(model4_remove, type='CR2')
coef_test(model4_remove, vcov=vcov4_remove)

vcov5_remove <- vcovCR(model5_remove, type='CR2')
coef_test(model5_remove, vcov=vcov5_remove)

vcov6_remove <- vcovCR(model6_remove, type='CR2')
coef_test(model6_remove, vcov=vcov6_remove)

vcov7_remove <- vcovCR(model7_remove, type='CR2')
coef_test(model7_remove, vcov=vcov7_remove)





