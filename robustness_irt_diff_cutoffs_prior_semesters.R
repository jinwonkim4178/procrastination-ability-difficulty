################################################################
########## Load Packages and a final analytic dataset ########## 
################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lme4)
library(clubSandwich)


# Alternative analytic datasets used for robustness checks:
# B+ threshold, B threshold, and prior-semester Rasch calibration.

df_bplus <- read_csv("./cleaned_data/final_analysis_dataset_b+.csv") 
df_b <- read_csv("./cleaned_data/final_analysis_dataset_b.csv") 
df_prior <- read_csv("./cleaned_data/final_analysis_dataset_prior_semesters.csv")
  
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

final_df_bplus <- df_bplus %>% 
  mutate(across(all_of(c('grade', 'procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))

final_df_b <- df_b %>% 
  mutate(across(all_of(c('grade', 'procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))

final_df_prior <- df_prior %>% 
  mutate(across(all_of(c('grade', 'procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))

##########################################################
# ========================================================
# Robustness Check (Table A8): Using a B+ Grade Cutoff
# ========================================================
##########################################################

# Model 1 (courseGrade ~ procrastination + (1 | student))
model1_bplus <- lmer(grade ~ procra_rank + (1 | student_id), data = final_df_bplus)
summary(model1_bplus)
sjPlot::tab_model(model1_bplus, show.se = TRUE, digits = 3)
performance::icc(model1_bplus)

# Model 2 (courseGrade ~ procrastination + courseDifficulty + (1 | student))
model2_bplus <- lmer(grade ~ procra_rank + diff + (1 | student_id), data = final_df_bplus)
summary(model2_bplus)
sjPlot::tab_model(model2_bplus, show.se = TRUE, digits = 3)
performance::icc(model2_bplus)

# model3 (courseGrade ~ procrastination + performancePropensity + courseDifficulty +(1 | student))
model3_bplus <- lmer(grade ~ procra_rank + diff + abilities + (1 | student_id), data = final_df_bplus)
summary(model3_bplus)
sjPlot::tab_model(model3_bplus, show.se = TRUE, digits = 3)
performance::icc(model3_bplus)

# model4 (courseGrade ~ procrastination*courseDifficulty + performancePropensity + (1 | student))
model4_bplus <- lmer(grade ~ procra_rank*diff + abilities + (1 | student_id), data = final_df_bplus)
summary(model4_bplus)
sjPlot::tab_model(model4_bplus, show.se = TRUE, digits = 3)
performance::icc(model4_bplus)

# model5 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + (1 | student))
model5_bplus <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + (1 | student_id), data = final_df_bplus)
summary(model5_bplus)
sjPlot::tab_model(model5_bplus, show.se = TRUE, digits = 3)
performance::icc(model5_bplus)

# model6 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + courseDifficulty*performancePropensity + (1 | student))
model6_bplus <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + diff*abilities + (1 | student_id), data = final_df_bplus)
summary(model6_bplus)
sjPlot::tab_model(model6_bplus, show.se = TRUE, digits = 3)
performance::icc(model6_bplus)

# model7 (courseGrade ~ procrastination*courseDifficulty*performancePropensity + (1 | student))
model7_bplus <- lmer(grade ~ procra_rank*diff*abilities + (1 | student_id), data = final_df_bplus)
summary(model7_bplus)
sjPlot::tab_model(model7_bplus, show.se = TRUE, digits = 3)
performance::icc(model7_bplus)


# Calculating Robust Standard Error via cluster-robust variance estimators 
vcov1_bplus <- vcovCR(model1_bplus, type='CR2')
coef_test(model1_bplus, vcov=vcov1_bplus)

vcov2_bplus <- vcovCR(model2_bplus, type='CR2')
coef_test(model2_bplus, vcov=vcov2_bplus)

vcov3_bplus <- vcovCR(model3_bplus, type='CR2')
coef_test(model3_bplus, vcov=vcov3_bplus)

vcov4_bplus <- vcovCR(model4_bplus, type='CR2')
coef_test(model4_bplus, vcov=vcov4_bplus)

vcov5_bplus <- vcovCR(model5_bplus, type='CR2')
coef_test(model5_bplus, vcov=vcov5_bplus)

vcov6_bplus <- vcovCR(model6_bplus, type='CR2')
coef_test(model6_bplus, vcov=vcov6_bplus)

vcov7_bplus <- vcovCR(model7_bplus, type='CR2')
coef_test(model7_bplus, vcov=vcov7_bplus)


anova(model1_bplus, model2_bplus, model3_bplus, model4_bplus, model5_bplus, model6_bplus, model7_bplus)

##########################################################
# ========================================================
# Robustness Check (Table A9): Using a B Grade Cutoff
# ========================================================
##########################################################

# Model 1 (courseGrade ~ procrastination + (1 | student))
model1_b <- lmer(grade ~ procra_rank + (1 | student_id), data = final_df_b)
summary(model1_b)
sjPlot::tab_model(model1_b, show.se = TRUE, digits = 3)
performance::icc(model1_b)

# Model 2 (courseGrade ~ procrastination + courseDifficulty + (1 | student))
model2_b <- lmer(grade ~ procra_rank + diff + (1 | student_id), data = final_df_b)
summary(model2_b)
sjPlot::tab_model(model2_b, show.se = TRUE, digits = 3)
performance::icc(model2_b)

# model3 (courseGrade ~ procrastination + performancePropensity + courseDifficulty +(1 | student))
model3_b <- lmer(grade ~ procra_rank + diff + abilities + (1 | student_id), data = final_df_b)
summary(model3_b)
sjPlot::tab_model(model3_b, show.se = TRUE, digits = 3)
performance::icc(model3_b)

# model4 (courseGrade ~ procrastination*courseDifficulty + performancePropensity + (1 | student))
model4_b <- lmer(grade ~ procra_rank*diff + abilities + (1 | student_id), data = final_df_b)
summary(model4_b)
sjPlot::tab_model(model4_b, show.se = TRUE, digits = 3)
performance::icc(model4_b)

# model5 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + (1 | student))
model5_b <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + (1 | student_id), data = final_df_b)
summary(model5_b)
sjPlot::tab_model(model5_b, show.se = TRUE, digits = 3)
performance::icc(model5_b)

# model6 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + courseDifficulty*performancePropensity + (1 | student))
model6_b <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + diff*abilities + (1 | student_id), data = final_df_b)
summary(model6_b)
sjPlot::tab_model(model6_b, show.se = TRUE, digits = 3)
performance::icc(model6_b)

# model7 (courseGrade ~ procrastination*courseDifficulty*performancePropensity + (1 | student))
model7_b <- lmer(grade ~ procra_rank*diff*abilities + (1 | student_id), data = final_df_b)
summary(model7_b)
sjPlot::tab_model(model7_b, show.se = TRUE, digits = 3)
performance::icc(model7_b)


# Calculating Robust Standard Error via cluster-robust variance estimators 
vcov1_b <- vcovCR(model1_b, type='CR2')
coef_test(model1_b, vcov=vcov1_b)

vcov2_b <- vcovCR(model2_b, type='CR2')
coef_test(model2_b, vcov=vcov2_b)

vcov3_b <- vcovCR(model3_b, type='CR2')
coef_test(model3_b, vcov=vcov3_b)

vcov4_b <- vcovCR(model4_b, type='CR2')
coef_test(model4_b, vcov=vcov4_b)

vcov5_b <- vcovCR(model5_b, type='CR2')
coef_test(model5_b, vcov=vcov5_b)

vcov6_b <- vcovCR(model6_b, type='CR2')
coef_test(model6_b, vcov=vcov6_b)

vcov7_b <- vcovCR(model7_b, type='CR2')
coef_test(model7_b, vcov=vcov7_b)

anova(model1_b, model2_b, model3_b, model4_b, model5_b, model6_b, model7_b)

################################################################################################################################
# ==============================================================================================================================
# Robustness Check (Table A7): Using Academic Performance Propensity and Course Difficulty Estimated from Prior-Semester Grades
# ==============================================================================================================================
################################################################################################################################

# Model 1 (courseGrade ~ procrastination + (1 | student))
model1_prior <- lmer(grade ~ procra_rank + (1 | student_id), data = final_df_prior)
summary(model1_prior)
sjPlot::tab_model(model1_prior, show.se = TRUE, digits = 3)
performance::icc(model1_prior)

# Model 2 (courseGrade ~ procrastination + courseDifficulty + (1 | student))
model2_prior <- lmer(grade ~ procra_rank + diff + (1 | student_id), data = final_df_prior)
summary(model2_prior)
sjPlot::tab_model(model2_prior, show.se = TRUE, digits = 3)
performance::icc(model2_prior)

# model3 (courseGrade ~ procrastination + performancePropensity + courseDifficulty +(1 | student))
model3_prior <- lmer(grade ~ procra_rank + diff + abilities + (1 | student_id), data = final_df_prior)
summary(model3_prior)
sjPlot::tab_model(model3_prior, show.se = TRUE, digits = 3)
performance::icc(model3_prior)

# model4 (courseGrade ~ procrastination*courseDifficulty + performancePropensity + (1 | student))
model4_prior <- lmer(grade ~ procra_rank*diff + abilities + (1 | student_id), data = final_df_prior)
summary(model4_prior)
sjPlot::tab_model(model4_prior, show.se = TRUE, digits = 3)
performance::icc(model4_prior)

# model5 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + (1 | student))
model5_prior <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + (1 | student_id), data = final_df_prior)
summary(model5_prior)
sjPlot::tab_model(model5_prior, show.se = TRUE, digits = 3)
performance::icc(model5_prior)

# model6 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + courseDifficulty*performancePropensity + (1 | student))
model6_prior <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + diff*abilities + (1 | student_id), data = final_df_prior)
summary(model6_prior)
sjPlot::tab_model(model6_prior, show.se = TRUE, digits = 3)
performance::icc(model6_prior)

# model7 (courseGrade ~ procrastination*courseDifficulty*performancePropensity + (1 | student))
model7_prior <- lmer(grade ~ procra_rank*diff*abilities + (1 | student_id), data = final_df_prior)
summary(model7_prior)
sjPlot::tab_model(model7_prior, show.se = TRUE, digits = 3)
performance::icc(model7_prior)


# Calculating Robust Standard Error via cluster-robust variance estimators 
vcov1_prior <- vcovCR(model1_prior, type='CR2')
coef_test(model1_prior, vcov=vcov1_prior)

vcov2_prior <- vcovCR(model2_prior, type='CR2')
coef_test(model2_prior, vcov=vcov2_prior)

vcov3_prior <- vcovCR(model3_prior, type='CR2')
coef_test(model3_prior, vcov=vcov3_prior)

vcov4_prior <- vcovCR(model4_prior, type='CR2')
coef_test(model4_prior, vcov=vcov4_prior)

vcov5_prior <- vcovCR(model5_prior, type='CR2')
coef_test(model5_prior, vcov=vcov5_prior)

vcov6_prior <- vcovCR(model6_prior, type='CR2')
coef_test(model6_prior, vcov=vcov6_prior)

vcov7_prior <- vcovCR(model7_prior, type='CR2')
coef_test(model7_prior, vcov=vcov7_prior)


anova(model1_prior, model2_prior, model3_prior, model4_prior, model5_prior, model6_prior, model7_prior)







