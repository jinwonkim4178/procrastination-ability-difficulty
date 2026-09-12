################################################################
########## Load Packages and a final analytic dataset ########## 
################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lme4)
library(ordinal)
library(clubSandwich)
library(interactions)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(emmeans)
library(cowplot)
library(grid)

df <- read_csv("./cleaned_data/final_analysis_dataset.csv")


########################################################################################################################################################
# ======================================================================================================================================================
# Standardize the outcome variable and all predictors (i.e., course grade, academic performance propensity, course difficulty, and procrastination index)

# procra_rank: procrastination index
# diff: course difficulty
# abilities: academic performance propensity
# grade: course grade
# ======================================================================================================================================================
########################################################################################################################################################

final_df <- df %>% 
  mutate(across(all_of(c('grade', 'procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))


############################################
# ==========================================
# Hierarchical linear models (HLM) analysis 
# ==========================================
############################################

# Model 1 (courseGrade ~ procrastination + (1 | student))
model1 <- lmer(grade ~ procra_rank + (1 | student_id), data = final_df)
summary(model1)
sjPlot::tab_model(model1, show.se = TRUE, digits = 3)
performance::icc(model1)

# Model 2 (courseGrade ~ procrastination + courseDifficulty + (1 | student))
model2 <- lmer(grade ~ procra_rank + diff + (1 | student_id), data = final_df)
summary(model2)
sjPlot::tab_model(model2, show.se = TRUE, digits = 3)
performance::icc(model2)

# model3 (courseGrade ~ procrastination + performancePropensity + courseDifficulty +(1 | student))
model3 <- lmer(grade ~ procra_rank + diff + abilities + (1 | student_id), data = final_df)
summary(model3)
sjPlot::tab_model(model3, show.se = TRUE, digits = 3)
performance::icc(model3)

# model4 (courseGrade ~ procrastination*courseDifficulty + performancePropensity + (1 | student))
model4 <- lmer(grade ~ procra_rank*diff + abilities + (1 | student_id), data = final_df)
summary(model4)
sjPlot::tab_model(model4, show.se = TRUE, digits = 3)
performance::icc(model4)

# model5 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + (1 | student))
model5 <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + (1 | student_id), data = final_df)
summary(model5)
sjPlot::tab_model(model5, show.se = TRUE, digits = 3)
performance::icc(model5)

# model6 (courseGrade ~ procrastination*courseDifficulty + procrastination*performancePropensity + courseDifficulty*performancePropensity + (1 | student))
model6 <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + diff*abilities + (1 | student_id), data = final_df)
summary(model6)
sjPlot::tab_model(model6, show.se = TRUE, digits = 3)
performance::icc(model6)

# model7 (courseGrade ~ procrastination*courseDifficulty*performancePropensity + (1 | student))
model7 <- lmer(grade ~ procra_rank*diff*abilities + (1 | student_id), data = final_df)
summary(model7)
sjPlot::tab_model(model7, show.se = TRUE, digits = 3)
performance::icc(model7)


###########################################################################
# =========================================================================
# Calculating Robust Standard Error via cluster-robust variance estimators 
# All reported results are based on robust standard errors.
# =========================================================================
###########################################################################

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

vcov7 <- vcovCR(model7, type='CR2')
coef_test(model7, vcov=vcov7)



######################################################################################################
# ===================================================================================================
# Checking distributional and modeling assumptions via visual inspection of standard diagnostic plots
# Diagnostic checks were conducted prior to reporting results with cluster-robust standard errors
# ===================================================================================================
######################################################################################################

# ============================================================
# Figure A1. Diagnostic Plots 
# ============================================================
# ============================================================
# 1. Prepare diagnostic data
# ============================================================
resid_vals  <- resid(model7)
fitted_vals <- fitted(model7)
scale_loc   <- sqrt(abs(resid_vals))
std_resid   <- resid_vals / sigma(model7)

diag_data <- data.frame(
  fitted_vals = fitted_vals,
  resid_vals  = resid_vals,
  scale_loc   = scale_loc,
  std_resid   = std_resid
)

# ============================================================
# 2. Common font and theme
# ============================================================
font_family <- "sans"

theme_diag <- theme_classic(
  base_size = 12,
  base_family = font_family
) +
  theme(
    text = element_text(family = font_family),
    plot.title = element_text(
      family = font_family,
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    axis.title = element_text(
      family = font_family,
      size = 12
    ),
    axis.text = element_text(
      family = font_family,
      size = 10
    ),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    plot.margin = margin(
      t = 8,
      r = 10,
      b = 8,
      l = 10
    )
  )

# ============================================================
# 3. Panel A: Normal Q-Q Plot
# ============================================================
p1 <- ggplot(diag_data, aes(sample = resid_vals)) +
  stat_qq(
    shape = 1,
    size = 0.65,
    alpha = 0.45
  ) +
  stat_qq_line(
    linewidth = 0.8
  ) +
  scale_x_continuous(
    breaks = c(-4, -2, 0, 2, 4)
  ) +
  scale_y_continuous(
    breaks = c(-4, -2, 0, 2)
  ) +
  labs(
    title = "Normal Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_diag

# ============================================================
# 4. Panel B: Residuals vs Fitted
# ============================================================
p2 <- ggplot(
  diag_data,
  aes(x = fitted_vals, y = resid_vals)
) +
  geom_point(
    shape = 1,
    size = 0.55,
    alpha = 0.30
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  scale_x_continuous(
    breaks = c(-3, -2, -1, 0, 1, 2)
  ) +
  scale_y_continuous(
    breaks = c(-4, -2, 0, 2)
  ) +
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_diag

# ============================================================
# 5. Panel C: Scale-Location
# ============================================================
p3 <- ggplot(
  diag_data,
  aes(x = fitted_vals, y = scale_loc)
) +
  geom_point(
    shape = 1,
    size = 0.55,
    alpha = 0.30
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    span = 0.6,
    linewidth = 0.9,
    color = "#3366FF"
  ) +
  scale_x_continuous(
    breaks = c(-3, -2, -1, 0, 1, 2)
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1.0, 1.5, 2.0)
  ) +
  labs(
    title = "Scale-Location",
    x = "Fitted Values",
    y = expression(sqrt("|Residuals|"))
  ) +
  theme_diag

# ============================================================
# 6. Panel D: Influence / Cook's Distance
# ============================================================
# leverage
lev_vals <- hatvalues(model7)

# Cook's distance
cook_vals <- tryCatch(
  cooks.distance(model7),
  error = function(e) rep(NA_real_, length(lev_vals))
)

# number of fixed-effect parameters for contour
n_par <- tryCatch(
  length(lme4::fixef(model7)),
  error = function(e) length(coef(model7))
)

influence_data <- data.frame(
  leverage  = lev_vals,
  std_resid = std_resid,
  cooks_d   = cook_vals,
  obs_id    = seq_along(lev_vals)
)

# label top 3 influential observations if Cook's distance is available
if (all(is.na(influence_data$cooks_d))) {
  label_data <- influence_data[order(abs(influence_data$std_resid), decreasing = TRUE)[1:3], ]
} else {
  label_data <- influence_data[order(influence_data$cooks_d, decreasing = TRUE)[1:3], ]
}

# Cook's distance contour (D = 0.8)
cook_level <- 0.8
h_seq <- seq(
  from = max(min(influence_data$leverage[influence_data$leverage > 0], na.rm = TRUE), 0.001),
  to   = max(influence_data$leverage, na.rm = TRUE) * 1.10,
  length.out = 400
)

cook_curve <- sqrt(cook_level * n_par * (1 - h_seq) / h_seq)

contour_df <- data.frame(
  leverage = c(h_seq, h_seq),
  std_resid = c(cook_curve, -cook_curve),
  group = rep(c("upper", "lower"), each = length(h_seq))
)

# label positions for contour text
label_df <- data.frame(
  leverage = c(h_seq[300], h_seq[300]),
  std_resid = c(cook_curve[300], -cook_curve[300]),
  label = c("0.8", "0.8")
)

p4 <- ggplot(
  influence_data,
  aes(x = leverage, y = std_resid)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6,
    color = "gray60"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6,
    color = "gray60"
  ) +
  geom_line(
    data = contour_df,
    aes(x = leverage, y = std_resid, group = group),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#39B27F"
  ) +
  geom_text(
    data = label_df,
    aes(x = leverage, y = std_resid, label = label),
    inherit.aes = FALSE,
    family = font_family,
    size = 4,
    color = "#39B27F"
  ) +
  geom_point(
    color = "#2C7FB8",
    alpha = 0.75,
    size = 1.6
  ) +
  geom_text(
    data = label_data,
    aes(label = obs_id),
    family = font_family,
    size = 3.2,
    color = "#2C7FB8",
    nudge_y = -1.2
  ) +
  scale_x_continuous(
    breaks = c(0.00, 0.02, 0.04),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_y_continuous(
    breaks = c(-10, 0, 10)
  ) +
  labs(
    title = "Influence/Cook’s Distance",
    x = "Leverage",
    y = "Standardized Residuals"
  ) +
  theme_diag

# ============================================================
# 7. Combine all four panels
# ============================================================
figure_A1 <- (
  p1 + p2
) / (
  p3 + p4
) +
  plot_layout(
    widths = c(1, 1),
    heights = c(1, 1)
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    text = element_text(family = font_family),
    plot.tag = element_text(
      family = font_family,
      size = 14,
      face = "bold"
    )
  )


ggsave(
  filename = "./figure/Figure_A1_Diagnostic_Plots.png",
  plot = figure_A1,
  width = 10,
  height = 6.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

###################################
# =================================
# Comparing the seven HLM models 
# =================================
###################################

anova(model1, model2, model3, model4, model5, model6, model7)


####################################################################################################################
# ==================================================================================================================
# Generating Figure 2 - three-way interaction effects of procrastination, academic performance propensity, and course difficulty on course performance
# For visualization, the Model 7 specification was refit using 
# course grade on its original 0-4 scale, while the predictors remained z-standardized.
# ==================================================================================================================
####################################################################################################################

final_df_interact <- df %>% 
  mutate(across(all_of(c('procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))

model7_interact <- lmer(grade ~ procra_rank*diff*abilities + (1 | student_id), data = final_df_interact)

# ============================================================
# 1. Basic settings
# ============================================================

font_family <- "sans"

panel_labels <- c(
  "Mean of Course Difficulty -1 SD",
  "Mean of Course Difficulty",
  "Mean of Course Difficulty +1 SD"
)

# Colors for academic performance propensity
col_high <- "#1F5A91"
col_mean <- "#5B8FC1"
col_low  <- "#8FC4F0"

# ============================================================
# 2. CR2 variance-covariance matrix
#    Use the same robust SE approach as the reported models
# ============================================================

vcov_m7_cr2 <- as.matrix(
  clubSandwich::vcovCR(
    model7,
    type = "CR2"
  )
)

# ============================================================
# 3. Main interaction plot
# ============================================================

p_base <- interact_plot(
  model7_interact,
  pred = procra_rank,
  modx = abilities,
  mod2 = diff,
  modx.values = c(-1, 0, 1),
  mod2.values = c(-1, 0, 1),
  
  # 95% confidence intervals
  interval = TRUE,
  int.type = "confidence",
  int.width = 0.95,
  
  # Use CR2 variance-covariance matrix
  vcov = vcov_m7_cr2,
  colors = c(col_low, col_mean, col_high),
  line.thickness = 0.85,
  
  x.label = "Procrastination Index",
  y.label = "Course Grade",
  
  legend.main = "Academic Performance Propensity",
  
  modx.labels = c("-1 SD", "Mean", "+1 SD"),
  mod2.labels = panel_labels
  ) +

  scale_y_continuous(
    limits = c(2, 4.05),
    breaks = seq(2, 4, by = 0.5)
  ) +
  scale_x_continuous(
    breaks = c(-1, 0, 1)
  ) +
  
  theme(
    text = element_text(family = font_family),
    strip.text = element_text(size = 17, face = "bold"),
    axis.title.x = element_text(size = 17, face = "bold", margin = margin(t = 10, b = 3)),
    axis.title.y = element_text(size = 17, face = "bold"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(
      size = 15
    ),
    legend.position = "none",
    panel.spacing = unit(1.2, "lines"),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      t = 8,
      r = 10,
      b = 2,
      l = 10
    )
  )

ribbon_idx <- which(
  vapply(
    p_base$layers,
    function(x) inherits(x$geom, "GeomRibbon"),
    logical(1)
  )
)

if (length(ribbon_idx) > 0) {
  
  for (i in ribbon_idx) {
    p_base$layers[[i]]$aes_params$alpha <- 0.45
    p_base$layers[[i]]$aes_params$linewidth <- 0.30
  }
}

p_base <- p_base +
  
  geom_line(
    data = p_base$data,
    aes(
      x = procra_rank,
      y = ymin,
      color = abilities,
      linetype = modx_group,
      group = modx_group
    ),
    linewidth = 0.28,
    alpha = 0.55,
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    data = p_base$data,
    aes(
      x = procra_rank,
      y = ymax,
      color = abilities,
      linetype = modx_group,
      group = modx_group
    ),
    linewidth = 0.28,
    alpha = 0.55,
    show.legend = FALSE,
    inherit.aes = FALSE
  )


# ============================================================
# 4. Annotation data
# Based on pred_grade results 
# ============================================================

pred_grade <- emmeans(
  model7_interact,
  ~ procra_rank * abilities * diff,
  at = list(
    procra_rank = c(-1, 1),
    abilities = c(-1, 1),
    diff = c(-1, 0, 1)
  )
)

ann_df <- data.frame(
  mod2_group = factor(
    panel_labels,
    levels = panel_labels
  ),
  
  label = c(
    paste0(
      "Procrastination Index: -1 SD \u2192 +1 SD\n",
      "Low propensity: 3.52 \u2192 3.31 (\u0394 = -0.21)\n",
      "High propensity: 3.96 \u2192 3.98 (\u0394 = +0.02)"
    ),
    
    paste0(
      "Procrastination Index: -1 SD \u2192 +1 SD\n",
      "Low propensity: 3.00 \u2192 2.78 (\u0394 = -0.22)\n",
      "High propensity: 3.74 \u2192 3.70 (\u0394 = -0.04)"
    ),
    
    paste0(
      "Procrastination Index: -1 SD \u2192 +1 SD\n",
      "Low propensity: 2.47 \u2192 2.24 (\u0394 = -0.23)\n",
      "High propensity: 3.52 \u2192 3.42 (\u0394 = -0.10)"
    )
  )
)

p_ann <- ggplot(
  ann_df,
  aes(
    x = 0, y = 0, label = label)
  ) +
  
  geom_label(family = font_family, 
             size = 4.9, lineheight = 1.08,
             fill = "white",color = "black",
    
    linewidth = 0.30,
    label.padding = unit(0.20, "lines")
  ) +
  
  facet_wrap(~ mod2_group, nrow = 1
  ) +
  
  scale_x_continuous(
    limits = c(-1.08, 1.08),
    breaks = NULL,
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = NULL,
    expand = expansion(mult = 0)
  ) +
  
  labs(
    x = NULL,
    y = "Course Grade"
  ) +
  
  theme_minimal(
    base_family = font_family
  ) +
  
  theme(
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 17,
      face = "bold",
      color = "transparent"
    ),
    axis.text.y = element_text(
      size = 15,
      color = "transparent"
    ),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 10
    )
  )

aligned <- align_plots(
  p_base,
  p_ann,
  align = "v",
  axis = "lr"
)

p_main_aligned <- aligned[[1]]
p_ann_aligned  <- aligned[[2]]


# ============================================================
# 5. Legend
# ============================================================

p_legend <- ggplot() +
  # Legend title
  annotate(
    "text",
    x = 0.13,
    y = 0.50,
    label = "Academic Performance Propensity",
    family = font_family,
    fontface = "bold",
    size = 5.0,
    hjust = 0,
    vjust = 0.5
  ) +
  
  # +1 SD
  annotate(
    "segment",
    x = 0.47,
    xend = 0.52,
    y = 0.50,
    yend = 0.50,
    color = col_high,
    linewidth = 1.1,
    linetype = "solid"
  ) +
  
  annotate(
    "text",
    x = 0.54,
    y = 0.50,
    label = "+1 SD",
    family = font_family,
    size = 4.7,
    hjust = 0,
    vjust = 0.5
  ) +
  
  # Mean
  annotate(
    "segment",
    x = 0.63,
    xend = 0.68,
    y = 0.50,
    yend = 0.50,
    color = col_mean,
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  
  annotate(
    "text",
    x = 0.70,
    y = 0.50,
    label = "Mean",
    family = font_family,
    size = 4.7,
    hjust = 0,
    vjust = 0.5
  ) +
  
  # -1 SD
  annotate(
    "segment",
    x = 0.78,
    xend = 0.83,
    y = 0.50,
    yend = 0.50,
    color = col_low,
    linewidth = 1.1,
    linetype = "dotted"
  ) +
  
  annotate(
    "text",
    x = 0.85,
    y = 0.50,
    label = "-1 SD",
    family = font_family,
    size = 4.7,
    hjust = 0,
    vjust = 0.5
  ) +
  
  coord_cartesian(
    xlim = c(0, 1),
    ylim = c(0, 1),
    clip = "off"
  ) +
  
  theme_void() +
  theme(
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = margin(0, 0, 0, 0)
  )

# ============================================================
# 6. Combine all components
# ============================================================

combined_plot <- plot_grid(
  p_main_aligned,
  p_ann_aligned,
  p_legend,
  ncol = 1,
  rel_heights = c(
    1.00,   # main plot
    0.19,   # annotation row
    0.07    # legend
  ),
  align = "v",
  axis = "lr"
)

# ============================================================
# 7. Final figure
# ============================================================

interaction_plot_final <- ggdraw() +
  draw_plot(combined_plot) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )

ggsave("./figure/three_way_interaction_plot.png", plot = interaction_plot_final, width = 16, height = 8)


#############################
# ==========================
# Formal Simple Slopes
# ==========================
#############################

vcov_cr2 <- clubSandwich::vcovCR(
  model7,
  type = "CR2"
)

simple_slopes_cr2 <- emmeans::emtrends(
  model7,
  specs = ~ abilities * diff,
  var = "procra_rank",
  at = list(
    abilities = c(-1, 0, 1),
    diff = c(-1, 0, 1)
  ),
  vcov. = vcov_cr2
)


simple_slopes_cr2_summary <- summary(
  simple_slopes_cr2,
  infer = c(TRUE, TRUE),
  level = 0.95,
  adjust = "none"
) %>%
  as.data.frame()


simple_slopes_table <- simple_slopes_cr2_summary %>%
  mutate(
    `Academic Performance Propensity` = case_when(
      abilities == -1 ~ "-1 SD",
      abilities ==  0 ~ "Mean",
      abilities ==  1 ~ "+1 SD"
    ),
    
    `Course Difficulty` = case_when(
      diff == -1 ~ "-1 SD",
      diff ==  0 ~ "Mean",
      diff ==  1 ~ "+1 SD"
    ),
    
    `Slope` = round(procra_rank.trend, 3),
    
    `95% CI` = paste0(
      "[",
      round(asymp.LCL, 3),
      ", ",
      round(asymp.UCL, 3),
      "]"
    ),
    
    `p-value` = case_when(
      p.value < .001 ~ "< .001",
      TRUE ~ sprintf("%.3f", p.value)
    )
  ) %>%
  
  select(
    `Academic Performance Propensity`,
    `Course Difficulty`,
    `Slope`,
    `95% CI`,
    `p-value`
  )

print(simple_slopes_table)

#######################################################
# =====================================================
# Robustness Check: Ordinal Logistic Regression Models
# =====================================================
#######################################################
library(sandwich)
grade_levels <- sort(unique(df$grade))

df_ordinal <- df %>%
  mutate(
    grade_clean = round(grade, 2)
  )

grade_levels <- sort(unique(df_ordinal$grade_clean[!is.na(df_ordinal$grade_clean)]))

df_ordinal <- df_ordinal %>%
  mutate(
    grade_ord = ordered(
      grade_clean,
      levels = grade_levels
    )
  ) %>% 
  mutate(across(all_of(c('procra_rank', 'diff', 'abilities')), ~ as.numeric(scale(.))))


model1_ord <- clm(grade_ord ~ procra_rank, link = "logit", Hess = TRUE, data = df_ordinal)
vcov1_ord_cluster <- vcovCL(model1_ord, cluster = df_ordinal$student_id, type = "HC0", cadjust = TRUE)
model1_results <- lmtest::coeftest(model1_ord, vcov. = vcov1_ord_cluster)
print(model1_results)
AIC(model1_ord)
BIC(model1_ord)

model2_ord <- clm(grade_ord ~ procra_rank + diff, link = "logit", Hess = TRUE, data = df_ordinal)
vcov2_ord_cluster <- vcovCL(model2_ord, cluster = df_ordinal$student_id, type = "HC0", cadjust = TRUE)
model2_results <- lmtest::coeftest(model2_ord, vcov. = vcov2_ord_cluster)
print(model2_results)
AIC(model2_ord)
BIC(model2_ord)

model3_ord <- clm(grade_ord ~ procra_rank + diff + abilities, link = "logit", Hess = TRUE, data = df_ordinal)
vcov3_ord_cluster <- vcovCL(model3_ord, cluster = df_ordinal$student_id, type = "HC0", cadjust = TRUE)
model3_results <- lmtest::coeftest(model3_ord, vcov. = vcov3_ord_cluster)
print(model3_results)
AIC(model3_ord)
BIC(model3_ord)

model4_ord <- clm(grade_ord ~ procra_rank*diff + abilities, link = "logit", Hess = TRUE, data = df_ordinal)
vcov4_ord_cluster <- vcovCL(model4_ord, cluster = df_ordinal$student_id, type = "HC0", cadjust = TRUE)
model4_results <- lmtest::coeftest(model4_ord, vcov. = vcov4_ord_cluster)
print(model4_results)
AIC(model4_ord)
BIC(model4_ord)

model5_ord <- clm(grade_ord ~ procra_rank*diff + procra_rank*abilities, link = "logit", Hess = TRUE, data = df_ordinal)
vcov5_ord_cluster <- vcovCL(model5_ord, cluster = df_ordinal$student_id, type = "HC0", cadjust = TRUE)
model5_results <- lmtest::coeftest(model5_ord, vcov. = vcov5_ord_cluster)
print(model5_results)
AIC(model5_ord)
BIC(model5_ord)

model6_ord <- clm(grade_ord ~ procra_rank*diff + procra_rank*abilities + diff*abilities, link = "logit", Hess = TRUE, data = df_ordinal)
vcov6_ord_cluster <- vcovCL(model6_ord, cluster = df_ordinal$student_id, type = "HC0", cadjust = TRUE)
model6_results <- lmtest::coeftest(model6_ord, vcov. = vcov6_ord_cluster)
print(model6_results)
AIC(model6_ord)
BIC(model6_ord)

model7_ord <- clm(grade_ord ~ procra_rank*diff*abilities, link = "logit", Hess = TRUE, data = df_ordinal)
vcov7_ord_cluster <- vcovCL(model7_ord, cluster = df_ordinal$student_id, type = "HC0", cadjust = TRUE)
model7_results <- lmtest::coeftest(model7_ord, vcov. = vcov7_ord_cluster)
print(model7_results)
AIC(model7_ord)
BIC(model7_ord)

# calculating McFadden pseudo-R2
model0_ord <- clm(
  grade_ord ~ 1,
  data = df_ordinal,
  link = "logit",
  Hess = TRUE
)

mcfadden_r2 <- function(model, null_model) {
  1 - as.numeric(logLik(model) / logLik(null_model))
}

models_ord <- list(
  Model1 = model1_ord,
  Model2 = model2_ord,
  Model3 = model3_ord,
  Model4 = model4_ord,
  Model5 = model5_ord,
  Model6 = model6_ord,
  Model7 = model7_ord
)

sapply(
  models_ord,
  mcfadden_r2,
  null_model = model0_ord
)

anova(model1_ord, model2_ord, model3_ord, model4_ord, model5_ord, model6_ord, model7_ord)



# Coefficients from model
b <- coef(model7_ord)

# Cluster-robust covariance matrix
V <- vcov7_ord_cluster

# Predictor terms only
terms_keep <- c(
  "procra_rank",
  "diff",
  "abilities",
  "procra_rank:diff",
  "procra_rank:abilities",
  "diff:abilities",
  "procra_rank:diff:abilities"
)

# Estimate
est <- b[terms_keep]

# Robust SE
se <- sqrt(diag(V))[terms_keep]

# z statistic
z <- est / se

# p value
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# Final table
model7_table <- data.frame(
  Effect = terms_keep,
  Estimate = est,
  SE = se,
  z = z,
  p = p,
  OR = exp(est),
  CI_low = exp(est - 1.96 * se),
  CI_high = exp(est + 1.96 * se),
  row.names = NULL
)

model7_table




##########################################################################
# ========================================================================
# Robustness Check: Adding the intercept for courses ("+ (1 | course_id)")
# ========================================================================
##########################################################################
model1_c <- lmer(grade ~ procra_rank + (1 | student_id) + (1 | course_id), data = final_df)
summary(model1_c)
sjPlot::tab_model(model1_c, show.se = TRUE, digits = 3)
performance::icc(model1_c)

model2_c <- lmer(grade ~ procra_rank + diff + (1 | student_id) + (1 | course_id), data = final_df)
summary(model2_c)
sjPlot::tab_model(model2_c, show.se = TRUE, digits = 3)
performance::icc(model2_c)

model3_c <- lmer(grade ~ procra_rank + diff + abilities + (1 | student_id) + (1 | course_id), data = final_df)
summary(model3_c)
sjPlot::tab_model(model3_c, show.se = TRUE, digits = 3)
performance::icc(model3_c)

model4_c <- lmer(grade ~ procra_rank*diff + abilities + (1 | student_id) + (1 | course_id), data = final_df)
summary(model4_c)
sjPlot::tab_model(model4_c, show.se = TRUE, digits = 3)
performance::icc(model4_c)

model5_c <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + (1 | student_id) + (1 | course_id), data = final_df)
summary(model5_c)
sjPlot::tab_model(model5_c, show.se = TRUE, digits = 3)
performance::icc(model5_c)

model6_c <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + diff*abilities + (1 | student_id) + (1 | course_id), data = final_df)
summary(model6_c)
sjPlot::tab_model(model6_c, show.se = TRUE, digits = 3)
performance::icc(model6_c)

model7_c <- lmer(grade ~ procra_rank*diff*abilities + (1 | student_id) + (1 | course_id), data = final_df)
summary(model7_c)
sjPlot::tab_model(model7_c, show.se = TRUE, digits = 3)
performance::icc(model7_c)



#####################################################################
# ==================================================================
# Robustness Check: Results of HLM with Student Major Fixed Effects
# ==================================================================
#####################################################################
model1_major <- lmer(grade ~ procra_rank + factor(major_combo) + (1 | student_id), data = final_df)
summary(model1_major)
sjPlot::tab_model(model1_major, show.se = TRUE, digits = 3)
performance::icc(model1_major)

model2_major <- lmer(grade ~ procra_rank + diff + factor(major_combo) + (1 | student_id), data = final_df)
summary(model2_major)
sjPlot::tab_model(model2_major, show.se = TRUE, digits = 3)
performance::icc(model2_major)

model3_major <- lmer(grade ~ procra_rank + diff + abilities + factor(major_combo) + (1 | student_id), data = final_df)
summary(model3_major)
sjPlot::tab_model(model3_major, show.se = TRUE, digits = 3)
performance::icc(model3_major)

model4_major <- lmer(grade ~ procra_rank*diff + abilities + factor(major_combo) + (1 | student_id), data = final_df)
summary(model4_major)
sjPlot::tab_model(model4_major, show.se = TRUE, digits = 3)
performance::icc(model4_major)

model5_major <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + factor(major_combo) + (1 | student_id), data = final_df)
summary(model5)
sjPlot::tab_model(model5_major, show.se = TRUE, digits = 3)
performance::icc(model5_major)

model6_major <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + diff*abilities + factor(major_combo) + (1 | student_id), data = final_df)
summary(model6_major)
sjPlot::tab_model(model6_major, show.se = TRUE, digits = 3)
performance::icc(model6_major)

model7_major <- lmer(grade ~ procra_rank*diff*abilities + factor(major_combo) + (1 | student_id), data = final_df)
summary(model7_major)
sjPlot::tab_model(model7_major, show.se = TRUE, digits = 3)
performance::icc(model7_major)

# Calculating Robust Standard Error via cluster-robust variance estimators 
vcov1_major <- vcovCR(model1_major, type='CR2')
coef_test(model1_major, vcov=vcov1_major)

vcov2_major <- vcovCR(model2_major, type='CR2')
coef_test(model2_major, vcov=vcov2_major)

vcov3_major <- vcovCR(model3_major, type='CR2')
coef_test(model3_major, vcov=vcov3_major)

vcov4_major <- vcovCR(model4_major, type='CR2')
coef_test(model4_major, vcov=vcov4_major)

vcov5_major <- vcovCR(model5_major, type='CR2')
coef_test(model5_major, vcov=vcov5)

vcov6_major <- vcovCR(model6_major, type='CR2')
coef_test(model6_major, vcov=vcov6_major)

vcov7_major <- vcovCR(model7_major, type='CR2')
coef_test(model7_major, vcov=vcov7_major)



##########################################################################
# ========================================================================
# Robustness Check: Adding demographics (female_cat / urm_cat)
# ========================================================================
##########################################################################
model1_d <- lmer(grade ~ procra_rank + female_cat + urm_cat + (1 | student_id), data = final_df)
summary(model1_d)
sjPlot::tab_model(model1_d, show.se = TRUE, digits = 3)
performance::icc(model1_d)

model2_d <- lmer(grade ~ procra_rank + diff + female_cat + urm_cat + (1 | student_id), data = final_df)
summary(model2_d)
sjPlot::tab_model(model2_d, show.se = TRUE, digits = 3)
performance::icc(model2_d)

model3_d <- lmer(grade ~ procra_rank + diff + abilities + female_cat + urm_cat + (1 | student_id), data = final_df)
summary(model3_d)
sjPlot::tab_model(model3_d, show.se = TRUE, digits = 3)
performance::icc(model3_d)

model4_d <- lmer(grade ~ procra_rank*diff + abilities + female_cat + urm_cat + (1 | student_id), data = final_df)
summary(model4_d)
sjPlot::tab_model(model4_d, show.se = TRUE, digits = 3)
performance::icc(model4_d)

model5_d <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + female_cat + urm_cat + (1 | student_id), data = final_df)
summary(model5_d)
sjPlot::tab_model(model5_d, show.se = TRUE, digits = 3)
performance::icc(model5_d)

model6_d <- lmer(grade ~ procra_rank*diff + procra_rank*abilities + diff*abilities + female_cat + urm_cat + (1 | student_id), data = final_df)
summary(model6_d)
sjPlot::tab_model(model6_d, show.se = TRUE, digits = 3)
performance::icc(model6_d)

model7_d <- lmer(grade ~ procra_rank*diff*abilities + female_cat + urm_cat + (1 | student_id), data = final_df)
summary(model7_d)
sjPlot::tab_model(model7_d, show.se = TRUE, digits = 3)
performance::icc(model7_d)


# Calculating Robust Standard Error via cluster-robust variance estimators 
vcov1_d <- vcovCR(model1_d, type='CR2')
coef_test(model1_d, vcov=vcov1_d)

vcov2_d <- vcovCR(model2_d, type='CR2')
coef_test(model2_d, vcov=vcov2_d)

vcov3_d <- vcovCR(model3_d, type='CR2')
coef_test(model3_d, vcov=vcov3_d)

vcov4_d <- vcovCR(model4_d, type='CR2')
coef_test(model4_d, vcov=vcov4_d)

vcov5_d <- vcovCR(model5_d, type='CR2')
coef_test(model5_d, vcov=vcov5_d)

vcov6_d <- vcovCR(model6_d, type='CR2')
coef_test(model6_d, vcov=vcov6_d)

vcov7_d <- vcovCR(model7_d, type='CR2')
coef_test(model7_d, vcov=vcov7_d)



##########################################################################
# ========================================================================
# Robustness Check: Three-Way Interaction by Disciplinary Field
# ========================================================================
##########################################################################
final_df_field <- final_df %>%
  mutate(
    major_field = case_when(
      
      # 1. Engineering & Computing
      major_combo %in% c(
        "MECHANICAL_ENGINEERING",
        "BIOMEDICAL_ENGINEERING",
        "CIVIL_ENGINEERING",
        "COMPUTER_ENGINEERING",
        "ELECTRICAL_ENGINEERING",
        "AEROSPACE_ENGINEERING",
        "CHEMICAL_ENGINEERING",
        "MATERIALS_SCIENCE_AND_ENGINEERING",
        "ENVIRONMENTAL_ENGINEERING",
        "DATA_SCIENCE",
        "GAME_DESIGN_AND_INTERACTIVE_MEDIA",
        "AEROSPACE_ENGINEERING_MECHANICAL_ENGINEERING"
      ) ~ "Engineering & Computing",
      
      
      # 2. Life & Health Sciences
      major_combo %in% c(
        "BIOLOGICAL_SCIENCES",
        "NURSING_SCIENCE",
        "PHARMACEUTICAL_SCIENCES",
        "PUBLIC_HEALTH_POLICY",
        "BIOLOGICAL_SCIENCES_MEDICAL_ANTHROPOLOGY",
        "HEALTH_INFORMATICS_PUBLIC_HEALTH_POLICY"
      ) ~ "Life & Health Sciences",
      
      
      # 3. Natural & Environmental Sciences
      major_combo %in% c(
        "CHEMISTRY",
        "PHYSICS",
        "ENVIRONMENTAL_SCIENCE_AND_POLICY"
      ) ~ "Natural & Environmental Sciences",
      
      
      # 4. Business, Economics & Other Fields
      major_combo %in% c(
        "BUSINESS_ADMINISTRATION",
        "BUSINESS_ECONOMICS",
        "FILM_AND_MEDIA_STUDIES",
        "BUSINESS_ECONOMICS_MANAGEMENT",
        "ACCOUNTING_BUSINESS_ECONOMICS"
      ) ~ "Business, Economics & Other Fields",
      
      TRUE ~ NA_character_
    )
  )


models_by_field <- final_df_field %>%
  filter(!is.na(major_field)) %>%
  split(.$major_field) %>%
  map(~ lmer(
    grade ~ procra_rank * diff * abilities + (1 | student_id),
    data = .x,
    REML = FALSE
  ))

threeway_by_field <- imap_dfr(
  models_by_field,
  ~ {
    V_CR2 <- vcovCR(
      .x,
      type = "CR2"
    )
    
    # CR2 coefficient test
    test_results <- coef_test(
      .x,
      vcov = V_CR2,
    )
    
    # Convert to data frame
    test_results <- as.data.frame(test_results)
    test_results$term <- rownames(test_results)
    
    # Keep only the three-way interaction
    result <- test_results %>%
      filter(term == "procra_rank:diff:abilities")
    
    tibble(
      major_field = .y,
      n_obs = nobs(.x),
      estimate = result$beta,
      std.error = result$SE,
      statistic = result$tstat,
      df = result$df_Satt,
      p.value = result$p_Satt
    )
  }
)

print(threeway_by_field)


# ============================================================
# STEM classification for the 81 primary majors
# Based on the existing major_stem_1 mapping
# Psychology is coded as Non-STEM (0)
# ============================================================
stem_lookup <- tibble::tribble(
  ~major1,                                   ~major_stem,
  "AEROSPACE ENGINEERING",                    1,
  "AFRICAN-AMERICAN STUDIES",                 0,
  "ANTHROPOLOGY",                             0,
  "APPLIED PHYSICS",                          1,
  "ART",                                      0,
  "ART HISTORY",                              0,
  "ASIAN AMERICAN STUDIES",                   0,
  "BIOCHEMISTRY AND MOLECULAR BIOLOGY",       1,
  "BIOLOGICAL SCIENCES",                      1,
  "BIOLOGY/EDUCATION",                        0,
  "BIOMEDICAL ENGINEERING",                   1,
  "BUSINESS ADMINISTRATION",                  0,
  "BUSINESS ECONOMICS",                       0,
  "BUSINESS INFORMATION MANAGEMENT",          0,
  "CHEMICAL ENGINEERING",                     1,
  "CHEMISTRY",                                1,
  "CHICANO/LATINO STUDIES",                   0,
  "CHINESE STUDIES",                          0,
  "CIVIL ENGINEERING",                        1,
  "CLASSICS",                                 0,
  "COGNITIVE SCIENCES",                       0,
  "COMPARATIVE LITERATURE",                   0,
  "COMPUTER ENGINEERING",                     1,
  "COMPUTER SCIENCE",                         1,
  "COMPUTER SCIENCE AND ENGINEERING",         1,
  "CRIMINOLOGY, LAW AND SOCIETY",             0,
  "DANCE",                                    0,
  "DATA SCIENCE",                             1,
  "DEVELOPMENTAL AND CELL BIOLOGY",           1,
  "DRAMA",                                    0,
  "EARTH SYSTEM SCIENCE",                     1,
  "EAST ASIAN CULTURES",                      0,
  "ECOLOGY AND EVOLUTIONARY BIOLOGY",         1,
  "ECONOMICS",                                0,
  "EDUCATION SCIENCES",                       0,
  "ELECTRICAL ENGINEERING",                   1,
  "ENGLISH",                                  0,
  "ENVIRONMENTAL ENGINEERING",                1,
  "ENVIRONMENTAL SCIENCE",                    1,
  "ENVIRONMENTAL SCIENCE AND POLICY",         1,
  "EUROPEAN STUDIES",                         0,
  "EXERCISE SCIENCES",                        1,
  "FILM AND MEDIA STUDIES",                   0,
  "FRENCH",                                   0,
  "GAME DESIGN AND INTERACTIVE MEDIA",        1,
  "GENDER AND SEXUALITY STUDIES",             0,
  "GENETICS",                                 1,
  "GERMAN STUDIES",                           0,
  "GLOBAL CULTURES",                          0,
  "GLOBAL MIDDLE EAST STUDIES",               0,
  "HISTORY",                                  0,
  "HUMAN BIOLOGY",                            1,
  "INFORMATICS",                              1,
  "INTERNATIONAL STUDIES",                    0,
  "JAPANESE LANGUAGE AND LITERATURE",         0,
  "KOREAN LITERATURE AND CULTURE",            0,
  "LANGUAGE SCIENCE",                         0,
  "LITERARY JOURNALISM",                      0,
  "MATERIALS SCIENCE AND ENGINEERING",        1,
  "MATHEMATICS",                              1,
  "MECHANICAL ENGINEERING",                   1,
  "MICROBIOLOGY AND IMMUNOLOGY",              1,
  "MUSIC",                                    0,
  "MUSIC THEATRE",                            0,
  "NEUROBIOLOGY",                             1,
  "NURSING SCIENCE",                          0,
  "PHARMACEUTICAL SCIENCES",                  0,
  "PHILOSOPHY",                               0,
  "PHYSICS",                                  1,
  "POLITICAL SCIENCE",                        0,
  "PSYCHOLOGY",                               0,
  "PUBLIC HEALTH POLICY",                     0,
  "PUBLIC HEALTH SCIENCES",                   0,
  "QUANTITATIVE ECONOMICS",                   0,
  "RELIGIOUS STUDIES",                        0,
  "SOCIAL ECOLOGY",                           0,
  "SOCIAL POLICY AND PUBLIC SERVICE",         0,
  "SOCIOLOGY",                                0,
  "SOFTWARE ENGINEERING",                     1,
  "SPANISH",                                  0,
  "URBAN STUDIES",                            0
)

full_sample <- read_csv('./cleaned_data/full_dataset_with_course_grades.csv')
full_sample <- 
  full_sample %>% 
  mutate(
    female = case_when(female == "yes" ~ 1, female == 'no' ~ 0, TRUE ~ NA),
    urm = case_when(urm == 1 ~ 1, urm == 0 ~ 0, TRUE ~ NA),
  ) %>% 
  mutate(
    female_cat = case_when(
      female == 1 ~ 1,
      female == 0 ~ 0,
      TRUE ~ 2
    ),
    female_cat = factor(
      female_cat,
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Not Reported")
    )
  ) %>% 
  mutate(
    urm_cat = case_when(
      urm == 1 ~ 1,
      urm == 0 ~ 0,
      TRUE ~ 2
    ),
    urm_cat = factor(
      urm_cat,
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Not Reported")
    )
  ) %>% 
  rename(student_id = mellon_id, course_id = canvas_course_id)


retained_keys <- df %>%
  distinct(student_id, course_id) %>%
  mutate(sample_status = "Retained")

comparison <- full_sample %>%
  select(
    student_id, course_id, grade, major1, female, urm, procra_rank, female_cat, urm_cat) %>%
  left_join(retained_keys,
    by = c("student_id", "course_id")) %>%
  mutate(
    sample_status = if_else(is.na(sample_status),"Excluded", sample_status),
    sample_status = factor(sample_status, levels = c("Retained","Excluded"))
  )

comparison <- comparison %>% 
  left_join(stem_lookup, by = "major1") %>%
  mutate(stem_status = factor(major_stem,levels = c(0, 1), labels = c("Non-STEM", "STEM")))



continuous_desc <- comparison %>%
  group_by(sample_status) %>%
  summarise(grade_n = sum(!is.na(grade)),
            grade_mean = sprintf("%.3f", mean(grade, na.rm = TRUE)),
    grade_sd = sprintf("%.3f", sd(grade, na.rm = TRUE)),
    procra_n = sum(!is.na(procra_rank)),
    procra_mean = sprintf("%.3f", mean(procra_rank, na.rm = TRUE)),
    procra_sd = sprintf("%.3f", sd(procra_rank, na.rm = TRUE)),
    .groups = "drop"
  )

continuous_desc


# Cohen's d function
# Direction = Retained - Excluded
cohens_d_manual <- function(data, variable) {
  
  x_retained <- data %>%
    filter(sample_status == "Retained") %>%
    pull({{ variable }})
  
  x_excluded <- data %>%
    filter(sample_status == "Excluded") %>%
    pull({{ variable }})
  
  x_retained <- x_retained[
    !is.na(x_retained)
  ]
  
  x_excluded <- x_excluded[
    !is.na(x_excluded)
  ]
  
  n1 <- length(x_retained)
  n2 <- length(x_excluded)
  
  m1 <- mean(x_retained)
  m2 <- mean(x_excluded)
  
  sd1 <- sd(x_retained)
  sd2 <- sd(x_excluded)
  
  pooled_sd <- sqrt(
    (
      (n1 - 1) * sd1^2 +
        (n2 - 1) * sd2^2
    ) /
      (n1 + n2 - 2)
  )
  
  d <- (
    m1 - m2
  ) / pooled_sd
  
  return(d)
}


d_grade <- cohens_d_manual(
  comparison,
  grade
)

d_procra <- cohens_d_manual(
  comparison,
  procra_rank
)

d_grade
d_procra


# DEMOGRAPHIC CHARACTERISTICS
# Unique-student level
# Retained student:
## appears at least once in final analytic sample
## Excluded student:
## appears in full comparison sample but NEVER
## appears in final analytic sample

student_status <- comparison %>%
  group_by(student_id) %>%
  summarise(
    retained_anywhere = any(sample_status == "Retained"), .groups = "drop") %>%
  mutate(sample_status = if_else(retained_anywhere, "Retained","Excluded")) %>%
  select(student_id, sample_status)


student_demo <- comparison %>%
  group_by(student_id) %>%
  summarise(
    female_cat = first(female_cat),
    urm_cat = first(urm_cat),
    .groups = "drop"
  ) %>%
  left_join(
    student_status,
    by = "student_id"
  ) %>%
  mutate(
    sample_status = factor(
      sample_status,
      levels = c(
        "Retained",
        "Excluded"
      )
    )
  )

student_n <- student_demo %>%
  count(sample_status)

# Gender distribution
gender_desc <- student_demo %>%
  count(
    sample_status,
    female_cat
  ) %>%
  group_by(sample_status) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  ungroup()

gender_desc


# URM distribution
urm_desc <- student_demo %>%
  count(
    sample_status,
    urm_cat
  ) %>%
  group_by(sample_status) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  ungroup()

urm_desc



# Cramer's V function
cramers_v_manual <- function(x, y) {
  
  tab <- table(
    x,
    y
  )
  
  chi <- suppressWarnings(
    chisq.test(
      tab,
      correct = FALSE
    )
  )
  
  chi2 <- as.numeric(
    chi$statistic
  )
  
  n <- sum(tab)
  
  k <- min(
    nrow(tab) - 1,
    ncol(tab) - 1
  )
  
  v <- sqrt(
    chi2 / (n * k)
  )
  
  return(v)
}


# Cramer's V for Gender and URM
v_gender <- cramers_v_manual(
  student_demo$sample_status,
  student_demo$female_cat
)

v_urm <- cramers_v_manual(
  student_demo$sample_status,
  student_demo$urm_cat
)

v_gender
v_urm

# STEM vs. Non-STEM distribution
# Student-course observation level
stem_desc <- comparison %>%
  filter(
    !is.na(stem_status)
  ) %>%
  
  count(
    sample_status,
    stem_status,
    name = "n"
  ) %>%
  
  group_by(sample_status) %>%
  
  mutate(
    percent =
      n / sum(n) * 100
  ) %>%
  
  ungroup()

stem_desc


# Cramer's V for STEM vs. Non-STEM
stem_effect_data <- comparison %>%
  filter(
    !is.na(stem_status)
  )

v_stem <- cramers_v_manual(
  stem_effect_data$sample_status,
  stem_effect_data$stem_status
)

v_stem



# PRIMARY MAJOR
# Student-course level
comparison <- comparison %>%
  mutate(
    major1 = case_when(
      major1 == "SOC POL & PUB SER" ~ "SOCIAL POLICY AND PUBLIC SERVICE",
      TRUE ~ major1
    )
  )

major_representation <- comparison %>%
  filter(
    !is.na(major1)
  ) %>%
  group_by(sample_status) %>%
  summarise(
    n_primary_majors = n_distinct(
      major1
    ),
    .groups = "drop"
  )

major_representation


# Primary-major distributions
major_desc <- comparison %>%
  filter(
    !is.na(major1)
  ) %>%
  count(
    sample_status,
    major1,
    name = "n"
  ) %>%
  group_by(sample_status) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  ungroup()

major_desc


# Create Appendix major table
# Every primary major will appear, even if retained n = 0.
major_table <- major_desc %>%
  complete(
    major1,
    sample_status,
    fill = list(
      n = 0,
      percent = 0
    )
  ) %>%
  mutate(
    value = paste0(
      format(
        n,
        big.mark = ",",
        scientific = FALSE
      ),
      " (",
      sprintf(
        "%.1f",
        percent
      ),
      "%)"
    )
  ) %>%
  select(
    major1,
    sample_status,
    value
  ) %>%
  pivot_wider(
    names_from = sample_status,
    values_from = value
  ) %>%
  arrange(major1)

major_table


major_totals <- comparison %>%
  count(
    sample_status,
    name = "n"
  ) %>%
  mutate(
    value = paste0(
      format(
        n,
        big.mark = ",",
        scientific = FALSE
      ),
      " (100.0%)"
    )
  ) %>%
  select(
    sample_status,
    value
  ) %>%
  pivot_wider(
    names_from = sample_status,
    values_from = value
  ) %>%
  mutate(
    major1 = "Total student-course observations"
  ) %>%
  select(
    major1,
    Retained,
    Excluded
  )

major_table_with_total <- bind_rows(
  major_table,
  major_totals
)


# Overall effect size for primary-major distribution
major_effect_data <- comparison %>%
  filter(
    !is.na(major1)
  )

v_major <- cramers_v_manual(
  major_effect_data$sample_status,
  major_effect_data$major1
)

v_major

# --------------------------------------------------
# Major-specific sample characteristics
# --------------------------------------------------

major_sample_table <- final_df %>%
  group_by(major_combo) %>%
  summarise(
    Students = n_distinct(student_id),
    Courses = n_distinct(course_id),
    `Student-course observations` = n(),
    .groups = "drop"
  ) %>%
  arrange(major_combo)

major_sample_table %>% 
  print(n = 26)


