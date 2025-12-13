# Load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(emmeans)
library(stringr)
library(glmmTMB)
# Read the data
#df <- read.csv("/home/rajesh/work/failed_milkings/codes/df_rate_ratios.csv")
#df <- read.csv("/home/rajesh/work/failed_milkings/codes/df_rate_ratios_without_device.csv
df<-read.csv('/home/rajesh/work/failed_milkings/codes/df_rate_ratios_monthly.csv')
library(forcats)
# Install if necessary: install.packages("tidyverse")
library(dplyr)
library(forcats)

# The variable to filter on is 'weeks'
# The variable to recode is 'visit_results_new'

df <- df %>%
  # 1. Recode factor levels (stopped by user and Other reasons)
  mutate(
    visit_results_new = fct_collapse(
      visit_results_new,
      # New_Level_Name = c("Old_Level_1", "Old_Level_2")
      "Other" = c("Stopped by user", "Other")
      # All other levels of visit_results_new will remain unchanged
    )
  ) %>%
  # 2. Filter observations (until 45 weeks, meaning 'weeks' <= 45)
  filter(weeks <= 45, failed_count<=10)

# Optional: Print the new factor levels and the number of rows

df$visit_results_new <- fct_relevel(df$visit_results_new, "Other", after = Inf)


library(glmmTMB)

# Mixed-effects Negative Binomial model, ##checked nb1--- it did not converage, again both of the models also checed 
# without the weeks as random slope but with random slope we got lower aic/bic


## Divide by (1. montly--test), 55week, line plot by parity showing observation..trend..., model 
nb_pen <- glmmTMB(
  failed_count ~ parity*visit_results_new+weeks + (weeks|Animal.Number)+(1|Device.Name),
  family = truncated_nbinom2, 
  data = df
)

nb <- glmmTMB(
  failed_count ~ parity+weeks+visit_results_new + (weeks|Animal.Number)+(1|Device.Name),
  family = truncated_nbinom2, 
  data = df
)

model_performance(nb)
model_performance(poisson)


# Predicted values and confidence intervals
  df_preds <- predict(nb, type = "link", se.fit = TRUE)
  df$predicted <- exp(df_preds$fit)
  df$se <- df_preds$se.fit
  
  df <- df %>%
    mutate(lower = exp(log(predicted) - 1.96 * se),
           upper = exp(log(predicted) + 1.96 * se))
  
  # === Post Hoc Comparisons (parity within visit_results_new) ===
  pairwise_contrasts <- emmeans(nb, pairwise ~ parity | visit_results_new)
  contrast_df <- summary(pairwise_contrasts$contrasts)
  
  # Create p-value label: "p < 0.05" or "p > 0.05"
  p_labels_parity <- contrast_df %>%
    mutate(p_label = ifelse(p.value < 0.05, "p < 0.05", "p > 0.05"),
           label = paste0(contrast, ": ", p_label)) %>%
    group_by(visit_results_new) %>%
    summarise(label = paste(label, collapse = "\n"), .groups = "drop")
  
  # Get top-right position for each facet
  label_positions <- df %>%
    group_by(visit_results_new) %>%
    summarise(x = max(weeks, na.rm = TRUE) - 0.3,
              y = max(predicted, na.rm = TRUE) * 0.95,
              .groups = "drop")
  
  # Merge positions with labels
  p_labels_parity <- left_join(p_labels_parity, label_positions, by = "visit_results_new")
  
  # === Final Plot ===
  ggplot(df, aes(x = weeks, y = predicted, color = as.factor(parity))) +
    geom_line() +
    geom_point(size = 0.5) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05, alpha = 0.05) +
    facet_wrap(~visit_results_new, scales = "fixed", ncol = 3) +
    theme_classic(base_size = 14) +
    labs(
      title = "Frequency of Failures by Lactation Weeks with Parity Comparisons",
      x = "Weeks", y = "Frequency", color = "Parity"
    ) +
    geom_text(
      data = p_labels_parity,
      aes(x = x, y = y, label = str_extract(label, "p\\s*[<>]=?\\s*0\\.\\d+")),
      inherit.aes = FALSE,
      hjust = 1, vjust = -3, size = 3.5, color = "black"
    ) +  # <--- Removed extra comma here
    theme(
      strip.text = element_text(size = 13),
      legend.position = "bottom")

#==================================================================================
  ## Plot of weeks vs counts 
#=================================================================================
  # --- Predicted counts ---
  df_preds <- predict(nb, type = "link", se.fit = TRUE)
  
  df$predicted <- exp(df_preds$fit)          # predicted weekly counts
  df$se <- df_preds$se.fit                   # SE on log scale
  
  # --- 95% CI ---
  df <- df %>%
    mutate(
      lower = exp(log(predicted) - 1.96 * se),
      upper = exp(log(predicted) + 1.96 * se)
    )
  
  df_weekly <- df %>%
    group_by(weeks) %>%
    summarise(
      weekly_pred = sum(predicted, na.rm = TRUE),
      weekly_lower = sum(lower, na.rm = TRUE),
      weekly_upper = sum(upper, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- Final Plot ---
  ggplot(df_weekly, aes(x = weeks, y = weekly_pred)) +
    geom_line() +
    geom_point(size = 1) +
    geom_errorbar(aes(ymin = weekly_lower, ymax = weekly_upper),
                  width = 0.2, alpha = 0.3) +
    theme_classic(base_size = 14) +
    labs(
      title = "Predicted failed milking count per week",
      x = "Weeks in lactation",
      y = "Predicted count "
    )
  
  
  
  
  
#==========================================================================================
## Model despersion validation
=========================================================================================



#========================================================================================
##Getting results in neat and clean format

#=======================================================================================
results <- tidy(nb, effects = "fixed", conf.int = TRUE)


results_rate <- tidy(nb, effects = "fixed", conf.int = TRUE)

library(dplyr)

results_rate <- results_rate %>%
  mutate(
    estimate = as.numeric(estimate),
    std.error = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    p.value = as.numeric(p.value)
  ) %>%
  mutate(
    estimate = formatC(round(estimate, 3), format = "f", digits = 3),
    std.error = formatC(round(std.error, 3), format = "f", digits = 3),
    OR = formatC(round(exp(as.numeric(estimate)), 3), format = "f", digits = 3),
    CI_lower = formatC(round(exp(as.numeric(conf.low)), 3), format = "f", digits = 3),
    CI_upper = formatC(round(exp(as.numeric(conf.high)), 3), format = "f", digits = 3),
    P_value = formatC(round(p.value, 3), format = "f", digits = 3)
  ) %>%
  dplyr::select(term, estimate, std.error, OR, CI_lower, CI_upper, P_value)






##==================================================================================================
## More model diagnositic using some packages
#===================================================================================================
library(DHARMa)
res <- simulateResiduals(fittedModel = nb, plot = TRUE)
testOutliers(res, type = "bootstrap")
testUniformity(res)
testDispersion(res)
testZeroInflation(res)

#========================================================================================
## Cat-plot for ranef- part of model valiation
#==================================================================================
library(sjPlot)
p <- sjPlot::plot_model(
  nb,
  type = "re",
  transform = NULL,     # ensures raw log-scale effects
  show.values = FALSE,
  show.p = FALSE,
  sort.est = TRUE
) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

p

#==============================================================================
## Plot of weeks vs parity vs total failed count
#=============================================================================

df1 <- df %>%
  group_by(weeks, parity) %>%
  summarize(total_failed = sum(failed_count, na.rm = TRUE)) %>%
  ungroup()
library(ggplot2)

ggplot(df1, aes(x = weeks, y = total_failed, color = as.factor(parity))) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, lwd = 1) +
  labs(
    x = "Weeks",
    y = "Total Failed Count",
    color = "Parity"
  ) +
  theme_minimal()


#=========================================================================
#plotting 
#========================================================================
library(ggplot2)
library(dplyr)      # For data manipulation (group_by, mutate, etc.)
library(scales)     # For percent formatting

# === 1. Define the two datasets ===

# Data 1: Overall Proportions (Modified to remove AS)
df_overall <- data.frame(
  Main_Reason = c(
    "Connection Time (CT)", 
    "Connection Attempts (CA)", 
    "Dead milk time (DT)"
    # "Automatic robot stops (AS)" - REMOVED
  ),
  Overall_Proportion = c(0.3512, 0.2188, 0.3538)
)

# Data 2: Teat-level *counts* (from your table)
df_teat_counts <- data.frame(
  Main_Reason = c(
    rep("Dead milk time (DT)", 4), 
    rep("Connection Attempts (CA)", 4)
  ),
  Teat = rep(c("LR", "RR", "RF", "LF"), 2),
  Count = c(
    434, 401, 508, 445,  # Counts for Dead milk time
    368, 499, 116, 123   # Counts for Connection attempts
  )
)

# === 2. Data Manipulation: Calculate Global Proportions ===
# (This section is unchanged, it will just work on the smaller df_overall)

# Calculate internal proportions for teat data
df_teat_props <- df_teat_counts %>%
  group_by(Main_Reason) %>%
  mutate(Internal_Proportion = Count / sum(Count)) %>%
  ungroup()

# Join with overall proportions to get the global proportion
df_teat_final <- df_teat_props %>%
  left_join(df_overall, by = "Main_Reason") %>%
  mutate(Global_Proportion = Internal_Proportion * Overall_Proportion) %>%
  mutate(Plot_Label = paste(Teat)) %>% # Create a clean label
  select(Main_Reason, Plot_Label, Global_Proportion)

# Prepare the simple causes (CT)
df_simple_final <- df_overall %>%
  filter(!Main_Reason %in% c("Dead milk time (DT)", "Connection Attempts (CA)")) %>%
  mutate(
    Plot_Label = Main_Reason,              # Label is just the cause itself
    Global_Proportion = Overall_Proportion
  ) %>%
  select(Main_Reason, Plot_Label, Global_Proportion)

# Combine them into one final data frame
df_plot_final <- bind_rows(df_teat_final, df_simple_final)


# === 3. Plotting (Modified) ===

# NEW: Set the factor levels to control the x-axis order
df_plot_final$Main_Reason <- factor(df_plot_final$Main_Reason, 
                                    levels = c("Dead milk time (DT)", 
                                               "Connection Attempts (CA)", 
                                               "Connection Time (CT)"))

ggplot(df_plot_final, 
       # MODIFIED: Use the new factor order, removed reorder()
       aes(x = Main_Reason, 
           y = Global_Proportion, 
           fill = Plot_Label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Failure Reason",
    y = "Proportion of Total Failures",
    fill = "Cause / Teat",
    title = "Combined FME Proportions with Teat-Level Breakdown"
  ) +
  theme_minimal()



#====================================================================================
# weekly plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(dplyr)
library(glmmTMB)
library(ggeffects)
library(ggplot2)

# 1. Aggregate: Sum failed_counts to get ONE row per cow per week
# We remove 'parity' from the grouping since it's no longer needed
df_weekly <- df %>%
  group_by(Animal.Number, weeks, Device.Name) %>% 
  summarise(
    total_failures = sum(failed_count, na.rm = TRUE),
    .groups = "drop"
  )

# Check the first few rows to ensure it looks right
head(df_weekly)
# 2. Fit the Model
# Note: We removed 'visit_results_new' from the predictors
nb <- glmmTMB(
  total_failures ~  weeks + (weeks | Animal.Number) + (1 | Device.Name),
  family = truncated_nbinom2, 
  data = df_weekly
)

# Optional: Check model summary
# summary(nb)
# 3. Generate Predictions
# 3. Generate Predictions
# We only predict for 'weeks' now. 
df_preds <- ggpredict(nb, terms = "weeks [all]")

# Ensure 'x' is numeric for proper line plotting
df_preds$x <- as.numeric(as.character(df_preds$x))

# 4. Plot
# Note: We removed 'color' and 'fill' aesthetics because there is only one group
ggplot(df_preds, aes(x = x, y = predicted)) +
  geom_line(linewidth = 1, color = "#2c7bb6") + # Single color (blue)
  # Confidence intervals
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#2c7bb6") +
  theme_classic(base_size = 14) +
  labs(
    title = "Predicted failed milking events across lactation",
    subtitle = "Model: glmmTMB (Adjusted for Cow and Robot effects)",
    x = "Weeks in lactation",
    y = "Predicted total failures per week"
  )