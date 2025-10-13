# Load libraries
library(ggplot2)
library(dplyr)
library(broom)
library(emmeans)
library(stringr)
library(glmmTMB)
# Read the data
df <- read.csv("/home/rajesh/work/failed_milkings/codes/df_rate_ratios.csv")
#df <- read.csv("/home/rajesh/work/failed_milkings/codes/df_rate_ratios_without_device.csv")
library(forcats)

df$visit_results_new <- fct_relevel(df$visit_results_new, "Other", after = Inf)


library(glmmTMB)

# Mixed-effects Negative Binomial model, ##checked nb1--- it did not converage, again both of the models also checed 
# without the weeks as random slope but with random slope we got lower aic/bic
nb <- glmmTMB(
  failed_count ~ parity+weeks+visit_results_new+Device.Name + (weeks|Animal.Number),
  family = truncated_nbinom2, 
  data = df
)

poisson <- glmmTMB(
  failed_count ~ parity+weeks+visit_results_new+ Device.Name+ (1 |Animal.Number),
  family = truncated_poisson,   
  data = df1
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

#==========================================================================================
## Model despersion validation
=========================================================================================



#========================================================================================
##Getting results in neat and clean format

#=======================================================================================
results <- tidy(nb, effects = "fixed", conf.int = TRUE)


results <- tidy(nb, effects = "fixed", conf.int = TRUE)

library(dplyr)

results <- results %>%
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




