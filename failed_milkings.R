library(lme4)
library(dplyr)
library(tidyr)
library(broom.mixed)


# Load your CSV file into R
data <- read.csv("/home/rajesh/work/failed_milkings/corrected_pen_df.csv")

data <- data %>%
  mutate(lac_no = if_else(lac_no == 1, "primiparous", "multiparous")) %>%
  mutate(lac_no = factor(lac_no))  # Convert to factor again
data$week <- floor(data$Lactation.Days / 7) + 1
data$week_c <- scale(data$week, center = TRUE, scale = FALSE)


# Convert appropriate columns to factors
cat_columns <- c('event_new_disease', 'lac_no', 'lac_stage', 'Animal.Number',
                 'thi_class', 'milk_yield_cat', 'milk_speed_cat', 'milking_int_cat', 'correct_pen', 'Device.Name')

data[cat_columns] <- lapply(data[cat_columns], as.factor)



data$failed <- as.factor(data$failed)
data$event_new_disease <- as.factor(data$event_new_disease)
data$lac_no <- as.factor(data$lac_no)
data$lac_no <- as.factor(data$lac_no)
data$lac_stage <- as.factor(data$lac_stage)
data$parity <- factor(ifelse(data$Gestation == 1, "Primiparous", "Multiparous"),
                      levels = c("Primiparous", "Multiparous"))
data$parity <- as.factor(data$parity)
data$thi_class<- as.factor(data$thi_class)
data$lac_no<-as.factor((data$lac_no))
data$milk_yield_cat<-as.factor((data$milk_yield_cat))
data$milk_speed_cat<-as.factor((data$milk_speed_cat))
data$milking_int_cat<-as.factor((data$milking_int_cat))
data$Animal.Number<-as.factor(data$Animal.Number)
data$event_new_disease <- relevel(data$event_new_disease, ref = "Healthy")

data$lac_stage <- relevel(data$lac_stage, ref = "Early")
data$milking_int_cat <- relevel(data$milking_int_cat, ref = "normal")
data$milk_speed_cat <- relevel(data$milk_speed_cat, ref = "High")
data$milk_yield_cat <- relevel(data$milk_yield_cat, ref = "High")
data$thi_class<- relevel(data$thi_class, ref = "no_heat_stress")
data$Device.Name <-as.factor(data$Device.Name)

# Drop NAs for model variables
model_vars <- c('event_new_disease', 'lac_no', 'lac_stage', 'milking_int_cat',
                'thi_class', 'milk_speed_cat', 'milk_yield_cat', 'Device.Name', 'Animal.Number', 'week')
data <- data %>% drop_na(all_of(model_vars))

# Control object with better optimizer and more iterations
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000), calc.derivs = TRUE)



#model_failed_pen_robot <- glmer(
#  failed ~ event_new_disease + thi_class + lac_no + lac_stage + milking_int_cat + milk_yield_cat + milk_speed_cat + correct_pen+
#    + (1 |Animal.Number)+ (1|correct_pen/Device.Name),
#  data = data,
#  family = binomial(link = "logit"),
#  control = ctrl
#)

#===========================================================================================================================



# Load libraries
library(glmmTMB)
library(performance)
library(broom.mixed)

formula_pen_robot <- failed ~ event_new_disease + thi_class + week * lac_no +
  milking_int_cat + milk_yield_cat  + milk_speed_cat+
  (1 | Animal.Number) + (1|Device.Name)+ (1|correct_pen)


formula_reduced<-failed ~ event_new_disease + thi_class + lac_no*lac_stage +
  milking_int_cat + milk_yield_cat + milk_speed_cat + correct_pen +
  (1 | Animal.Number)

model_pen_robot <- glmmTMB(
  formula_pen_robot,
  data = data,
  family = binomial(link = "logit")
)

model_pen_reduced <- glmmTMB(
  formula_reduced,
  data = data,
  family = binomial(link = "logit")
)

anova(model_independent, model_pen_reduced, test = "Chisq")

data <- data %>%
  arrange(Animal.Number, Date) %>%
  group_by(Animal.Number) %>%
  mutate(time_factor = as.factor(row_number())) %>%
  ungroup() # convert to numeric for AR(1)


model_ar1 <- glmmTMB(
  failed ~ event_new_disease + thi_class + lac_no + lac_stage +
    milking_int_cat + milk_yield_cat + milk_speed_cat + correct_pen + Device.Name+
    ar1(time_factor + 0 | Animal.Number) ,
  data = data,
  family = binomial(link = "logit")
)


model_cs <- glmmTMB(
  failed ~ event_new_disease + thi_class + lac_no + lac_stage +
    milking_int_cat + milk_yield_cat + milk_speed_cat + correct_pen +
    cs(0 + time_factor | Animal.Number),
  data = data,
  family = binomial(link = "logit")
)

model_list <- list(
  independent = model_independent,
  ar1 = model_ar1,
  compound_symmetry = model_cs
)

compare_performance(model_list, metrics = c("AIC", "BIC"))


simulateResiduals(model_independent, plot = TRUE)
simulateResiduals(model_cs, plot = TRUE)


results <- tidy(model_pen_robot, effects = "fixed", conf.int = TRUE)

# Compute odds ratios and format results with rounding (no scientific notation)
results <- tidy(model_pen_robot, effects = "fixed", conf.int = TRUE)

# Compute odds ratios and format results with rounding (no scientific notation)
results <- results %>%
  mutate(estimate = formatC(round(estimate, 3), format = "f", digits = 3),
         std.error = formatC(round(std.error, 3), format = "f", digits = 3),
         OR = formatC(round(exp(as.numeric(estimate)), 3), format = "f", digits = 3),
         CI_lower = formatC(round(exp(as.numeric(conf.low)), 3), format = "f", digits = 3),
         CI_upper = formatC(round(exp(as.numeric(conf.high)), 3), format = "f", digits = 3),
         P_value = formatC(round(p.value, 3), format = "f", digits = 3)) %>%
  select(term, estimate, std.error, OR, CI_lower, CI_upper, P_value)

# Print results as a formatted tablefa
print(results, row.names = FALSE, n= Inf)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#plotting predicted probabilites over the lactation weeks 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ============================================================
# Predicted probabilities with 95% CI by parity × lactation week
# ============================================================

# ============================================================
# Predicted probabilities with 95% CI by parity × lactation week
# Using glmmTMB::predict (ROBUST METHOD)
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# Create prediction grid
# ------------------------------------------------------------
newdata <- expand.grid(
  week = seq(min(data$week), max(data$week), by = 1),
  lac_no = levels(data$lac_no),
  event_new_disease = "Healthy",
  thi_class = "no_heat_stress",
  milking_int_cat = "normal",
  milk_yield_cat = "High",
  milk_speed_cat = "High"
)

# ------------------------------------------------------------
# Predict on link scale with SE (fixed effects only)
# ------------------------------------------------------------
pred <- predict(
  model_pen_robot,
  newdata = newdata,
  type = "link",
  se.fit = TRUE,
  re.form = NA
)

# ------------------------------------------------------------
# Compute probabilities and 95% CI
# ------------------------------------------------------------
newdata <- newdata %>%
  mutate(
    fit_link = pred$fit,
    se_link  = pred$se.fit,
    lwr_link = fit_link - 1.96 * se_link,
    upr_link = fit_link + 1.96 * se_link,
    fit = plogis(fit_link),
    lwr = plogis(lwr_link),
    upr = plogis(upr_link)
  )

# ------------------------------------------------------------
# Plot predicted probabilities with confidence ribbons
# ------------------------------------------------------------
ggplot(newdata, aes(x = week, y = fit, color = lac_no, fill = lac_no)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, color = NA) +
  labs(
    x = "Lactation week",
    y = "Predicted probability of failed milking",
    color = "Parity",
    fill = "Parity"
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.001)
  ) +
  coord_cartesian(
    ylim = c(0, max(newdata$upr) * 1.05)
  ) +
  theme_classic(base_size = 14)


#=============================================================================

res <- simulateResiduals(fittedModel = model_pen_robot, plot = TRUE)
testOutliers(res, type = "bootstrap")
testUniformity(res)
testDispersion(res)
testZeroInflation(res)

#=========================================================================================================================
# Ploting random effects 
#=======================================================================================================================
library(sjPlot)
p <- sjPlot::plot_model(
  model_pen_robot,
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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#getting a table to do post hoc test comparisions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(emmeans)
library(dplyr)

# 1. GENERATE THE EMMEANS OBJECTS
# -------------------------------------------------------
# This calculates the marginal means from your model
emm <- emmeans(model_pen_robot, ~ lac_no | lac_stage)

# 2. CALCULATE CONTRASTS
# -------------------------------------------------------
# Table A: Parity effects inside each Stage (Primi vs Multi)
contrasts_parity <- pairs(emm, reverse = TRUE, type = "response")

# Table B: Stage effects inside each Parity (Mid/Late vs Early)
contrasts_stage <- pairs(emmeans(model_pen_robot, ~ lac_stage | lac_no), 
                         reverse = TRUE, type = "response")

# 3. FORMATTING FUNCTION
# -------------------------------------------------------
# This function converts the raw emmeans output into a clean table
make_pub_table <- function(contrast_obj) {
  
  # Step A: Get Confidence Intervals (and merge with P-values)
  # confint() gets the CI but drops the p-value, summary() gets the p-value
  ci_df <- confint(contrast_obj) %>% as.data.frame()
  p_df  <- summary(contrast_obj) %>% as.data.frame()
  
  # Step B: Merge and Clean
  final_table <- ci_df %>%
    mutate(p.value = p_df$p.value) %>% # Add p-value back in
    rename(
      Contrast = contrast,
      OR = odds.ratio,
      CI_Lower = asymp.LCL,
      CI_Upper = asymp.UCL,
      P_Value = p.value
    ) %>%
    mutate(
      # Rounding for clean presentation
      OR = sprintf("%.3f", OR),
      CI_Lower = sprintf("%.3f", CI_Lower),
      CI_Upper = sprintf("%.3f", CI_Upper),
      P_Value = sprintf("%.4f", P_Value),
      
      # Create a combined "95% CI" column (Optional, but often requested)
      `95% CI` = paste0("(", CI_Lower, " – ", CI_Upper, ")")
    ) %>%
    # Select and reorder columns
    select(matches("lac_stage|lac_no"), Contrast, OR, CI_Lower, CI_Upper, P_Value)
  
  return(final_table)
}

# 4. GENERATE AND VIEW TABLES
# -------------------------------------------------------
table_1_parity <- make_pub_table(contrasts_parity)
table_2_stage  <- make_pub_table(contrasts_stage)

# Print to console (ready to copy)
print("TABLE 1: Parity Effect within Stage")
print(table_1_parity)

print("TABLE 2: Stage Effect within Parity")
print(table_2_stage)

# OPTIONAL: Export to CSV to open in Excel/Word
write.csv(table_1_parity, "Table1_Parity_Interaction.csv", row.names = FALSE)
write.csv(table_2_stage,  "Table2_Stage_Interaction.csv",  row.names = FALSE)
