library(lme4)
library(dplyr)
library(tidyr)
library(broom.mixed)


# Load your CSV file into R
data <- read.csv("/home/rajesh/work/failed_milkings/corrected_pen_df.csv")

data <- data %>%
  mutate(lac_no = if_else(lac_no == 1, "primiparous", "multiparous")) %>%
  mutate(lac_no = factor(lac_no))  # Convert to factor again


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
data$milk_speed_cat <- relevel(data$milk_speed_cat, ref = "Mid")
data$milk_yield_cat <- relevel(data$milk_yield_cat, ref = "Mid")
data$thi_class<- relevel(data$thi_class, ref = "no_heat_stress")
data$Device.Name <-as.factor(data$Device.Name)

# Drop NAs for model variables
model_vars <- c('event_new_disease', 'lac_no', 'lac_stage', 'milking_int_cat',
                'thi_class', 'milk_speed_cat', 'milk_yield_cat', 'Device.Name', 'Animal.Number')
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

formula_pen_robot <- failed ~ event_new_disease + thi_class + lac_no * lac_stage +
  milking_int_cat + milk_yield_cat + milk_speed_cat + correct_pen+
  (1 | Animal.Number) + (1|correct_pen/Device.Name)


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

library(performance)
model_performance(model_failed_pen_robot)

#=============================================================================

res <- simulateResiduals(fittedModel = model_pen_robot, plot = TRUE)
testOutliers(res, type = "bootstrap")
testUniformity(res)
testDispersion(res)
testZeroInflation(res)