library(lme4)
library(dplyr)
library(tidyr)
library(broom.mixed)
setwd("/work/failed_milkings")

# Load your CSV file into R
data <- read.csv("/home/rajesh/work/failed_milkings/final_df_with_days_relative.csv")

data <- data %>%
  mutate(lac_no = if_else(lac_no == 1, "primiparous", "multiparous")) %>%
  mutate(lac_no = factor(lac_no))  # Convert to factor again


# Convert appropriate columns to factors
cat_columns <- c('event_new_disease', 'lac_no', 'lac_stage', 'Animal.Number',
                 'thi_class', 'milk_yield_cat', 'milk_speed_cat', 'milking_int_cat')

data[cat_columns] <- lapply(data[cat_columns], as.factor)

# Stratified sampling (~20% of each 'failed' group)

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

# Drop NAs for model variables
model_vars <- c('event_new_disease', 'lac_no', 'lac_stage', 'milking_int_cat',
                'thi_class', 'milk_speed_cat', 'milk_yield_cat')
data <- data %>% drop_na(all_of(model_vars))

# Control object with better optimizer and more iterations
ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000), calc.derivs = TRUE)




