# Load packages
library(tidyverse)
library(tidymodels)

# Load customer data
load('customers.RData')
str(customers)

# Configure parallel execution of code
cl <- parallel::makePSOCKcluster(parallel::detectCores())
doParallel::registerDoParallel(cl)

# Colorbind-safe palette:
pal <- c("#E69F00", "#56B4E9", "#009E73", 
         "#0072B2", "#D55E00", "#CC79A7", "#F0E442")

##########################################################################
# Split the data:
# * The analysis set will contain all observations from the start
#   of record keeping through the end of calendar year 2023
# * The assessment set will contain all observations from calendar
#   year 2024.


# about 80% of the data is in the analysis set:
prop_analysis <- summarise(customers, p = sum(year < 2024)/n())$p
prop_analysis

# set the seed for reproducibility
set.seed(101)

# `initial_time_split` splits the data based on row ordering. The `customers`
# data frame is already sorted by year, so by splitting the first
# `prop_analysis` observations into the analysis set, the analysis set will
# contain all pre-2024 observations, and the assessment set will contain all
# 2024 observations.
split <- initial_time_split(customers, prop = prop_analysis)
analysis_set <- training(split)

# Create CV folds. By choosing v = 5, the train/test split within each CV fold
# will be about 80% training and 20% test. That roughly matches the proportion
# that will eventually be used for the final assessment.
folds <- vfold_cv(training(split), v = 5, repeats = 4)


##########################################################################
# Fit a simple logistic regression as a baseline model. Nothing fancy.

# Define a workflow for logistic regression
wf_logistic <-
  workflow() |> 
  add_recipe(
    recipe(formula = churn ~ ., data = analysis_set) |> 
      update_role(id, new_role = 'metadata') |> 
      step_dummy(all_nominal_predictors()) |> 
      step_interact(~all_predictors():all_predictors()) |> 
      step_zv(all_predictors())) |> 
  add_model(logistic_reg(engine = 'glm'))

# Evaluate the baseline model using cross validation. I use the `tune_grid`
# function for this even though there is no tuning to be done. The
# `suppressWarnings` call at the end stops `tune_grid` from printing a warning
# about the lack of tunable parameters.
cv_results_logistic <- 
  wf_logistic |> 
  tune_grid(folds,
            metrics = metric_set(roc_auc, recall, precision,
                                 kap, bal_accuracy, f_meas),
            control = control_grid(save_pred = TRUE)) |> 
  suppressWarnings()

# Baseline statistics calculated from CV predictions:
cv_results_logistic |> 
  collect_metrics()

  

##########################################################################
# TODO: Train a better predicting model and evaluate its predictions for 2024

# check data imbalance or not (if imbalance may use themis::step_downsample.)
prop.table(table(customers$churn)) # probability
# visualization
ggplot(customers, aes(x = churn)) +
  geom_bar(fill = "skyblue") +
  ggtitle("Churn Status Distribution") +
  xlab("Churn Status") +
  ylab("Count")

# lasso workflow
wf_lasso <- 
  workflow() |> 
  add_recipe(
    recipe(formula = churn ~ ., data = analysis_set) |> 
      update_role(id, new_role = 'metadata') |> 
      step_normalize(all_numeric_predictors()) |> 
      step_dummy(all_nominal_predictors()) |> 
      step_interact(~all_predictors():all_predictors()) |> 
      step_zv(all_predictors())) |> 
  add_model(logistic_reg(penalty = tune(), mixture = 1, engine = 'glmnet'))

metrics <- metric_set(roc_auc, recall, precision, f_meas, bal_accuracy, kap)

# tuning lasso model
lasso_results <- 
  wf_lasso |> 
  tune_grid(
    resamples = folds,
    grid = grid_regular(penalty(range = c(-5, -2), trans = log10_trans()), levels = 200),
    metrics = metrics
  )

# lasso statistics
lasso_results |> 
  collect_metrics()



# random forest workflow
rf_model_tune <-
  rand_forest(mtry = tune(), trees = 500) |>
  set_mode("classification") |>
  set_engine("ranger", importance = "permutation")

# 不加interaction 因為RF 本來就可以捕捉非線性和交互關係
wf_rf <- 
  workflow() |> 
  add_recipe(
    recipe(formula = churn ~ ., data = analysis_set) |> 
      update_role(id, new_role = 'metadata') |> 
      step_dummy(all_nominal_predictors()) |> 
      step_zv(all_predictors())) |> 
  add_model(rf_model_tune)

# tuning RF model
rf_results <- 
  wf_rf |> 
  tune_grid(
    resamples = folds,
    grid = grid_regular(
      mtry(range = c(1, 5)),
      levels = 5
    ),
    metrics = metrics
  )

rf_results |> 
  collect_metrics()


logistic_metrics <- cv_results_logistic |> collect_metrics()
lasso_metrics <- lasso_results |> collect_metrics()  # 收集所有指標
rf_metrics <- rf_results |> collect_metrics()  # 收集所有指標

lasso_metrics |> 
  filter(.metric == "roc_auc") |>  # 選擇關鍵指標
  ggplot(aes(x = penalty, y = mean)) +
  geom_line() +
  geom_point() +
  labs(title = "Lasso: ROC-AUC vs Penalty", x = "Penalty", y = "Mean ROC-AUC") +
  theme_minimal()


rf_metrics |> 
  filter(.metric == "roc_auc") |> 
  ggplot(aes(x = mtry, y = mean)) +
  geom_line() +
  geom_point() +
  labs(title = "Random Forest: ROC-AUC vs mtry", x = "mtry", y = "Mean ROC-AUC") +
  theme_minimal()


comparison <- bind_rows(
  logistic_metrics |> mutate(model = "Simple Logistic"),
  lasso_metrics |> mutate(model = "Lasso"),
  rf_metrics |> mutate(model = "Random Forest")
)

# 篩選關鍵指標進行比較
comparison_filtered <- comparison |> filter(.metric %in% c("roc_auc", "recall", "precision", "f_meas"))

# 打印比較結果
print(comparison_filtered)

comparison_filtered |> 
  ggplot(aes(x = .metric, y = mean, fill = model)) +
  geom_col(position = "dodge") +
  labs(title = "Model Comparison: Logistic vs Lasso vs Random Forest", 
       y = "Metric Value", x = "Metric") +
  theme_minimal()



# stop parallel execution:
parallel::stopCluster(cl)



########################################################################## 
# TODO: Determine how many customers we should contact as part of the new 
#       retention program







