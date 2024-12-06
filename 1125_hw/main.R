# Set wd before running
# setwd("1125_hw")

library(tidymodels)
library(tidyverse)

#####################
# Configuration
my_seed <- 10000
#####################

# Part I
# Read data from file
set.seed(my_seed)
load("houses.RData")

# Data Preprocessing
houses$house_id <- NULL
houses$postal_code <- as.character(houses$postal_code)
# houses$postal_code <- as.factor(houses$postal_code)
# str(houses)

# Split 75% data into training set, 25% data into testing set
h_split <- initial_split(houses, prop = .75)
h_train <- training(h_split)
h_test <- testing(h_split)

# Part II
linear_reg_recipe <-
  recipe(market_value ~ ., data = h_train) |>
  #step_rm(house_id) |>
  step_dummy(all_nominal_predictors()) |>
  # interact every predictor with every other predictor
  # the : operator specifies pairwise interactions in R formulas
  step_interact(~ all_predictors():all_predictors()) |>
  # remove any resulting variables that have only one value
  # and thus zero variance ("zv")
  step_zv(all_predictors()) |>
  # normalize the predictors to have mean 0 and SD 1
  step_normalize(all_predictors())

# Baking Data
h_train_baked <-
  linear_reg_recipe |>
  prep(h_train) |>
  bake(h_train)

h_train_baked |>
  summarise(across(everything(), list(m = mean, s = sd))) |>
  mutate(across(everything(), ~ round(.x, 2)))

## lasso linear regression models
# mixture = 1 means lasso
lasso_linear_reg <-
  linear_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

lasso_linear_reg |> translate()

# Combine model and recipe into workflow
lasso_wf <-
  workflow() |>
  add_recipe(linear_reg_recipe) |>
  add_model(lasso_linear_reg)

# Tuning grids
grid_lasso <-
  grid_regular(penalty(c(-2, 2), trans = log10_trans()),
    levels = 50
  )

# Part III
cv_folds <- vfold_cv(h_train, v = 10)

lasso_tune <-
  lasso_wf |>
  tune_grid(
    resamples = cv_folds,
    grid = grid_lasso,
    metrics = metric_set(rmse, rsq_trad, mae)
  )

lasso_tune_metrics <-
  lasso_tune |>
  collect_metrics()

lasso_tune_metrics |>
  filter(.metric == "rmse") |>
  ggplot(aes(
    x = penalty, y = mean,
    ymin = mean - std_err, ymax = mean + std_err
  )) +
  geom_pointrange(alpha = 0.5) +
  scale_x_log10() +
  labs(y = "RMSE", x = expression(lambda)) +
  theme_bw()

lasso_tune |>
  autoplot() +
  theme_bw()

lasso_tune |>
  show_best(metric = "rmse")

lasso_1se_model <-
  lasso_tune |>
  select_by_one_std_err(metric = "rmse", desc(penalty))
lasso_1se_model

lasso_tune |>
  show_best(n = 10, metric = "rmse")

lasso_wf_tuned <-
  lasso_wf |>
  finalize_workflow(lasso_1se_model)
lasso_wf_tuned


lasso_last_fit <-
  lasso_wf_tuned |>
  last_fit(h_split, metrics = metric_set(rmse, mae, rsq_trad))

lasso_test_metrics <-
  lasso_last_fit |>
  collect_metrics()

lasso_test_metrics <-
  lasso_test_metrics |>
  select(.metric, .estimate) |>
  mutate(model = "lasso")

lasso_last_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  # only non-zero coefficients
  filter(estimate != 0) |>
  arrange(desc(abs(estimate))) |>
  print(n = 100)

# lasso_last_fit |>
#   # add predictions to the test set
#   augment() |>
#   transmute(Limit, Rating, Income,
#     Student = 1L * (Student == "Yes"),
#     .resid
#   ) |>
#   pivot_longer(c(-.resid)) |>
#   ggplot(aes(x = value, y = .resid)) +
#   geom_point() +
#   facet_wrap(~name, scale = "free_x") +
#   theme_bw()

# Part IV