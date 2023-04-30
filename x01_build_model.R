# Libraries ---------------------------------------------------------------
library(tidyverse)
library(randomForest)
library(doParallel)
library(caret)

# Parameters --------------------------------------------------------------
global_seed   <- 42
load_data     <- TRUE
make_rf_mod   <- TRUE
make_test_mod <- FALSE

# Data --------------------------------------------------------------------
if (load_data) {
  x <- read_csv("model_input.csv", show_col_types = FALSE)

  x <- data.frame("state"                      = factor(x$state),
                  "county"                     = factor(x$county),
                  "question_cat"               = factor(x$question_cat),
                  "is_answered"                = factor(as.numeric(x$is_answered)),
                  "state_workload_overall"     = as.numeric(x$state_workload_overall),
                  "client_ethnicity"           = factor(x$client_ethnicity),
                  "client_age"                 = factor(x$client_age),
                  "client_gender"              = factor(x$client_gender), # Note: Female as baseline
                  "is_married"                 = factor(x$is_married),
                  "is_veteran"                 = factor(x$is_veteran),
                  "is_during_covid"            = factor(as.numeric(x$is_during_covid)),
                  "client_household_size"      = as.numeric(x$client_household_size),
                  "client_income"              = as.numeric(x$client_income),
                  "circuit"                    = factor(as.numeric(x$circuit)),
                  "asked_year"                 = factor(x$asked_year),
                  "asked_month"                = factor(x$asked_month),
                  "question_nchar"             = as.numeric(x$question_nchar),
                  "is_urgent"                  = factor(as.numeric(x$is_urgent)),
                  "is_below_poverty_line"      = factor(as.numeric(x$is_below_poverty_line)),
                  "has_balance_info"           = factor(as.numeric(x$has_balance_info)),
                  "county_n_cases_overall"     = as.numeric(x$county_n_cases_overall),
                  "county_n_attorneys_overall" = as.numeric(x$county_n_attorneys_overall),
                  "county_workload_overall"    = as.numeric(x$county_workload_overall),
                  "county_n_cases_yr"          = as.numeric(x$county_n_cases_yr),
                  "county_n_attorneys_yr"      = as.numeric(x$county_n_attorneys_yr),
                  "county_workload_yr"         = as.numeric(x$county_workload_yr),
                  "county_cat_n_cases_yr"      = as.numeric(x$county_cat_n_cases_yr),
                  "county_cat_n_attorneys_yr"  = as.numeric(x$county_cat_n_attorneys_yr),
                  "county_cat_workload_yr"     = as.numeric(x$county_cat_workload_yr),
                  "county_cat_n_cases_m"       = as.numeric(x$county_cat_n_cases_m),
                  "county_cat_n_attorneys_m"   = as.numeric(x$county_cat_n_attorneys_m),
                  "county_cat_workload_m"      = as.numeric(x$county_cat_workload_m),
                  "client_rel_income"          = as.numeric(x$client_rel_income))

  set.seed(global_seed)
  in_train <- sample(c(TRUE, FALSE), nrow(x), replace = TRUE, prob = c(1 - 0.33, 0.33))
  x_train <- x[in_train, ]
  x_test <- x[!in_train, ]
}

# Arman RF model ----------------------------------------------------------
if (make_rf_mod) {
  mod_rf <- randomForest(formula    = is_answered ~ . - state - county,
                         data       = x_train,
                         mtry       = 13,
                         ntree      = 500,
                         importance = TRUE)
}

# Parallelization ---------------------------------------------------------
if (make_test_mod) {
  rf_control <- trainControl(method          =  "cv"
                             , number        =  10
                             , repeats       =  NA
                             , allowParallel =  TRUE
                             , verboseIter   =  FALSE)


  n_cores <- detectCores() - 2
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  mod1 <- train(formula  = is_answered ~ .,
                data     = x_train,
                method   = "rf",
                rControl = rf_control)
  stopCluster(cl)
}

# Clear workspace ---------------------------------------------------------
rm(global_seed, load_data, make_rf_mod, make_test_mod)

