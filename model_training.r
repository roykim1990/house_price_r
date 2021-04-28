# importing libraries
library(tidymodels)
library(readr)

# importing data, Ames housing data is available through R datasets
data(ames, package='modeldata')

# train/test split
ames_split <- initial_split(ames, prop=0.8, strata=Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

# getting the log of Sale_Price, to normalize the distribution
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

# setting random seed for reproducibility
set.seed(777)

# save the train and test data for later use
write_csv(ames_train, "baseline.csv")
write_csv(ames_test, "sample.csv")

# feature engineering
ames_recipe <- 
    # selecting certain columns to predict Sale_Price
    recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type +
           Latitude + Longitude, data=ames_train) %>%
    # deriving log of Gr_Liv_Area
    step_log(Gr_Liv_Area, base=10) %>%
    # reassigning neighborhoods that are less than 1% of total as "other"
    step_other(Neighborhood, threshold=0.01) %>%
    # dummying nominal (categorical and factor) features
    step_dummy(all_nominal_predictors()) %>%
    # interaction term between log(Gr_Liv_Area) and all "Bldg_Type_" dummied features
    step_interact(~ Gr_Liv_area:starts_with("Bldg_Type_")) %>%
    # adding spline representation
    step_ns(Latitude, Longitude, deg_free=20) 

# fitting the model, simple linear regression
lm_model <- linear_reg() %>% set_engine('lm')

# creating workflow by combining recipe and model
lm_workflow <-
    workflow() %>%
    add_model(lm_model) %>%
    add_recipe(ames_recipe)

# training the model
lm_fit <- fit(lm_workflow, ames_train)

# predicting on new data
train_preds <- predict(lm_fit, ames_train)
test_preds <- predict(lm_fit, ames_test)

# binding predictions to original dataframes
train_scored <- bind_cols(train_preds, ames_train)
test_scored <- bind_cols(test_preds, ames_test)

# evaluate outcomes
metrics <- metric_set(rmse, rsq, mae)
metrics(train_scored, truth=Sale_Price, estimate=.pred)
metrics(test_scored, truth=Sale_Price, estimate=.pred)

# rename columns: Sale_Price -> ground_truth, .pred -> prediction
train_scored <- rename(train_scored, ground_truth = Sale_Price, prediction = .pred) %>% relocate(ground_truth)
test_scored <- rename(test_scored, ground_truth = Sale_Price, prediction = .pred) %>% relocate(ground_truth)

# save "scored" dataframes for later analysis
write_csv(train_scored, "baseline_scored.csv")
write_csv(test_scored, "sample_scored.csv")

# persisting the fit model
save(lm_fit, file="trained_model.RData")

# ------------------------------------------------------

# testing loading and predicting with fit model
# run following code without running above code to test
# importing libraries
library(tidymodels)
library(readr)

# loading fit model
load("trained_model.RData")

# loading test data
test_data <- read_csv("sample.csv")

# predicting
predict(lm_fit, test_data)
