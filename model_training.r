# importing libraries
library(tidymodels)
library(readr)
library(yardstick)
library(jsonlite)
library(purrr)
library(stringr)

# function to write json files
write_json <- function(object, filename){
    s <- toJSON(object)
    s <- substring(s, 2, nchar(s) - 1)
    s <- str_replace_all(s, "\\},", "\\}\n")
    write(s, filename)
}

# importing data, Ames housing data is available through R datasets
data(ames, package='modeldata')

# getting the log of Sale_Price, to normalize the distribution
ames <- mutate(ames, Sale_Price_log = log10(Sale_Price))

# setting random seed for reproducibility
set.seed(777)

# train/test split
ames_split <- initial_split(ames, prop=0.8, strata=Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)


# save the train and test data for later use
# write_csv(ames_train, "baseline.csv")
# write_csv(ames_test, "sample.csv")
write_json(ames_train, "baseline.json")
write_json(ames_test, "sample.json")

# feature engineering
ames_recipe <- 
    # selecting certain columns to predict Sale_Price_log
    recipe(Sale_Price_log ~ Neighborhood + Gr_Liv_Area + Year_Built + 
        Bldg_Type + Latitude + Longitude, data=ames_train) %>%
    # deriving log of Gr_Liv_Area
    step_log(Gr_Liv_Area, base=10) %>%
    # reassigning neighborhoods that are less than 1% of total as "other"
    step_other(Neighborhood, threshold=0.01) %>%
    # dummying nominal (categorical and factor) features
    step_dummy(all_nominal_predictors()) %>%
    # interaction term between log(Gr_Liv_Area) and all "Bldg_Type_" dummied features
    step_interact(terms=~Gr_Liv_Area:starts_with("Bldg_Type")) %>%
    # adding spline representation
    step_ns(Latitude, Longitude, deg_free=20)
    # prep(verbose = TRUE, log_changes = TRUE)

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
train_preds <- 10^predict(lm_fit, ames_train)
test_preds <- 10^predict(lm_fit, ames_test)

# binding predictions to original dataframes
train_scored <- bind_cols(train_preds, ames_train)
test_scored <- bind_cols(test_preds, ames_test)

# evaluate outcomes
metrics <- metric_set(rmse, rsq, mae)
metrics(train_scored, truth=Sale_Price, estimate=.pred)
metrics(test_scored, truth=Sale_Price, estimate=.pred)

# rename columns: Sale_Price -> ground_truth, .pred -> prediction
# use relocate to place new fields as the initial keys
train_scored <- rename(
    train_scored, ground_truth = Sale_Price, prediction = .pred) %>% 
    relocate(ground_truth, prediction)
test_scored <- rename(
    test_scored, ground_truth = Sale_Price, prediction = .pred) %>% 
    relocate(ground_truth, prediction)

# save "scored" dataframes for later analysis
write_json(train_scored, "baseline_scored.json")
write_json(test_scored, "sample_scored.json")

# persisting the fit model
save(lm_fit, file="trained_model.RData")

# ------------------------------------------------------

# testing loading and predicting with fit model
# run following code without running above code to test
# importing libraries
library(tidymodels)
library(readr)
library(jsonlite)

# loading fit model
load("trained_model.RData")

# re-assigning model for clarity
model <- lm_fit

# loading test data (flatten to prevent nesting)
data_in <- stream_in(file("sample.json"),flatten = TRUE)

# predicting (model predicts log of Sale_Price)
10^predict(model, data_in)
