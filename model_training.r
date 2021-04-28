# importing libraries
library(tidymodels)
library(tidyverse)

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

# fitting the model
lm_model <- linear_reg() %>% set_engine('lm')

lm_workflow <-
    workflow() %>%
    add_model(lm_model) %>%
    add_recipe(ames_recipe)


lm_fit <- fit(lm_workflow, ames_train)

predict(lm_fit, ames_test)
