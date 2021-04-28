library(tidymodels)
library(readr)

# modelop.init
begin <- function() {
    load("trained_model.RData")
    # reassigning model artifact to a new variable 
    model <<- lm_fit
}

# modelop.score
action <- function(datum) {
    preds <- predict(model, datum)
    emit(preds)
}

# modelop.metrics
metrics <- function(data){
    get_metrics <- metric_set(rmse, rsq, mae)
    output <- data %>% select(ground_truth, prediction) %>% get_metrics(truth=ground_truth, prediction=prediction)
    emit(output)
}