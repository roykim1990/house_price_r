library(tidymodels)
library(readr)

# modelop.init
begin <- function() {
    load("trained_model.RData")
    # Assigning model artifact to a global variable
    model <<- lm_fit
}

# modelop.score
action <- function(datum) {
    df <- data.frame(datum, stringsAsFactors=F)
    # model predicts log of Sale_Price
    preds <- 10^predict(model, df)
    output <- list(ground_truth=df$Sale_Price, prediction=preds$.pred)
    emit(output)
}

# modelop.metrics
metrics <- function(data){
    df <- data.frame(data)
    # Compute RMSE, R_squared, MAE, given scored and labeled data
    get_metrics <- metric_set(rmse, rsq, mae)
    output <- get_metrics(data=df, truth=ground_truth, estimate=prediction)
    emit(output)
}