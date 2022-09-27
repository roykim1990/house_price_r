library(tidymodels)
library(readr)

# modelop.init
begin <- function() {
    readRDS("trained_model.RData")
    # reassigning model artifact to a new variable
    model <<- lm_fit
}

# modelop.score
action <- function(datum) {
    df <- data.frame(datum, stringsAsFactors=F)
    preds <- predict(model, df)
    output <- list(ground_truth=df$Sale_Price, prediction=preds$.pred)
    emit(output)
}

# modelop.metrics
metrics <- function(data){
    df <- data.frame(data)
    get_metrics <- metric_set(rmse, rsq, mae)
    output <- get_metrics(data=df, truth=X0.ground_truth, estimate=X0.prediction)
    emit(output)
}
