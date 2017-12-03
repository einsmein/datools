#' Strip a fitted GLM model
#'
#' Reduce the size of a fitted GLM model by a massive factor. This is crucial in
#' deploying R in production settings as the standard GLM is way to liberal with
#' storing stuff we don't need for prediction.
#'
#' @param a the glm model object to strip
#'
#' @return a stripped GLM model
#' @export
#'
#' @examples
#' mylm <- glm(dist ~ speed, data = cars)
#' print(object.size(mylm))
#' mylm <- stripGlmModel(mylm)
#' print(object.size(mylm))
#' print(predict(mylm, cars))
stripGlmModel <- function(a) {
    # Make the GLM object more lightweight while still keeping predictive abilities summary and all 
    # other things are broken though...
    a$y <- NULL
    a$data <- NULL
    a$qr$qr <- NULL
    a$weights <- NULL
    a$effects <- NULL
    a$residuals <- NULL
    a$na.action <- NULL
    a$fitted.values <- NULL
    a$prior.weights <- NULL
    a$linear.predictors <- NULL
    a
}

