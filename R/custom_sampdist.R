#' A generic function used to generate a set of sampling distribution curves to compare test statistics to critical values for a given type of analysis
#' 
#' @param model The stored model object created from a function in the `doingRbest` package to run a statistical model.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param Fdist For regression models, set equal to `TRUE` to display a sampling distribution for the whole model F-statistic.
#' @param tdist For regression models, set equal to `TRUE` to display a sampling distribution for the first slope coefficient's t-statistic (if not named in `slope` argument).
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return A `plotly` plot
#' @export
#' 
#' @examples
#' # Simple Regression Sampling Distribution(s)
#' model <- regress(disp ~ mpg, data = mtcars)
#' sampdist(model)
#' # Multiple Regression Sampling Distribution(s)
#' model <- regress(disp ~ mpg + wt, data = mtcars)
#' sampdist(model)
#' # Moderated Regression Sampling Distribution(s)
#' model <- regress(disp ~ mpg*wt, data = mtcars)
#' sampdist(model)
#' # Single Sample t-test Sampling Distribution(s)
#' model <- ttest(outcome = "disp", data = mtcars, type = "s", mu = 100)
#' sampdist(model, outcome = "Fuel Displacement")
#' # Independent Samples t-test Sampling Distribution(s)
#' model <- ttest(outcome = "hardness", predictor = "location", data = PASWR2::APPLE, type = "independent")
#' sampdist(model)
#' # Dependent Samples t-test Sampling Distribution(s) for "wide" format data
#' early <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' late <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(early, late)
#' model <- ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("early", "late"))
#' sampdist(model)
#' # Dependent Samples t-test Sampling Distribution(s) for "long" format data
#' SubID <- factor(sort(rep(1:10,2)))
#' Grp <- factor(rep(c("Ctrl","Trt"),10))
#' DV <- rnorm(20,10,2)
#' the_data <- data.frame(SubID, Grp, DV)
#' model <- ttest(outcome = "DV", predictor = "Grp", data = the_data, data_format = "long", type = "dependent")
#' sampdist(model)

sampdist <- function(model = NULL,
                     predictors = NULL,
                     outcome = NULL,
                     Fdist = TRUE,
                     tdist = TRUE,
                     ...) {
  type <- attr(model, "args")$method
  if (length(type) == 0) {
    output <- regression.sampdist(model      = model,
                                  Fdist      = Fdist,
                                  tdist      = tdist,
                                  predictors = predictors,
                                  outcome    = outcome)
  } else if (type == "t_test") {
    output <- ttest.sampdist(model      = model,
                             predictors = predictors,
                             outcome    = outcome)
  } else if (type == "anova_test") {
    output <- anova.sampdist(model      = model,
                             predictors = predictors,
                             outcome    = outcome)
  } else {
    print("TBD")
  }
  output
}

