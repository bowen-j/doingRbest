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
#'  Simple Regression Sampling Distribution(s)
#' model <- regress(disp ~ mpg, data = mtcars)
#' sampdist(model)
#' # Multiple Regression Sampling Distribution(s)
#' model <- regress(disp ~ mpg + wt, data = mtcars)
#' sampdist(model)
#' # Moderated Regression Sampling Distribution(s)
#' model <- regress(disp ~ mpg*wt, data = mtcars)
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

