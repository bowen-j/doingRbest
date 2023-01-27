#' A generic function used to generate an APA-style results writeup for a given type of analysis
#' 
#' @param model The stored model object created from a function in the `doingRbest` package to run a statistical model.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return An HTML page with the text of the results writeup (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Single Sample t-test Writeup
#' model <- ttest(outcome = "disp", data = mtcars, type = "s", mu = 100)
#' writeup(model, outcome = "Fuel Displacement")
#' # Independent Samples t-test Writeup
#' model <- ttest(outcome = "hardness", predictor = "location", data = PASWR2::APPLE, type = "independent")
#' writeup(model)
#' # Dependent Samples t-test Writeup for "wide" format data
#' early <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' late <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(early, late)
#' model <- ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("early", "late"))
#' writeup(model)
#' # Dependent Samples t-test Writeup for "long" format data
#' SubID <- factor(sort(rep(1:10,2)))
#' Grp <- factor(rep(c("Ctrl","Trt"),10))
#' DV <- rnorm(20,10,2)
#' the_data <- data.frame(SubID, Grp, DV)
#' model <- ttest(outcome = "DV", predictor = "Grp", data = the_data, data_format = "long", type = "dependent")
#' writeup(model)

writeup <- function(model          = NULL,
                    predictors     = NULL,
                    outcome        = NULL,
                    ...) {
  type <- attr(model, "args")$method
  if (length(type) == 0) {
    written <- regression.writeup(model          = model,
                                predictors     = predictors,
                                outcome        = outcome)
  } else if (type == "t_test") {
    written <- ttest.writeup(model           = model,
                           predictors      = predictors,
                           outcome         = outcome)
  } else if (type == "anova_test") {
    written <- anova.writeup(model          = model,
                           predictors     = predictors,
                           outcome        = outcome)
  } else {
    print("TBD")
  }
  written
}
