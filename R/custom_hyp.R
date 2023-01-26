#' A generic function used to generate a set of statistical hypotheses for all relevant statistics for the analysis in question
#' 
#' @param model The stored model object created from a function in the `doingRbest` package to run a statistical model.
#' 
#' @return An HTML page with a series of null and alternative hypotheses (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Simple Regression Hypothesis Test
#' model <- regress(disp ~ mpg, data = mtcars)
#' hyp(model)
#' # Multiple Regression Hypothesis Tests
#' model <- regress(disp ~ mpg + wt, data = mtcars)
#' hyp(model)
#' # Moderated Regression Hypothesis Tests
#' model <- regress(disp ~ mpg*wt, data = mtcars)
#' hyp(model)
#' # Single Sample t-test Hypothesis Test
#' model <- ttest(outcome = "disp", data = mtcars, type = "s", mu = 100)
#' hyp(model, outcome = "Fuel Displacement")
#' # Independent Samples t-test Hypothesis Test
#' model <- ttest(outcome = "hardness", predictor = "location", data = PASWR2::APPLE, type = "independent")
#' hyp(model)
#' # Dependent Samples t-test Hypothesis Test for "wide" format data
#' early <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' late <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(early, late)
#' model <- ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("early", "late"))
#' hyp(model)
#' # Dependent Samples t-test Hypothesis Test for "long" format data
#' SubID <- factor(sort(rep(1:10,2)))
#' Grp <- factor(rep(c("Ctrl","Trt"),10))
#' DV <- rnorm(20,10,2)
#' the_data <- data.frame(SubID, Grp, DV)
#' model <- ttest(outcome = "DV", predictor = "Grp", data = the_data, data_format = "long", type = "dependent")
#' hyp(model)

hyp <- function(model          = NULL,
                  ...) {
  type <- attr(model, "args")$method
  if (length(type) == 0) {
    htest <- regression.hyp(model          = model)
  } else if (type == "t_test") {
    htest <- ttest.hyp(model           = model)
  } else if (type == "anova_test") {
    htest <- anova.hyp(model          = model)
  } else {
    print("TBD")
  }
  htest
}
