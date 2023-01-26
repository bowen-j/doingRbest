#' A generic function used to create a table of analysis-specific output for common statistical models
#' 
#' @param model The stored model object created from a function in the doingRbest package to run a statistical model.
#' @param interpretation Set equal to `TRUE` for an explanatory paragraph beneath the output table interpreting the numeric values.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param sim_slopes For moderated regression or factorial ANOVA output, set equal to `TRUE` for a simple slopes/simple effects analysis in place of a traditional output table for the analysis.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return An output table as an HTML object (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples 
#' # Simple Regression Output Table
#' model <- regress(disp ~ mpg, data = mtcars)
#' output(model)

output <- function(model          = NULL,
                   interpretation = TRUE,
                   predictors     = NULL,
                   outcome        = NULL,
                   sim_slopes     = FALSE,
                   ...) {
  type <- attr(model, "args")$method
  if (length(type) == 0) {
    if(any(grepl(":", names(model$coefficients)))==T & length(model$coefficients) == 4) {
      if(sim_slopes == TRUE) {
        output <- regression.slopes.output(model = model,
                                           predictors = predictors,
                                           outcome = outcome,
                                           interpretation = interpretation)  
      } else {
        output <- regression.interaction.output(model = model,
                                                predictors = predictors,
                                                outcome = outcome,
                                                interpretation = interpretation)
      }
    } else {
      output <- regression.output(model = model,
                                  predictors = predictors,
                                  outcome = outcome,
                                  interpretation = interpretation)
    }
  } else if (type == "t_test") {
    output <- ttest.output(model          = model,
                          interpretation  = interpretation,
                          predictors      = predictors,
                          outcome         = outcome)
  } else if (type == "anova_test") {
    output <- anova.output(model         = model,
                          interpretation = interpretation,
                          predictors     = predictors,
                          outcome        = outcome)
  } else {
    print("TBD")
  }
  output
}
