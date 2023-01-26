#' A generic function used to create analysis-specific plots of results for common statistical models
#' 
#' @param model The stored model object created from a function in the `doingRbest` package to run a statistical model.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param sim_slopes For moderated regression or factorial ANOVA plots, set equal to `TRUE` for a simple slopes/simple effects plot in place of a traditional plot for the analysis.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return A `plotly` plot (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Simple Regression Graph (scatterplot with line)
#' model <- regress(disp ~ mpg, data = mtcars)
#' graph(model)
#' # Multiple Regression Graph (3-D scatterplot with plane - 2 predictors maximum to render in 3-D)
#' model <- regress(disp ~ mpg + wt, data = mtcars)
#' graph(model)
#' # Moderated Regression Graph (3-D scatterplot with curved plane)
#' model <- regress(disp ~ mpg*wt, data = mtcars)
#' graph(model)
#' # Moderated Regression Simple Slopes Graph
#' model <- regress(disp ~ mpg*wt, data = mtcars)
#' graph(model, sim_slopes = TRUE)
#' # Single Sample t-test Output Table
#' model <- ttest(outcome = "disp", data = mtcars, type = "s", mu = 100)
#' graph(model, outcome = "Fuel Displacement")
#' # Independent Samples t-test Output Table
#' model <- ttest(outcome = "hardness", predictor = "location", data = PASWR2::APPLE, type = "independent")
#' graph(model)
#' # Dependent Samples t-test Output Table for "wide" format data
#' early <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' late <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(early, late)
#' model <- ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("early", "late"))
#' graph(model)
#' # Dependent Samples t-test Output Table for "long" format data
#' SubID <- factor(sort(rep(1:10,2)))
#' Grp <- factor(rep(c("Ctrl","Trt"),10))
#' DV <- rnorm(20,10,2)
#' the_data <- data.frame(SubID, Grp, DV)
#' model <- ttest(outcome = "DV", predictor = "Grp", data = the_data, data_format = "long", type = "dependent")
#' graph(model)

graph <- function(model      = NULL,
                  predictors = NULL,
                  outcome    = NULL,
                  sim_slopes = FALSE,
                  ...) {
  type = attr(model, "args")$method
  if (length(type) == 0) {
    if(any(grepl(":", names(model$coefficients)))==T & length(model$coefficients)==4) {
      if(sim_slopes == TRUE) {
        output <- regression.slopes.graph(model = model,
                                         predictors = predictors,
                                         outcome = outcome)
      } else if(sim_slopes == FALSE) {
        output <- regression.int.graph(model = model,
                                      predictors = predictors,
                                      outcome = outcome)
      }
    } else {
      output <- regression.graph(model = model,
                                predictors = predictors,
                                outcome = outcome)
    }
  } else if (type == "t_test") {
    output = ttest.graph(model      = model,
                         predictors = predictors,
                         outcome    = outcome)
  } else if (type == "anova_test") {
    output = anova.graph(model      = model,
                         predictors = predictors,
                         outcome    = outcome)
  } else {
    print("TBD")
  }
  output
}




