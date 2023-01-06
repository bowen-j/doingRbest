#' A function to construct different kinds of linear regression models
#'
#' @param formula  A symbolic description of the model to be fitted of the form y ~ x...
#' @param data A data frame containing the variables specified in the formula and contained in the model.
#'
#' @return An object of `class` `"lm"`.
#' @export
#'
#' @examples
#' regress(formula = disp~am, data = mtcars)


regress <- function(formula = NULL,
                    data    = NULL,
                    ...) {

  regress_model <- lm(formula = formula,
                      data    = data)

  return(regress_model)
}
