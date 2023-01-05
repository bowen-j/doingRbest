#' A function to construct different kinds of t-test models
#'
#' @param outcome A numeric variable named as a character.
#' @param predictor If not single-sample, a factor variable with 2 levels named as a character.
#' @param data A data frame containing the relevant named variables.
#' @param type A character string specifying the type of  t-test as "single" sample, "independent" samples, or "dependent" samples.
#' @param data_format For dependent samples, a character string specifying whether the data are in "long" or "wide" format.
#' @param mu A numeric value specifying the mean value under the null hypothesis; usually 0 and not specified for independent-samples and dependent-samples t-tests.
#' @param cols For dependent-samples t-tests with "wide" format data, a character vector of column names for within-subjects variables to be transposed to "long" format.
#'
#' @return A data frame
#' @export
#'
#' @importFrom dplyr `%>%`
#' @importFrom rstatix t_test
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' # Single-Sample t-test
#' ttest(outcome = "disp", data = mtcars, type = "s", mu = 75) 
#' # Independent-Samples t-test
#' ttest(outcome = "disp", predictor = "am", data = mtcars, type = "independent") 
#' # Dependent-Samples t-tests
#' # constructing a data frame in wide format
#' pre <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' post <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(pre, post)
#' ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("pre", "post"))
#' # constructing a data frame in long format
#' SubID <- factor(sort(rep(1:10, 2)))
#' Group <- factor(rep(c("Ctrl", "Trt"), 10))
#' DV <- rnorm(20, 10, 2)
#' the_data <- data.frame(SubID, Group, DV)
#' ttest(outcome = "DV", predictor = "Group", data = the_data, data_format = "long", type = "dependent")

ttest <- function(outcome     = NULL,
                  predictor  = NULL,
                  data        = NULL,
                  type        = c("single", "independent", "dependent"),
                  data_format = c("long", "wide"),
                  mu          = 0,
                  cols        = NULL,
                  ...) {

  type        <- match.arg(type)
  data_format <- match.arg(data_format)

  if(type == "dependent" && data_format == "wide") {
    data <- data %>%
      pivot_longer(
        cols      = all_of(cols),
        names_to  = predictor,
        values_to = outcome
      )
  }

  predictor  <- ifelse(type == "single", "1", predictor)
  formula_txt <- paste0(outcome, "~", predictor)
  formula     <- eval(parse(text = formula_txt))

  if(type == "single") {
    ttest_model <- t_test(data    = data,
                          formula = formula,
                          mu      = mu)
  }
  else if(type == "independent") {
    ttest_model <- t_test(formula   = formula,
                          data      = data,
                          var.equal = TRUE)
  }
  else {
    ttest_model <- t_test(formula = formula,
                          data    = data,
                          paired  = TRUE)
  }

  ttest_model$type <- type

  return(ttest_model)
}

