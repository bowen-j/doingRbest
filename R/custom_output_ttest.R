#' The t-test model method for the `output` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `ttest` function in the `doingRbest` package.
#' @param interpretation Set equal to `TRUE` for an explanatory paragraph beneath the output table interpreting the numeric values.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return An output table as an HTML object (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Single Sample t-test Output Table
#' model <- ttest(outcome = "disp", data = mtcars, type = "s", mu = 100)
#' output(model, outcome = "Fuel Displacement")
#' # Independent Samples t-test Output Table
#' model <- ttest(outcome = "hardness", predictor = "location", data = PASWR2::APPLE, type = "independent")
#' output(model)
#' # Dependent Samples t-test Output Table for "wide" format data
#' early <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' late <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(early, late)
#' model <- ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("early", "late"))
#' output(model)
#' # Dependent Samples t-test Output Table for "long" format data
#' SubID <- factor(sort(rep(1:10,2)))
#' Grp <- factor(rep(c("Ctrl","Trt"),10))
#' DV <- rnorm(20,10,2)
#' the_data <- data.frame(SubID, Grp, DV)
#' model <- ttest(outcome = "DV", predictor = "Grp", data = the_data, data_format = "long", type = "dependent")
#' output(model)

ttest.output <- function(model          = NULL,
                         interpretation = TRUE,
                         predictors     = NULL,
                         outcome        = NULL,
                         ...) {
  if (model$type[1] == "single") {
    output_table <- ttest.simp.output(model          = model,
                                      interpretation = interpretation,
                                      outcome        = outcome)

  } else if (model$type[1] == "independent") {
    output_table <- ttest.indep.output(model          = model,
                                       interpretation = interpretation,
                                       predictors     = predictors,
                                       outcome        = outcome)

  } else {
    output_table <- ttest.dep.output(model          = model,
                                     interpretation = interpretation,
                                     predictors     = predictors,
                                     outcome        = outcome)
  }
  output_table
}


ttest.simp.output <- function(model          = NULL,
                              interpretation = TRUE,
                              outcome        = NULL,
                              ...) {
  formula <- attr(model, "args")$formula
  mu      <- attr(model, "args")$mu
  data    <- attr(model, "args")$data
  model   <- t_test(data     = data,
                    formula  = formula,
                    mu       = mu,
                    detailed = TRUE)
  outcome_name <- model$.y.
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  interp <- ""
  if(interpretation) {
    interp <- paste0("Interpretation: ",
                     ttest.simp.output.interp(model   = model,
                                              outcome = outcome))
  }
  math_t    <- huxtable('<em>t</em>')
  math_p    <- huxtable('<em>p</em>')
  math_df   <- huxtable('<em>df</em>')
  math_mu   <- huxtable('<em>&#956</em>')
  math_d    <- huxtable('<em>d</em>')
  loCI      <- txtRound(model$conf.low,2)
  hiCI      <- txtRound(model$conf.high,2)
  CI        <- paste0("[",as.character(loCI),", ",as.character(hiCI),"]")
  model     <- model[,c(1,2,6,7,8)]
  model[,6] <- CI
  if(model[,4] < .001) {
    model[,4] <- " < 0.001"
  } else {
    model[,4] <- txtRound(model[,4],digits=3)
  }
  model[,1] <- txtRound(as.numeric(model[,1]),2)
  model[,3] <- txtRound(as.numeric(model[,3]),2)
  model[,5] <- txtRound(as.numeric(model[,5]),2)
  model     <- model[c(2,1,6,5,3,4)]
  model[,1] <- outcome
  model[,7] <- txtRound(mu,2)
  model     <- model[c(1,2,7,3,4,5,6)]
  model[8]  <- txtRound(cohens_d(data = data, formula = formula, mu = mu)$effsize,2)
  colnames(model) <- c("Variable",
                       "Mean",
                       math_mu,
                       "95% CI",
                       math_df,
                       math_t,
                       math_p,
                       paste0("Cohen's ",math_d))
  model[,5] <- txtRound(as.numeric(model[,5]),0) 
  output_table <- model %>%
    addHtmlTableStyle(pos.caption = "bottom",
                      align       = c('l',rep('c',5)),
                      css.cell    = "padding: 5px;") %>%
    htmlTable(rnames = FALSE,
              cgroup   = list(c(paste0('Single Sample t-Test', ""))),
              n.cgroup = list(c(8)), # check this 8 (6?)
              options  = list(dom = 't'),
              caption  = interp)
  output_table
}

ttest.simp.output.interp <- function(model   = NULL,
                                     alpha   = .05,
                                     outcome = NULL,
                                     ...) {
  outcome_name <- model$.y.
  data         <- attr(model, "args")$data
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  outcome <- tolower(outcome)
  y       <- data[outcome_name][,1]
  mean1   <- txtRound(mean(y, na.rm = T),2)
  mean2   <- txtRound(attr(model, "args")$mu,2)
  loCI    <- txtRound(model$conf.low,2)
  hiCI    <- txtRound(model$conf.high,2)
  var1    <- var(y, na.rm = T)
  pval    <- model$p
  formula <- attr(model, "args")$formula
  math_mu <- huxtable('<em>&#956</em>')
  math_t  <- huxtable('<em>t</em>')
  math_p  <- huxtable('<em>p</em>')
  math_d  <- huxtable('<em>d</em>')
  interp  <- paste0(
    "Our analysis revealed a ",
    ifelse(model$p < alpha,
           "significant ",
           "non-significant "),
    "difference between the mean of ",
    outcome,
    " in our sample and the population value of ",
    math_mu," = ",mean2," under the null hypothesis (",
    math_t,"(",model$df,") = ",txtRound(as.numeric(model[6]),digits=2),", ",math_p,
    ifelse(pval < .001,
           " < .001",
           paste0(" = ",txtRound(as.numeric(model[7]),digits=2))),"). The 95% confidence
    interval for the population mean (",math_mu,") ranged from ",loCI,
    " to ",hiCI,". The Cohen's ",math_d," measure of effect size
    for the sample mean was ",txtRound((mean(y,na.rm=T)-attr(model,"args")$mu)/sqrt(var1),2),"."
  )
  interp
}

ttest.indep.output <- function(model          = NULL,
                               interpretation = TRUE,
                               predictors     = NULL,
                               outcome        = NULL,
                               ...) {
  formula <- attr(model, "args")$formula
  data <- attr(model, "args")$data
  model <- t_test(data = data,
                  formula = formula,
                  var.equal = TRUE,
                  detailed = TRUE)
  predictor_name <- tail(strsplit(toString(attr(model, "args")$formula),", ")[[1]],1)
  outcome_name <- model$.y.
  if(is.null(predictors)) {
    predictors <- predictor_name
  }
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  interp <- ""
  if(interpretation) {
    interp <- paste0("Interpretation: ",
                     ttest.indep.output.interp(model = model,
                                               predictors = predictors,
                                               outcome = outcome))
  }
  math_t <- huxtable('<em>t</em>')
  math_p <- huxtable('<em>p</em>')
  math_df <- huxtable('<em>df</em>')
  math_d <- huxtable('<em>d</em>')
  estimate1 <- model$group1
  estimate2 <- model$group2
  loCI <- txtRound(model$conf.low,2)
  hiCI <- txtRound(model$conf.high,2)
  CI <- paste0('[',as.character(loCI),", ",as.character(hiCI),']')
  model <- model[,c(1,2,3,4,9,10,11)]
  model[,8] <- CI
  if(model[,6] < .001) {
    model[,6] <- " < 0.001"
  } else {
    model[,6] <- txtRound(model[,6],digits=3)
  }
  model[,c(1,2,3)] <- txtRound(model[,c(1,2,3)],2)
  model[,5] <- txtRound(as.numeric(model[,5]),2)
  model[,7] <- round(as.numeric(model[,7]),2)
  model <- model[c(4,2,3,1,8,7,5,6)]
  model[,1] <- outcome
  model[,2] <- txtRound(as.numeric(model[,2]),2)
  model[,3] <- txtRound(as.numeric(model[,3]),2)
  model[,4] <- txtRound(as.numeric(model[,4]),2)
  model[,9] <- txtRound(cohens_d(data = data, formula = formula)$effsize,2)
  colnames(model) <- c('Outcome',
                       paste0("Mean of ",predictors," = '",estimate1,"'"),
                       paste0("Mean of ",predictors," = '",estimate2,"'"),
                       huxtable('<em>M</em><sub>1</sub>-<em>M</em><sub>2</sub>'),
                       "95% CI",
                       math_df,math_t,math_p,paste0("Cohen's ",math_d))
  output_table <- model %>%
    addHtmlTableStyle(pos.caption = "bottom",
                      align = c('l',rep('c',8)),
                      css.cell = "padding: 5px;") %>%
    htmlTable(rnames = FALSE,
              cgroup = list(c(paste0('Independent-Samples t-Test'))),
              n.cgroup = list(c(9)),
              options = list(dom = 't'),
              caption = interp)
  output_table
}

ttest.indep.output.interp <- function(model      = NULL,
                                      alpha      = .05,
                                      predictors = NULL,
                                      outcome    = NULL,
                                      ...) {
  predictor_name <- tail(strsplit(toString(attr(model, "args")$formula),", ")[[1]],1)
  outcome_name <- model$.y.
  if(is.null(predictors)) {
    predictors <- predictor_name
  }
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  predictors <- tolower(predictors)
  outcome <- tolower(outcome)
  estimate1 <- model$group1
  estimate2 <- model$group2
  loCI <- txtRound(model$conf.low,2)
  hiCI <- txtRound(model$conf.high,2)
  data <- attr(model, "args")$data
  math_t <- huxtable('<em>t</em>')
  math_p <- huxtable('<em>p</em>')
  math_M <- huxtable('<em>M</em>')
  math_SD <- huxtable('<em>SD</em>')
  math_d <- huxtable('<em>d</em>')
  formula <- attr(model, "args")$formula
  x <- data[predictor_name][[1]]
  y <- data[outcome_name][[1]]
  mean1 <- txtRound(mean(y[x==sort(x)[1]],na.rm=T),2)
  mean2 <- txtRound(mean(y[x==sort(x,decreasing=T)[1]],na.rm=T),2)
  var1 <- var(y[x==sort(x)[1]],na.rm=T)
  var2 <- var(y[x==sort(x,decreasing=T)[1]],na.rm=T)
  interp <- paste0(
    "Our analysis revealed a ",
    ifelse(model$p < .05,
           "significant ",
           "non-significant "),
    "difference between the means of ",
    outcome,
    " in the '",
    estimate1,
    "' and '",
    estimate2,
    "' groups of ",
    predictors,
    ", ",
    math_t,
    "(",
    length(y)-2,
    ") = ",
    format(round(model$statistic, digits = 2), nsmall = 2),
    ", ",
    math_p,
    ifelse(model$p >= .001,
           paste0(" = ", rd(round(model$p,2),2), ". "),
           " < .001. "),
    "Those in the '",estimate1,"' group (",math_M," = ",mean1,", ",math_SD," = ",txtRound(sqrt(var1),2),")
    had ",
    if(model$p < .05) {
      "significantly "
    } else if(model$p == 1) {
      ""
    } else {
      "non-significantly "
    },
    if(mean1 > mean2) {
      "greater "
    } else if(mean1 < mean2) {
      "lower "
    } else {
      "no difference in "
    },
    "values for ",outcome," than those in the '",estimate2,"' group (",math_M," = ",mean2,", ",math_SD," = ",txtRound(sqrt(var2),2),").
    The 95% confidence interval for the difference in population means ranged from ",
    loCI, " to ", hiCI, ". The Cohen's ",math_d," mesure of effect size for this difference in sample means was ",
    txtRound(cohens_d(data=data,formula=formula,)$effsize,2),"."
  )
  interp
}

ttest.dep.output <- function(model,
                             interpretation = TRUE,
                             predictors = NULL,
                             outcome = NULL,
                             ...) {
  formula <- attr(model, "args")$formula
  data <- attr(model, "args")$data
  model <- t_test(data = data,
                  formula = formula,
                  paired = TRUE,
                  detailed = TRUE)
  predictor_name <- tail(strsplit(toString(formula),", ")[[1]],1)
  outcome_name <- model$.y.
  if(is.null(predictors)) {
    predictors <- predictor_name
  }
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  interp <- ""
  if(interpretation == TRUE) {
    interp <- paste0("Interpretation: ",
                     ttest.dep.output.interp(model = model,
                                             predictors = predictors,
                                             outcome = outcome))
  }
  math_t <- huxtable('<em>t</em>')
  math_p <- huxtable('<em>p</em>')
  math_d <- huxtable('<em>d</em>')
  math_df <- huxtable('<em>df</em>')
  estimate1 <- model$group1
  estimate2 <- model$group2
  x1 <- data[outcome_name][data[predictor_name]==estimate1]
  x2 <- data[outcome_name][data[predictor_name]==estimate2]
  mean1 <- round(mean(x1, na.rm=T),2)
  mean2 <- round(mean(x2, na.rm=T),2)
  cohen <- cohensD(x2, x1, method = 'paired')
  loCI <- round(model$conf.low,2)
  hiCI <- round(model$conf.high,2)
  CI <- paste0('[',as.character(loCI),", ",as.character(hiCI),']')
  model <- model[,c(1,2,7,8,9)]
  model[,6] <- CI
  model[,7] <- mean1
  model[,8] <- mean2
  model[,9] <- cohen
  if(model[,4] < .001) {
    model[,4] <- " < 0.001"
  } else {
    model[,4] <- txtRound(model[,4], digits=3)
  }
  model[,1] <- txtRound(as.numeric(model[,1]),2)
  model[,3] <- txtRound(as.numeric(model[,3]),2)
  model[,5] <- txtRound(as.numeric(model[,5]),2)
  model[,9] <- txtRound(as.numeric(model[,9]),2)
  model <- model[,c(2,7,8,1,6,5,3,4,9)]
  model[,1] <- outcome
  colnames(model) <- c("Outcome",
                       paste0("Mean (",predictors,": ",estimate1,")"),
                       paste0("Mean (",predictors,": ",estimate2,")"),
                       huxtable('<em>M</em><sub>(X<sub>1</sub>-X<sub>2</sub>)</sub>'),
                       "95% CI",
                       math_df,
                       math_t,
                       math_p,
                       paste0("Cohen's ",math_d))
  model[,6] <- txtRound(as.numeric(model[,6]))
  output_table <- model %>%
    addHtmlTableStyle(pos.caption = "bottom",
                      align = c('l',rep('c',8)),
                      css.cell = "padding: 5px;") %>%
    htmlTable(rnames = FALSE,
              cgroup = list(c(paste0('Dependent-Samples t-Test'))),
              n.cgroup = list(c(9)),
              options = list(dom = 't'),
              caption = interp)
}

ttest.dep.output.interp <- function(model,
                                    alpha = .05,
                                    predictors = NULL,
                                    outcome = NULL,
                                    ...) {
  formula <- attr(model, "args")$formula
  predictor_name <- tail(strsplit(toString(formula),", ")[[1]],1)
  outcome_name <- model$.y.
  data <- attr(model, "args")$data
  if(is.null(predictors)) {
    predictors <- predictor_name
  }
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  outcome <- tolower(outcome)
  predictors <- tolower(predictors)
  estimate1 <- model$group1
  estimate2 <- model$group2
  x1 <- data[outcome_name][data[predictor_name]==estimate1]
  x2 <- data[outcome_name][data[predictor_name]==estimate2]
  cohen <- cohensD(x2, x1, method = 'paired')
  loCI <- round(model$conf.low,2)
  hiCI <- round(model$conf.high,2)
  y <- data[outcome_name][,1]
  loCI <- txtRound(model$conf.low,2)
  hiCI <- txtRound(model$conf.high,2)
  mean1 <- round(mean(x1, na.rm=T),2)
  mean2 <- round(mean(x2, na.rm=T),2)
  var1 <- round(var(x1,na.rm=T),2)
  var2 <- round(var(x2,na.rm=T),2)
  math_t <- huxtable('<em>t</em>')
  math_p <- huxtable('<em>p</em>')
  math_d <- huxtable('<em>d</em>')
  math_M <- huxtable('<em>M</em>')
  math_SD <- huxtable('<em>SD</em>')
  interp <- paste0(
    "Our analysis revealed a ",
    ifelse(model$p < .05,
           "significant ",
           "non-significant "),
    "difference between the means of ",
    outcome,
    " in the '",
    estimate1,
    "' and '",
    estimate2,
    "' groups of ",
    predictors,
    ", ",
    math_t,
    "(",
    model$df,
    ") = ",
    format(round(model$statistic, digits = 2), nsmall = 2),
    ", ",
    math_p,
    ifelse(model$p >= .001,
           paste0(" = ", rd(round(model$p,2),2), ". "),
           " < .001. "),
    "Those in the '",estimate1,"' group (",math_M," = ",mean1,", ",math_SD," = ",txtRound(sqrt(var1),2),")
    had ",
    if(model$p < .05) {
      "significantly "
    } else if(model$p == 1) {
      ""
    } else {
      "non-significantly "
    },
    if(mean1 > mean2) {
      "greater "
    } else if(mean1 < mean2) {
      "lower "
    } else {
      "no difference in "
    },
    "values for ",outcome," than those in the '",estimate2,"' group (",math_M," = ",mean2,", ",math_SD," = ",txtRound(sqrt(var2),2),").
    The 95% confidence interval for the population mean of differences ranged from ",
    loCI, " to ", hiCI, ". The Cohen's ",math_d," mesure of effect size for this sample mean of differences was ",
    txtRound(cohen,2),"."
  )
  interp
}

