#' The ttest model method for the `sampdist` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `ttest` function in the `doingRbest` package to run a statistical model.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return A `plotly` plot
#' @export
#' 
#' @examples
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

ttest.sampdist <- function(model      = NULL,
                           predictors = NULL,
                           outcome    = NULL,
                           ...) {
  if (model$type[1] == "single") {
    output_graph <- ttest.simp.sampdist(model   = model,
                                        outcome = outcome)

  } else {
    output_graph <- ttest.indep.dep.sampdist(model      = model,
                                             predictors = predictors,
                                             outcome    = outcome)
  }
  output_graph
}


ttest.simp.sampdist <- function(model   = NULL,
                                outcome = NULL,
                                ...) {
  outcome_name <- model$.y.
  if (is.null(outcome)) {
    outcome <- outcome_name
  }
  tstat <- round(model$statistic, 2)
  graph_title <- paste0("Distribution of t-statistic for ", outcome, 
                        " (df = ",model$df,")")
  sampdist <- ggplotly(
    ggplot(data = data.frame(x=seq(-5,5,.1),y=seq(0,.4,.004)), aes(x, y)) +
      stat_function(fun=dt,args=list(df=model$df),geom="line",col="darkblue",lwd=1) +
      stat_function(fun=dt,args=list(df=model$df),geom="area",xlim=c(-4,(qt(.025,model$df))),fill="red",alpha=.33) +
      stat_function(fun=dt,args=list(df=model$df),geom="area",xlim=c(qt(.975,model$df),4),fill="red",alpha=.33) +
      geom_segment(aes(x=model$statistic,xend=tstat,y=0, yend = max(y)),color="darkgreen",lwd=1) +
      labs(x="t-value",y=NULL,title=graph_title) +
      scale_x_continuous(limits = c(min(-4,-model$statistic-.1, model$statistic-.1),
                                    max(4,-model$statistic+.1,model$statistic+.1))),
    tooltip = "xend"
  )
  sampdist
}


ttest.indep.dep.sampdist <- function(model      = NULL,
                                     predictors = NULL,
                                     outcome    = NULL,
                                     ...) {
  predictor_name <- tail(strsplit(toString(attr(model, "args")$formula), ", ")[[1]], 1)
  outcome_name <- model$.y.
  if (is.null(predictors)) {
    predictors <- predictor_name
    outcome <- outcome_name
  }
  tstat <- round(model$statistic, 2)
  graph_title <- paste0("Distribution of t-statistic for differences in ", outcome, " across ", predictors, 
                        " (df = ",model$df,")")
  sampdist <- ggplotly(
    ggplot(data = data.frame(x=seq(-5,5,.1),y=seq(0,.4,.004)), aes(x, y)) +
      stat_function(fun=dt,args=list(df=model$df),geom="line",col="darkblue", lwd=1) +
      stat_function(fun=dt,args=list(df=model$df),geom="area",xlim=c(-4,(qt(.025,model$df))),fill="red",alpha=.33) +
      stat_function(fun=dt,args=list(df=model$df),geom = "area",xlim=c(qt(.975,model$df),4),fill="red",alpha=.33) +
      geom_segment(aes(x=model$statistic,xend=tstat,y=0,yend=max(y)),color="darkgreen",lwd=1) +
      labs(x="t-value",y=NULL,title=graph_title) +
      scale_x_continuous(limits = c(min(-4,-model$statistic-.1, model$statistic-.1),
                                    max(4,-model$statistic+.1,model$statistic+.1))),
    tooltip="xend"
  )
  sampdist
}
