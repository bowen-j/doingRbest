#' The regression model method for the `sampdist` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `regress` function in the `doingRbest` package to run a statistical model.
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

regression.sampdist <- function(model=NULL,
                                Fdist=FALSE,
                                tdist=FALSE,
                                slope=NULL,
                                predictors=NULL,
                                outcome=NULL,
                                ...) {
  sampdist_graph <- regress.sampdist(model=model,
                                     Fdist=Fdist,
                                     tdist=tdist,
                                     slope=slope,
                                     predictors=predictors,
                                     outcome=outcome)
  sampdist_graph
}

regress.sampdist <- function(model=NULL,
                             Fdist=FALSE,
                             tdist=FALSE,
                             slope=NULL,
                             predictors=NULL,
                             outcome=NULL,
                             ...) {
  predictor_name <- ifelse(is.null(slope),colnames(model$model)[2],slope)
  outcome_name <- colnames(model$model)[1]
  predictors <- ifelse(is.null(predictors),predictor_name,predictors)
  outcome <- ifelse(is.null(outcome),outcome_name,outcome)
  data <- model$model
  formula <- formula(model$terms)
  aov <- tidy(aov(formula, data))
  Fstat <- round(as.numeric(summary(model)$fstatistic[1]),2)
  tstat <- round(summary(model)$coefficients[,3][rownames(summary(model)$coefficients)==predictors[1]],2)
  df_num <- as.numeric(aov[1,2])
  df_denom <- as.numeric(aov[nrow(aov),2])

if(Fdist == TRUE && tdist == TRUE) {
  Fdist_output <- regression.Fdist.sampdist(model = model,
                                            Fstat = Fstat,
                                            df_num = df_num,
                                            df_denom = df_denom,
                                            predictors = predictors,
                                            outcome = outcome)
  tdist_output <- regression.tdist.sampdist(model = model,
                                          tstat = tstat,
                                          df_num = df_num,
                                          df_denom = df_denom,
                                          predictors = predictors,
                                          outcome = outcome)
  Fandt <- plotly::subplot(list(Fdist_output, tdist_output),
                           nrows = 1,
                           margin = c(.05,.05,.12,.05),
                           shareX = FALSE,
                           shareY = FALSE,
                           titleX = TRUE,
                           titleY = TRUE) %>%
    layout(title = paste0("F-distribution for full model and t-distribution for slope of ",predictors[1]))
  Fandt
} else if(Fdist == TRUE) {
  Fdist_output <- regression.Fdist.sampdist(model = model,
                                            Fstat = Fstat,
                                            df_num = df_num,
                                            df_denom = df_denom,
                                            predictors = predictors,
                                            outcome = outcome)
  Fdist_output
} else if(tdist == TRUE) {
  tdist_output <- regression.tdist.sampdist(model = model,
                                          tstat = tstat,
                                          df_num = df_num,
                                          df_denom = df_denom,
                                          predictors = predictors,
                                          outcome = outcome)
  tdist_output
}
}

regression.Fdist.sampdist <- function(model = NULL,
                                      Fstat = NULL,
                                      df_num = NULL,
                                      df_denom = NULL,
                                      predictors = NULL,
                                      outcome = NULL,
                                      ...) {
  sampdistF <- ggplotly(
    ggplot(data=data.frame(x=seq(0,15,.15),y=seq(0,.4,.004)), aes(x, y)) +
      stat_function(fun=df,args=list(df1=df_num,df2=df_denom),col="darkblue",lwd=1,geom="line") +
      stat_function(fun=df,args=list(df1=df_num,df2=df_denom),fill="red",alpha=.33,geom="area",xlim=c(qf(.05,df_num,df_denom,lower=F),10)) +
      geom_segment(aes(x=Fstat,xend=Fstat,y=0,yend=max(y)),color="darkgreen",lwd=1) +
      labs(x = "F (whole model)", y = NULL,
           title = paste0("Distribution of F-statistic for regression model predicting ", outcome)) +
      scale_x_continuous(limits = c(0,max(10,Fstat+.1))),
    tooltip = "xend"
  )
  sampdistF
}

regression.tdist.sampdist <- function(model = NULL,
                                      tstat = NULL,
                                      df_denom = NULL,
                                      predictors = NULL,
                                      outcome = NULL,
                                      ...) {
  sampdistt <- ggplotly(
    ggplot(data=data.frame(x=seq(-5,5,.1),y=seq(0,.4,.004)), aes(x,y)) +
      stat_function(fun=dt,args=list(df=df_denom),geom="line",col="darkblue",lwd=1) +
      stat_function(fun=dt,args=list(df=df_denom),geom="area",fill="red",alpha=.33,xlim=c(-4,(qt(.025,df_denom)))) +
      stat_function(fun=dt,args=list(df=df_denom),geom="area",fill="red",alpha=.33,xlim=c(qt(.975,df_denom),4)) +
      geom_segment(aes(x=tstat,xend=tstat,y=0,yend=max(y)),color="darkgreen",lwd=1) +
      labs(x="t (slope)",y=NULL,
           title = paste0("Distribution of t-statistic for slope of ", predictors[1], " on ", outcome)) +
      scale_x_continuous(limits=c(min(-4,-tstat-.1,tstat-.1),max(4,-tstat+.1,tstat+.1))),
    tooltip="xend"
  )
  sampdistt
}