#' The t-test model method for the `calcs` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `ttest` function in the `doingRbest` package.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return An HTML page with a series of solved equations (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Single Sample t-test Calculations
#' model <- ttest(outcome = "disp", data = mtcars, type = "s", mu = 100)
#' calcs(model, outcome = "Fuel Displacement")
#' # Independent Samples t-test Calculations
#' model <- ttest(outcome = "hardness", predictor = "location", data = PASWR2::APPLE, type = "independent")
#' calcs(model)
#' # Dependent Samples t-test Calculations for "wide" format data
#' early <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' late <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(early, late)
#' model <- ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("early", "late"))
#' calcs(model)
#' # Dependent Samples t-test Calculations for "long" format data
#' SubID <- factor(sort(rep(1:10,2)))
#' Grp <- factor(rep(c("Ctrl","Trt"),10))
#' DV <- rnorm(20,10,2)
#' the_data <- data.frame(SubID, Grp, DV)
#' model <- ttest(outcome = "DV", predictor = "Grp", data = the_data, data_format = "long", type = "dependent")
#' calcs(model)


# Constructor:
#-------------------------------------------------------------------------------
ttest.calcs <- function(model          = NULL,
                        predictors     = NULL,
                        outcome        = NULL,
                         ...) {
  if (model$type[1] == "single") {
    calculations <- ttest.simp.calcs(model          = model,
                                     outcome        = outcome)
    
  } else if (model$type[1] == "independent") {
    calculations <- ttest.indep.calcs(model          = model,
                                      predictors     = predictors,
                                      outcome        = outcome)
    
  } else {
    calculations <- ttest.dep.calcs(model          = model,
                                    predictors     = predictors,
                                    outcome        = outcome)
  }
  calculations
}


ttest.simp.calcs <- function(model          = NULL,
                             outcome        = NULL,
                              ...) {
  data <- attr(model, "args")$data
  formula <- attr(model, "args")$formula
  xbar <- mean(data[,model$.y.] ,na.rm=T)
  n <- model$n
  mu <- attr(model, "args")$mu
  mod_det <- t_test(data,formula,mu,detailed=T)
  sx <- sd(data[,model$.y.] ,na.rm=T)
  se <- sx/sqrt(n)
  tstat <- model$statistic
  df <- model$df
  d <- cohens_d(data,formula,mu)$effsize
  tcrit <- qt(.975,df)
  loCI <- mod_det$conf.low
  hiCI <- mod_det$conf.high
  calc_t <- withMathJax(
    TeX("$$\\textbf{Calculations for a Single-Sample t-test}$$"),
    "Standard Error:",
    sprintf(
      "$$s_{\\bar{X}}=\\frac{s_X}{\\sqrt{n}}=\\frac{%.02f}{\\sqrt{%.0f}}=%.02f$$",
      sx,n,se
      ),
    "T-Statistic:",
    sprintf(
      "$$t=\\frac{\\bar{X}-\\mu_X}{s_{\\bar{X}}}=\\frac{%.02f-%.02f}{%.02f}=%.02f$$",
      xbar,mu,se,tstat      
      ),
    "Degrees of Freedom:",
    sprintf(
      "$$df=n-1=%.0f-1=%.0f$$",
      n,df      
      ),
    "Cohen's d (Effect Size):",
    sprintf(
      "$$d=\\frac{\\bar{X}-\\mu_{X}}{s_X}=\\frac{%.02f-%.02f}{%.02f}=%.02f$$",
      xbar,mu,sx,d
    ),
    "95% Confidence Interval:",
    sprintf(
      "$$\\mu=\\bar{X}\\pm(t_{crit})(s_{\\bar{X}})=%.02f\\pm(%.02f)(%.02f)=[%.02f,%.02f]$$",
      xbar,tcrit,se,loCI,hiCI
    )
    )
  html_print(calc_t)
}

ttest.indep.calcs <- function(model          = NULL,
                              predictors     = NULL,
                              outcome        = NULL,
                              ...) {
  data <- attr(model, "args")$data
  formula <- attr(model, "args")$formula
  xvar_name <- strsplit(as.character(formula)," ")[[3]] 
  df1 <- model$n1-1
  df2 <- model$n2-1
  n1 <- model$n1
  n2 <- model$n2
  var1 <- var(data[,model$.y.][data[,xvar_name]==model$group1] ,na.rm=T)
  var2 <- var(data[,model$.y.][data[,xvar_name]==model$group2] ,na.rm=T)
  pooled <- (df1*var1+df2*var2)/(n1+n2-2)
  se <- sqrt(pooled/n1+pooled/n2)
  xbar1 <- mean(data[,model$.y.][data[,xvar_name]==model$group1] ,na.rm=T)
  xbar2 <- mean(data[,model$.y.][data[,xvar_name]==model$group2] ,na.rm=T)
  tstat <- model$statistic
  d <- cohens_d(data, formula, var.equal = T)$effsize
  tcrit <- qt(.975,df1+df2)
  mod_det <- t_test(data,formula,var.equal=T,detailed=T)
  loCI <- mod_det$conf.low
  hiCI <- mod_det$conf.high
  calc_t <- withMathJax(
    TeX("$$\\textbf{Calculations for an Independent-Samples t-test}$$"),
    "Pooled Variance:",
    sprintf(
      "$$s^2_p=\\frac{(df_1)(s^2_{x_1})+(df_2)(s^2_{x_2})}{n_1+n_2-2}=
      \\frac{(%.0f)(%.02f)+(%.0f)(%.02f)}{%.0f+%.0f-2}=
      \\frac{%.02f+%.02f}{%.0f}=%.02f$$",
      df1,var1,df2,var2,n1,n2,df1*var1,df2*var2,n1+n2-2,pooled
    ),
    "Standard Error:",
    sprintf(
      "$$s_{(\\bar{X}_1-\\bar{X}_2)}=\\sqrt{\\frac{s^2_p}{n_1}+\\frac{s^2_p}{n_2}}=
      \\sqrt{\\frac{%.02f}{%.0f}+\\frac{%.02f}{%.0f}}=
      \\sqrt{%.02f+%.02f}=%.02f$$",
      pooled,n1,pooled,n2,pooled/n1,pooled/n2,se
    ),
    "T-Statistic:",
    sprintf(
      "$$t_{(\\bar{X}_1-\\bar{X}_2)}=\\frac{(\\bar{X}_1-\\bar{X}_2)-(\\mu_1-\\mu_2)}{s_{(\\bar{X}_1-\\bar{X}_2)}}=
      \\frac{(%.02f-%.02f)-0}{%.02f}=\\frac{%.02f}{%.02f}=%.02f$$",
      xbar1,xbar2,se,xbar1-xbar2,se,tstat
    ),
    "Degrees of Freedom:",
    sprintf(
      "$$df=n_1+n_2-2=%.0f+%.0f-2=%.0f$$",
      n1,n2,n1+n2-2
    ),
    "Cohen's d (Effect Size):",
    sprintf(
      "$$d=\\frac{\\bar{X}_1-\\bar{X}_2}{s^2_p}=
      \\frac{%.02f-%.02f}{%.02f}=%.02f$$",
      xbar1,xbar2,pooled,d
    ),
    "95% Confidence Interval:",
    sprintf(
      "$$\\mu_1-\\mu_2=(\\bar{X}_1-\\bar{X}_2)\\pm(t_{crit})(s_{(\\bar{X}_1-\\bar{X}_2)})=
      %.02f\\pm(%.02f)(%.02f)=[%.02f,%.02f]$$",
      xbar1-xbar2,tcrit,se,loCI,hiCI
    )
  )
  html_print(calc_t)
}

ttest.dep.calcs <- function(model          = NULL,
                            predictors     = NULL,
                            outcome        = NULL,
                            ...) {
  data <- attr(model, "args")$data
  formula <- attr(model, "args")$formula
  xvarname <- strsplit(as.character(formula)," ")[[3]] 
  diffs <- data[,model$.y.][data[,xvarname]==model$group1]-
           data[,model$.y.][data[,xvarname]==model$group2]
  sD <- sd(diffs, na.rm=T)
  nD <- length(diffs)
  seD <- sD/sqrt(nD)
  xbarD <- mean(diffs, na.rm=T)
  tstat <- model$statistic
  d <- cohens_d(data,formula,paired=T)$effsize
  tcrit <- qt(.975,nD-1)
  mod_det <- t_test(data,formula,paired=T,detailed=T)
  loCI <- mod_det$conf.low
  hiCI <- mod_det$conf.high
  calc_t <- withMathJax(
    TeX("$$\\textbf{Calculations for a Dependent-Samples t-test}$$"),
    "Standard Error:",
    sprintf(
      "$$s_{(\\overline{X_1-X_2})}=\\frac{s_{(X_1-X_2)}}{\\sqrt{n_{(X_1-X_2)}}}=
      \\frac{%.02f}{\\sqrt{%.0f}}=%.02f$$",
      sD,nD,seD
    ),
    "T-Statistic:",
    sprintf(
      "$$t=\\frac{(\\overline{X_1-X_2})-\\mu_{(X_1-X_2)}}{s_{(\\overline{X_1-X_2})}}=
      \\frac{%.02f-0}{%.02f}=%.02f$$",
      xbarD,seD,tstat      
    ),
    "Degrees of Freedom:",
    sprintf(
      "$$df=n_{(X_1-X_2)}-1=%.0f-1=%.0f$$",
      nD,nD-1      
    ),
    "Cohen's d (Effect Size):",
    sprintf(
      "$$d=\\frac{\\overline{X_1-X_2}}{s_{(X_1-X_2)}}=
      \\frac{%.02f}{%.02f}=%.02f$$",
      xbarD,sD,d
    ),
    "95% Confidence Interval:",
    sprintf(
      "$$\\mu_{(X_1-X_2)}=\\overline{X_1-X_2}\\pm(t_{crit})(s_{(\\overline{X_1-X_2})})=
      %.02f\\pm(%.02f)(%.02f)=[%.02f,%.02f]$$",
      xbarD,tcrit,seD,loCI,hiCI
    )
  )
  html_print(calc_t)
}
