#' The ttest model method for the `hyp` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `ttest` function in the `doingRbest` package.
#' 
#' @return An HTML page with a series of null and alternative hypotheses (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
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

ttest.hyp <- function(model = NULL,
                      ...) {
  if(model$type[1] == "single") {
    hyp_test <- ttest.simp.hyp(model = model)
  } else if(model$type[1] == "independent") {
    hyp_test <- ttest.indep.hyp(model = model)
  } else {
    hyp_test <- ttest.dep.hyp(model = model)
  }
  hyp_test
}

ttest.simp.hyp <- function(model = NULL,
                           ...) {
  data <- attr(model, "args")$data
  xbar <- mean(data[,model$.y.], na.rm = T)
  n <- model$n
  mu <- attr(model, "args")$mu
  sx <- sd(data[,model$.y.] ,na.rm=T)
  se <- sx/sqrt(n)
  tstat <- model$statistic
  df <- model$df
  tcrit <- qt(.975, df)
  pval <- pt(tstat,df,lower.tail=ifelse(tstat>=0,FALSE,TRUE))*2
  hyp_t <- withMathJax(
    TeX("$$\\textbf{Hypothesis Test for a Two-Tailed Single-Sample t-test } (\\alpha = .05)$$"),
    sprintf("$$-----------------------------------------$$"),
    "Statistical Hypotheses:",
    sprintf(
      "$$H_0:\\mu_X=%.02f \\qquad H_1:\\mu_X\\ne%.02f$$",
      mu,mu
    ),
    "Test Statistic:",
    sprintf(
      "$$t=\\frac{\\bar{X}-\\mu_X}{s_{\\bar{X}}}=\\frac{%.02f-%.02f}{%.02f}=%.02f$$",
      xbar,mu,se,tstat      
    ),
    "Critical Value:",
    sprintf(
      "$$t_{crit}=\\pm\\ \\texttt{qt(p = .975, df = %.0f)}=\\pm%.02f$$",
      df,tcrit
    ),
    "P-value:",
    sprintf(
      "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
      tstat,df,ifelse(tstat>=0,"F","T"),pval
    ),
    "Decision:",
    sprintf(
      "$$t%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
      ifelse(tstat<tcrit,"<",">"),ifelse(pval<.05,"<",">"),ifelse(pval<.05,"Reject ","Fail to reject ")
    )
  )
  html_print(hyp_t)
}

ttest.indep.hyp <- function(model = NULL,
                            ...) {
  data <- attr(model, "args")$data
  formula <- attr(model, "args")$formula
  xvar_name <- strsplit(as.character(formula)," ")[[3]] 
  n1 <- model$n1
  n2 <- model$n2
  df1 <- n1-1
  df2 <- n2-1
  var1 <- var(data[,model$.y.][data[,xvar_name]==model$group1] ,na.rm=T)
  var2 <- var(data[,model$.y.][data[,xvar_name]==model$group2] ,na.rm=T)
  pooled <- (df1*var1+df2*var2)/(n1+n2-2)
  se <- sqrt(pooled/n1+pooled/n2)
  xbar1 <- mean(data[,model$.y.][data[,xvar_name]==model$group1] ,na.rm=T)
  xbar2 <- mean(data[,model$.y.][data[,xvar_name]==model$group2] ,na.rm=T)
  tstat <- model$statistic
  df_total <- df1+df2
  tcrit <- qt(.975, df_total)
  pval <- pt(tstat,df_total,lower.tail=ifelse(tstat>=0,FALSE,TRUE))*2
  hyp_t <- withMathJax(
    TeX("$$\\textbf{Hypothesis Test for a Two-Tailed Independent-Samples t-test } (\\alpha = .05)$$"),
    sprintf("$$-----------------------------------------$$"),
    "Statistical Hypotheses:",
    sprintf(
      "$$H_0:\\mu_1-\\mu_2=0 \\qquad H_1:\\mu_1-\\mu_2\\ne0$$"
    ),
    "Test Statistic:",
    sprintf(
      "$$t=\\frac{(\\bar{X}_1-\\bar{X}_2)-(\\mu_1-\\mu_2)}{s_{(\\bar{X}_1-\\bar{X}_2)}}=
      \\frac{%.02f-0}{%.02f}=%.02f$$",
     xbar1-xbar2,se,tstat
    ),
    "Critical Value:",
    sprintf(
      "$$t_{crit}=\\pm\\ \\texttt{qt(p = .975, df = %.0f)}=\\pm%.02f$$",
      df_total,tcrit
    ),
    "P-value:",
    sprintf(
      "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
      tstat,df_total,ifelse(tstat>=0,"F","T"),pval
    ),
    "Decision:",
    sprintf(
      "$$t%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
      ifelse(tstat<tcrit,"<",">"),ifelse(pval<.05,"<",">"),ifelse(pval<.05,"Reject ","Fail\\ to\\ reject ")
    )
  )
  html_print(hyp_t)
}

ttest.dep.hyp <- function(model = NULL,
                          ...) {
  data <- attr(model, "args")$data
  formula <- attr(model, "args")$formula
  xvarname <- strsplit(as.character(formula)," ")[[3]] 
  diffs <- data[,model$.y.][data[,xvarname]==model$group1]-
    data[,model$.y.][data[,xvarname]==model$group2]
  sD <- sd(diffs, na.rm=T)
  nD <- length(diffs)
  df <- nD-1
  seD <- sD/sqrt(nD)
  xbarD <- mean(diffs, na.rm=T)
  tstat <- model$statistic
  tcrit <- qt(.975,nD-1)
  pval <- pt(tstat,df,lower.tail=ifelse(tstat>=0,FALSE,TRUE))*2
  hyp_t <- withMathJax(
    TeX("$$\\textbf{Hypothesis Test for a Two-Tailed Dependent-Samples t-test } (\\alpha = .05)$$"),
    sprintf("$$-----------------------------------------$$"),
    "Statistical Hypotheses:",
    sprintf(
      "$$H_0:\\mu_{(X_1-X_2)}=0 \\qquad H_1:\\mu_{(X_1-X_2)}\\ne0$$"
    ),
    "Test Statistic:",
    sprintf(
      "$$t=\\frac{(\\overline{X_1-X_2})-\\mu_{(X_1-X_2)}}{s_{(\\overline{X_1-X_2})}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
      xbarD,seD,tstat      
    ),
    "Critical Value:",
    sprintf(
      "$$t_{crit}=\\pm\\ \\texttt{qt(p = .975, df = %.0f)}=\\pm%.02f$$",
      df,tcrit
    ),
    "P-value:",
    sprintf(
      "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
      tstat,df,ifelse(tstat>=0,"F","T"),pval
    ),
    "Decision:",
    sprintf(
      "$$t%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
      ifelse(tstat<tcrit,"<",">"),ifelse(pval<.05,"<",">"),ifelse(pval<.05,"Reject ","Fail\\ to\\ reject ")
    )
  )
  html_print(hyp_t)
}
