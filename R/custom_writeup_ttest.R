#' The ttest model method for the `writeup` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `ttest` function in the `doingRbest` package.
#' 
#' @return An HTML page with the text of the results writeup (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Single Sample t-test Writeup
#' model <- ttest(outcome = "disp", data = mtcars, type = "s", mu = 100)
#' writeup(model, outcome = "Fuel Displacement")
#' # Independent Samples t-test Writeup
#' model <- ttest(outcome = "hardness", predictor = "location", data = PASWR2::APPLE, type = "independent")
#' writeup(model)
#' # Dependent Samples t-test Writeup for "wide" format data
#' early <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' late <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(early, late)
#' model <- ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("early", "late"))
#' writeup(model)
#' # Dependent Samples t-test Writeup for "long" format data
#' SubID <- factor(sort(rep(1:10,2)))
#' Grp <- factor(rep(c("Ctrl","Trt"),10))
#' DV <- rnorm(20,10,2)
#' the_data <- data.frame(SubID, Grp, DV)
#' model <- ttest(outcome = "DV", predictor = "Grp", data = the_data, data_format = "long", type = "dependent")
#' writeup(model)

ttest.writeup <- function(model          = NULL,
                          predictors     = NULL,
                          outcome        = NULL,
                          ...) {
  if (model$type[1] == "single") {
    results <- ttest.simp.writeup(model          = model,
                                     outcome        = outcome)
    
  } else if (model$type[1] == "independent") {
    results <- ttest.indep.writeup(model          = model,
                                      predictors     = predictors,
                                      outcome        = outcome)
    
  } else {
    results <- ttest.dep.writeup(model          = model,
                                    predictors     = predictors,
                                    outcome        = outcome)
  }
  results
}


ttest.simp.writeup <- function(model          = NULL,
                             outcome        = NULL,
                             ...) {
  data <- attr(model, "args")$data
  formula <- attr(model, "args")$formula
  mu <- attr(model, "args")$mu
  model   <- t_test(data     = data,
                    formula  = formula,
                    mu       = mu,
                    detailed = TRUE)
  outcome_name <- model$.y.
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  xbar <- mean(data[,model$.y.] ,na.rm=T)
  n <- model$n
  mod_det <- t_test(data,formula,mu,detailed=T)
  sx <- sd(data[,model$.y.] ,na.rm=T)
  se <- sx/sqrt(n)
  tstat <- model$statistic
  df <- model$df
  d <- cohens_d(data,formula,mu)$effsize
  tcrit <- qt(.975,df)
  pval <- pt(tstat,df,lower.tail=ifelse(tstat>=0,FALSE,TRUE))*2
  loCI <- mod_det$conf.low
  hiCI <- mod_det$conf.high
  writeup_t <- 
    h3(paste0("Recall that the goal of this study was to determine 
          whether the mean for ",outcome," obtained in our sample was 
          different from the population value of ",mu,". To investigate this, 
          we conducted a single-sample t-test comparing our sample mean of ",
          txtRound(xbar,2)," to the value of ",txtRound(mu,2)," obtained from 
          the relevant population. This analysis revealed a ",
          ifelse(pval<.05,"signficant ","non-significant "),"difference 
          between our sample mean and the population mean (t(",df,") = ",
          txtRound(tstat,2),", p ",
          ifelse(pval>=.05,paste0(" = ",txtRound(pval,3)),
                 ifelse(pval<.05 & pval>=.01," < .05",
                        ifelse(pval<.01 & pval>=.001," < .01"," < .001"))),
          ", 95% CI = [",txtRound(loCI,2),", ",
          txtRound(hiCI,2),"], d = ",txtRound(d,2),"), suggesting that 
          our sample ",ifelse(pval<.05,"may not have ","likely did "),
          "come from the given population with a known mean of ",
          txtRound(mu,2)," for ",outcome,"."))

  html_print(writeup_t)
}

ttest.indep.writeup <- function(model          = NULL,
                              predictors     = NULL,
                              outcome        = NULL,
                              ...) {
  data <- attr(model, "args")$data
  formula <- attr(model, "args")$formula
  xvar_name <- strsplit(as.character(formula)," ")[[3]] 
  predictor_name <- tail(strsplit(toString(attr(model, "args")$formula),", ")[[1]],1)
  outcome_name <- model$.y.
  if(is.null(predictors)) {
    predictors <- predictor_name
  }
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  estimate1 <- model$group1
  estimate2 <- model$group2
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
  df_total <- df1+df2
  pval <- pt(tstat,df_total,lower.tail=ifelse(tstat>=0,FALSE,TRUE))*2
  writeup_t <- 
    h3(paste0("Recall that the goal of this study was to determine whether 
           there were differences in ",outcome," between the ",estimate1,
           " and ",estimate2," groups. To investigate this, we conducted 
           an independent-samples t-test comparing ",predictors," means ",
           "between the ",estimate1," (M = ",txtRound(xbar1,2),", SD = 
           ",txtRound(sqrt(var1),2),")"," and ",estimate2," (M = ",
           txtRound(xbar2,2),", SD = ",txtRound(sqrt(var2),2),") groups. 
           This analysis revealed a ",ifelse(pval<.05,"significant ",
                                             "non-significant "),
           "difference between the two means (t(",df_total,") = ",
           txtRound(tstat,2),", p ",
           ifelse(pval>=.05,paste0(" = ",txtRound(pval,3)),
                  ifelse(pval<.05 & pval>=.01," < .05",
                         ifelse(pval<.01 & pval>=.001," < .01"," < .001"))),
           ", 95% CI = [",txtRound(loCI,2),", ",
           txtRound(hiCI,2),"], d = ",txtRound(d,2),"), suggesting that ",
           predictors,ifelse(pval<.05," likely "," may not have greatly "),
           "affected ",outcome,"."))
  html_print(writeup_t)
}

ttest.dep.writeup <- function(model          = NULL,
                            predictors     = NULL,
                            outcome        = NULL,
                            ...) {
  data <- attr(model, "args")$data
  formula <- attr(model, "args")$formula
  predictor_name <- tail(strsplit(toString(formula),", ")[[1]],1)
  outcome_name <- model$.y.
  if(is.null(predictors)) {
    predictors <- predictor_name
  }
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  xvar_name <- strsplit(as.character(formula)," ")[[3]]
  xbar1 <- mean(data[,model$.y.][data[,xvar_name]==model$group1] ,na.rm=T)
  xbar2 <- mean(data[,model$.y.][data[,xvar_name]==model$group2] ,na.rm=T)
  sd1 <- sd(data[,model$.y.][data[,xvar_name]==model$group1] ,na.rm=T)
  sd2 <- sd(data[,model$.y.][data[,xvar_name]==model$group2] ,na.rm=T)
  estimate1 <- model$group1
  estimate2 <- model$group2
  diffs <- data[,model$.y.][data[,xvar_name]==model$group1]-
    data[,model$.y.][data[,xvar_name]==model$group2]
  sD <- sd(diffs, na.rm=T)
  nD <- length(diffs)
  seD <- sD/sqrt(nD)
  xbarD <- mean(diffs, na.rm=T)
  dfD <- nD-1
  tstat <- model$statistic
  d <- cohens_d(data,formula,paired=T)$effsize
  tcrit <- qt(.975,nD-1)
  mod_det <- t_test(data,formula,paired=T,detailed=T)
  loCI <- mod_det$conf.low
  hiCI <- mod_det$conf.high
  pval <- pt(tstat,dfD,lower.tail=ifelse(tstat>=0,FALSE,TRUE))*2
  writeup_t <- 
    h3(paste0("Recall that the goal of this study was to determine whether 
           there were differences in ",outcome," between the ",estimate1,
           " and ",estimate2," conditions. To investigate this, we conducted 
           a dependent-samples t-test comparing ",predictors," means ",
           "between the ",estimate1," (M = ",txtRound(xbar1,2),", SD = 
           ",txtRound(sd1,2),")"," and ",estimate2," (M = ",
           txtRound(xbar2,2),", SD = ",txtRound(sd2,2),") conditions. 
           This analysis revealed a ",ifelse(pval<.05,"significant ",
                                             "non-significant "),
           "difference between the two means (t(",dfD,") = ",
           txtRound(tstat,2),", p ",
           ifelse(pval>=.05,paste0(" = ",txtRound(pval,3)),
                  ifelse(pval<.05 & pval>=.01," < .05",
                         ifelse(pval<.01 & pval>=.001," < .01"," < .001"))),
           ", 95% CI = [",txtRound(loCI,2),", ",
           txtRound(hiCI,2),"], d = ",txtRound(d,2),"), suggesting that ",
           predictors,ifelse(pval<.05," likely "," may not have greatly "),
           "affected ",outcome,"."))
  html_print(writeup_t)
}
