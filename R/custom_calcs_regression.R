#' The regression model method for the `calcs` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `regress` function in the `doingRbest` package.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return An HTML page with a series of solved equations (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Simple Regression Calculations
#' model <- regress(disp ~ mpg, data = mtcars)
#' calcs(model)
#' # Multiple Regression Calculations (limited with >2 predictors)
#' model <- regress(disp ~ mpg + wt, data = mtcars)
#' calcs(model)
#' # Moderated Regression Calculations 
#' model <- regress(disp ~ mpg*wt, data = mtcars)
#' calcs(model)

regression.calcs <- function(model = NULL,
                             predictors = NULL,
                             outcome= NULL) {
  tidy_model <- tidy(model)
  predictor_names <- attr(model$terms, "term.labels")
  outcome_name <- colnames(model$model)[1]
  if(length(predictors)==0) {
    predictors <- predictor_names
  }
  if(length(outcome)==0) {
    outcome <- outcome_name
  }
  n_predictors <- length(predictor_names)
  r_sq <- summary(model)$r.squared
  Fstat <- as.numeric(summary(model)$fstat[1])
  int <- coef(model)[1]
  slope1 <- coef(model)[2]
  se1 <- tidy_model$std.error[2]
  tstat1 <- tidy_model$statistic[2]
  amodel <- broom::tidy(stats::anova(model))
  df_pred <- n_predictors
  dfresid <- amodel$df[nrow(amodel)]
  SST <- sum(amodel$sumsq)
  SSR <- amodel$sumsq[nrow(amodel)]
  SSM <- SST-SSR
  MSR <- SSR/dfresid
  MSM <- SSM/n_predictors
  covxy <- cov(model$model[,2],model$model[,1],use="pairwise.complete.obs")
  varx <- var(model$model[,2],na.rm=T)
  ybar <- mean(model$model[,1],na.rm=T)
  xbar <- mean(model$model[,2],na.rm=T)
  tcrit <- qt(.975,dfresid)
  loCI1 <- confint(model)[2,1]  
  hiCI1 <- confint(model)[2,2]
  if(n_predictors==1) {
    calc_reg <- withMathJax(
      TeX("$$\\textbf{Calculations for a Simple Regression Analysis}$$"),
      sprintf("$$-----------------------------------------$$"),
      sprintf("$$\\hat{Y}_i=\\hat{b}_0+\\hat{b}_1X_i$$"),
      sprintf("$$\\widehat{%s}_i=%.02f%s%.02f(%s_i)$$",
              outcome,int,ifelse(slope1>=0,"+",""),slope1,predictors[1]),
      sprintf("$$-----------------------------------------$$"),
      "Slope:",
      sprintf("$$\\hat{b}_1=\\frac{cov_{XY}}{s^2_X}=
              \\frac{%.02f}{%.02f}=%.02f$$",
              covxy,varx,slope1),
      "Intercept:",
      sprintf("$$\\hat{b}_0=\\bar{Y}_i-\\hat{b}_1\\bar{X}_i=
              \\overline{%s}_i-\\hat{b}_1\\overline{%s}_i=
              %.02f-(%.02f)(%.02f)=%.02f$$",
              outcome,predictors[1],ybar,slope1,xbar,int),
      "Total Sum of Squares:",
      sprintf("$$SS_T=\\sum (Y_i-\\bar{Y})^2=
              \\sum (%s_i-\\overline{%s})^2=
              %.02f$$",
              outcome,outcome,SST),
      "Residual Sum of Squares:",
      sprintf("$$SS_R=\\sum (Y_i-\\widehat{Y}_i)^2=
              \\sum (%s_i-\\widehat{%s}_i)^2=
              %.02f$$",
              outcome,outcome,SSR),
      "Model Sum of Squares:",
      sprintf("$$SS_M=\\sum (\\hat{Y}_i-\\bar{Y}_i)^2=
              \\sum (\\widehat{%s}_i-\\overline{%s}_i)^2=
              SS_T-SS_R=%.02f-%.02f=%.02f$$",
              outcome,outcome,SST,SSR,SSM),
      "R-squared (Effect Size):",
      sprintf("$$R^2=\\frac{SS_M}{SS_T}=
              \\frac{%.02f}{%.02f}=%.02f$$",
              SSM,SST,r_sq),
      "F-statistic of Overall Model Fit:",
      sprintf("$$F=\\frac{MS_M}{MS_R}=
              \\frac{SS_M/df_M}{SS_R/df_R}=
              \\frac{%.02f/%.0f}{%.02f/%.0f}=
              \\frac{%.02f}{%.02f}=%.02f$$",
              SSM,df_pred,SSR,dfresid,MSM,MSR,Fstat),
      "Standard Error of Slope Coefficient:",
      sprintf("$$SE_{b_1}=\\frac{\\sqrt{MS_R}}{\\sqrt{\\sum (X_i-\\bar{X})^2}}=
              \\frac{\\sqrt{%.02f}}{\\sqrt{\\sum (%s_i-\\overline{%s})^2}}=
              \\frac{%.02f}{%.02f}=%.02f$$",
              MSR,predictors[1],predictors[1],sqrt(MSR),sqrt(sum((model$model[,2]-xbar)^2,na.rm=T)),se1),
      "T-Statistic for Slope Coefficient:",
      sprintf("$$t_{b_1}=\\frac{b_1-b^*_1}{SE_{b_1}}=
              \\frac{%.02f-0}{%.02f}=%.02f$$",
              slope1,se1,tstat1),
      "Degress of Freedom for Slope Coefficient:",
      sprintf("$$df=n-k-1=%.0f-%.0f-1=%.0f$$",
              nrow(model$model),n_predictors,dfresid),
      "95% Confidence Interval for Slope Coefficient:",
      sprintf("$$b^*_1=\\hat{b}_1\\pm(t_{crit})(SE_{b_1})=
              %.02f\\pm(%.02f)(%.02f)=[%.02f,%.02f]$$",
              slope1,tcrit,se1,loCI1,hiCI1)
    )
  } else if(n_predictors == 2) {
    slope2 <- coef(model)[3]
    se2 <- tidy_model$std.error[3]
    tstat2 <- tidy_model$statistic[3]
    varx2 <- var(model$model[,3],na.rm=T)
    loCI2 <- confint(model)[3,1]
    hiCI2 <- confint(model)[3,2]
    corx1x2 <- cor(model$model[,2],model$model[,3],use="pairwise.complete.obs")
    calc_reg <- withMathJax(
      TeX("$$\\textbf{Calculations for a 2-Predictor Multiple Regression Analysis}$$"),
      "*Note: Slopes and intercept are not conventionally computed by hand with >1 predictor",
      sprintf("$$-----------------------------------------$$"),
      sprintf("$$\\hat{Y}_i=\\hat{b}_0+\\hat{b}_1X_{1i}+\\hat{b}_2X_{2i}$$"),
      sprintf("$$\\widehat{%s}_i=%.02f%s%.02f(%s_i)%s%.02f(%s_i)$$",
              outcome,int,ifelse(slope1>=0,"+",""),slope1,predictors[1],ifelse(slope2>=0,"+",""),slope2,predictors[2]),
      sprintf("$$-----------------------------------------$$"),
  "Total Sum of Squares:",
  sprintf("$$SS_T=\\sum (Y_i-\\bar{Y})^2=
              \\sum (%s_i-\\overline{%s})^2=
              %.02f$$",
          outcome,outcome,SST),
  "Residual Sum of Squares:",
  sprintf("$$SS_R=\\sum (Y_i-\\widehat{Y}_i)^2=
              \\sum (%s_i-\\widehat{%s}_i)^2=
              %.02f$$",
          outcome,outcome,SSR),
  "Model Sum of Squares:",
  sprintf("$$SS_M=\\sum (\\hat{Y}_i-\\bar{Y}_i)^2=
              \\sum (\\widehat{%s}_i-\\overline{%s}_i)^2=
              SS_T-SS_R=%.02f-%.02f=%.02f$$",
          outcome,outcome,SST,SSR,SSM),
  "R-squared (Effect Size):",
  sprintf("$$R^2=\\frac{SS_M}{SS_T}=
              \\frac{%.02f}{%.02f}=%.02f$$",
          SSM,SST,r_sq),
  "F-statistic of Overall Model Fit:",
  sprintf("$$F=\\frac{MS_M}{MS_R}=
              \\frac{SS_M/df_M}{SS_R/df_R}=
              \\frac{%.02f/%.0f}{%.02f/%.0f}=
              \\frac{%.02f}{%.02f}=%.02f$$",
          SSM,df_pred,SSR,dfresid,MSM,MSR,Fstat),
  "Standard Errors of Slope Coefficients:",
  sprintf("$$SE_{b_1}=\\frac{\\sqrt{MS_R}}{s_{X_1}\\sqrt{n-1}\\sqrt{1-r^2_{X_1X_2}}}=
              \\frac{\\sqrt{%.02f}}{%.02f\\sqrt{%.0f-1}\\sqrt{1-%.02f}}=
              \\frac{%.02f}{%.02f}=%.02f$$",
          MSR,sqrt(varx),nrow(model$model),corx1x2^2,sqrt(MSR),sqrt(varx)*sqrt(nrow(model$model)-1)*sqrt(1-corx1x2^2),se1),
  sprintf("$$SE_{b_2}=\\frac{\\sqrt{MS_R}}{s_{X_2}\\sqrt{n-1}\\sqrt{1-r^2_{X_2X_1}}}=
              \\frac{\\sqrt{%.02f}}{%.02f\\sqrt{%.0f-1}\\sqrt{1-%.02f}}=
              \\frac{%.02f}{%.02f}=%.02f$$",
          MSR,sqrt(varx2),nrow(model$model),corx1x2^2,sqrt(MSR),sqrt(varx2)*sqrt(nrow(model$model)-1)*sqrt(1-corx1x2^2),se2),
  "T-Statistics for Slope Coefficients:",
  sprintf("$$t_{b_1}=\\frac{b_1-b^*_1}{SE_{b_1}}=
              \\frac{%.02f-0}{%.02f}=%.02f$$",
          slope1,se1,tstat1),
  sprintf("$$t_{b_2}=\\frac{b_2-b^*_2}{SE_{b_2}}=
          \\frac{%.02f-0}{%.02f}=%.02f$$",
          slope2,se2,tstat2),
          "Degress of Freedom for Slope Coefficients:",
          sprintf("$$df=n-k-1=%.0f-%.0f-1=%.0f$$",
                  nrow(model$model),n_predictors,dfresid),
          "95% Confidence Intervals for Slope Coefficients:",
          sprintf("$$b^*_1=\\hat{b}_1\\pm(t_{crit})(SE_{b_1})=
              %.02f\\pm(%.02f)(%.02f)=[%.02f,%.02f]$$",
                  slope1,tcrit,se1,loCI1,hiCI1),
          sprintf("$$b^*_2=\\hat{b}_2\\pm(t_{crit})(SE_{b_2})=
                  %.02f\\pm(%.02f)(%.02f)=[%.02f,%.02f]$$",
                  slope2,tcrit,se2,loCI2,hiCI2)
      )
  } else if(n_predictors > 2) {
    calc_reg <- withMathJax(
      TeX("$$\\textbf{Calculations for a Multiple Regression Analysis with 3 or more Predictors}$$"),
      "*Note: Slopes, intercept, standard errors, t-statistics, and confidence intervals are not conventionally computed by hand with >2 predictors",
      sprintf("$$-----------------------------------------$$"),
      if(n_predictors == 3) {
        slope2 <- coef(model)[3]
        slope3 <- coef(model)[4]
        withMathJax(
          sprintf("$$\\hat{Y}_i=\\hat{b}_0+\\hat{b}_1X_{1i}+\\hat{b}_2X_{2i}+\\hat{b}_3X_{3i}$$"),
          sprintf("$$\\widehat{%s}_i=%.02f%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)$$",
                  outcome,int,
                  ifelse(slope1>=0,"+",""),slope1,predictors[1],
                  ifelse(slope2>=0,"+",""),slope2,predictors[2],
                  ifelse(slope3>=0,"+",""),slope3,predictors[3])
        )
      },
      if(n_predictors == 4) {
        slope2 <- coef(model)[3]
        slope3 <- coef(model)[4]
        slope4 <- coef(model)[5]
        withMathJax(
          sprintf("$$\\hat{Y}_i=\\hat{b}_0+\\hat{b}_1X_{1i}+\\hat{b}_2X_{2i}+\\hat{b}_3X_{3i}+\\hat{b}_4X_{4i}$$"),
          sprintf("$$\\widehat{%s}_i=%.02f%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)$$",
                  outcome,int,
                  ifelse(slope1>=0,"+",""),slope1,predictors[1],
                  ifelse(slope2>=0,"+",""),slope2,predictors[2],
                  ifelse(slope3>=0,"+",""),slope3,predictors[3],
                  ifelse(slope4>=0,"+",""),slope4,predictors[4])
        )
      },
      if(n_predictors == 5) {
        slope2 <- coef(model)[3]
        slope3 <- coef(model)[4]
        slope4 <- coef(model)[5]
        slope5 <- coef(model)[6]
        withMathJax(
          sprintf("$$\\hat{Y}_i=\\hat{b}_0+\\hat{b}_1X_{1i}+\\hat{b}_2X_{2i}+\\hat{b}_3X_{3i}+\\hat{b}_4X_{4i}+\\hat{b}_5X_{5i}$$"),
          sprintf("$$\\widehat{%s}_i=%.02f%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)$$",
                  outcome,int,
                  ifelse(slope1>=0,"+",""),slope1,predictors[1],
                  ifelse(slope2>=0,"+",""),slope2,predictors[2],
                  ifelse(slope3>=0,"+",""),slope3,predictors[3],
                  ifelse(slope4>=0,"+",""),slope4,predictors[4],
                  ifelse(slope5>=0,"+",""),slope5,predictors[5])
        )
      },
      if(n_predictors == 6) {
        slope2 <- coef(model)[3]
        slope3 <- coef(model)[4]
        slope4 <- coef(model)[5]
        slope5 <- coef(model)[6]
        slope6 <- coef(model)[7]
        withMathJax(
          sprintf("$$\\hat{Y}_i=\\hat{b}_0+\\hat{b}_1X_{1i}+\\hat{b}_2X_{2i}+\\hat{b}_3X_{3i}+\\hat{b}_4X_{4i}+\\hat{b}_5X_{5i}+\\hat{b}_6X_{6i}$$"),
          sprintf("$$\\widehat{%s}_i=%.02f%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)$$",
                  outcome,int,
                  ifelse(slope1>=0,"+",""),slope1,predictors[1],
                  ifelse(slope2>=0,"+",""),slope2,predictors[2],
                  ifelse(slope3>=0,"+",""),slope3,predictors[3],
                  ifelse(slope4>=0,"+",""),slope4,predictors[4],
                  ifelse(slope5>=0,"+",""),slope5,predictors[5],
                  ifelse(slope6>=0,"+",""),slope6,predictors[6])
        )
      },
      if(n_predictors > 6) {
        slope2 <- coef(model)[3]
        slope3 <- coef(model)[4]
        slope4 <- coef(model)[5]
        slope5 <- coef(model)[6]
        slope6 <- coef(model)[7]
        slope_last <- coef(model)[length(coef(model))]
        withMathJax(
          sprintf("$$\\hat{Y}_i=\\hat{b}_0+\\hat{b}_1X_{1i}+\\hat{b}_2X_{2i}+\\hat{b}_3X_{3i}+\\hat{b}_4X_{4i}+\\hat{b}_5X_{5i}+\\hat{b}_6X_{6i}+...+\\hat{b}_nX_{ni}$$"),
          sprintf("$$\\widehat{%s}_i=%.02f%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)%s%.02f(%s_i)+...%s%.02f(%s_i)$$",
                  outcome,int,
                  ifelse(slope1>=0,"+",""),slope1,predictors[1],
                  ifelse(slope2>=0,"+",""),slope2,predictors[2],
                  ifelse(slope3>=0,"+",""),slope3,predictors[3],
                  ifelse(slope4>=0,"+",""),slope4,predictors[4],
                  ifelse(slope5>=0,"+",""),slope5,predictors[5],
                  ifelse(slope6>=0,"+",""),slope6,predictors[6],
                  ifelse(slope_last>0,"+",""),slope_last,predictors[n_predictors])
        )
      },
      sprintf("$$-----------------------------------------$$"),
      "Total Sum of Squares:",
      sprintf("$$SS_T=\\sum (Y_i-\\bar{Y})^2=
              \\sum (%s_i-\\overline{%s})^2=
              %.02f$$",
              outcome,outcome,SST),
      "Residual Sum of Squares:",
      sprintf("$$SS_R=\\sum (Y_i-\\widehat{Y}_i)^2=
              \\sum (%s_i-\\widehat{%s}_i)^2=
              %.02f$$",
              outcome,outcome,SSR),
      "Model Sum of Squares:",
      sprintf("$$SS_M=\\sum (\\hat{Y}_i-\\bar{Y}_i)^2=
              \\sum (\\widehat{%s}_i-\\overline{%s}_i)^2=
              SS_T-SS_R=%.02f-%.02f=%.02f$$",
              outcome,outcome,SST,SSR,SSM),
      "R-squared (Effect Size):",
      sprintf("$$R^2=\\frac{SS_M}{SS_T}=
              \\frac{%.02f}{%.02f}=%.02f$$",
              SSM,SST,r_sq),
      "F-statistic of Overall Model Fit:",
      sprintf("$$F=\\frac{MS_M}{MS_R}=
              \\frac{SS_M/df_M}{SS_R/df_R}=
              \\frac{%.02f/%.0f}{%.02f/%.0f}=
              \\frac{%.02f}{%.02f}=%.02f$$",
              SSM,df_pred,SSR,dfresid,MSM,MSR,Fstat)
    )
  }      
  html_print(calc_reg)
}