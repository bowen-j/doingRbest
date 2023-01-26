#' The regression model method for the `hyp` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `regress` function in the `doingRbest` package.
#' 
#' @return An HTML page with a series of null and alternative hypotheses (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Simple Regression Hypothesis Test
#' model <- regress(disp ~ mpg, data = mtcars)
#' hyp(model)
#' # Multiple Regression Hypothesis Tests
#' model <- regress(disp ~ mpg + wt, data = mtcars)
#' hyp(model)
#' # Moderated Regression Hypothesis Tests
#' model <- regress(disp ~ mpg*wt, data = mtcars)
#' hyp(model)

regression.hyp <- function(model = NULL,
                           ...) {
  tidy_model <- tidy(model)
  n_predictors <- ncol(model$model)-1
  r_sq <- summary(model)$r.squared
  Fstat <- as.numeric(summary(model)$fstat[1])
  slope1 <- coef(model)[2]
  se1 <- tidy_model$std.error[2]
  tstat1 <- tidy_model$statistic[2]
  df_pred <- n_predictors
  amodel <- tidy(aov(model))
  dfresid <- amodel$df[nrow(amodel)]
  SST <- sum(amodel$sumsq)
  SSR <- amodel$sumsq[nrow(amodel)]
  SSM <- SST-SSR
  MSR <- SSR/dfresid
  MSM <- SSM/n_predictors
  tcrit <- qt(.975, dfresid)
  Fcrit <- qf(.95,df_pred,dfresid)
  pval_F <- pf(Fstat,df_pred,dfresid,lower=F)
  pval_t1 <- pt(tstat1,dfresid,lower.tail=ifelse(tstat1>=0,FALSE,TRUE))*2
  if(n_predictors > 1) {
    slope2 <- coef(model)[3]
    se2 <- tidy_model$std.error[3]
    tstat2 <- tidy_model$statistic[3]
    pval_t2 <- pt(tstat2,dfresid,lower.tail=ifelse(tstat2>=0,FALSE,TRUE))*2
  }
  if(n_predictors > 2) {
    slope3 <- coef(model)[4]
    se3 <- tidy_model$std.error[4]
    tstat3 <- tidy_model$statistic[4]
    pval_t3 <- pt(tstat3,dfresid,lower.tail=ifelse(tstat3>=0,FALSE,TRUE))*2
  }
  if(n_predictors > 3) {
    slope4 <- coef(model)[5]
    se4 <- tidy_model$std.error[5]
    tstat4 <- tidy_model$statistic[5]
    pval_t4 <- pt(tstat4,dfresid,lower.tail=ifelse(tstat4>=0,FALSE,TRUE))*2
  }
  if(n_predictors > 4) {
    slope5 <- coef(model)[6]
    se5 <- tidy_model$std.error[6]
    tstat5 <- tidy_model$statistic[6]
    pval_t5 <- pt(tstat5,dfresid,lower.tail=ifelse(tstat5>=0,FALSE,TRUE))*2
  }
  if(n_predictors > 5) {
    slope6 <- coef(model)[7]
    se6 <- tidy_model$std.error[7]
    tstat6 <- tidy_model$statistic[7]
    pval_t6 <- pt(tstat6,dfresid,lower.tail=ifelse(tstat6>=0,FALSE,TRUE))*2
  }
  if(n_predictors > 6) {
    slope7 <- coef(model)[8]
    se7 <- tidy_model$std.error[8]
    tstat7 <- tidy_model$statistic[8]
    pval_t7 <- pt(tstat7,dfresid,lower.tail=ifelse(tstat7>=0,FALSE,TRUE))*2
  }
  if(n_predictors > 7) {
    slope8 <- coef(model)[9]
    se8 <- tidy_model$std.error[9]
    tstat8 <- tidy_model$statistic[9]
    pval_t8 <- pt(tstat8,dfresid,lower.tail=ifelse(tstat8>=0,FALSE,TRUE))*2
  }
  if(n_predictors > 8) {
    slope9 <- coef(model)[10]
    se9 <- tidy_model$std.error[10]
    tstat9 <- tidy_model$statistic[10]
    pval_t9 <- pt(tstat9,dfresid,lower.tail=ifelse(tstat9>=0,FALSE,TRUE))*2
  }
  if(n_predictors == 10) {
    slope10 <- coef(model)[11]
    se10 <- tidy_model$std.error[11]
    tstat10 <- tidy_model$statistic[11]
    pval_t10 <- pt(tstat10,dfresid,lower.tail=ifelse(tstat10>=0,FALSE,TRUE))*2
  }
  hyp_reg <- withMathJax(
    TeX("$$\\textbf{Hypothesis Tests for a Regression Analysis } (\\alpha = .05)$$"),
    sprintf("$$-----------------------------------------$$"),
    TeX("$$\\underline{\\textrm{Full Model Hypothesis Test}}$$"),
    "Statistical Hypotheses:",
    sprintf(
      "$$H_0:R^2=0 \\qquad H_1:R^2>0$$"
    ),
    "Test Statistic:",
    sprintf(
      "$$F=\\frac{MS_M}{MS_R}=\\frac{%.02f}{%.02f}=%.02f$$",
      MSM,MSR,Fstat
    ),
    "Alternate Formula for Test Statistic:",
    sprintf(
      "$$F=\\frac{R^2/k}{(1-R^2)/(n-k-1)}=\\frac{%.02f/%.0f}{%.02f/%.0f}=%.02f$$",
      r_sq,n_predictors,1-r_sq,dfresid,Fstat
    ),
    "Critical Value for F-statistic:",
    sprintf(
      "$$F_{crit}=\\texttt{qf(p = .95, df1 = %.0f, df2 = %.0f, lower = F)}=%.02f$$",
      df_pred,dfresid,Fcrit
    ),
    "P-value for F-test:",
    sprintf(
      "$$p=\\texttt{pf(q = %.02f, df1 = %.0f, df2 = %.0f, lower = F)}=%.03f$$",
      Fstat,df_pred,dfresid,pval_F
    ),
    "Decision for F-test:",
    sprintf(
      "$$F%sF_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
      ifelse(Fstat<Fcrit,"<",">"),ifelse(pval_F<.05,"<",">"),ifelse(pval_F<.05,"Reject ","Fail\\ to\\ reject ")
    ),
    sprintf("$$-----------------------------------------$$"),
    TeX("$$\\underline{\\textrm{Hypothesis Test(s) for Slopes}}$$"),
    "Statistical Hypotheses:",
    sprintf(
      "$$H_0:b_i^*=0 \\qquad H_1:b_i^*\\ne0$$"
    ),
    "Test Statistic(s):",
    sprintf(
      "$$t_{b_1}=\\frac{b_1-b_1^*}{SE_{b_1}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
      slope1,se1,tstat1
    ),
    if(n_predictors > 1) {
      sprintf(
        "$$t_{b_2}=\\frac{b_2-b_2^*}{SE_{b_2}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope2,se2,tstat2
      )
    },
    if(n_predictors > 2) {
      sprintf(
        "$$t_{b_3}=\\frac{b_3-b_3^*}{SE_{b_3}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope3,se3,tstat3
      )
    },
    if(n_predictors > 3) {
      sprintf(
        "$$t_{b_4}=\\frac{b_4-b_4^*}{SE_{b_4}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope4,se4,tstat4
      )
    },
    if(n_predictors > 4) {
      sprintf(
        "$$t_{b_5}=\\frac{b_5-b_5^*}{SE_{b_5}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope5,se5,tstat5
      )
    },
    if(n_predictors > 5) {
      sprintf(
        "$$t_{b_6}=\\frac{b_6-b_6^*}{SE_{b_6}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope6,se6,tstat6
      )
    },
    if(n_predictors > 6) {
      sprintf(
        "$$t_{b_7}=\\frac{b_7-b_7^*}{SE_{b_7}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope7,se7,tstat7
      )
    },
    if(n_predictors > 7) {
      sprintf(
        "$$t_{b_8}=\\frac{b_8-b_8^*}{SE_{b_8}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope8,se8,tstat8
      )
    },
    if(n_predictors > 8) {
      sprintf(
        "$$t_{b_9}=\\frac{b_9-b_9^*}{SE_{b_9}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope9,se9,tstat9
      )
    },
    if(n_predictors == 10) {
      sprintf(
        "$$t_{b_10}=\\frac{b_10-b_10^*}{SE_{b_10}}=\\frac{%.02f-0}{%.02f}=%.02f$$",
        slope10,se10,tstat10
      )
    },
    "Critical Value for t-statistic(s):",
    sprintf(
      "$$t_{crit}=\\pm\\ \\texttt{qt(p = .975, df = %.0f)}=\\pm%.02f$$",
      dfresid,tcrit
    ),
    "P-value(s) for t-test(s):",
    sprintf(
      "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
      tstat1,dfresid,ifelse(tstat1>=0,"F","T"),pval_t1
    ),
    if(n_predictors > 1) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat2,dfresid,ifelse(tstat2>=0,"F","T"),pval_t2
      )
    },
    if(n_predictors > 2) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat3,dfresid,ifelse(tstat3>=0,"F","T"),pval_t3
      )
    },
    if(n_predictors > 3) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat4,dfresid,ifelse(tstat4>=0,"F","T"),pval_t4
      )
    },
    if(n_predictors > 4) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat5,dfresid,ifelse(tstat5>=0,"F","T"),pval_t5
      )
    },
    if(n_predictors > 5) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat6,dfresid,ifelse(tstat6>=0,"F","T"),pval_t6
      )
    },
    if(n_predictors > 6) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat7,dfresid,ifelse(tstat7>=0,"F","T"),pval_t7
      )
    },
    if(n_predictors > 7) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat8,dfresid,ifelse(tstat8>=0,"F","T"),pval_t8
      )
    },
    if(n_predictors > 8) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat9,dfresid,ifelse(tstat9>=0,"F","T"),pval_t9
      )
    },
    if(n_predictors == 10) {
      sprintf(
        "$$p=\\texttt{pt(q = %.02f, df = %.0f, lower = %s) * 2}=%.03f$$",
        tstat10,dfresid,ifelse(tstat10>=0,"F","T"),pval_t10
      )
    },
    "Decision(s):",
    sprintf(
      "$$t_{b_1}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
      ifelse(tstat1<tcrit,"<",">"),ifelse(pval_t1<.05,"<",">"),ifelse(pval_t1<.05,"Reject ","Fail\\ to\\ reject ")
    ),
    if(n_predictors > 1) {
      sprintf(
        "$$t_{b_2}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat2<tcrit,"<",">"),ifelse(pval_t2<.05,"<",">"),ifelse(pval_t2<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    },
    if(n_predictors > 2) {
      sprintf(
        "$$t_{b_3}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat3<tcrit,"<",">"),ifelse(pval_t3<.05,"<",">"),ifelse(pval_t3<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    },
    if(n_predictors > 3) {
      sprintf(
        "$$t_{b_4}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat4<tcrit,"<",">"),ifelse(pval_t4<.05,"<",">"),ifelse(pval_t4<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    },
    if(n_predictors > 4) {
      sprintf(
        "$$t_{b_5}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat5<tcrit,"<",">"),ifelse(pval_t5<.05,"<",">"),ifelse(pval_t5<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    },
    if(n_predictors > 5) {
      sprintf(
        "$$t_{b_6}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat6<tcrit,"<",">"),ifelse(pval_t6<.05,"<",">"),ifelse(pval_t6<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    },
    if(n_predictors > 6) {
      sprintf(
        "$$t_{b_7}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat7<tcrit,"<",">"),ifelse(pval_t7<.05,"<",">"),ifelse(pval_t7<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    },
    if(n_predictors > 7) {
      sprintf(
        "$$t_{b_8}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat8<tcrit,"<",">"),ifelse(pval_t8<.05,"<",">"),ifelse(pval_t8<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    },
    if(n_predictors > 8) {
      sprintf(
        "$$t_{b_9}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat9<tcrit,"<",">"),ifelse(pval_t9<.05,"<",">"),ifelse(pval_t9<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    },
    if(n_predictors == 10) {
      sprintf(
        "$$t_{b_10}%st_{crit},\\ p%s\\alpha,\\ %s\\ H_0!$$",
        ifelse(tstat10<tcrit,"<",">"),ifelse(pval_t10<.05,"<",">"),ifelse(pval_t10<.05,"Reject ","Fail\\ to\\ reject ")
      )  
    }
  )
  html_print(hyp_reg)
}