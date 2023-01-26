#' A generic function used to create a table of analysis-specific output for common statistical models
#' 
#' @param model The stored model object created from a function in the doingRbest package to run a statistical model.
#' @param interpretation Set equal to `TRUE` for an explanatory paragraph beneath the output table interpreting the numeric values.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param sim_slopes For moderated regression or factorial ANOVA output, set equal to `TRUE` for a simple slopes/simple effects analysis in place of a traditional output table for the analysis.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return An output table as an HTML object (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples 
#' # Simple Regression Output Table
#' model <- regress(disp ~ mpg, data = mtcars)
#' output(model)

regression.output <- function(model          = NULL,
                              predictors     = NULL,
                              outcome        = NULL,
                              interpretation = FALSE,
                              ...) {
  # Defining model and variables
  tidy_model <- tidy(model)
  predictor_names <- attr(model$terms, "term.labels")
  outcome_name <- colnames(model$model)[1]
  if(length(predictors) == 0) {
    predictors <- predictor_names
  }
  if(length(outcome) == 0) {
    outcome <- outcome_name
  }
  # Calculating relevant model values
  n_predictors <- as.numeric(length(predictor_names))
  r_sq <- summary(model)$r.squared
  Fstat <- as.numeric(summary(model)$fstat[1])
  # Defining math script terms for table
  math_p <- huxtable('<em>p</em>')
  math_t <- huxtable('<em>t</em>')
  math_b <- huxtable('<em>b</em>')
  math_SE <- huxtable('<em>SE</em>')
  math_Rsq <- huxtable('<em>R<sup>2</sup></em>')
  math_df <- huxtable('<em>df</em>')
  math_F <- huxtable('<em>F</em>')
  math_beta <- huxtable('<em>&#946</em>')
  # Relabeling table values part 1
  tidy_model[1,1] <- "Intercept"
  tidy_model[,2:4] <- round(tidy_model[,2:4],2)
  tidy_model[,5] <- txtRound(tidy_model[,5], digits = 3)
  for(i in 2:(n_predictors+1)) {
    tidy_model[i,1] <- paste0("Slope: ", predictors[i-1])
    if(tidy_model[i,5] == "0.000") {
      tidy_model[i,5] <- "< .001"
    } else {
      tidy_model[i,5] <- tidy_model[i,5]
    }
  }
  for(i in 2:ncol(model$model)) {
    if(class(model$model[,i]) == "factor") {
      tidy_model[i,1] <- paste0("Slope: ",
                                predictors[i-1],
                                " (0 = '",
                                levels(model$model[,i])[1],
                                "', 1 = '",
                                levels(model$model[,i])[2],
                                "')")
    }
  }
  tidy_model[1,3:5] <- NA
  # Computing CI, p, and df for table
  loCI <- confint(model)[2:(ncol(model$model)),1]
  hiCI <- confint(model)[2:(ncol(model$model)),2]
  CI <- paste0("[",txtRound(loCI,2),", ",txtRound(hiCI,2),"]")
  tidy_model[2:(ncol(model$model)),6] <- CI
  tidy_aov <- tidy(aov(model))
  pval <- tidy_aov[1,6]
  df_num <- ncol(model$model)-1
  df_denom <- as.numeric(tidy_aov$df[ncol(model$model)])
  tidy_model[2:(ncol(model$model)),7] <- df_denom
  # First reordering of table columns
  tidy_model <- tidy_model[,c(1,2,3,6,7,4,5)]
  # Rounding off and adding more table values
  tidy_model[,3] <- txtRound(tidy_model[,3],digits=2)
  tidy_model[,2] <- txtRound(tidy_model[,2],digits=2)
  tidy_model[,6] <- txtRound(tidy_model[,6],digits=2)
  tidy_model[-1,8] <- txtRound(as.numeric(coef(lm.beta::lm.beta(model)))[-1],digits=2)
  # Final reordering of table columns
  tidy_model <- tidy_model[,c(1,2,3,8,4,5,6,7)]
  # Relabeling column names
  colnames(tidy_model) <- c(paste0("Outcome: ",outcome),math_b,math_SE,
                            math_beta,"95% CI",math_df,math_t,math_p)
  # Finalizing table format
  interp <- regression.output.interp(model, predictors, outcome)
  output_table <- if(length(predictors)<length(predictor_names)) {
    "Error:  no output table was created --> If you supply any custom predictor names, you must provide as many names as there are predictor variables!"
  } else {tidy_model %>%
      addHtmlTableStyle(pos.caption = "bottom",
                        align = c('l',rep('c',7)),
                        css.cell = "padding: 5px;") %>%
      htmlTable(rownames = F,
                cgroup = list(c(paste0(ifelse(n_predictors == 1,
                                              "Simple Regression Analysis: ",
                                              "Multiple Regression Analysis: "),
                                       math_Rsq," = ",txtRound(r_sq,digits=2),", ",
                                       math_F,"(",df_num,", ",df_denom,") = ",txtRound(Fstat,2),", ",
                                       math_p,ifelse(pval<.001," < .001",txtRound(pval,digits=3))))),
                n.cgroup = list(c(8)),
                options = list(dom = 't'),
                caption = ifelse(interpretation == TRUE,
                                 paste0("Interpretation: ",interp),
                                 paste0("")))
  }
  output_table
}

regression.output.interp <- function(model      = NULL,
                                     predictors = c(),
                                     outcome    = NULL,
                                     ...) {
  # Computing values
  b <- coef(model)
  b_pval <- tidy(model)$p.value
  predictors <- tolower(predictors)
  outcome <- tolower(outcome)
  r_sq <- summary(model)$r.squared
  tidy_model <- tidy(model)
  n_predictors <- length(tidy_model$term[-1])
  tidy_aov <- tidy(aov(model))
  pval <- tidy_aov[1,6]
  df_num <- ncol(model$model)-1
  df_denom <- as.numeric(tidy_aov$df[ncol(model$model)])
  tstat <- as.numeric(summary(model)$coefficients[,3])
  Fstat <- round(as.numeric(summary(model)$fstatistic[1]),2)
  loCI <- as.numeric(confint(model)[,1])
  hiCI <- as.numeric(confint(model)[,2])
  # Defining math script terms
  math_F <- huxtable('<em>F</em>')
  math_p <- huxtable('<em>p</em>')
  math_Rsq <- huxtable('<em>R<sup>2</sup></em>')
  math_t <- huxtable('<em>t</em>')
  math_b <- huxtable('<em>b</em>')
  math_SE <- huxtable('<em>SE</em>')
  # Pasting together interpretation text
  interp <- paste0("According to this regression model predicting ",
                   outcome, " from ",
                   if(n_predictors==1) {
                     predictors[1]
                   } else if(n_predictors==2) {
                     paste0(predictors, collapse = " and ")
                   } else {
                     paste0(paste0(predictors[-n_predictors], collapse = ", "),
                            ", and ", predictors[n_predictors])
                   },
                   ", when ",
                   if(n_predictors==1) {
                     paste0(predictors[1]," is ")
                   } else if(n_predictors==2) {
                     paste0(paste0(predictors, collapse = " and "), " are ")
                   } else {
                     "all predictors are "
                   },
                   "equal to 0, ",
                   outcome, " should be equal to ",txtRound(b[1],digits=2),
                   ".",
                   if(n_predictors==1) {
                     paste0(" For every 1-unit increase in ",
                            predictors[1],", there should be a corresponding ",
                            txtRound(abs(b[2]),digits=2),"-unit ",
                            ifelse(b[2]>=0,"increase ","decrease "),"in ",
                            outcome,". The model accounted for ",
                            txtRound(r_sq*100,2),"% of the variance in ",outcome,
                            " (",math_Rsq," = ",txtRound(r_sq,2),"). This degree
                           of model fit was ",ifelse(pval<.05,
                                                     "statistically significant (",
                                                     "not statistically significant ("),
                            math_F,"(",df_num,", ",df_denom,") = ",
                            txtRound(Fstat,2),", ",math_p,ifelse(pval>=.001,paste0(" = ",txtRound(pval,digits=2)),
                                                                 " < .001"),"). Furthermore, the
                           coefficient for the slope of the relationship between ",predictors[1],
                            " and ",outcome," was ",ifelse(pval<.05,"statistically significant (",
                                                           "not statistically significant ("),
                            math_t,"(",df_denom,") = ",txtRound(tstat,2),", ",math_p,ifelse(pval>=.001,
                                                                                            paste0(" = ",txtRound(pval,digits=2))," < .001"),").
                           The 95% confidence interval for this slope coefficient ranged from ",txtRound(loCI[2],2)," to ",txtRound(hiCI[2],2),".")[1]
                   } else if(n_predictors==2) {
                     paste0(" For every 1-unit increase in ",
                            predictors[1],", controlling for ",
                            predictors[2],", there should be a corresponding ",
                            txtRound(abs(b[2]),digits=2),"-unit ",
                            ifelse(b[2]>=0,"increase ","decrease "),"in ",
                            outcome,". For every 1-unit increase in ",
                            predictors[2],", controlling for ",
                            predictors[1],", there should be a corresponding ",
                            txtRound(abs(b[3]),digits=2),"-unit ",
                            ifelse(b[3]>=0,"increase ","decrease "),"in ",
                            outcome,". The model accounted for ",
                            txtRound(r_sq*100,2),"% of the variance in ",outcome,
                            " (",math_Rsq," = ",txtRound(r_sq,2),"). This degree
                           of model fit was ",ifelse(pval<.05,
                                                     "statistically significant (",
                                                     "not statistically significant ("),
                            math_F,"(",df_num,", ",df_denom,") = ",
                            txtRound(Fstat,2),", ",math_p,ifelse(pval>=.001,paste0(" = ",txtRound(pval,digits=2)),
                                                                 " < .001"),"). The
                           coefficient for the partial slope of the relationship between ",predictors[1],
                            " and ",outcome,", controlling for ",predictors[2],", was ",ifelse(b_pval[2]<.05,"statistically significant (",
                                                                                               "not statistically significant ("),
                            math_t,"(",df_denom,") = ",txtRound(tstat[2],2),", ",math_p,ifelse(b_pval[2]>=.001,
                                                                                               paste0(" = ",txtRound(b_pval[2],2))," < .001"),").
                           The 95% confidence interval for this slope coefficient ranged from ",txtRound(loCI[2],2)," to ",txtRound(hiCI[2],2),".
                           The coefficient for the partial slope of the relationship between ",predictors[2],
                            " and ",outcome,", controlling for ",predictors[1],", was ",ifelse(b_pval[3]<.05,"statistically significant (",
                                                                                               "not statistically significant ("),
                            math_t,"(",df_denom,") = ",txtRound(tstat[3],2),", ",math_p,ifelse(b_pval[3]>=.001,
                                                                                               paste0(" = ",txtRound(b_pval[3],2))," < .001"),").
                           The 95% confidence interval for this slope coefficient ranged from ",txtRound(loCI[3],2)," to ",txtRound(hiCI[3],2),".")
                   } else {
                     paste0(
                       str_flatten(paste0(" For every 1-unit increase in ",
                                          predictors,", controlling for all other predictors,
                             there should be a corresponding ",
                                          txtRound(abs(b[-1]),digits=2),"-unit ",
                                          ifelse(b[-1]>=0,"increase ","decrease "),"in ",
                                          outcome,".")), " The model accounted for ",
                       txtRound(r_sq*100,2),"% of the variance in ",outcome,
                       " (",math_Rsq," = ",txtRound(r_sq,2),"). This degree
                           of model fit was ",ifelse(pval<.05,
                                                     "statistically significant (",
                                                     "not statistically significant ("),
                       math_F,"(",df_num,", ",df_denom,") = ",
                       txtRound(Fstat,2),", ",math_p,ifelse(pval>=.001,paste0(" = ",txtRound(pval,digits=2)),
                                                            " < .001"),").",
                       str_flatten(paste0(" The
                           coefficient for the partial slope of the relationship between ",predictors,
                                          " and ",outcome,", controlling for all other predictors, was ",ifelse(b_pval[-1]<.05,"statistically significant (",
                                                                                                                "not statistically significant ("),
                                          math_t,"(",df_denom,") = ",txtRound(tstat[-1],2),", ",math_p,ifelse(b_pval[-1]>=.001,
                                                                                                              paste0(" = ",txtRound(b_pval[-1],2))," < .001"),").
                           The 95% confidence interval for this slope coefficient ranged from ",txtRound(loCI[-1],2)," to ",txtRound(hiCI[-1],2),".")))
                   }
  )
  interp
}

regression.interaction.output <- function(model          = NULL,
                                          predictors     = NULL,
                                          outcome        = NULL,
                                          interpretation = FALSE,
                                          ...) {
  tidy_model <- tidy(model)
  predictor_names <- attr(model$terms,'term.labels')
  outcome_name <- colnames(model$model)[1]
  if(length(predictors) == 0) {
    predictors <- predictor_names
  }
  if(length(outcome) == 0) {
    outcome <- outcome_name
  }
  if(length(predictors) == 2) {
    predictors[3] <- paste0(predictors[1], " x ",predictors[2])
  }
  n_predictors <- length(predictor_names)
  if(n_predictors != 3) {
    output_table <- "Sorry, right now this function can only handle 2 predictors and their interaction for a moderated regression model."
  }
  r_sq <- summary(model)$r.squared
  Fstat <- as.numeric(summary(model)$fstat[1])
  math_p <- huxtable('<em>p</em>')
  math_t <- huxtable('<em>t</em>')
  math_b <- huxtable('<em>b</em>')
  math_SE <- huxtable('<em>SE</em>')
  math_Rsq <- huxtable('<em>R<sup>2</sup></em>')
  math_df <- huxtable('<em>df</em>')
  math_F <- huxtable('<em>F</em>')
  math_beta <- huxtable('<em>&#946</em>')
  tidy_model[1,1] <- "Intercept"
  tidy_model[,2:4] <- round(tidy_model[,2:4],2)
  tidy_model[,5] <- txtRound(tidy_model[,5], digits=3)
  for(i in 2:(n_predictors+1)) {
    if(grepl(":",predictors[i-1])==TRUE) {
      tidy_model[i,1] <- paste0("Interaction: ",sub(":.*","",predictors[i-1])," x ",sub(".*:","",predictors[i-1]))
    } else {
      tidy_model[i,1] <- paste0("Slope: ",predictors[i-1])
    }
    if(tidy_model[i,5] == "0.000") {
      tidy_model[i,5] <- "< .001"
    } else {
      tidy_model[i,5] <- tidy_model[i,5]
    }
  }
  for(i in 2:ncol(model$model)) {
    if(class(model$model[,i]) == "factor") {
      tidy_model[i,1] <- paste0("Slope: ",
                                predictors[i-1],
                                " (0 = '",
                                levels(model$model[,i])[1],
                                "', 1 = '",
                                levels(model$model[,i])[2],
                                "')")
    }
  }
  tidy_model[1,3:5] <- NA
  loCI <- confint(model)[2:(n_predictors+1),1]
  hiCI <- confint(model)[2:(n_predictors+1),2]
  CI <- paste0("[",txtRound(loCI,2),", ",txtRound(hiCI,2),"]")
  tidy_model[2:(n_predictors+1),6] <- CI
  tidy_aov <- tidy(aov(model))
  pval <- tidy_aov[1,6]
  df_num <- as.numeric(summary(model)$fstat[2])
  df_denom <- as.numeric(summary(model)$fstat[3])
  tidy_model[2:(n_predictors+1),7] <- df_denom
  tidy_model <- tidy_model[,c(1,2,3,6,7,4,5)]
  tidy_model[,3] <- txtRound(tidy_model[,3],digits=2)
  tidy_model[,2] <- txtRound(tidy_model[,2],digits=2)
  tidy_model[,6] <- txtRound(tidy_model[,6],digits=2)
  tidy_model[-1,8] <- txtRound(as.numeric(coef(lm.beta::lm.beta(model)))[-1],digits=2)
  tidy_model <- tidy_model[,c(1,2,3,8,4,5,6,7)]
  colnames(tidy_model) <- c(paste0("Outcome: ",outcome),math_b,math_SE,
                            math_beta,"95% CI",math_df,math_t,math_p)
  interp <- regression.output.interaction.interp(model, predictors, outcome)
  output_table <- if(length(predictors)<length(predictor_names)) {
    "Error:  no output table was created --> if you supply any custom predictor names, you must provide as many names as there are predictor variables!"
  } else {
    tidy_model %>%
      addHtmlTableStyle(pos.caption = "bottom",
                        align = c('l',rep('c',7)),
                        css.cell = "padding: 5px;") %>%
      htmlTable(rownames = F,
                cgroup = list(c(paste0("Moderated Regression Analysis: ",
                                       math_Rsq," = ",round(r_sq,2),', ',
                                       math_F,"(",df_num,", ",df_denom,") = ",txtRound(Fstat,2),", ",
                                       math_p,ifelse(pval<.001," < .001",txtRound(pval,3))))),
                n.cgroup = list(c(8)),
                options = list(dom = 't'),
                caption = ifelse(interpretation == TRUE,
                                 paste0("Interpretation: ",interp),
                                 paste0("")))
  }
  suppressWarnings(output_table)
}


regression.output.interaction.interp <- function(model      = NULL,
                                                 predictors = NULL,
                                                 outcome    = NULL,
                                                 ...) {
  # Computing values
  b <- coef(model)
  b_pval <- tidy(model)$p.value
  predictors <- tolower(predictors)
  outcome <- tolower(outcome)
  r_sq <- summary(model)$r.squared
  tidy_model <- tidy(model)
  n_predictors <- length(tidy_model$term[-1])
  tidy_aov <- tidy(aov(model))
  pval <- tidy_aov[1,6]
  df_num <- n_predictors
  df_denom <- as.numeric(tidy_aov$df[n_predictors+1])
  tstat <- as.numeric(summary(model)$coefficients[,3])
  Fstat <- round(as.numeric(summary(model)$fstatistic[1]),2)
  loCI <- as.numeric(confint(model)[,1])
  hiCI <- as.numeric(confint(model)[,2])
  # Defining math script terms
  math_F <- huxtable('<em>F</em>')
  math_p <- huxtable('<em>p</em>')
  math_Rsq <- huxtable('<em>R<sup>2</sup></em>')
  math_t <- huxtable('<em>t</em>')
  math_b <- huxtable('<em>b</em>')
  math_SE <- huxtable('<em>SE</em>')
  # Pasting together interpretation text
  interp <- paste0("According to this regression model predicting ",
                   outcome, " from ",
                   predictors[1], ", ", predictors[2], ", and their interaction,
                   when both predictors and their interaction are equal to 0, ",
                   outcome, " should be equal to ", txtRound(b[1], digits = 2),
                   ". For every 1-unit increase in ", predictors[1], " with ",
                   predictors[2],huxtable('<em> fixed at 0</em>'),", there should
                   be a corresponding ",txtRound(abs(b[2]),digits=2),"-unit ",
                   ifelse(b[2]>=0,"increase ","decrease "),"in ",
                   outcome,". For every 1-unit increase in ", predictors[2],
                   " with ",predictors[1],huxtable('<em> fixed at 0</em>'),", there should
                   be a corresponding ",txtRound(abs(b[3]),digits=2),"-unit ",
                   ifelse(b[3]>=0,"increase ","decrease "),"in ",
                   outcome,". The interaction term tells us that for every 1-unit increase in ",
                   predictors[1],", the ",huxtable('<em>slope of the relationship </em>'),
                   "between ",predictors[2]," and ",outcome," will ",
                   ifelse(b[4]>=0,"increase ","decrease "),"by ",txtRound(abs(b[4]),digits=2),
                   " units. Equivalently, the interaction term tells us that for every 1-unit increase in ",
                   predictors[2],", the ",huxtable('<em>slope of the relationship </em>'),
                   "between ",predictors[1]," and ",outcome," will ",
                   ifelse(b[4]>=0,"increase ","decrease "),"by ",txtRound(abs(b[4]),digits=2),
                   " units. The model accounted for ",txtRound(r_sq*100,digits=2),
                   "% of the variance in ",outcome," (",math_Rsq," = ",txtRound(r_sq,digits=2),").
                   This degree of model fit was ",ifelse(pval<.05,"statistically significant (",
                                                         "not statistically significant ("),
                   math_F,"(",df_num,", ",df_denom,") = ",txtRound(Fstat,digits=2),
                   ", ",math_p,ifelse(pval>=.001,paste0(" = ",txtRound(pval,digits=2)),
                                      " < .001"),"). Furthermore, the coefficient for the partial slope of
                   the relationship between ",predictors[1]," and ",outcome," when ",predictors[2]," = 0 was ",
                   ifelse(b_pval[2]<.05,"statistically significant (",
                          "not statistically significant ("),
                   math_t,"(",df_denom,") = ",txtRound(tstat[2],digits=2),", ",math_p,
                   ifelse(b_pval[2]>=.001,paste0(" = ",txtRound(b_pval[2],digits=2))," < .001"),").
                   The 95% confidence interval for this slope coefficient ranged from ",
                   txtRound(loCI[2],digits=2)," to ",txtRound(hiCI[2],digits=2),".
                   The coefficient for the partial slope of the relationship between ",predictors[2]," and ",
                   outcome," when ",predictors[1]," = 0 was ",ifelse(b_pval[3]<.05,"statistically significant (",
                                                                     "not statistically significant ("),
                   math_t,"(",df_denom,") = ",txtRound(tstat[3],digits=2),", ",math_p,
                   ifelse(b_pval[3]>=.001,paste0(" = ",txtRound(b_pval[3],digits=2))," < .001"),").
                   The 95% confidence interval for this slope coefficient ranged from ",
                   txtRound(loCI[3],digits=2)," to ",txtRound(hiCI[3],digits=2),".
                   The coefficient for the interaction between ",predictors[1]," and ",predictors[2],
                   " was ",ifelse(b_pval[4]<.05,"statistically significant (",
                                  "not statistically significant ("),
                   math_t,"(",df_denom,") = ",txtRound(tstat[4],digits=2),", ",math_p,
                   ifelse(b_pval[4]>=.001,paste0(" = ",txtRound(b_pval[4],digits=2))," < .001"),").
                   The 95% confidence interval for this interaction slope coefficient ranged from ",
                   txtRound(loCI[4],digits=2)," to ",txtRound(hiCI[4],digits=2),". Due to the ",
                   ifelse(b_pval[4]<.05,"statistically significant interaction between ",
                          "lack of a statistically significant interaction between "),predictors[1],
                   " and ",predictors[2],ifelse(b_pval[4]<.05,", it is wise to follow up with a simple slopes analysis
                                                to determine how the slopes for the relationship between each predictor
                                                and the outcome change at varying levels of the other predictor.",
                                                " it is not generally necessary to follow up with a simple slopes analysis."))
  interp
}


regression.slopes.output <- function(model          = NULL,
                                     predictors     = NULL,
                                     outcome        = NULL,
                                     interpretation = FALSE,
                                     flip_moderator = F,
                                     ...) {
  tidy_model <- tidy(model)
  predictor_names <- colnames(model$model)[-1]
  outcome_name <- colnames(model$model)[1]
  if(length(predictors) == 0) {
    predictors <- predictor_names
  }
  if(length(outcome) == 0) {
    outcome <- outcome_name
  }
  if(length(predictors) == 2) {
    predictors[3] <- paste0(predictors[1], " x ",predictors[2])
  }
  ss <- simple_slopes(model, confint = TRUE)
  tidy_ss <- tibble(ss)
  if(class(model$model[,2]) == "numeric" &
     class(model$model[,3]) == "numeric") {
    mod1 <- tidy_ss[4:6,]
    mod2 <- tidy_ss[1:3,]
  } else if(class(model$model[,2]) == "numeric" &
            class(model$model[,3]) == "factor") {
    mod1 <- tidy_ss[4:5,]
    mod2 <- tidy_ss[1:3,]
  } else if(class(model$model[,2]) == "factor" &
            class(model$model[,3]) == "numeric") {
    mod1 <- tidy_ss[3:5,]
    mod2 <- tidy_ss[1:2,]
  } else if(class(model$model[,2]) == "factor" &
            class(model$model[,3]) == "factor") {
    mod1 <- tidy_ss[3:4,]
    mod2 <- tidy_ss[1:2,]
  }
  ifelse(flip_moderator == F, output_table <- mod1, output_table <- mod2)
  pred <- model$model[,colnames(output_table)[1]]
  moderator <- model$model[,colnames(output_table)[2]]
  orig_table <- output_table
  output_table[,3:7] <- txtRound(output_table[,3:7],digits = 2)
  output_table[,8] <- txtRound(output_table[,8],digits=0)
  output_table[,9] <- txtRound(output_table[,9], digits = 3)
  for(i in 1:nrow(output_table)) {ifelse(output_table[i,9] == "0.000",
                             output_table[i,9] <- "< .001",
                             output_table[i,9] <- txtRound(output_table[i,9],digits = 3))}
  for(i in 1:nrow(output_table)) {
    output_table[i,10] <- paste0("[",output_table[i,5],", ",output_table[i,6],"]")
  }
  output_table <- output_table[,c(2,3,4,10,8,7,9)]
  colnames(output_table) <- c("Moderator Level",
                              huxtable('<em>b</em>'),
                              huxtable('<em>SE</em>'),
                              "95% CI",
                              huxtable('<em>df</em>'),
                              huxtable('<em>t</em>'),
                              huxtable('<em>p</em>'))

  if(nrow(output_table) == 3) {
    output_table[,1] <- c(paste0("@ +1SD ", colnames(orig_table)[2], " (",txtRound(mean(moderator,na.rm=T)+sd(moderator,na.rm=T),digits=2),")"),
                          paste0("@ Mean ", colnames(orig_table)[2], " (",txtRound(mean(moderator,na.rm=T),digits=2),")"),
                          paste0("@ -1SD ", colnames(orig_table)[2], " (",txtRound(mean(moderator,na.rm=T)-sd(moderator,na.rm=T),digits=2),")"))
  } else if(nrow(output_table) == 2) {
    output_table[,1] <- c(paste0("@ ", colnames(orig_table)[2], " = ", levels(moderator)[1]),
                          paste0("@ ", colnames(orig_table)[2], " = ", levels(moderator)[2]))
  }

  interp <- paste0("This follow-up simple slopes analysis to our interaction between ",
                   predictors[1]," and ",predictors[2]," allows us to track how the slope
                   of the relationship between ",predictors[1]," and ",outcome,
                   " changes at varying levels of ",predictors[2],".",
                   if(class(model$model[,3])=="numeric") {
                     paste0(" Overall, we can see that at higher and higher levels of ",
                            predictors[2]," the slope of the relationship between ",
                            predictors[1]," and ",outcome,
                            if(as.numeric(output_table[1,2])>as.numeric(output_table[3,2]) &
                               as.numeric(output_table[3,2])>0) {
                              " gets more and more positive."
                            }
                            else if(as.numeric(output_table[1,2])<as.numeric(output_table[3,2]) &
                                    as.numeric(output_table[1,2])>0) {
                              " gets less and less positive."
                            }
                            else if(as.numeric(output_table[1,2])<as.numeric(output_table[3,2]) &
                                    as.numeric(output_table[3,2])<0) {
                              " gets more and more negative."
                            }
                            else if(as.numeric(output_table[1,2])>as.numeric(output_table[3,2]) &
                                    as.numeric(output_table[1,2])<0) {
                              " gets less and less negative."
                            }
                            else if(as.numeric(output_table[1,2])>as.numeric(output_table[3,2]) &
                                    as.numeric(output_table[1,2])>0 &
                                    as.numeric(output_table[3,2])<0) {
                              " flips from negative to positive."
                            }
                            else if(as.numeric(output_table[1,2])<as.numeric(output_table[3,2]) &
                                    as.numeric(output_table[1,2])<0 &
                                    as.numeric(output_table[3,2])>0) {
                              " flips from positive to negative."
                            },
                            " Specifically, we found that at high levels of ",
                            predictors[2]," (1 SD above the mean), there was a ",
                            ifelse(output_table[1,7]<.05,
                                   "statistically significant ",
                                   "non-statistically significant "),
                            ifelse(output_table[1,2]<0,
                                   "negative ",
                                   "positive "),
                            "relationship between ",predictors[1]," and ",outcome,".
                            At average levels of ",predictors[2],
                            ", there was a ",
                            ifelse(output_table[2,7]<.05,
                                   "statistically significant ",
                                   "non-statistically significant "),
                            ifelse(output_table[2,2]<0,
                                   "negative ",
                                   "positive "),
                            "relationship between ",predictors[1]," and ",outcome,".
                            Finally, at low levels of ",predictors[2],
                            " (1 SD below the mean), there was a ",
                            ifelse(output_table[3,7]<.05,
                                   "statistically significant ",
                                   "non-statistically significant "),
                            ifelse(output_table[3,2]<0,
                                   "negative ",
                                   "positive "),
                            "relationship between ",predictors[1]," and ",outcome,"."
                            )
                   }
                   else if(class(model$model[,3])=="factor") {
                     paste0(" Overall, we can see that as we move from the ",
                            levels(model$model[,3])[1]," to ",levels(model$model[,3])[2],
                            " group of ",predictors[2],", the slope of the relationship between ",
                            predictors[1]," and ",outcome,
                            if(as.numeric(output_table[2,2])>as.numeric(output_table[1,2]) &
                               as.numeric(output_table[1,2])>0) {
                              " gets more strongly positive."
                            }
                            else if(as.numeric(output_table[2,2])<as.numeric(output_table[1,2]) &
                                    as.numeric(output_table[2,2])>0) {
                              " gets less strongly positive."
                            }
                            else if(as.numeric(output_table[2,2])<as.numeric(output_table[1,2]) &
                                    as.numeric(output_table[1,2])<0) {
                              " gets more strongly negative."
                            }
                            else if(as.numeric(output_table[2,2])>as.numeric(output_table[1,2]) &
                                    as.numeric(output_table[2,2])<0) {
                              " gets less strongly negative."
                            }
                            else if(sign(as.numeric(output_table[2,2]))!=sign(as.numeric(output_table[1,2])) &
                                    as.numeric(output_table[2,2])>as.numeric(output_table[1,2])) {
                              " flips from negative to positive."
                            }
                            else if(sign(as.numeric(output_table[2,2]))!=sign(as.numeric(output_table[1,2])) &
                                    as.numeric(output_table[2,2])<as.numeric(output_table[1,2])) {
                              " flips from positive to negative."
                            },
                            " Specifically, we found that in the ",
                            levels(model$model[,3])[1]," group of ",
                            predictors[2],", there was a ",
                            ifelse(output_table[1,7]<.05,
                                   "statistically significant ",
                                   "non-statistically significant "),
                            ifelse(output_table[1,2]<0,
                                   "negative ",
                                   "positive "),
                            "relationship between ",predictors[1]," and ",outcome,".
                            In the ",levels(model$model[,3])[2]," group of ",
                            predictors[2],", there was a ",
                            ifelse(output_table[2,7]<.05,
                                   "statistically significant ",
                                   "non-statistically significant "),
                            ifelse(output_table[2,2]<0,
                                   "negative ",
                                   "positive "),
                            "relationship between ",predictors[1]," and ",outcome,"."
                     )
                   }
                )
 if(length(predictors)<length(predictor_names)) {
   output_table <- "Error:  no output table was created --> if you supply any custom predictor names, you must provide as many names as there are predictor variables!"
  } else {
   output_table <- output_table %>%
      addHtmlTableStyle(pos.caption = "bottom",
                        align = c('l',rep('c',6)),
                        css.cell = "padding: 5px;") %>%
      htmlTable(rownames = F,
                cgroup = list(c(paste0("Simple Slopes Analysis:",
                                       huxtable('<br>'),
                                       "Predictor = ",predictors[1],
                                       ", Moderator = ",predictors[2],
                                       ", Outcome = ",outcome))),
                n.cgroup = list(c(7)),
                options = list(dom = 't'),
                caption = ifelse(interpretation == TRUE,
                                 paste0("Interpretation: ",interp),
                                 paste0("")))
  }
  output_table
}

