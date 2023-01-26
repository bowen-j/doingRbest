#' The regression model method for the `graph` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `regress` function in the `doingRbest` package.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return A `plotly` plot (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Simple Regression Graph (scatterplot with line)
#' model <- regress(disp ~ mpg, data = mtcars)
#' graph(model)
#' # Multiple Regression Graph (3-D scatterplot with plane - 2 predictors maximum to render in 3-D)
#' model <- regress(disp ~ mpg + wt, data = mtcars)
#' graph(model)
#' # Moderated Regression Graph (3-D scatterplot with curved plane)
#' model <- regress(disp ~ mpg*wt, data = mtcars)
#' graph(model)
#' # Moderated Regression Simple Slopes Graph
#' model <- regress(disp ~ mpg*wt, data = mtcars)
#' graph(model, sim_slopes = TRUE)

regression.graph <- function(model,
                             predictors = c(),
                             outcome = NULL,
                             ...) {
  tidy_model <- tidy(model)
  predictor_names <- tidy_model$term[-1]
  outcome_name <- colnames(model$model)[1]
  if(length(predictors) == 0) {
    predictors <- predictor_names
  }
  if(length(outcome) == 0) {
    outcome <- outcome_name
  }
  n_predictors <- length(predictor_names)
  if(n_predictors > 2) {
    plot <- "Error: A graph cannot be generated for a standard regression model with more than 2 predictors because it would require more than 3 dimensions to render it :)"
  } else {
    yvar <- model$model[,1]
    x1 <- as.numeric(model$model[,2])
    if(n_predictors == 2) {
      x2 <- model$model[,3]
    }
  }
  if(n_predictors == 1) {
    data <- data.frame(yvar, x1)
    x_range <- seq(min(x1),max(x1),length.out=100)
    y_pred <- coef(model)[1]+coef(model)[2]*x_range
    plot <- plot_ly(data,
                    x=~x1,
                    y=~yvar,
                    name="observed data",
                    type="scatter",
                    mode="markers",
                    hovertemplate = paste(paste0(predictors[1],': %{x}'),
                                          paste0('<br>',outcome,': %{y}'))
    ) %>%
      add_trace(x=~x_range,
                y=~y_pred,
                mode="lines",
                name = "model predictions") %>%
      add_ribbons(data = augment(model, se_fit=T),
                  ymin = ~.fitted - 1.96*.se.fit,
                  ymax = ~.fitted + 1.96*.se.fit,
                  line = list(color = 'rgba(7,164,181.05)',
                              width = .001),
                  fillcolor = 'rgba(7,164,181,.2',
                  name = '95% CI fit') %>%
      layout(xaxis = list(title = predictors[1]),
             yaxis = list(title = outcome))
    
  } else if(n_predictors == 2) {
    x1.seq <- seq(min(x1),max(x1),length.out=25)
    x2.seq <- seq(min(x2),max(x2),length.out=25)
    z <- t(outer(X=x1.seq,Y=x2.seq,
                 FUN=function(x1,x2) coef(model)[1]+
                   coef(model)[2]*x1+coef(model)[3]*x2))
    plot <- plot_ly(data = model$model,
                    x = ~x1,
                    y = ~x2,
                    z = yvar,
                    name = "observed data",
                    type = "scatter3d",
                    mode = "markers",
                    hovertemplate = paste(paste0(predictors[1],': %{x}'),
                                          paste0('<br>',predictors[2],': %{y}'),
                                          paste0('<br>',outcome,': %{z}'))
    ) %>%
      add_trace(x = ~x1.seq,
                y = ~x2.seq,
                z = ~z,
                name = "model predictions",
                type = "surface",
                opacity = .5) %>%
      layout(scene = list(
        xaxis = list(title = predictors[1]),
        yaxis = list(title = predictors[2]),
        zaxis = list(title = outcome)
      )
      )
  }
  if(n_predictors == 1 & class(model$model[,2]) == "factor") {
    plot <- "Please use a t-test model and a bar graph for this single-predictor model, as the predictor is dichotomous."
  }
  plot
}

regression.int.graph <- function(model,
                                 predictors = c(),
                                 outcome = NULL,
                                 ...) {
  if(class(model$model[,2]) == "factor" &
     class(model$model[,3]) == "factor") {
    plot <- "Please use an ANOVA model for these predictor variables, as they are both categorical :)"
  }
  tidy_model <- tidy(model)
  predictor_names <- tidy(model)$term[-1]
  outcome_name <- colnames(model$model)[1]
  if(length(predictors) == 0) {
    predictors <- predictor_names
  }
  if(length(outcome) == 0) {
    outcome <- outcome_name
  }
  if(length(predictors) == 2) {
    predictors[3] <- paste0(predictors[1]," x ",predictors[2])
  }
  n_predictors <- length(predictor_names)
  if(n_predictors != 3) {
    plot <- "Sorry, right now this function can only handle 2 predictors and their interaction for a 3-D graph."
  }
  yvar <- model$model[,1]
  x1 <- model$model[,2]
  x2 <- model$model[,3]
  x1.seq <- if(class(x1)=="numeric") {
    seq(min(x1),max(x1),length.out=25)
  } else if(class(x1)=="factor") {
    seq(min(as.numeric(x1)-1),max(as.numeric(x1)-1),length.out=25)
  }
  x2.seq <- if(class(x2)=="numeric") {
    seq(min(x2),max(x2),length.out=25)
  } else if(class(x2)=="factor") {
    seq(min(as.numeric(x2)-1),max(as.numeric(x2)-1),length.out=25)
  }
  z <- t(outer(X=x1.seq,Y=x2.seq,
               FUN=function(x1,x2) coef(model)[1]+
                 coef(model)[2]*x1+coef(model)[3]*x2+coef(model)[4]*x1*x2))
  plot <- plot_ly(data = model$model,
                  x = ~x1,
                  y = ~x2,
                  z = yvar,
                  name = "observed data",
                  type = "scatter3d",
                  mode = "markers",
                  hovertemplate = paste(paste0(predictors[1],': %{x}'),
                                        paste0('<br>',predictors[2],': %{y}'),
                                        paste0('<br>',outcome,': %{z}'))) %>%
    add_trace(x = ~x1.seq,
              y = ~x2.seq,
              z = ~z,
              name = "model predictions",
              type = "surface",
              opacity = .5) %>%
    layout(scene = list(
      xaxis = list(title = predictors[1]),
      yaxis = list(title = predictors[2]),
      zaxis = list(title = outcome)
    )
    )
  plot
}

regression.slopes.graph <- function(model = model,
                                    predictors = c(),
                                    outcome = NULL,
                                    ...) {
  if(class(model$model[,2]) == "factor" &
     class(model$model[,3]) == "factor") {
    plot <- "Please use an ANOVA model for these predictor variables, as they are both categorical :)"
  }
  if(class(model$model[,2]) == "numeric" & 
     class(model$model[,3]) == "numeric") {
    varnames <- names(attr(model$terms,"dataClasses"))
    form <- as.formula(paste(varnames[1],"~",varnames[2],"*",varnames[3]))
    model_lm <- lm(form, get_data(model))
    x <- model_lm$model[,2]
    mod <- model_lm$model[,3]
    y <- model_lm$model[,1]
    d <- data.frame(y,x,mod)
    model_lm2 <- lm(y~x*mod,d)
    lo_mod <- mean(mod,na.rm=T)-sd(mod,na.rm=T)
    m_mod <- mean(mod,na.rm=T)
    hi_mod <- mean(mod,na.rm=T)+sd(mod,na.rm=T)
    effs <- effect(term = "x:mod",
                   mod = model_lm2,
                   x.var = "x",
                   xlevels = list(
                     x = pretty(x),
                     mod = c(lo_mod,m_mod,hi_mod)
                   )
    ) %>%
      as_tibble %>%
      mutate(mod_label = mod %>% factor(levels = c(lo_mod,m_mod,hi_mod), labels = c("Lower (-1SD)","Mean","Upper (+1SD)")))
    if(length(predictors)==0) {
      predictors[1] <- varnames[2]
      predictors[2] <- varnames[3]
    }
    if(length(outcome)==0) {
      outcome <- varnames[1]
    }
    plot <- plot_ly(data=d,
                    x=~x,
                    y=~y,
                    name="observed data",
                    type="scatter",
                    mode="markers",
                    hovertemplate = paste(paste0(predictors[1],': %{x}'),
                                          paste0('<br>',outcome,': %{y}'))
                    ) %>%
      add_trace(data=effs,
                x=~x,
                y=~fit,
                mode="lines",
                name = ~mod_label,
                color = ~mod_label,
                linetype = ~mod_label) %>%
      add_ribbons(data=effs,
                x=~x,
                y=~fit,
                name = paste0(rep("95% CI fit: ",length(effs$mod_label)), effs$mod_label),
                color=~mod_label,
                ymin=~lower,
                ymax=~upper,
                opacity = .25,
                line = list(width = .001)) %>%
      layout(xaxis = list(title = predictors[1]),
             yaxis = list(title = outcome),
             legend = list(title = list(text = predictors[2])),
             title = paste0("Simple Slopes of ",predictors[1]," on ",outcome," at levels of ",predictors[2]))
  } else if(class(model$model[,2]) == "factor" | 
       class(model$model[,3]) == "factor") {
    varnames <- names(attr(model$terms,"dataClasses"))
    form <- as.formula(paste(varnames[1],"~",varnames[2],"*",varnames[3]))
    model_lm <- lm(form, get_data(model))
    x <- if(class(model_lm$model[,2])=="numeric") {
      model_lm$model[,2]
    } else if(class(model_lm$model[,3])=="numeric") {
      model_lm$model[,3]
    }
    mod <- if(class(model_lm$model[,2])=="factor") {
      model_lm$model[,2]
    } else if(class(model_lm$model[,3])=="factor") {
      model_lm$model[,3]
    }
    y <- model_lm$model[,1]
    d <- data.frame(y,x,mod)
    model_lm2 <- lm(y~x*mod,d)
    grp1_mod <- levels(mod)[1]
    grp2_mod <- levels(mod)[2]
    effs <- effect(term = "x:mod",
                   mod = model_lm2,
                   x.var = "x",
                   xlevels = list(
                     x = pretty(x),
                     mod = c(grp1_mod,grp2_mod)
                   )
    ) %>%
      as_tibble
    if(length(predictors)==0) {
     predictors[1] <- names(r6$model[,2:3])[sapply(r6$model[,2:3],is.numeric)]
     predictors[2] <- names(r6$model[,2:3])[sapply(r6$model[,2:3],is.factor)]
    }
    if(length(outcome)==0) {
      outcome <- varnames[1]
    }
    plot <- plot_ly(data=d,
                    x=~x,
                    y=~y,
                    name="observed data",
                    type="scatter",
                    mode="markers",
                    hovertemplate = paste(paste0(predictors[1],': %{x}'),
                                          paste0('<br>',outcome,': %{y}'))
                    ) %>%
      add_trace(data=effs,
                x=~x,
                y=~fit,
                mode="lines",
                name = ~mod,
                color = ~mod,
                linetype = ~mod) %>%
      add_ribbons(data=effs,
                  x=~x,
                  y=~fit,
                  name=c(rep(paste0("95% CI fit: ", levels(mod)[1]),length(effs$x[effs$mod==effs$mod[1]])),
                         rep(paste0("95% CI fit: ", levels(mod)[2]),length(effs$x[effs$mod==effs$mod[2]]))),
                  color=~mod,
                  ymin=~lower,
                  ymax=~upper,
                  opacity = .25,
                  line = list(width = .001)) %>%
      layout(xaxis = list(title = predictors[1]),
             yaxis = list(title = outcome),
             legend = list(title = list(text = predictors[2])),
             title = paste0("Simple Slopes of ",predictors[1]," on ",outcome," at levels of ",predictors[2]))
    
  }
  plot
}
















