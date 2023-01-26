#' The t-test model method for the `graph` function from the `doingRbest` package
#' 
#' @param model The stored model object created from the `ttest` function in the `doingRbest` package.
#' @param predictors An optional character vector of custom names for the x-variables in the model.
#' @param outcome An optional custom name (character vector of length 1) for the y-variable in the model.
#' @param ... Extra named arguments that may be available upon package updates.
#'
#' @return A `plotly` plot (in a separate window or the Viewer pane)
#' @export
#' 
#' @examples
#' # Single Sample t-test Output Table
#' model <- ttest(outcome = "disp", data = mtcars, type = "s", mu = 100)
#' graph(model, outcome = "Fuel Displacement")
#' # Independent Samples t-test Output Table
#' model <- ttest(outcome = "hardness", predictor = "location", data = PASWR2::APPLE, type = "independent")
#' graph(model)
#' # Dependent Samples t-test Output Table for "wide" format data
#' early <- c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' late <- c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' my_data <- data.frame(early, late)
#' model <- ttest(outcome = "weight", predictor = "group", data = my_data, data_format = "wide", type = "dependent", cols = c("early", "late"))
#' graph(model)
#' # Dependent Samples t-test Output Table for "long" format data
#' SubID <- factor(sort(rep(1:10,2)))
#' Grp <- factor(rep(c("Ctrl","Trt"),10))
#' DV <- rnorm(20,10,2)
#' the_data <- data.frame(SubID, Grp, DV)
#' model <- ttest(outcome = "DV", predictor = "Grp", data = the_data, data_format = "long", type = "dependent")
#' graph(model)

ttest.graph <- function(model      = NULL,
                        predictors = NULL,
                        outcome    = NULL,
                        ...) {
  if (model$type[1] == "single") {
    output_graph <- ttest.simp.graph(model   = model,
                                     outcome = outcome)

  } else if (model$type[1] == "independent") {
    output_graph <- ttest.indep.graph(model      = model,
                                      predictors = predictors,
                                      outcome    = outcome)

  } else {
    output_graph <- ttest.dep.graph(model      = model,
                                    predictors = predictors,
                                    outcome    = outcome)
  }
  output_graph
}


ttest.simp.graph <- function(model = NULL,
                             outcome = NULL,
                             ...) {
  yvar <- attr(model, "args")$data[,model$.y.]
  d <- data.frame(yvar)
  mu1 <- attr(model, "args")$mu
  outcome_name <- model$.y.
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  graph_title <- paste0("Distribution of ",str_to_title(outcome))
  graph <- plot_ly(
    data = d,
    y = ~yvar,
    type = "box",
    name = outcome,
    color = I('green3'),
    boxpoints = "Potential Outliers",
    line = list(color = 'rgb(8,81,156)'),
    marker = list(color = 'rgb(8,81,156)')) %>%
    layout(title = graph_title,
           showlegend = FALSE,
           yaxis = list(title = str_to_title(outcome)),
           xaxis = list(showticklabels = FALSE),
           margin = list(t=50,b=50))
  
  graph
}


ttest.indep.graph <- function(model      = NULL,
                              outcome    = NULL,
                              predictors = NULL,
                              ...) {
  outcome_name <- model$.y.
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  predictor_name <- tail(strsplit(toString(attr(model, "args")$formula),", ")[[1]],1)
  if(is.null(predictors)) {
    predictors <- predictor_name
  }
  graph_title <- paste0("Difference in means of ",str_to_title(outcome)," across ",str_to_title(predictors))
  xvar        <- attr(model, "args")$data[,tail(strsplit(toString(attr(model, "args")$formula),", ")[[1]],1)]
  Mean        <- attr(model, "args")$data[,model$.y.]
  my_data <- data.frame(xvar, Mean)
  lim_vals <- my_data %>% group_by(xvar) %>%
    dplyr::summarize(lims = mean_cl_normal(Mean))
  limits <- lim_vals$lims$ymax-lim_vals$lims$ymin
  groups <- str_to_title(levels(xvar))
  means <- c(mean(Mean[xvar == levels(xvar)[1]],na.rm=T),
             mean(Mean[xvar == levels(xvar)[2]],na.rm=T))
  d <- data.frame(groups, means, limits)
  
  graph <- plot_ly(data = d,
                   x = ~groups,
                   y = ~means,
                   name = ~groups,
                   type = "bar",
                   hovertemplate = paste('Mean: %{y}'),
                   error_y = ~list(array = limits,
                                   color = '#000000')) %>%
    layout(title = list(text = paste0(graph_title,
                                      '<br>',
                                      '<sup>',
                                      '*Error bars represent 95% Confidence Intervals')),
           yaxis = list(title = str_to_title(outcome)),
           xaxis = list(title = str_to_title(predictors[1])),
           margin = list(t=50,b=75))
  
  graph
}

ttest.dep.graph <- function(model      = NULL,
                            outcome    = NULL,
                            predictors = NULL,
                            ...) {
  outcome_name <- model$.y.
  if(is.null(outcome)) {
    outcome <- outcome_name
  }
  predictor_name <- tail(strsplit(toString(attr(model, "args")$formula),", ")[[1]],1)
  if(is.null(predictors)) {
    predictors <- predictor_name
  }
  xvar      <- attr(model, "args")$data[,tail(strsplit(toString(attr(model, "args")$formula),", ")[[1]],1)]
  xvar      <- as.data.frame(xvar)[,1]
  Mean      <- attr(model, "args")$data[,model$.y.]
  Mean      <- as.data.frame(Mean)[,1]
  grandMean <- mean(as.data.frame(Mean)[,1], na.rm = T)
  pMeans    <- NA
  data      <- attr(model, "args")$data

  for(i in 1:nrow(data)) {
    if(is.even(row(data)[i])==FALSE) {
      pMeans[i] <- mean(c(Mean[i],Mean[i+1]),na.rm=T)
    } else if(is.even(row(data)[i])==TRUE) {
      pMeans[i] <- mean(c(Mean[i],Mean[i-1]),na.rm=T)
    }
  }
  adj         <- grandMean-pMeans
  Mean_adj    <- Mean + adj
  graph_title <- paste0("Difference in means of ",str_to_title(outcome)," across ",str_to_title(predictors))
  xvar <- as.factor(xvar)
  groups <- levels(as.factor(xvar))
  means <- c(mean(Mean_adj[xvar==levels(xvar)[1]],na.rm=T),
             mean(Mean_adj[xvar==levels(xvar)[2]],na.rm=T))
  my_data <- data.frame(xvar, Mean_adj)
  lim_vals <- my_data %>% group_by(xvar) %>%
    dplyr::summarize(lims = mean_cl_normal(Mean_adj))
  limits <- lim_vals$lims$ymax-lim_vals$lims$ymin
  d <- data.frame(groups,means,limits)
  
  graph <- plot_ly(data = d,
                   x = ~groups,
                   y = ~means,
                   name = ~groups,
                   type = "bar",
                   hovertemplate = paste('Adjusted Mean: %{y}'),
                   error_y = ~list(array = limits,
                                   color = '#000000')) %>%
    layout(title = list(text = paste0(graph_title,
                                      '<br>',
                                      '<sup>',
                                      '*Error bars represent 95% Confidence Intervals')),
           yaxis = list(title = str_to_title(outcome)),
           xaxis = list(title = str_to_title(predictors[1])),
           margin = list(t=50,b=75))
  
  graph
}
