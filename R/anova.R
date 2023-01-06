#' A function to construct different types of ANOVA models
#'
#' @param data A data frame containing the variables to be specified in the model.
#' @param type Unnecessary if formula is specified correctly: a character string specifying the type of ANOVA as "between" subjects, "within" subjects or (if factorial) "mixed."
#' @param formula An optional formula specifying the ANOVA model to be fit.
#' @param outcome If formula is unspecified, a numeric variable named as a character.
#' @param between If formula is unspecified, a between-subjects factor variable named as a character.
#' @param within If formula is unspecified, a within-subjects factor variable named as a character.
#' @param id If formula is unspecified, a factor variable named as a character which uniquely identifies each subject.
#' @param detailed If TRUE, returns additional information in the ANOVA table.
#'   
#' @return A data frame
#' @export
#' 
#' @importFrom rstatix anova_test
#' @importFrom tidyr `%>%` pivot_longer
#' @import PASWR2
#' 
#' @examples
#' # One-Way Between-Subjects ANOVA
#' anova(data = ToothGrowth, formula = len ~ dose)
#' anova(data = ToothGrowth, type = "between", outcome = "len", between = "dose")
#' # Two-Way Between-Subjects ANOVA
#' mtcars$am <- factor(mtcars$am)
#' anova(data = mtcars, formula = disp ~ vs*am)
#' anova(data = mtcars, type = "between", outcome = "disp", between = c("vs", "am"))
#' # One-Way Within-Subjects ANOVA
#' wrestlers <- PASWR2::HSWRESTLER %>% pivot_longer(cols = c("hwfat", "tanfat", "skfat"), names_to = "type", values_to = "measurement")
#' wrestlers$id <- factor(sort(rep(1:78, 3)))
#' anova(data = wrestlers, formula = measurement ~ type + Error(id/type))
#' anova(data = wrestlers, type = "within", outcome = "measurement", within = "type", id = "id")
#' # Two-Way Within-Subjects ANOVA
#' expt <- data.frame(PID = factor(sort(rep(1:50,4))), image = factor(rep(c("Angry","Angry","Happy","Happy"),50)), music = rep(c("Disney","Horror"),100), stress = rnorm(200,60,10))
#' anova(data = expt, formula = stress ~ image*music + Error(PID/(image*music)))
#' anova(data = expt, type = "within", outcome = "stress", within = c("image", "music"), id = "PID")
#' # Two-Way Mixed ANOVA
#' ChickWeight$Time <- factor(ChickWeight$Time)
#' ChickWeight$Chick <- factor(ChickWeight$Chick)
#' anova(data = ChickWeight, formula = weight ~ Diet*Time + Error(Chick/Time))
#' anova(data = ChickWeight, type = "mixed", outcome = "weight", within = "Time", between = "Diet", id = "Chick")

anova = function(data     = NULL,
                          type     = c("between", "within", "mixed"),
                          formula  = NULL,
                          outcome  = NULL,
                          between  = NULL,
                          within   = NULL,
                          id       = NULL,
                          detailed = TRUE,
                          ...) {
  type <- match.arg(type)
  
  if(!is.null(formula)) {
    anova_model <- anova_test(data     = data,
                              formula  = formula,
                              detailed = detailed,
                              type = 2)
  }
  else if(type == "between") {
    anova_model <- anova_test(data     = data,
                              dv       = all_of(outcome),
                              between  = all_of(between),
                              detailed = detailed,
                              type = 2)
  }
  else if(type == "within") {
    anova_model <- anova_test(data     = data,
                              dv       = all_of(outcome),
                              wid      = id,
                              within   = all_of(within),
                              detailed = detailed,
                              type = 2)
  }
  else {
    anova_model <- anova_test(data     = data,
                              dv       = all_of(outcome),
                              wid      = id,
                              between  = all_of(between),
                              within   = all_of(within),
                              detailed = detailed,
                              type = 2)
  }

  anova_model$type = type
  
  return(anova_model)
}

