#' Effect size for one-sample t-test
#' 
#' @author Rongrui Huo, 2022/05/29
#' 
#' @description 
#' To calculate an effect size, called Cohen's d, for the one-sample t-test. 
#' Recall that, t-test conventional effect sizes, proposed by Cohen J. (1998), 
#' are: 0.2 (small effect), 0.5 (moderate effect) and 0.8 (large effect) (Cohen 1998). 
#'
#' @param x a numeric vector.
#' @param mu the mean against which the mean of x is compared (default value is mu = 0)
#' 
cohen_d <- function(x, mu = 0){
  abs(mean(x, na = TRUE) - mu) / sd(x, na.rm = TRUE)
}
