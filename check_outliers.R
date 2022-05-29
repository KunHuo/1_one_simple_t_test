#' Identify univariate outliers
#' 
#' @author Rongrui Huo, 2022/05/29
#' 
#' @description 
#' Detect outliers using boxplot methods. Boxplots are a popular and an easy 
#' method for identifying outliers. There are two categories of outlier: 
#' (1) outliers and (2) extreme points. Values above Q3 + 1.5xIQR or below 
#' Q1 - 1.5xIQR are considered as outliers. Values above Q3 + 3xIQR or below 
#' Q1 - 3xIQR are considered as extreme points (or extreme outliers). Q1 and Q3 
#' are the first and third quartile, respectively. IQR is the interquartile 
#' range (IQR = Q3 - Q1).
#'
#' @param x a numeric vector.
#'
check_outliers <- function(x){
  Q1 <- quantile(x, probs = 0.25, na.rm = TRUE)
  Q3 <- quantile(x, probs = 0.75, na.rm = TRUE)
  IQR <- IQR(x, na.rm = TRUE)
  
  out <- lapply(1:length(x), function(i){
    if(!is.na(x[i])){
      is.outlier <- FALSE
      is.extreme <- FALSE
      if((x[i] < Q1 - 1.5 * IQR) | (x[i] > Q3 + 1.5 * IQR)){
        is.outlier <- TRUE
      }
      if((x[i] < Q1 - 3 * IQR) | (x[i] > Q3 + 3 * IQR)){
        is.extreme <- TRUE
      }
      data.frame(row.index = i , is.outlier, is.extreme)
    }
  })
  
  out <- do.call(rbind, out)
  out <- out[out$is.outlier == TRUE | out$is.extreme == TRUE, ,drop = FALSE]
  row.names(out) <- NULL
  out
}
