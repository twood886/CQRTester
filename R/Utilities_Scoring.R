#' @title Convert Data Array to Quantiles
#' @description Function to calculate quantiles.
#' @details Used in Alpha Testing Functions
#' @param x a numeric array to be quantiled
#' @param fftile integer number of fractiles to use in splitting data
#' @return ftile
#' @import tidyverse
#' @import DescTools
#' @import ggplot2
#' @import forcats
#' @export
ctq <- function(x, fftile){
  b<-sum(!is.na(unique(x)))
  labels<-gettextf("Q%s", 1:fftile)
  if(b>=fftile){
    xnames <- names(x)
    qs<-round(rank(x, na.last = "keep")/sum(!is.na(x))/(1/fftile)+.4999)
    qs<-ifelse(qs<1,1,qs)
    ftile<-factor(ggplot2::cut_interval(qs,n=fftile,labels=labels), ordered = T)
    ftile<-forcats::fct_na_value_to_level(ftile, level = "NA")
    names(ftile) <- xnames
  }else{
    ftile<-factor(
      rep(NA, times = length(x)),
      levels = labels,
      ordered = T)
    ftile<-forcats::fct_na_value_to_level(retval, level = "NA")
  }
  return(ftile)
}

#' @title Calculate Z-Score with Winsorization
#' @description Function to calculate normalized value with windsorization.
#' @details Used in Alpha Testing Functions
#' @param x a numeric vector to be winsorized and normalized.
#' @param win.prob numeric vector of probabilities with values in [0,1]
#' as used in quantile.
#' @return A vector of the same length as the original data x containing the
#' winsorized and normalized data.
#' @import tidyverse
#' @import DescTools
#' @export
ctz <- function(x, win.prob = c(0,1)){
  win.x <- DescTools::Winsorize(x = x, probs = win.prob, na.rm = T)
  norm.x <- (win.x - mean(win.x, na.rm = T)) / sd(win.x, na.rm = T)
  return(norm.x)
}

#' @title Quantile & Z-Scoring
#' @description Add Windsorized Z-Score and Quantile Score to Data
#' @details Used in Alpha Testing Functions
#' @param data dataframe containing column with data to be scored
#' @param fname character column name of factor
#' @param fftile integer number of fractiles to use in spliting data
#' @param winsor pair of numeric to bound data in windsorization
#' @return data
#' @import tidyverse
#' @import DescTools
#' @export
f_scoring <- function(data, fname, fftile, winsor = c(0,1)){
  data<-data %>%
    mutate(fzscore = scale(Winsorize(.data[[fname]], probs = winsor, na.rm = T))) %>%
    mutate(fgroup = ctq(fzscore,{{fftile}}))
  return(data)
}
