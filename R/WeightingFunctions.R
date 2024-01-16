#' @export
#' @importFrom tidyr replace_na
ZscoreWeighting <- function(factorZscore, longonly = F){
  
  if(longonly == T) return(NA)
  
  return(
    tidyr::replace_na(factorZscore / (sum(abs(factorZscore), na.rm = T)/2),0))
}
