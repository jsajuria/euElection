#' Subset Conservative function
#' 
#' This function creates a subset from the tweets dataframe (collected and parsed using ''streamR'') that corresponds to a group of political terms related to Conservative Party
#' @author Javier Sajuria
##' @param df is the dataframe of tweets. The text of the tweets whould be under a column named "text"
#' @keywords Twitter, subset
#' @export
#' @examples
#' subset_cons()

subset_cons <- function(df){
  a <-c('conservative', '^tory','^cameron^')
  x <- clean.tweets(df$text)
  y <- subset(df, grepl(paste(a,collapse="|"), x), drop = TRUE)
  return(y)
}
