#' Subset LibDem function
#' 
#' This function creates a subset from the tweets dataframe (collected and parsed using ''streamR'') that corresponds to a group of political terms related to LibDem
#' @author Javier Sajuria
##' @param df is the dataframe of tweets. The text of the tweets whould be under a column named "text"
#' @keywords Twitter, subset

#' @examples
#' subset_libdem()

subset_libdem <- function(df){
  a <-c('libdem','liberal democrat','clegg')
  x <- clean_tweets(df$text)
  y <- subset(df, grepl(paste(a,collapse="|"), x), drop = TRUE)
  return(y)
}
