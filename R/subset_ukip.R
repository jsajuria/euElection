#' Subset UKIP function
#' 
#' This function creates a subset from the tweets dataframe (collected and parsed using ''streamR'') that corresponds to a group of political terms related to UKIP
#' @author Javier Sajuria
##' @param df is the dataframe of tweets. The text of the tweets whould be under a column named "text"
#' @keywords Twitter, subset
#' @examples
#' subset_ukip()

subset_ukip <- function(df){
  a <-c('ukip','farage')
  x <- clean_tweets(df$text)
  y <- subset(df, grepl(paste(a,collapse="|"), x), drop = TRUE)
  return(y)
}
