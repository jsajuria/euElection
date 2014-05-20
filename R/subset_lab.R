#' Subset UKIP function
#' 
#' This function creates a subset from the tweets dataframe (collected and parsed using ''streamR'') that corresponds to a group of political terms related to Labour
#' @author Javier Sajuria
##' @param df is the dataframe of tweets. The text of the tweets whould be under a column named "text"
#' @keywords Twitter, subset
#' @examples
#' subset_lab()

subset_lab <- function(df){
  labour <-c('miliband', 'labour')
  x <- clean_tweets(df$text)
  y <- subset(df, grepl(paste(labour,collapse="|"), x), drop = TRUE)
  return(y)
}
