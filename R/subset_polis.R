#' Subset polis function
#' 
#' This function creates a subset from the tweets dataframe (collected and parsed using ''streamR'') that corresponds to a group of political terms:
#' 'europe','ukip','farage','labour','miliband','conservative','cameron','libdem','clegg','lucas','green party','tory','politic','election','EU','parliament','government','pmq','vote','opposition'
##' @param df is the dataframe of tweets. The text of the tweets whould be under a column named "text"
#' @keywords Twitter, subset

#' @examples
#' subset_polis()


subset_polis <- function(df){
  searchterms<- c('europe','ukip','farage','labour','miliband','conservative','cameron','libdem','clegg','lucas','green party','tory','politic','election','EU','parliament','government','pmq',
                  'vote','opposition')
  x <- clean.tweets(df$text)
  y <- subset(df, grepl(paste(searchterms,collapse="|"), x), drop = TRUE)
  return(y)
}
