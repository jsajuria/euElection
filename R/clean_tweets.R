#' Clean Tweets function
#' 
#' This function is based on Pablo Barbera's workshop in Internet data collection
##' @param text is the text column of the tweets data frame
#' @keywords Twitter, subset
#' @author Javier Sajuria
#' @export
#' @examples
#' clean.tweets()

clean_tweets <- function(text) {
  # loading required packages
  lapply(c("tm", "Rstem", "stringr"), require, c = T, q = T)
  words <- removePunctuation(text)
  words <- stemDocument(words)
  # spliting in words
  words <- str_split(text, " ")
  return(words)
}
