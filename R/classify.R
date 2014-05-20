#' Classify function
#' 
#' This function is based on Pablo Barbera's workshop in Internet data collection
##' @param words is the text, pos.words and neg.words come from the lexicon. Use the example first
#' @keywords Twitter, subset
#' @author Javier Sajuria
#' @examples
#' lexicon <- read.csv("https://raw.githubusercontent.com/pablobarbera/workshop/master/lexicon.csv", stringsAsFactors = F)
#' pos.words <- lexicon$word[lexicon$polarity == "positive"]
#' neg.words <- lexicon$word[lexicon$polarity == "negative"]
#' clean.tweets()

classify <- function(words, pos.words, neg.words) {
  # count number of positive and negative word matches
  pos.matches <- sum(words %in% pos.words)
  neg.matches <- sum(words %in% neg.words)
  return(pos.matches - neg.matches)
}
