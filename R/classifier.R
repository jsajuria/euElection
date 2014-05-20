#' Classify function
#' 
#' This function is based on Pablo Barbera's workshop in Internet data collection. You need to load the lexicon first and create the pos.words and neg.words objects first (see example)
##' @param tweets is the dataframe of tweets. pos.words and neg.words come from the lexicon (see example). The keyword is the topic used to subset the data
#' @keywords Twitter, subset
#' @author Javier Sajuria
#' @examples
#' lexicon <- read.csv("https://raw.githubusercontent.com/jsajuria/euElection/lexicon.csv", stringsAsFactors = F)
#' pos.words <- lexicon$word[lexicon$polarity == "positive"]
#' neg.words <- lexicon$word[lexicon$polarity == "negative"]
#' classifier(tweets, pos.words, neg.words, keyword = "")

classifier <- function(tweets, pos.words, neg.words, keyword) {
  # subsetting tweets that contain the keyword
  relevant <- grep(keyword, tweets$text, ignore.case = TRUE)
  # preparing tweets for analysis
  clean.tweets <- function(text) {
    # loading required packages
    lapply(c("tm", "Rstem", "stringr"), require, c = T, q = T)
    words <- removePunctuation(text)
    words <- wordStem(words)
    # spliting in words
    words <- str_split(text, " ")
    return(words)
  }
  words <- clean.tweets(tweets$text[relevant])
  # classifier
  classify <- function(words, pos.words, neg.words) {
    # count number of positive and negative word matches
    pos.matches <- sum(words %in% pos.words)
    neg.matches <- sum(words %in% neg.words)
    return(pos.matches - neg.matches)
  }
  scores <- unlist(lapply(words, classify, pos.words, neg.words))
  n <- length(scores)
  positive <- as.integer(length(which(scores > 0))/n * 100)
  negative <- as.integer(length(which(scores < 0))/n * 100)
  neutral <- 100 - positive - negative
  cat(n, "tweets about", keyword, ":", positive, "% positive,", negative,
      "% negative,", neutral, "% neutral")
}
