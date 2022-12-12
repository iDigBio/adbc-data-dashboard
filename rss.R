library(RCurl)
library(XML)


getFeedFromUrl <- function(feedUrl, auth = NULL) {
  feedUfl <- cleanFeedURL(feedUrl)
  opts <- curlOptions()
  
  if (!is.null(auth))
    opts <- curlOptions(userpwd = auth)
  
  rssText <- getURL(feedUfl, .opts = opts)
  getFeedFromContent(rssText)
}

getFeedFromFile <- function(filename) {
  getFeedFromContent(filename)
}

getFeedFromContent <- function(textOrFile) {
  content <- xmlTreeParse(textOrFile)$doc
  if (!is.null(names(content$children$rss))) {
    results <- parseRSS(content)
  } else if (!is.null(names(content$children$feed))) {
    results <- parseAtom(content)
  }
  return(results)
}

# Return an object with these fields and values:
# header: feed children except entry (e.g. link, description)
# items: feed/entry elements renamed to item and modified to have these values:
#   creator: entry/author
#   description: entry/summary
#   comments: entry/link/rel
#   guid: entry/atom/id
#   categories: entry/category values joined on "; "
parseAtom <- function(content) {
  results <- list()
  results$header <-
    lapply(content$children$feed[names(content$children$feed) != "entry"], xmlToList)
  results$items <-
    lapply(content$children$feed[names(content$children$feed) == "entry"], xmlToList)
  results$items <- lapply(results$items, AtomToRSS)
  names(results$items) <- rep("item", length(results$items))
  return(results)
}

# Return an object with these fields and values:
# header: rss/channel children except entry (e.g. link, description)
# items: rss/channel/item elements modified to have these values:
#   creator: item/author
#   description: item/summary
#   comments: item/link/rel
#   guid: item/atom/id
#   categories: item/category values joined on "; "
parseRSS <- function(content) {
  channel <- xmlToList(content$children$rss[['channel']])
  results <- list()
  results$header <- channel[names(channel) != "item"]
  results$items <- channel[names(channel) == "item"]
  results$items <- lapply(results$items, cleanRSS)
  names(results$items) <- rep("item", length(results$items))
  return(results)
}

# Return a copy of atom, with the following modifications:
# Move field author to creator
# Move field summary to description
# Copy link.rel value to comments
# Join category values on "; " and move to categories
# Move atom.id to guid
# Copy published to pubDate
# Copy content attr "base" value to source
AtomToRSS <- function(atom) {
  item <- atom
  item$creator <-   atom$author
  item$author <- NULL
  item$description <- atom$summary
  item$summary <- NULL
  for (i in atom[grep("link", names(atom))]) {
    if (i["rel"] == "replies")
      item$comments <- i["href"]
  }
  for (i in atom[grep("category", names(atom))]) {
    item$categories <- paste(item$categories, i['term'], sep = "; ")
  }
  item$categories <- sub(";", "", item$categories)
  item <- item[names(item) != "category"]
  item$guid <- atom$id
  item$id <- NULL
  item$pubDate <- as.POSIXlt(atom$published)
  item$source <-  atom$content$.attrs['base']
  return(item)
}

# Take as input an rss object; return a copy, with the 
# following modifications:
# pubDate: parsed to a date if format is yyyy-mm-dd or Fri, 21 Feb 1997 00:00:00 (e.g.)
# categories: values of rss categories fields concatenated on '; '; category fields removed
# comment: value of rss commentRss field; commentRss field removed
cleanRSS <- function(rss) {
  # Make a copy of rss
  item <- rss
  
  # Set item pubDate field to a date parsed from the value of
  # the rss pubDate field.  Try first by parsing in yyyy-mm-dd
  # format, then in "Fri, 21 Feb 1997 00:00:00" format; failing
  # that, just set the item pubDate str value
  tryCatch(
    item$pubDate <- as.POSIXlt(rss$pubDate),
    error = function(e) {
      tryCatch(
        item$pubDate <<-
          as.POSIXlt(rss$pubDate, format = "%a, %d %b %Y %H:%M:%S"),
        error = function(e) {
          item$pubDate <- rss$pubDate
        }
      )
    }
  )
  # Concatenate the contents of whatever "category" elements
  # appear in the rss object on "; " and assign the result to
  # item$categories; Each rss category element value might
  # itself be such a concatenation
  for (i in rss[grep("category", names(rss))]) {
    item$categories <- paste(item$categories, i, sep = "; ")
  }
  # Strip out the first semicolon
  item$categories <- sub(";", "", item$categories)
  # Remove all "category" fields
  item <- item[names(item) != "category"]
  # Rename the commentsRss field to comments
  item$comments <- item$commentRss
  item$commentRss <- NULL
  return(item)
}

# Replace non-http/https protocol in url with "http"
# and return the resulting url string
cleanFeedURL <- function(feed) {
  if (!grepl("http://|https://", feed)) {
    if (grepl("://", feed)) {
      feed <- strsplit(feed, "://")[[1]][2]
    }
    feed <- paste("http://", feed, sep = "")
  }
  return(feed)
}
