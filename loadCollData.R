library(rvest)
library(dplyr)
library(jsonlite)
library(ridigbio)

source("rss.R")

# Recordsets published by VertNet are a special case regarding counts
VertNetUuid <- "e699547d-080e-431a-8d9b-3d56e39808f0"

# Static data
collsJsonPath          <- "data/gbif_colls_20221213.json"
collsRsetCountsCsvPath <- "data/coll_uuid_to_counts_20221213.csv"
idbRssPath             <- "data/idigbio_ipt_rss_20221213.xml"
vertNetRssPath         <- "data/vertnet_ipt_rss_20221213.xml"
idbApiRsetsJsonPath    <- "data/idb_api_recordsets_20221213.json"
idbApiPubsJsonPath     <- "data/idb_api_publishers_20221213.json"
freshRsetsCsvPath      <- "data/fresh-recordsets-last-360-days_20221214.csv"

#' Read a JSON file of all iDigBio collections in GBIF's GrSciColl Registry.
#' Then merge in counts of recordsets per collection from another file.
#' 
#' @return dataframe: institution, collection, lat, lon, collection_uuid,
#' collection_url, recordsets, recordsetQuery, UniqueNameUUID, size
loadCollData <- function() {

  # This is the old file, still up, but last updated in 2018
  #idigbio_colls_url <-
    "http://idigbio.github.io/idb-us-collections/collections.json"

  # This is where the data lives now; it is hosted by GBIF takes a minute or so
  # to download.  To use it put the url into the fromJSON call below.
  #gbifCollsUrl <- "https://api.gbif.org/v1/external/idigbio/collections"
  
  # The call to the web service takes a minute to download the data, so I saved
  # the results in this file.
  f <- file(collsJsonPath)
  
  colls <- fromJSON(f) %>%
    select("institution", "collection", "lat", "lon", "collection_uuid",
           "collection_url", "recordsets", "recordsetQuery", "UniqueNameUUID")
  
  # Merge recordset counts into the colls dataframe.  This file was created by
  # running queries stored in colls$recordsetQuery for each coll that has them.
  collUuidsToCounts <-
    read.csv(collsRsetCountsCsvPath, stringsAsFactors = F)
  
  colls <- merge(
    x = colls,
    y = collUuidsToCounts[, c("collection_uuid", "size")],
    by = "collection_uuid",
    all.x = T
  )
  
  return(colls)
}

#' Query the iDigBio search API for the/each item in the list represented by
#' @param rsqString
#' 
#' @return 0 if rsqString is NA, otherwise the results of the call
#' @examples 
#' "{\"recordset\":\"5ace330b-5888-4a46-a5ac-e428535ed4f3\"}"
#' "{\"recordset\":[\"e48bb88f-9594-461e-8230-522a3a5572fe\"]}"
#' "{\"recordset\":\"7450a9e3-ef95-4f9e-8260-09b498d2c5e6\",\"collectioncode\":\"ECON\"}"
#' "[{\"recordset\":\"e40bcf8a-0d64-4d88-a02d-fa047b195e8b\"},{\"recordset\":\"5975bbda-cd92-4084-8a09-ce1e28e6164f\",\"institutioncode\":\"osac\"}]"
#' "{\"recordset\":[\"2eb8ff2f-4826-4fc3-be68-22d805bcae88\",\"d6c6e57a-4ccb-4707-b87d-c91d01d6aa42\",\"0bc60df1-a162-4173-9a73-c51e09031843\"]}"
fetchMissingCount <- function(rsqString) {
  
  if (is.na(rsqString)) {
    return(0)
  }
  
  # Make singleton json objects into lists to eliminate edge case
  if (substring(rsqString, 1, 1) != "[") {
    rsqString <- paste0("[", rsqString, "]")
  }
  
  # Iterate over list of queries, calling search API on each
  countsList <- c()
  rqList = fromJSON(rsqString, simplifyVector = F)
  for (i in 1:length(rqList)) {
    queryCounts <-
      idig_count_records(rq = rqList[[i]])
    countsList <- c(countsList, queryCounts)
  }
  return(sum(countsList))
}

#' Count the <item> elements in the <rss> for iDigBio's IPT as saved to a file.
loadIDigTotals <- function() {
  
  #feed <- getFeedFromUrl("https://ipt.idigbio.org/rss.do")
  feed <- getFeedFromFile(idbRssPath)
  length(feed$items)
}

#' Count the <item> elements in the <rss> for iDigBio's IPT as saved to a file.
loadVertNetTotals <- function() {
  
  #feed <- getFeedFromUrl("http://ipt.vertnet.org:8080/ipt/rss.do")
  feed <- getFeedFromFile(vertNetRssPath)
  length(feed$items)
}

#' Return a dataframe of all iDigBio recordsets as read from a file.
#' 
#' @return dataframe of fields data.logo_url, data.eml_link, indexTerms.name,
#' indexTerms.publisher (i.e. uuid)
loadRecordsets <- function() {

  # This call will return about 1600 records as of 2022
  # rsets <-
  #   fromJSON("https://search.idigbio.org/v2/search/recordsets?limit=3000",
  #            flatten = T)$items
  
  rsets <- fromJSON(file(idbApiRsetsJsonPath),
                    flatten = T)$items
  
  rsets <- rsets %>%
    select(data.logo_url, data.eml_link, indexTerms.name, indexTerms.publisher)

  return(rsets)
}

#' Read in a file of recordset rows from Postgres that have bptj been slated for
#' ingestion but not yet processed and discovered by the ingest crawler within
#' 360 days of the time of export.  Omit VertNet recordsets from result.
#' 
#' @return dataframe: name, publisher_uuid, file_link, first_seen, pub_date
loadFreshRecordsets <- function() {
  
  # This is the old file, it is stil up but has not been updated since 2018
  # n <-getURL(
  #   "https://www.idigbio.org/sites/default/files/internal-docs/AC/fresh-recordsets-report.tsv",
  #   ssl.verifypeer = TRUE
  #   )
  
  f <- file(freshRsetsCsvPath)
  rsets <- read.csv(f, stringsAsFactors = F)

  rsets <- rsets %>%
    select(name, publisher_uuid, file_link, first_seen, pub_date) %>%
    filter(!publisher_uuid == VertNetUuid)
  
  return(rsets)
}

#' Read in a file of all publishers as saved from a query to the iDigBIo search
#' API.
#' 
#' @return dataframe, with fields uuid, data.name, and data.rss_url
loadPublishers <- function() {
  
  # pubs <- fromJSON(
  #   paste0(
  #     "https://search.idigbio.org/v2/search/publishers"),
  #   flatten = T)$items
  
  pubs <- fromJSON(
    file(idbApiPubsJsonPath), flatten=T)$items
  
  pubs <- pubs %>% select(uuid, data.name, data.rss_url)
  return(pubs)
}

#' Fetch the publisher rss and parse it for the web master's name.
#' 
#' @return feed/header/webMaster
fetchPublisherWebmasterByRssUrl <- function(rssUrl) {
  feed <- getFeedFromUrl(rssUrl)
  return(feed$header$webMaster)
}

#' Scrape the github repo wiki for menu items.  Currently (2022), these are the
#' "Home", "About the Dashboard", and "Dashboard Architecture" links on the
#' right nav pane of the wiki, minus the "Home" link
#' 
#' @return a list of menuSubItem objects
loadDashboardWikiMenuItems <- function(wikiUrl) {

  wikiUrl <- "https://github.com/iDigBio/adbc-data-dashboard/wiki"
  path <- "//*[@id=\"wiki-pages-box\"]/div/div[2]/ul"
  
  wPages <- wikiUrl %>%
    read_html() %>%
    html_nodes(xpath = path) %>%
    html_nodes("li") %>%
    html_text() %>%
    gsub("\n", "", .) %>%
    trimws()
  
  wLinks <- wPages %>% gsub(" ", "-", .)
  
  # Store the links with mappings to their labels in a dataframe "mItems".
  # Drop the "Home" link.
  mItems <-
    data.frame(pages = wPages, links = wLinks) %>%
    rowwise() %>%
    mutate(links = paste0(wikiUrl, "/", links)) %>%
    filter(!pages == "Home")
  
  # Now make the dataframe into a list of menuSubItems
  mItems <-
    apply(mItems, 1, function(row) {
      menuSubItem(row[["pages"]], href = row[["links"]])
    })
}

#' Scrape the idigbio wiki Data Ingest Report for a list of collections and
#' dates submitted, accepted, and ingested. Not currently used in this app.
#' 
#' @return dataframe with fields Redmine/Data Source, Contact, Submitted,
#' Accepted, Ingested
loadRecentDatasetsFromWiki <- function() {
  
  reportUrl <- "https://www.idigbio.org/wiki/index.php/Data_Ingestion_Report"

  rsets <- read_html("data/Data_Ingestion_Report-20221213.htm")
  
  # Create an empty dataframe with columns named by these html table headings
  # (Some past years also have columns for the number of datasets/organisms)
  cols <- c("Redmine/Data Source", "Contact", "Submitted", "Accepted", "Ingested")
  no_rows <- matrix(nrow = 0, ncol = length(cols))
  colnames(no_rows) <- cols
  rsets <- data.frame(no_rows)
  
  # Append each year's table to the bottom of the dataframe
  for (year in 2014:2022) {
    path <-
      paste0("//span[@id='Data_Ingested_During_", year, "']/following::table[1]")
    
    new_rows <- html_element(h, xpath=path) %>%
      html_table(convert = F) %>%
      select(all_of(cols))
    
    rsets <- rbind(rsets, new_rows)
  }

  # The ingestion queue follows the same pattern, but with id="Ingestion_queue"
  path <- "//span[@id='Ingestion_queue']/following::table[1]"
  
  new_rows <- html_element(h, xpath=path) %>%
    html_table(convert = F) %>%
    select(all_of(cols))
  
  rsets <- rbind(rsets, new_rows)
  
  return(rsets)
}

#' Take  dataframe of publishers as loaded from idb_api_publishers.json.  Fetch
#' the rss urls for all the publishers to get a list of their recordsets.  Not
#' currently used in this application but I'll keep it here for reference.  This
#' dataframe it generates lists about twice as many recordsets as we have
#' ingested and took about 10 min on account of long default RCurl timeouts.
fetchRecordsetsFromPublishers <- function(publishers) {
  # Create an empty dataframe.
  cols <- c(
    "name", "publisher_uuid", "file_link", "pub_date"
  )
  no_rows <- matrix(nrow = 0, ncol = length(cols))
  colnames(no_rows) <- cols
  rsets <- data.frame(no_rows)
  
  # Iterate over publishers 
  for (i in 1:nrow(publishers$items)) {
    
    # Fetch the rss
    pubUuid <- publishers$items[i, "uuid"]
    print(paste("working on ", pubUuid))
    
    feed <- NULL
    tryCatch({
      rssUrl <- publishers$items[i, "data.rss_url"]
      print(rssUrl)
      feed <- getFeedFromUrl(rssUrl)
    },
    error=function(cond) {
      feed <- NULL
    })
    if (is.null(feed)) next
    
    # Iterate over the <item>s, extracting title, link, and pubDate,
    # and adding them ass new rows in the dataframe.  Has to be a better way.
    # (Yes, those are strings and not actual NA values; actual NA values
    # confuse rbind.)
    names_list <-sapply(feed$items,
                        function(item) ifelse(is.null(item$title), "NA", item$title))
    pub_uuid_list <- sapply(feed$items,
                            function(item) pubUuid)
    link_list <- sapply(feed$items,
                        function(item) ifelse(is.null(item$link), "NA", item$link))
    pub_date_list <-
      sapply(feed$items,
             function(item) ifelse(length(as.character(item$pubDate)) == 0, "NA", as.character(item$pubDate)))
    
    rsets <- rbind(rsets, data.frame(
      name = names_list,
      publisher_uuid = pub_uuid_list,
      file_link = link_list,
      pub_date = pub_date_list
    ))
  }
  # Remove any rows containing the string "NA" 
  rsets <- filter(rsets, name != "NA" & publisher_uuid != "NA" & file_link != "NA" & pub_date != "NA")
  return(rsets);
}