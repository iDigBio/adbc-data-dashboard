library(rvest)
library(dplyr)
library(jsonlite)
library(ridigbio)

# This file handles all of the RSS parsing
source("rss.R")

# Return institution, collection, lat, lon, collection_uuid, collection_url,
# recordsets, recordsetQuery, UniqueNameUUID from call to GBIF GrSciColl API
loadCollData <- function() {
  # This is the old file, which was updated manually until four years ago
  # idigbio_colls_url <- "http://idigbio.github.io/idb-us-collections/collections.json"

  # This is where the data lives now; it is managed by GBIF but we have our own API call
  gbif_colls_url <- "https://api.gbif.org/v1/external/idigbio/collections"

  # Let's use the static file we downloaded from gbif
  f <- file("data/sample_gbif_colls.json")
  
  colls <- fromJSON(f) %>%
    select("institution", "collection", "lat", "lon", "collection_uuid",
           "collection_url", "recordsets", "recordsetQuery", "UniqueNameUUID")
  
  # Add a column "size" to indicate how many records in the collection.
  
  # This file was created by running the recordsetQuery queries for every
  # collection (1604 of them).
  
  # Note that collection record counts do change from time to time, so the
  # file should be updated regularly.
  collUuidsToCounts <- read.csv("data/coll_uuid_to_counts.csv", stringsAsFactors = F)
  colls <- merge(
    x = colls,
    y = collUuidsToCounts[, c("collection_uuid", "size")],
    by = "collection_uuid",
    all.x = T
  )
  
  return(colls)
}

# Some examples of recordsetQuery values:
# rsq1 <- "{\"recordset\":\"7450a9e3-ef95-4f9e-8260-09b498d2c5e6\",\"collectioncode\":\"ECON\"}"
# rsq2 <- "{\"recordset\":[\"2eb8ff2f-4826-4fc3-be68-22d805bcae88\",\"d6c6e57a-4ccb-4707-b87d-c91d01d6aa42\",\"0bc60df1-a162-4173-9a73-c51e09031843\"]}"
# rsq3 <- "{\"recordset\":\"5ace330b-5888-4a46-a5ac-e428535ed4f3\"}"
# rsq4 <- "[{\"recordset\":\"e40bcf8a-0d64-4d88-a02d-fa047b195e8b\"},{\"recordset\":\"5975bbda-cd92-4084-8a09-ce1e28e6164f\",\"institutioncode\":\"osac\"}]"
# rsq5 <- "[{\"recordset\":\"e73fedf0-90ee-4c6d-88dd-49399878fc54\"},{\"recordset\":\"5975bbda-cd92-4084-8a09-ce1e28e6164f\",\"institutioncode\":\"ansp\"}]"
# rsq6 <- "{\"recordset\":[\"e48bb88f-9594-461e-8230-522a3a5572fe\"]}"

# Query the search API for each item in the list represented by rsqString.
# Return 0 if rsqString is NA.
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

loadIDigTotals <- function() {
  #feed <- getFeedFromUrl("https://ipt.idigbio.org/rss.do")
  feed <- getFeedFromFile("data/sample_idigbio_rss.xml")
  length(feed$items)
}

loadVertNetTotals <- function() {
  #feed <- getFeedFromUrl(http://ipt.vertnet.org:8080/ipt/rss.do")
  feed <- getFeedFromFile("data/sample_vertnet_rss.xml")
  length(feed$items)
}

# this is a list of ProgreSQL un-deleted recordset objects as published via the search API
loadRecordsets <- function() {
  
  # For testing, can use this static list of recordsets
  #rsets <- fromJSON(file("data/search_api_recordsets.json"), flatten = T)$items
  
  rsets <-
    fromJSON("https://search.idigbio.org/v2/search/recordsets?limit=3000",flatten = T)$items
  
  rsets <-
    rsets %>% select(data.logo_url, data.eml_link, indexTerms.name, indexTerms.publisher)

  return(rsets)
}

# This is a list of record sets that have been slated for ingestion but not yet
# processed, according to an exported Postgres query.
loadFreshRecordsets <- function() {
  # n <-
  #   getURL(
  #     "https://www.idigbio.org/sites/default/files/internal-docs/AC/fresh-recordsets-report.tsv",
  #     ssl.verifypeer = TRUE
  #   )
  f <- file("data/fresh-recordsets-last-360-days_20221214.csv")
  csv <-
    read.csv(f, stringsAsFactors = F)

  # Do not include record sets published by VertNet for some reason
  vertNetUuid <- "e699547d-080e-431a-8d9b-3d56e39808f0"
  csv <-
    csv %>% select(name:pub_date) %>% filter(!publisher_uuid == vertNetUuid)
  
  return(csv)
}

publishersByUuid <- function(uuids) {
  fromJSON(
    paste0("https://search.idigbio.org/v2/search/publishers?pq={%22uuid%22:[%22",
           paste0(uuids, collapse = "%22,%22"),
           "%22]}"),
    flatten = T)$items %>%
    select(uuid, data.name, data.rss_url)
}

fetchPublisherWebmasterByRssUrl <- function(rssUrl) {
  feed <- getFeedFromUrl(rssUrl)
  return(feed$header$webMaster)
}


# Scrape the github repo wiki for menu items
# Currently, the "Home", "About the Dashboard", and
# "Dashboard Architecture" links on the right nav pane
# of the below mentioned url, minus the "Home" link
loadDashboardWikiMenuItems <- function(wikiUrl) {
  path <- "//*[@id=\"wiki-pages-box\"]/div/div[2]/ul"
  wikiUrl <- "https://github.com/iDigBio/adbc-data-dashboard/wiki"
  
  wPages <- wikiUrl %>%
    read_html() %>%
    html_nodes(xpath = path) %>%
    html_nodes("li") %>%
    html_text() %>%
    gsub("\n", "", .) %>%
    trimws()
  
  # Convert labels of form "About the Dashboard" to
  # "About-the-Dashboard"
  wLinks <- wPages %>% gsub(" ", "-", .)
  
  # Expand the dash-labels to their anchor hrefs resolvable outside their wiki
  # e.g. https://github.com/iDigBio/adbc-data-dashboard/wiki/About-the-Dashboard/
  # and store the links with mappings to their labels in a dataframe "mItems".
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

# Scrape the idigbio wiki Data Ingest Report for a list
# of collections (? verify w/ Dan) and dates of ingest
# Columns:  redmine/name, contact, date submitted, date ingested
loadRecentDatasetsFromWiki <- function() {
  
  reportUrl <- "https://www.idigbio.org/wiki/index.php/Data_Ingestion_Report"

  h <- read_html("data/Data_Ingestion_Report-20221213.htm")
  
  # Create an empty dataframe with columns named by these html table headings
  # (Some past years also have columns for the number of datasets/organisms)
  cols <- c("Redmine/Data Source", "Contact", "Submitted", "Accepted", "Ingested")
  no_rows <- matrix(nrow = 0, ncol = length(cols))
  colnames(no_rows) <- cols
  df <- data.frame(no_rows)
  
  # Append each year's table to the bottom of the dataframe
  for (year in 2014:2022) {
    path <-
      paste0("//span[@id='Data_Ingested_During_", year, "']/following::table[1]")
    
    new_rows <- html_element(h, xpath=path) %>%
      html_table(convert = F) %>%
      select(all_of(cols))
    
    df <- rbind(df, new_rows)
  }

  # The ingestion queue follows the same pattern, but with id="Ingestion_queue"
}

# This function takes as an argument a dataframe of publishers as loaded from
# api_publishers.json, which was read from the search api list of all publishers.
# The function polls the rss urls for all the publishers to get a list of recordsets.
# It is not called in this application but I'll keep it here for reference.  This
# dataframe it generates lists about twice as many recordsets as we have ingested.
# Took about 10 min to fetch all the data on account of long default RCurl timeouts.
fetchRecordsetsFromPublishers <- function(publishers) {
  # Create an empty dataframe.
  cols <- c(
    "name", "publisher_uuid", "file_link", "pub_date"
  )
  no_rows <- matrix(nrow = 0, ncol = length(cols))
  colnames(no_rows) <- cols
  recsets <- data.frame(no_rows)
  
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
    
    recsets <- rbind(recsets, data.frame(
      name = names_list,
      publisher_uuid = pub_uuid_list,
      file_link = link_list,
      pub_date = pub_date_list
    ))
  }
  # Remove any rows containing an "NA" 
  recsets <- filter(recsets, name != "NA" & publisher_uuid != "NA" & file_link != "NA" & pub_date != "NA")
  return(recsets);
}