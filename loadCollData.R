library(rvest)
library(dplyr)
library(jsonlite)
library(ridigbio)

loadCollData <- function() {
  # This is the old file, which was updated manually until four years ago when that person left
  idigbio_colls_url <- "http://idigbio.github.io/idb-us-collections/collections.json"

  # This is where the data lives now; it is managed by GBIF but we have our own API call
  gbif_colls_url <- "https://api.gbif.org/v1/external/idigbio/collections"

  # Let's use the static file we downloaded from gbif
  f <- file("data/sample_gbif_colls.json")
  
  colls <- fromJSON(f) %>% select("institution", "collection", "lat", "lon", "collection_uuid",
                                  "collection_url", "recordsets", "recordsetQuery", "UniqueNameUUID")
  
  # Substitute null for empty strings in recordsetQuery and UniqueNameUUID;
  colls <- cleanCollections(colls)
  
  # Add funding type field "type" w/ value in {"Publishing data",
  # "Unfunded collections (not publishing)", "Funded participants (not publishing)"}
  # Depends on recordsetQuery and UniqueNameUUID
  colls <- addFundingType(colls)
  
  # Add a column "size" containing the number of records in the collection
  colls <- loadRecordCounts(colls) 
  
  # Add a column "coll_portal_url"
  colls <- addCollPortalUrl(colls)
  
  # Add an idx column; this is for looking up the row from a click event
  colls <- cbind(colls, idx = as.numeric(row.names(colls)))

  return(colls)
}

# Convert empty string values for "recordsetQuery" and UniqueNameUUID to null
# values.  I don't think this happens in GBIF data, but we'll make sure.
cleanCollections <- function(colls) {
  # This test prevents an error in assigning a replacement with 1 row to
  # a slice with no rows
  hasEmptyRsq <- sum(colls$recordsetQuery %in% "") > 0
  if (hasEmptyRsq) {
    colls[colls$recordsetQuery %in% "", ]$recordsetQuery <- NA
  }
  hasEmptyUnu <- sum(colls$UniqueNameUUID %in% "") > 0
  if (hasEmptyUnu) {
    colls[colls$UniqueNameUUID %in% "", ]$UniqueNameUUID <- NA
  }
  return(colls)  
}


addFundingType <- function(collsJson) {
  # Create a "type" field to indicate whether the collection is published
  collsJson <- cbind(collsJson, type="")

  # If there is a recordsetQuery value, we have data from these collections
  collsJson[!is.na(collsJson$recordsetQuery), ]$type <- "Publishing data"
  
  # Otherwise we do not, assume they are unfunded
  collsJson[is.na(collsJson$recordsetQuery), ]$type <-
    "Unfunded collections (not publishing)"
  # But if there is a non-empty UniqueNameUUID, assume they are funded, just not publishing
  collsJson[is.na(collsJson$recordsetQuery) &
              !is.na(collsJson$UniqueNameUUID), ]$type <-
    "Funded participants (not publishing)"
  return(collsJson)
}

addCollPortalUrl <- function(collsJson) {
  # Add a coll_url column containing a collection link
  collsJson %>%
    mutate(coll_portal_url = paste0(
      "https://www.idigbio.org/portal/collections/",
      gsub("urn:uuid:", "", collection_uuid)
    ))
}



# Add a column "size" to indicate how many records in the collection,
# allowing for collections to be composed of multiple record sets.
# Actually, the converse is also true, but in those cases, we trust
# the record set query will select only the relevant records, e.g. the query
# "{\"recordset\":\"7450a9e3-ef95-4f9e-8260-09b498d2c5e6\",\"collectioncode\":\"ECON\"}"
loadRecordCounts <- function(colls) {
  # For now, just use a static file
  colls <- loadRecordCountsFromFile(colls)
  
  # But augment it with results counts from the search API
  # when the file lacks data for the collection
  colls <- updateMissingCounts(colls)
  
  return(colls)
}

# This will merge counts into colls from a static file, which may not have data
# for every collection.  The merge will put nulls in the "size"field for those.
loadRecordCountsFromFile <- function(colls) {
  collUuidsToCounts <- read.csv("data/coll_uuid_to_counts.csv", stringsAsFactors = F)
  df <- merge(
    x = colls,
    y = collUuidsToCounts[, c("collection_uuid", "size")],
    by = "collection_uuid",
    all.x = T
    )
  
  return(df)
}

# If colls has null in its "size" field, do a query based on its recordsetQuery
# field.  If that field doesn't exist, set size to 0.
updateMissingCounts <- function(colls) {
  for (i in seq_along(colls$recordsetQuery)) {

    recs <- colls$size[i]
    if (! is.na(recs)) {
      next
    }
    
    rsq <- colls$recordsetQuery[i]
    if (is.na(rsq)) {
      colls$size[i] <- 0
    }
    else {
      if (is.data.frame(fromJSON(rsq))) {
        colls$size[i] <- getCount(rsq)
      }
      else {
        colls$size[i] <- idig_count_records(fromJSON(rsq))
      }
    }
  }
  return(colls)
}

getCount <- function(recordQueries) {
  counts_list <- c()
  rq_list = fromJSON(recordQueries, simplifyVector = F)
  for (i in 1:length(rq_list)) {
    query_counts <-
      idig_count_records(rq = rq_list[[i]])
    counts_list <- c(counts_list, query_counts)
  }
  
  sum(counts_list)
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

countItemsInRss <- function(rssUrl) {
  # For now, we'll just hard-code these
  # We could read them from sample files sample_idigbio_rss.xml and
  # sample_vertnet_rss.xml
  
  # Return this value if we are fetching rss
  
}

# Data had been here:
# https://www.idigbio.org/sites/default/files/internal-docs/AC/datasets_new_last360days.txt

# Presumably this is intended to be a list of record sets that have been slated for ingestion
# but not yet processed.  This data now lives in a different format on a manually
# updated wiki page here: https://www.idigbio.org/wiki/index.php/Data_Ingestion_Report
loadNewDatasetsData <- function() {
  # n <-
  #   getURL(
  #     "https://www.idigbio.org/sites/default/files/internal-docs/AC/fresh-recordsets-report.tsv",
  #     ssl.verifypeer = TRUE
  #   )
  f <- file("data/fresh-recordsets-report.old.tsv")
  tsv <-
    read.csv(
      f,
      stringsAsFactors = F,
      sep = "\t",
      header = F
    )
  names(tsv) <-
    c(
      'uuid',
      'name',
      'publisher_uuid',
      'file_link',
      'first_seen',
      'pub_date',
      'file_harvest_date'
    )
  
  # Do not include record sets published by VertNet for some reason
  vertNetUuid <- "e699547d-080e-431a-8d9b-3d56e39808f0"
  tsv <-
    tsv %>% select(name:pub_date) %>% filter(!publisher_uuid == vertNetUuid)
  return(tsv)
}

# Presumably this is a list of record sets actually ingested recently.  This data is
# also in Data_Ingestion_Report these days
loadRecentDatasetsData <- function() {
  # m  <-
  #   getURL(
  #     "https://www.idigbio.org/sites/default/files/internal-docs/AC/datasets_new_last360days.txt",
  #     ssl.verifypeer = TRUE
  #   )
  f <- file("data/datasets_new_last360days.old.txt")
  tsv <-
    read.csv(f, stringsAsFactors = F, sep = "\t") %>% select(name:pub_date)
  return(tsv)
}

# For now, use static list of recordsets
# https://search.idigbio.org/v2/search/recordsets?limit=3000
apiDatasets <- fromJSON(file("data/search_api_recordsets.json"), flatten = T)$items
loadApiDatasetsData <- function() {
  return(apiDatasets)
}

loadNewAndRecentDatasetsData <- function() {
  newDatasets <- loadNewDatasetsData()
  recentDatasets <- loadRecentDatasetsData()
  
  # Append recently ingested data sets to the list of un-ingested data sets, removing dupes and VertNet
  newRows <- setdiff(newDatasets, recentDatasets)
  
  # Do not include record sets published by VertNet for some reason
  vertNetUuid <- "e699547d-080e-431a-8d9b-3d56e39808f0"
  newDatasets <-
    rbind(recentDatasets, newRows) %>% filter(!publisher_uuid == vertNetUuid)
  
  # List un-deleted ingested record sets from Postgres
  # For the time being, read this from a static file.
  apiDatasets <- loadApiDatasetsData()
  
  # Remove record sets that match a file link in the Postgres record set file links
  newDatasets <-
    newDatasets[!newDatasets$file_link %in% apiDatasets$indexTerms.indexData.link, ]
  
}

urlForPublisher <- function(publisherUuid) {
  paste0(
    "https://search.idigbio.org/v2/search/publishers?pq={%22uuid%22:%22",
    publisherUuid,
    "%22}"
  )
}

# For now, use static list of publishers
publishers <- fromJSON(file("data/api_publishers.json"), flatten = T)

publisherByUuid <- function(pubUuid) {
  filt <- publishers$items$indexTerms.uuid == pubUuid
  if (sum(filt) == 1) {
    matched <- publishers$items %>% filter(filt)
    return(matched)
  }
  else {
    return(NULL)
  }
}

publisherNameByUuid <- function(pubUuid) {
  p <- publisherByUuid(pubUuid)
  p$indexTerms.name
}

publishserRssUrlByUuid <- function(pubUuid) {
  publisher <- publisherByUuid(pubUuid)
  rssUrl <- publisher$data.rss_url
}

fetchPublisherWebmasterByUuid <- function(pubUuid) {
  rssUrl <- publishserRssUrlByUuid(pubUuid)
  feed <- getFeedFromUrl(rssUrl)
  return(feed$header$webMaster)
}

createPublisherSummary <- function(newDatasets) {
  pubSummary <- plyr::count(newDatasets, "publisher_uuid") %>%
    rowwise() %>%
    mutate(publisher = publisherNameByUuid(publisher_uuid)) %>%
    select(publisher, freq) %>%
    arrange(desc(freq))
  
  names(pubSummary) <- c("Publisher", "New Datasets")
  return(pubSummary)
}

# Return a named list of publisher uuids sorted in order
# of decreasing number of new datasets published
getPublisherChoiceList <- function(newDatasets) {
  p <- plyr::count(newDatasets, "publisher_uuid") %>%
    arrange(desc(freq)) %>%
    rowwise() %>%
    mutate(publisher = publisherNameByUuid(publisher_uuid)) %>%
    select(publisher, publisher_uuid)
  choices <- p$publisher_uuid
  names(choices) <- p$publisher
  return(choices)
}

# Select Symbiota record set uuids on the basis of having "collicon" in data.logo_url field
selectSymbiotaRecordsets <- function(apiDatasetData) {
  apiDatasetData[grepl("collicon", apiDatasetData$data.logo_url), ]
}

# Select collections on the basis of having one of these record set uuid in the recordsets field
selectCollectionsByRSUuid <- function(collData, rsUuids) {
  collData %>% filter(recordsets %in% rsUuids) 
}

countSymbiotaColls <- function(collData) {
  # ProgreSQL un-deleted recordset objects as published via the search API
  apiDatasetData <- loadApiDatasetsData()
  # It's a Symbiota recordset if it has "collicon" data.logo_url
  symbiotaRSUuids <- selectSymbiotaRecordsets(apiDatasetData)$uuid
  # It's a Symbiota collection if there is a Symbiota recordset associated with the collection
  symbiotaColls <- selectCollectionsByRSUuid(collData, symbiotaRSUuids)
  # Count the rows
  nrow(symbiotaColls)
}

countSymbiotaDatasets <- function() {
  # ProgreSQL un-deleted recordset objects as published via the search API
  apiDatasetData <- loadApiDatasetsData()
  # It's a Symbiota recordset if it has "collicon" data.logo_url
  symbiotaRecordsets <- selectSymbiotaRecordsets(apiDatasetData)
  # Count rows 
  nrow(symbiotaRecordsets)
}

# Select Specify record set uuids on the basis of having "specify" in data.eml_link field
selectSpecifyRecordsets <- function(apiDatasetData) {
  apiDatasetData[grepl("specify", apiDatasetData$data.eml_link), ]
}

# Select collections on the basis of having a Specify record set uuid in the recordsets field
countSpecifyColls <- function(collData) {
  # ProgreSQL un-deleted recordset objects as published via the search API
  apiDatasetData <- loadApiDatasetsData()
  # It's a Specify recordset if it has "specify" data.eml_link
  specifyRSUuids <- selectSpecifyRecordsets(apiDatasetData)$uuid
  # It's a Specify collection if there is a Specify recordset associated with the collection
  specifyColls <- selectCollectionsByRSUuid(collData, specifyRSUuids)
  # Count the rows
  nrow(specifyColls)
}

countSpecifyDatasets <- function() {
  # ProgreSQL un-deleted recordset objects as published via the search API
  apiDatasetData <- loadApiDatasetsData()
  # It's a Symbiota recordset if it has "collicon" data.logo_url
  specifyRecordsets <- selectSpecifyRecordsets(apiDatasetData)
  # Count rows 
  nrow(specifyRecordsets)
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
