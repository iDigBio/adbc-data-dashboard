library(shiny)
library(shinydashboard)
library(plotly)

# This file handles all of the RSS parsing
source("rss.R")

source("loadCollData.R")

# This script uses the iDigBio collections JSON endpoint:
# http://idigbio.github.io/idb-us-collections/collections.json
# to generate counts of records from the collections in the catalog,
# i.e. collections in scope for ADBC
#####################################################################

# Fetch our dataframe from the collections endpoint
# This data comes from our own resource in GBIF's Registry API
# https://api.gbif.org/v1/external/idigbio/collections
colls <- loadCollData()

# Substitute null for empty strings in recordsetQuery and UniqueNameUUID;
colls$recordsetQuery <- na_if(colls$recordsetQuery, "")
colls$UniqueNameUUID <- na_if(colls$UniqueNameUUID, "")

# Funding type  will be used in the map to select layers.
publishing <- "Publishing data"
unfunded <- "Unfunded collections (not publishing)"
funded <- "Funded participants (not publishing)"

fundingTypes <- c(funded, publishing, unfunded)

# If there is a recordsetQuery value, they're publishing.
# Overwrite this value if appropriate in the next steps.
colls <- cbind(colls, fundingType = publishing)

colls[is.na(colls$recordsetQuery) & # No recordsetQuery: not publishing.
        is.na(colls$UniqueNameUUID), ]$fundingType <- unfunded

colls[is.na(colls$recordsetQuery) & # No UniqueNameUUID: not funded.
        ! is.na(colls$UniqueNameUUID), ]$fundingType <- funded

# Use ridigbio to query the search API to obtain record counts for
# collections that didn't have counts in the static files
colls[is.na(colls$size), ]$size <-
  sapply(colls[is.na(colls$size), ]$recordsetQuery, fetchMissingCount)

# Add a column "coll_portal_url" generated from the collection_uuid value
colls <- cbind(colls,
               coll_portal_url = paste0(
                 "https://www.idigbio.org/portal/collections/",
                 gsub("urn:uuid:", "", colls$collection_uuid)
               ))

# Add an idx column; this is for looking up the row from a click event
colls <- cbind(colls, idx = as.numeric(row.names(colls)))


####################################################
# Number of specimen-based datasets published by iDigBio IPT
# Number of specimen-based datasets published by VertNet IPT
# iDigBio may or may not have ingested these yet

# Number of <item> elements in the iDigBio IPT rss feed
iDigBioIptDatasetCount <- loadIDigTotals()
# Number of <item> elements in the VertNet IPT rss feed
vertNetIptDatasetCount <- loadVertNetTotals()

# Known US Collections: this counts all the collections found in our collections
# resource on the GBIF Registry.  https://github.com/gbif/registry/issues/229
# GBIF lists a collection under the /idigbio/collections url if the collection
# has a machine tag "CollectionUUID" in the iDigBIo namespace "iDigBIo.org"
# https://github.com/gbif/registry/blob/dev/registry-persistence/src/main/resources/org/gbif/registry/persistence/mapper/collections/external/IDigBioMapper.xml
knownUSCollectionsCount <- length(colls$collection)

# If the collection record has a UniqueNameUUID, we say it's funded
# https://github.com/iDigBio/idb-us-collections
# "UniqueNameUUID this property is used by iDigBio staff to maintain a
# hierarchical relationship between institutions and collections"
adbcFundedCollectionsCount <- sum(colls$fundingType == funded)

adbcFundedInstitutionsCount <- length(unique(colls$UniqueNameUUID))

collectionsProvidingDataCount <-
  length(colls[colls$fundingType == publishing, ]$collection)

# This is the sum total of contributed data from US Collections
totalAdbcSpecimenRecords <- sum(colls$size)

############################
# From our recordset data, can we tell how many come from Specify and Symbiota?

# Depends on search API recordset data.logo_url and coll data recordsets column
symbiotaDatasetCount <- countSymbiotaDatasets()

# Depends on search API recordset data.eml_link and coll data recordsets column
specifyCloudDatasetCount <- countSpecifyDatasets()


#####################################################
# We need to know how many funded ADBC institutions
# let's use the UniqueNameUUID

#adbcInst <- length(unique(colls$UniqueNameUUID))


#####################################################
# Build a report for NEW un-ingested data from known
# publishers (except VertNet).  Table returned has
# columns name, publisher_uuid, file_link, first_seen,
# and 'pub_date'.
newDatasets <- loadNewAndRecentDatasetsData()

# Retrieve publisher names from search.idigbio for each publisher uuid
# in our new datasets file;  sort the publishers in decreasing order of
# new data sets.
newDatasetsByPublisher <- createPublisherSummary(newDatasets)

# This is a named list of publisher uuids, using the publisher names.
# It is used as the list of choices to select from when viewing the
# table of new datasets by publisher.  It is ordered by decreasing
# number of new datasets published by the publisher.
publisherChoices <- getPublisherChoiceList(newDatasets)


############################################
# Scrape the github repo wiki for menu items
wikiUrl <- "https://github.com/iDigBio/adbc-data-dashboard/wiki"
dashboardWikiMenuItems <- loadDashboardWikiMenuItems(wikiUrl)


#################################
# Shiny Dashboard
#################################

# Define the UI
ui <- dashboardPage(
  skin = "yellow",
  
  ### Title ###
  dashboardHeader(title = "ADBC Data Dashboard"),
  
  ### Left Sidebar ###
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        "Data Publisher Reports",
        icon = icon("th"),
        tabName = "reports",
        badgeLabel = "new",
        badgeColor = "green"
      ),
      menuItem(
        "Collection Reports",
        icon = icon("th"),
        tabName = "collections",
        badgeLabel = "new",
        badgeColor = "green"
      ),
      
      hr(),
      
      menuItem("ADBC Data Dashboard Wiki", href = wikiUrl),
      
      dashboardWikiMenuItems
    )
  ),
  dashboardBody(tabItems(
    
    ### Home/Dashboard Tab ###
    tabItem(
      tabName = "dashboard",
      
      ### Collections Counts ###
      fluidRow(
        infoBox(
          "Known US Collections",
          knownUSCollectionsCount,
          icon = icon("university"),
          fill = TRUE
        ),
        infoBox(
          "ADBC Funded Collections",
          adbcFundedCollectionsCount,
          icon = icon("archive"),
          fill = TRUE
        ),
        infoBox(
          "ADBC Funded Institutions",
          adbcFundedInstitutionsCount,
          icon = icon("university"),
          fill = TRUE
        )
      ),
      fluidRow(
        column(width = 12,
               fluidRow(
                 infoBox(
                   "Collections Providing Data",
                   collectionsProvidingDataCount,
                   icon = icon("database"),
                   fill = TRUE
                   ),
                 infoBox(
                   "Total ADBC Specimen Records",
                   prettyNum(totalAdbcSpecimenRecords, big.mark = ","),
                   icon = icon("barcode"),
                   fill = TRUE
                   )))
        ),
      
      ### The Map ###
      fluidRow(
        column(width = 12,
               plotlyOutput("plot1"),
               verbatimTextOutput("coll_summary"))
      ),
      
      ### Dataset Counts ###
      fluidRow(
        infoBox(
          "iDigBio IPT Datasets",
          iDigBioIptDatasetCount,
          icon = icon("database"),
          fill = TRUE
        ),
        infoBox(
          "VertNet IPT Datasets",
          vertNetIptDatasetCount,
          icon = icon("database"),
          fill = TRUE
        ),
        infoBox(
          "Symbiota Datasets",
          symbiotaDatasetCount,
          icon = icon("database"),
          fill = TRUE
        ),
        infoBox(
          "SpecifyCloud Datasets",
          specifyCloudDatasetCount,
          icon = icon("database"),
          fill = TRUE
        )
      )
    ),
    
    ### Reports Tab ###
    tabItem(
      tabName = "reports",
      tabsetPanel(
        type = "tabs",
        
        ### New Dataset Counts by Publisher List ###
        tabPanel("Summary",
                 fluidRow(
                   box(width = 12,
                       DT::dataTableOutput('newdatasetReport')))
                 ),
        ### New Datasets List Per Publisher Detail ###
        tabPanel("Table",
                 fluidRow(
                   box(width = 12,
                       
                       ### Select a Publisher ###
                       selectInput("dataset", "Choose a Publisher:", choices = publisherChoices),
                       verbatimTextOutput("datasetRSS"),
                       
                       ### Download Button ###
                       downloadButton("downloadData", "Download"))
                   ),
                 fluidRow(
                   box(width = 12, DT::dataTableOutput('newdatasets')))
                 )
        )
      )
    # This is where we will display individual collection contributions (Glossy reports)
    #,
    #tabItem(tabName = "collections",
    #        tabsetPanel(type = "tabs",
    #                    tabPanel("Summary",
    #                             fluidRow(plotOutput('collHist')),
    #                             fluidRow(plotOutput('collTaxa'))
    #                    ),
    #                    tabPanel("Data",
    #                             DT::dataTableOutput('colltable')
    #                    )
    #        )
    #
    #)
  ))
)


# Define the server code
server <- function(input, output, session) {

  ### Collections Map ###
  
  # This is an outline-based Plotly "geo map"
  # https://plotly.com/r/map-configuration/
  output$plot1 <- renderPlotly({
    # Map it!
    # geo styling
    g <- list(
      # Uncomment these two lines to show the US
      scope = "usa",
      projection = list(type = "albers usa"),

      # Uncomment this line to show the world
      # Note that we have collections in Guam.
      #scope = "world",
      
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    level.order <- fundingTypes
    
    ### Map point markers and mouseover ###
    p <-  plot_geo(colls, lat = ~ lat, lon = ~ lon) %>%
      add_markers(
        text = ~ paste0(institution, "<br /> (", collection, ")"),
        hoverinfo = 'text',
        color = ~ fundingType,
        symbol = I("circle"),
        size = I(8),
        key = ~ idx
      ) %>%
      layout(title = 'U.S. Collections Contributing Data to iDigBio', geo = g)
    p
    
  })
  
  ## Clicking points on the map will display coll summary below the map
  output$coll_summary <- renderText({
    d <- event_data("plotly_click")
    if (length(d) == 0) { "Click on a point to view data" }
    else {
      c <- colls[d$key, ]
      paste(c$institution, c$collection, c$coll_portal_url, sep = "\n")
    }
  })
  
  ## Data reports output table
  output$newdatasetReport <- DT::renderDataTable(DT::datatable(
    newDatasetsByPublisher,
    options = list(paging = FALSE),
    rownames = FALSE
  ))
  
  ## Reporting table subset
  df_subset <- reactive({
    uuidP <- input$dataset
    a <-
      subset(newDatasets, publisher_uuid == uuidP) %>% select(-publisher_uuid)
    tryCatch({
      a$contact <- fetchPublisherWebmasterByUuid(uuidP)
    },
    error = function(cond) {
      return(a$contact <- NULL)
    },
    warning = function(cond) {
      return(a$contact <- NULL)
    },
    finally = {
      a
    })
    return(a)
  })
  output$newdatasets <- DT::renderDataTable(DT::datatable(
    df_subset(),
    options = list(paging = FALSE),
    rownames = FALSE
  ))
  df_rss <- reactive({
    uuidP <- input$dataset
    rssUrl <- publishserRssUrlByUuid(uuidP)
    return(rssUrl)
  })
  output$datasetRSS <- renderPrint({
    df_rss()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub(" ", "_", input$dataset), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df_subset(), file, row.names = FALSE)
    }
  )
  
  # Collection Histograms
  # output$collHist <- renderPlot({
  #
  #   tmp <- df %>% filter(count>0) %>% select(count,collection_uuid) %>% filter(count<=100000)
  #   histBreaks <- hist(tmp$count, main = paste0("Collection Record Counts \n (Collection size less than 100,000 published records) \n n = ",nrow(tmp)),xlab = "Collections Size (Records)",col = "grey",xaxt="no")
  #   axis(1, at=seq(0,100000,by=20000),labels = c("0","20,000","40,000","60,000","80,000","100,000"))
  #   d <- density(tmp$count)
  #   lines(x = d$x, y = d$y * length(tmp$count) * diff(hist(tmp$count,plot = F)$breaks)[1], lwd = 2,col="red")
  # })
  # output$colltable <- DT::renderDataTable(DT::datatable(rsDF))
  # output$collTaxa <- renderPlot({
  #   taxa <- "arthropoda"
  #   taxaDF <- rsDF[grepl(taxa,rsDF$type,ignore.case = T),]
  #   histBreaks <- hist(taxaDF$count, main = paste0("Collection Record Counts \n (",taxa,") \n n = ",nrow(taxaDF)),xlab = "Collections Size (Records)",col = "grey",xaxt="no")
  #   #axis(1, at=seq(0,100000,by=20000),labels = c("0","20,000","40,000","60,000","80,000","100,000"))
  #   d <- density(taxaDF$count)
  #   lines(x = d$x, y = d$y * length(taxaDF$count) * diff(hist(taxaDF$count,plot = F)$breaks)[1], lwd = 2,col="red")
  # })
  
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)