library(shiny)
library(shinydashboard)
library(jsonlite)
library(plotly)
library(rvest)
library(ridigbio)
library(dplyr)

# This file handles all of the RSS parsing
source("rss.R")
source("loadCollData.R")

# This script uses the iDigBio collections JSON endpoint:
# http://idigbio.github.io/idb-us-collections/collections.json
# to generate counts of records from the collections in the catalog,
# i.e. collections in scope for ADBC
#####################################################################

# Fetch our dataframe from the collections endpoint
cc <- loadCollData()

# Funding types: these will be used in the map to select layers on the map
fundingTypes <- unique(cc$type)

####################################################
# Number of specimen-based datasets in the iDigBio IPT
# Number of specimen-based datasets in the VertNet IPT

# Number of <item> elements in the iDigBio IPT rss feed
iDigBioIptDatasetCount <- loadIDigTotals()
# Number of <item> elements in the VertNet IPT rss feed
vertNetIptDatasetCount <- loadVertNetTotals()

# Known US Collections: this is wrong, it includes non-US collections
knownUSCollectionsCount <- length(cc$collection)
adbcFundedCollectionsCount <- length(cc[!cc$UniqueNameUUID == "", ]$collection)
adbcFundedInstitutionsCount <- length(unique(cc[!cc$UniqueNameUUID == "", ]$UniqueNameUUID))
collectionsProvidingDataCount <- length(cc[!is.na(cc$recordsetQuery), ]$collection)

# This is the sum total of contributed data from US Collections
totalAdbcSpecimenRecords <- sum(cc$size)

############################
# From our recordset data, can we tell how many come from Specify and Symbiota?

# Depends on search API recordset data.logo_url and coll data recordsets column
symbiotaDatasetCount <- countSymbiotaDatasets()

# Depends on search API recordset data.eml_link and coll data recordsets column
specifyCloudDatasetCount <- countSpecifyDatasets()


#####################################################
# We need to know how many funded ADBC institutions
# let's use the UniqueNameUUID

#adbcInst <- length(unique(cc$UniqueNameUUID))


#####################################################
# Build a report for NEW un-ingested data from known
# publishers (except VertNet).  Table returned has
# columns name, publisher_uuid, file_link, first_seen,
# and 'pub_date'.
newDatasets <- loadDatasetsData()

# Retrieve publisher names from search.idigbio for each publisher uuid in our new datasets file;
# sort the publishers in decreasing order of new data sets.
newDatasetsByPublisher <- createPublisherSummary(newDatasets)

# Not at all sure what this is, I think it just matches publisher name to publisher uuid
# but in the same order as the publishers appear in newDatasetsByPublisher (i.e., decreasing
# order of number of new datasets)
publisherNameToUuid <- getPublisherSummaryUuids(newDatasets)


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
               verbatimTextOutput("click"))
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
                       selectInput("dataset", "Choose a Publisher:", choices = publisherNameToUuid$publisher),
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
  output$plot1 <- renderPlotly({
    # Map it!
    # geo styling
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    level.order <- fundingTypes
    
    ### Map point markers and mouseover ###
    p <-  plot_geo(cc, lat = ~ lat, lon = ~ lon) %>%
      add_markers(
        text = ~ paste0(institution, "<br /> (", collection, ")"),
        hoverinfo = 'text',
        color = ~ type,
        symbol = I("circle"),
        size = I(8),
        key =  ~ paste(institution, collection, sep = "\n")
      ) %>%
      layout(title = 'U.S. Collections Contributing Data to iDigBio', geo = g)
    p
    
  })
  ## Clicking points on the map will render the data
  output$click <- renderText({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      "Click on a point to view data"
    } else {
      if (d$curveNumber == 0) {
        typeQ <- "Funded participants (not publishing)"
      } else if (d$curveNumber == 1) {
        typeQ <- "Publishing data"
      } else {
        typeQ <- "Unfunded collections (not publishing)"
      }
      fu <- mm[mm$type == typeQ, ]
      row.names(fu) <- NULL
      fu <- fu[d$pointNumber + 1, ]
      paste(fu$institution, fu$collection, fu$hover, sep = "\n")
    }
  })
  ## Data reports output table
  output$newdatasetReport <- DT::renderDataTable(DT::datatable(
    newDatasetsByPublisher,
    options = list(paging = FALSE),
    rownames = FALSE
  ))
  ## Reporting table subset
  # Sample publisher query:
  # https://search.idigbio.org/v2/search/publishers?pq={%22uuid%22:%221c29be70-24e7-480b-aab1-61224ded0f34%22}
  df_subset <- reactive({
    publ_name <- input$dataset
    uuidP <- publisherNameToUuid[publisherNameToUuid$publisher == publ_name, ]$publisher_uuid
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
    publ <- input$dataset
    uuidP <- publisherNameToUuid[publisherNameToUuid$publisher == publ, ]$publisher_uuid
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