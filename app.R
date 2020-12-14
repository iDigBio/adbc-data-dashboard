library(shiny)
library(shinydashboard)
library(jsonlite)
library(plotly)
library(rvest)
library(ridigbio)
library(dplyr)

# This file handles all of the RSS parsing
source("rss.R")

# This script uses the iDigBio collections JSON endpoint:
# http://idigbio.github.io/idb-us-collections/collections.json
# to generate counts of records from the collections in the catalog,
# i.e. collections in scope for ADBC
#####################################################################


# Fetch our dataframe from the collections endpoint
cc <- fromJSON("http://idigbio.github.io/idb-us-collections/collections.json")

# There will likely be many rounds of data cleaning, this is the first step
cc[cc$recordsetQuery=="",]$recordsetQuery <- NA

# For mapping we only really care about a few things here, so let's subset to those
cc$type <- "" ##Create the variable we will store this data in
cc[!is.na(cc$recordsetQuery),]$type <- "Publishing data" ##We have data from these collections
cc[is.na(cc$recordsetQuery),]$type <- "Unfunded collections (not publishing)" ##No data and not ADBC GUID
cc[is.na(cc$recordsetQuery)&!cc$UniqueNameUUID=="",]$type <- "Funded participants (not publishing)" ##ADBC GUID but no data


# Create a hover link for mapping data points
mm <- cc %>% select(institution,collection,collection_url,lat,lon,type,collection_uuid) %>% mutate(hover=paste0("https://www.idigbio.org/portal/collections/",gsub("urn:uuid:", "", collection_uuid)))

# Also, some recordsetQuery values will be multi-valued, 
# this function will handle these edge cases
getCount <- function(rqs){
  m <- c()
  for(ii in 1:length(fromJSON(rqs,simplifyVector = F))){
    y <- idig_count_records(rq=fromJSON(rqs,simplifyVector = F)[[ii]])
    m <- c(m,y)
  } 
  sum(m) 
}

# Time to generate our counts

for(q in seq_along(cc$recordsetQuery)){
  if(is.na(cc$recordsetQuery[q])){ cc$size[q]<- 0}else{
    if(is.data.frame(fromJSON(cc$recordsetQuery[q]))){
      cc$size[q] <- getCount(cc$recordsetQuery[q])
    } else{
      cc$size[q] <- idig_count_records(fromJSON(cc$recordsetQuery[q]))
    }
  }
}

# This is the sum total of contributed data from US Collections
# sum(cc$size) 


####################################################
# Number of specimen-based datasets in the iDigBio IPT
# Number of specimen-based datasets in the VertNet IPT

iDigTotals <- length(getFeed("https://ipt.idigbio.org/rss.do")$items)
VertNetTotals <- length(getFeed("http://ipt.vertnet.org:8080/ipt/rss.do")$items)

#####################################################
# We need to know how many funded ADBC institutions
# let's use the UniqueNameUUID

#adbcInst <- length(unique(cc$UniqueNameUUID))


###################################################
# Build a report for NEW un-ingested data
# from known publishers
# Data are currently here: 
# https://www.idigbio.org/sites/default/files/internal-docs/AC/datasets_new_last360days.txt

n <- getURL("https://www.idigbio.org/sites/default/files/internal-docs/AC/fresh-recordsets-report.tsv", ssl.verifypeer = TRUE)
newDatasetsFile <- read.csv(textConnection(n), stringsAsFactors = F,sep = "\t",header = F) 
m  <- getURL("https://www.idigbio.org/sites/default/files/internal-docs/AC/datasets_new_last360days.txt", ssl.verifypeer = TRUE)
newDatasetsFileOld <- read.csv(textConnection(m),stringsAsFactors = F,sep = "\t") %>% select(name:pub_date)
names(newDatasetsFile) <- c('uuid','name','publisher_uuid','file_link','first_seen','pub_date','file_harvest_date')
newDatasetsFile <- newDatasetsFile %>% select(name:pub_date) %>% filter(!publisher_uuid=="e699547d-080e-431a-8d9b-3d56e39808f0")

## Let's diff the two versions and see what we can make
test <- setdiff(newDatasetsFile,newDatasetsFileOld)
newDatasetsFile <- rbind(newDatasetsFileOld,test) %>% filter(!publisher_uuid=="e699547d-080e-431a-8d9b-3d56e39808f0")

# One last cleaning attempt before building our dataframe
rsData <- fromJSON("https://search.idigbio.org/v2/search/recordsets?limit=3000",flatten = T)$items
newDatasetsFile <- newDatasetsFile[!newDatasetsFile$file_link %in% rsData$indexTerms.indexData.link,]


sumR <- plyr::count(newDatasetsFile,"publisher_uuid") %>% rowwise() %>% mutate(publisher=fromJSON(paste0("https://search.idigbio.org/v2/search/publishers?pq={%22uuid%22:%22",publisher_uuid,"%22}"),flatten = T)$items$indexTerms.name) %>% select(publisher,freq)%>% arrange(desc(freq))

##Need a little test code:
#
#
# sumR <- plyr::count(newDatasetsFile,"publisher_uuid")
# for(i in seq_along(sumR$publisher_uuid)){
#   sumR$publisher[i] <- fromJSON(paste0("http://search.idigbio.org/v2/search/publishers?pq={%22uuid%22:%22",sumR$publisher_uuid[i],"%22}"),flatten = T)$items$indexTerms.name
# }
# sumR <- sumR %>% select(publisher,freq)%>% arrange(desc(freq))

names(sumR) <- c("Publisher","New Datasets")

sumP <- plyr::count(newDatasetsFile,"publisher_uuid") %>% arrange(desc(freq)) %>% rowwise() %>% mutate(publisher=fromJSON(paste0("https://search.idigbio.org/v2/search/publishers?pq={%22uuid%22:%22",publisher_uuid,"%22}"),flatten = T)$items$indexTerms.name) %>% select(publisher,publisher_uuid)


############################
# From our recordset data, can we tell how many come from Specify and Symbiota?
# 

sym <- rsData[grepl("collicon",rsData$data.logo_url),]$uuid
symDF <- cc %>% filter(recordsets %in% sym) %>% filter(!recordsets=="")
symbiotaCols <- nrow(symDF)

specifyCloud <- nrow(rsData[grepl("specify",rsData$data.eml_link),])

##########################################
# This may eventually provide insight
# into individual taxonomic contributions
##########################################
# df <- fromJSON("http://idigbio.github.io/idb-us-collections/collections.json")
# #only collections with data
# df <- df %>% filter(recordsetQuery!="")
# for (i in seq_along(df$collection_uuid)){
#   if(nrow(as.data.frame(fromJSON(df$recordsetQuery[i])))>1 &length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))>1 & !grepl(pattern = "a6e02b78-6fc6-4cb6-bb87-8d5a443f2c2a|271a9ce9-c6d3-4b63-a722-cb0adc48863f|9d8ced48-62c5-4ce0-99e7-a03550c674c0|b000920c-6f7d-49d3-9d0f-2bb630d2e01a|042dbdba-a449-4291-8777-577a5a4045de|9dce915b-3de4-4a7d-a68d-e4c4c15809ce|5386d272-06c6-4027-b5d5-d588c2afe5e5",x = df$recordsetQuery[i])){
#     countVector <- c()
#     typeVector <- c()
#     for(ii in 1:length(fromJSON(df$recordsetQuery[i],simplifyVector = list()))){
#       rqCount <- fromJSON(paste0("http://search.idigbio.org/v2/search/records?rq=",URLencode(
#         substr(toJSON(fromJSON(df$recordsetQuery[i])[ii,]),2,nchar(toJSON(fromJSON(df$recordsetQuery[i])[ii,]))-1)
#       )))$itemCount
#       countVector <- c(countVector,rqCount)
#       rqType <- names(fromJSON(paste0("http://search.idigbio.org/v2/summary/top/records?top_fields=[\"phylum\"]&rq=",URLencode(
#         substr(toJSON(fromJSON(df$recordsetQuery[i])[ii,]),2,nchar(toJSON(fromJSON(df$recordsetQuery[i])[ii,]))-1)
#       )))$phylum)
#       typeVector <-c(typeVector,rqType)
#     }
#     df$count[i] <- sum(countVector)
#     df$type[i] <- paste0(unique(typeVector),collapse = ", ")
#   }else{
#     df$count[i] <- idig_count_records(rq=fromJSON(df$recordsetQuery[i]))
#     df$type[i] <- names(idig_top_records(rq=fromJSON(df$recordsetQuery[i]),top_fields ="phylum" )[[1]])[1]
#   }
#   
# }

##clean messy
#rsDF <- df %>% filter(count>0) %>% select(institution,collection,type,count)

###############################





############################################
# Scrape the github repo wiki for menu items
path <- "//*[@id=\"wiki-pages-box\"]/div/div[2]/ul"
gUrl <- "https://github.com/iDigBio/adbc-data-dashboard/wiki"
wPages <- gUrl %>%
  read_html() %>%
  html_nodes(xpath=path) %>%
  html_nodes("li")%>%
  html_text() %>%
  gsub("\n","",.) %>%
  trimws()
wLinks <- wPages %>% gsub(" ","-",.)
mItems <- data.frame(pages=wPages,links=wLinks) %>% rowwise() %>% mutate(links=paste0("https://github.com/iDigBio/adbc-data-dashboard/wiki/",links)) %>% filter(!pages=="Home")
mItems <- apply(mItems, 1, function(row) {menuSubItem(row[["pages"]],href=row[["links"]])})


#################################
# Shiny Dashboard 
#################################

# Define the UI
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "ADBC Data Dashboard"
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Publisher Reports", icon = icon("th"), tabName = "reports",badgeLabel = "new", badgeColor = "green"),
      menuItem("Collection Reports", icon = icon("th"), tabName = "collections",badgeLabel = "new", badgeColor = "green"),
      hr(),
      menuItem("ADBC Data Dashboard Wiki",href = gUrl),
      mItems
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                infoBox("Known US Collections", length(cc$collection), icon = icon("university"), fill = TRUE),
                infoBox("ADBC Funded Collections", length(cc[!cc$UniqueNameUUID=="",]$collection), icon = icon("archive"), fill = TRUE),
                infoBox("ADBC Funded Institutions", length(unique(cc[!cc$UniqueNameUUID=="",]$UniqueNameUUID)), icon = icon("university"), fill = TRUE)
              ),
              fluidRow(
                column(width=12,
                       fluidRow(
                       infoBox("Collections Providing Data", length(cc[!is.na(cc$recordsetQuery),]$collection), icon = icon("database"), fill = TRUE),
                       infoBox("Total ADBC Specimen Records", prettyNum(sum(cc$size),big.mark = ","), icon = icon("barcode"), fill = TRUE)
                       )
                )
              ),
              fluidRow(
                column(width=12,
                       plotlyOutput('plot1'),
                       verbatimTextOutput("click")
                )
              ),
              fluidRow(
                infoBox("iDigBio IPT Datasets", iDigTotals, icon = icon("database"), fill = TRUE),
                infoBox("VertNet IPT Datasets", VertNetTotals, icon = icon("database"), fill = TRUE),
                infoBox("Symbiota Datasets", symbiotaCols, icon = icon("database"), fill = TRUE),
                infoBox("SpecifyCloud Datasets", specifyCloud, icon = icon("database"), fill = TRUE)
              )
      ),
      tabItem(tabName = "reports",
              tabsetPanel(type = "tabs",
                          tabPanel("Summary", 
                                   fluidRow(box(width = 12, DT::dataTableOutput('newdatasetReport')))
                                   ),
                          tabPanel("Table", 

                                   fluidRow(
                                   box(width = 12,selectInput("dataset", "Choose a Publisher:",choices = sumP$publisher),
                                   verbatimTextOutput("datasetRSS"),
                                   # Button
                                   downloadButton("downloadData", "Download"))),
                                   fluidRow(box(width = 12, DT::dataTableOutput('newdatasets')))
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
    )
  )
)





# Define the server code
server <- function(input, output, session) {

### Collection map  
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
    ##This seems like a hack, but to get the key to be respected, I have to sort the results first
    #mm$type <- factor(mm$type)
    level.order <- c("Funded participants (not publishing)","Unfunded collections (not publishing)","Publishing data")
    mm <- mm[order(factor(mm$type, levels = level.order)),]
    p <-  plot_geo(mm,lat = ~lat, lon = ~lon)%>%
       add_markers(
         text = ~paste0(institution,"<br /> (",collection,")"),
         hoverinfo = 'text',
         #I may want to group these according to taxa
         color = ~type, 
         symbol = I("circle"), size = I(8),
         key =~paste(institution,collection,sep="\n")
       ) %>%
      layout(
        title = 'U.S. Collections Contributing Data to iDigBio', geo = g
      )
    p
    
  })
  ## Clicking points on the map will render the data
  output$click <- renderText({
    d <- event_data("plotly_click")
    if (is.null(d)){"Click on a point to view data"} else{ 
     if(d$curveNumber==0){typeQ<-"Funded participants (not publishing)"}else if(d$curveNumber==1){typeQ<-"Publishing data"}else{typeQ<-"Unfunded collections (not publishing)"}  
      fu <- mm[mm$type==typeQ,]
      row.names(fu) <- NULL
      fu <- fu[d$pointNumber+1,]
      paste(fu$institution,fu$collection,fu$hover,sep = "\n")
    }
  })
## Data reports output table  
  output$newdatasetReport <- DT::renderDataTable(
    DT::datatable(sumR,options = list(paging = FALSE),rownames= FALSE)
  )
## Reporting table subset  
  df_subset <- reactive({
    publ <- input$dataset
    uuidP <- sumP[sumP$publisher==publ,]$publisher_uuid
    a <- subset(newDatasetsFile, publisher_uuid == uuidP) %>% select(-publisher_uuid)
    tryCatch(
      {
        a$contact <- getFeed(fromJSON(paste0("https://search.idigbio.org/v2/search/publishers?pq={%22uuid%22:%22",uuidP,"%22}"),flatten = T)$items$data.rss_url)$header$webMaster
      },
      error=function(cond) {
        return(a$contact<-NULL)
      },
      warning=function(cond) {
        return(a$contact<-NULL)
      },
        finally={
          a
        })      
    return(a)
  })
  output$newdatasets <- DT::renderDataTable(
    DT::datatable(df_subset(),options = list(paging = FALSE),rownames= FALSE)
  )
  df_rss <- reactive({
    publ <- input$dataset
    uuidP <- sumP[sumP$publisher==publ,]$publisher_uuid
    a <- fromJSON(paste0("https://search.idigbio.org/v2/search/publishers?pq={%22uuid%22:%22",uuidP,"%22}"),flatten = T)$items$data.rss_url
    return(a)
  })
  output$datasetRSS <- renderPrint({ df_rss() })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub(" ","_",input$dataset), ".csv", sep = "")
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