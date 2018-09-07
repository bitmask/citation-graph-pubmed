require(plyr)
require(XML)
library(xml2)
require(RCurl)
require(readxl)
require(visNetwork)


# original by https://github.com/guillaumelobet/citation-graph-pubmed/blob/master/citation_graph_pubmed.R



# We start with a list PubMed ids, for which we would like to have the citation graph
pmids <- c(
    16159925,
    17646311,
    17937790,
    18227117,
    18718939,
    24195708,
    19180177,
    20651255,
    26430702
)

get_cited_articles <- function(pmid) {
    tryCatch({
        path <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_refs&id=", pmid, sep="")
        f <- file(path)
        data <- readLines(f, warn=F)
        close(f)
    }, warning = function(w) {
      message(w)
    }, error = function(e) {
      message(e)
    })
    cited_mess <- xmlToList(xmlParse(data, asText=T))$LinkSet$LinkSetDb
    cited <- c()
    for (x in 3:length(cited_mess)) { 
        cited[[x]] <- cited_mess[x]$Link$Id
    }
    return(cited)
}

get_citing_articles <- function(pmid) {
# We build the URL to retrieve the citing pmids
    tryCatch({
        path <- paste("https://www.ncbi.nlm.nih.gov/pubmed?linkname=pubmed_pubmed_citedin&from_uid=",
                      pmid,
                      "&report=uilist&format=text&dispmax=200", sep="")
        f <- file(path)
        data <- readLines(f, warn = F)
        close(f)
    }, warning = function(w) {
      message(w)
    }, error = function(e) {
      message(e)
    })
    citing <-strsplit(xmlToList(xmlParse(data, asText = T))[1], "\n")[[1]]
    return(citing)
}

get_metadata <- function(pmid) {
    tryCatch({
        path <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&retmode=xml&id=", pmid, sep="")
        f <- file(path)
        data <- readLines(f, warn=F)
        close(f)
    }, warning = function(w) {
      message(w)
    }, error = function(e) {
      message(e)
    })
    first_author <- ""
    year <- ""
    title <- ""
    x <- read_xml(paste(data, collapse="")) # wtf xml?
    for (child in xml_children(xml_children(x))) {
        if (xml_has_attr(child, "Name") & xml_attr(child, "Name") == "AuthorList") {
            authors <- xml_children(child)
            if (xml_has_attr(authors[1], "Name")) {
                first_author = xml_text(authors[1])
            }
        }
        if (xml_has_attr(child, "Name") & xml_attr(child, "Name") == "PubDate") {
            year <- xml_text(child) #TODO: remove month, day
        }
        if (xml_has_attr(child, "Name") & xml_attr(child, "Name") == "Title") {
            title <- xml_text(child)
        }
    }
    return(data.frame(id=as.numeric(pmid), pmid=pmid, author=first_author, year=year, mstitle=title))
}

'%nin%' <- Negate('%in%')

edges <- NULL
nodes <- NULL
for(pmid in pmids){
    citing <- get_citing_articles(pmid)
    cited <- get_cited_articles(pmid)
    if (pmid %nin% nodes$pmid) {
        metadata <- get_metadata(pmid)
        nodes <- rbind(nodes, metadata)
    }

    for(pm in unique(citing)){
        edges <- rbind(edges, data.frame(from=as.numeric(pm), to=as.numeric(pmid), arrows="to", dashes=TRUE))
        if (pm %nin% nodes$pmid) {
            metadata <- get_metadata(pm) # TODO: bulk call to efetch?
            nodes <- rbind(nodes, metadata)
        }
    }
    for (pm in unique(cited)) {
        edges <- rbind(edges, data.frame(from=as.numeric(pmid), to=as.numeric(pm), arrows="to", dashes=FALSE))
        if (pm %nin% nodes$pmid) {
            metadata <- get_metadata(pm)
            nodes <- rbind(nodes, metadata)
        }
    }
}


# We use visNetwork for the vizualisation of the network
# For that we need a nodes and edges tables. 

nodes$color <- "lightblue"
nodes[nodes$pmid %in% pmids,]$color <- "red"
nodes$title <- nodes$pmid
nodes$label <- paste(nodes$author, nodes$year)

visNetwork(nodes, edges, width = "100%")
