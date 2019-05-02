library(shiny)
library(visNetwork)

plot.stanza <- function(n, a) {
    INDEX=toString(n)
    if(INDEX=="-1") {
        nodes <- read.csv("nodes.csv", header=T, as.is=T)
    links <- read.csv("edges.csv", header=T, as.is=T)
        
    } else {
        if(a) {
          nodes <- read.csv("nodes.csv", header=T, as.is=T)
          links <- read.csv(paste(paste("edges-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T)    
        } else {
          nodes <- read.csv(paste(paste("nodes-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T)
          links <- read.csv(paste(paste("edges-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T)        
          }
        
    }
    
    library("igraph")
    net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
    library("visNetwork") 
    
    vis.nodes <- nodes
    vis.links <- links
    
    vis.nodes$shape  <- "dot"  
    vis.nodes$shadow <- FALSE # Nodes will drop shadow
    #vis.nodes$title  <- vis.nodes$media # Text on click
    vis.nodes$label  <- vis.nodes$id # Node label
    vis.nodes$title  <- vis.nodes$weight
    vis.nodes$size   <- 10+vis.nodes$weight*4# Node size
    vis.nodes$borderWidth <- .5 # Node border width
    
    #vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$id]
    vis.nodes$color.border <- "black"
    vis.nodes$color.highlight.background <- "orange"
    vis.nodes$color.highlight.border <- "red"
    
    #edge properties
    #vis.links$width <- 1+links$weight/8 # line width
    vis.links$color <- c("navy","navy","navy")[links$type] # line color  
    vis.links$arrows <- "middle"
    vis.links$smooth <- TRUE    # should the edges be curved?
    vis.links$shadow <- FALSE    # edge shadow
    vis.links$dashes <- c(FALSE, TRUE, TRUE)[links$type]
    if(INDEX=="-1") {
        visnet <- visNetwork(vis.nodes, vis.links, width="100%", height="1100px") %>% visPhysics(stabilization = FALSE)
    } else {
        visnet <- visNetwork(vis.nodes, vis.links, width="100%", height="1000px") %>% visPhysics(stabilization = FALSE)
    }
    return(visnet)
   
}

function(input, output, session) {
  output$network <- renderVisNetwork({
    # minimal example
    plot.stanza(as.numeric(input$select)-1, input$nodetype)
  })
}