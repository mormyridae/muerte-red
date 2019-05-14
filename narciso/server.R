library(shiny)
library(visNetwork)

plot.stanza <- function(n, a, s) {
    INDEX=toString(n)
    if(INDEX=="-1") {
        nodes <- read.csv("nodes.csv", header=T, as.is=T)
    links <- read.csv("edges.csv", header=T, as.is=T)
        
    } else {
      if(s) {
        links <- read.csv(paste(paste("edges-sound-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T) 
        nodes <- read.csv("nodes-2.csv", header=T, as.is=T)
      } else {
          if(a) {
          nodes <- read.csv("nodes.csv", header=T, as.is=T)
        } else {
          nodes <- read.csv(paste(paste("nodes-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T)
        }
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
    vis.nodes$label.cex  <- 50 # Node label
    vis.nodes$title  <- vis.nodes$weight
    vis.nodes$size   <- 15+(60*log10((vis.nodes$weight-1)/20*2+1))# Node size
    vis.nodes$borderWidth <- .5 # Node border width
    
    #vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$id]
    vis.nodes$color.border <- "black"
    vis.nodes$color.highlight.background <- "orange"
    vis.nodes$color.highlight.border <- "red"
    
    #edge properties
    #vis.links$width <- 1+links$weight/8 # line width
    vis.links$color <- c("navy","navy","navy", "orange")[links$type] # line color  
    vis.links$edge.arrow.mode <- c(2,2,2,3)[links$type]
    vis.links$arrows <- "middle"
    vis.links$smooth <- TRUE    # should the edges be curved?
    vis.links$shadow <- FALSE    # edge shadow
    vis.links$dashes <- c(FALSE, TRUE, TRUE, TRUE)[links$type]
    if(INDEX=="-1") {
        visnet <- visNetwork(vis.nodes, vis.links, width="100%", height="1100px") %>% visPhysics(stabilization = FALSE)
    } else {
        visnet <- visNetwork(vis.nodes, vis.links, width="100%", height="1000px") %>% visPhysics(stabilization = FALSE)
    }
    return(visnet)
   
}

function(input, output, session) {
  output$network <- renderVisNetwork({
    plot.stanza(as.numeric(input$select)-1, input$nodetype, input$soundon)
  })
}
