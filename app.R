library(shiny)
require(visNetwork)
library(markdown)

# Define UI for app ----
ui <- fluidPage(style="padding-top: 0px;",
                visNetworkOutput("network",height="1000"),
                absolutePanel(
                  top = 20, left = 20, width = 200,
                  draggable = TRUE,
                  wellPanel(
                    selectInput(inputId="select", label = h4("Select stanza"), 
                                choices = list("Stanza 1" = 1, "Stanza 2" = 2, "Stanza 3" = 3, "Stanza 4" = 4,
                                               "Stanza 5" = 5, "Stanza 6" = 6, "Stanza 7" = 7,"Stanza 8" = 8,
                                               "Stanza 9" = 9, "Stanza 10" = 10, "Stanza 11" = 11, "Stanza 12" = 12,
                                               "Stanza 13" = 13,"Stanza 14" = 14, "Stanza 15" = 15, "Stanza 16" = 16, "Whole poem" = 0),
                                selected = 1),
                    checkboxInput(inputId="nodetype","All Nodes", value = TRUE),
                    checkboxInput(inputId="soundon","Aural Edges", value = FALSE)
                  ),
                  style = "opacity: 0.92"
                )
)


# Define server logic required to draw network
server <- function(input, output, session) {
  output$network <- renderVisNetwork({
    plot.stanza(as.numeric(input$select)-1, input$nodetype, input$soundon)
  })
}


plot.stanza <- function(n, a, s) {
  INDEX=toString(n)
  if(INDEX=="-1") {
    nodes <- read.csv("./narciso/nodes.csv", header=T, as.is=T)
    
    if(s) {
      links <- read.csv("./narciso/edges-sound.csv", header=T, as.is=T)
    } else {
      links <- read.csv("./narciso/edges.csv", header=T, as.is=T)
    }  
    
  } else {
    if(s) {
      links <- read.csv(paste(paste("./narciso/edges-sound-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T) 
      nodes <- read.csv(paste(paste("./narciso/nodes-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T)
    } else {
      if(a) {
        nodes <- read.csv("./narciso/nodes.csv", header=T, as.is=T)
      } else {
        nodes <- read.csv(paste(paste("./narciso/nodes-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T)
      }
      links <- read.csv(paste(paste("./narciso/edges-",INDEX,sep=""),".csv",sep=""), header=T, as.is=T)
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


shinyApp(ui, server)


