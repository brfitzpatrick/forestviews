#' All paths through a single tree from a random forest  
#'
#' \code{tree_paths} is a low level function called by \code{\link{rf_all_paths}} to find all paths from root to leaf nodes through a single tree extracted from a random forest.
#' 
#' @param rf an object of class \code{randomForest} returned by \code{\link[randomForest]{randomForest}}
#' @param tree a number indexing the tree constituent to \code{rf} which will be extracted and converted to an \code{igraph} network
#' @param plot.network if \code{TRUE} the network is plotted. If \code{FALSE} the network is not plotted
#' @param plot.vert.size a number setting the size of the circles which represent the nodes in the plot of the network
#' @param label.vert.with.name if \code{TRUE} each vertex (node) is labeled with the covariate that defined the binary partition represented by that vertex. If \code{FALSE} each vertex is labeled with the name assigned to it by \code{igraph}
#' @param circular.layout if \code{TRUE} the network is plotted with the circular layout from the igraph package. If \code{FALSE} the network is plotted with the default layout from the \code{igraph} package
#' @param vertex.label.size a number controlling the size of the text used for the vertex labels in the plot
#' @param edge.arrow.size a number controlling the size of arrows that represent the graph edges in the plot
#' @seealso \code{\link{rf_all_paths}}
#' @export
tree_paths <- function(rf, tree, plot.network = FALSE, plot.vert.size = 0, label.vert.with.name = TRUE, circular.layout = TRUE, vertex.label.size = 0.75, arrow.size = 0.25){
  rf.tree <- randomForest::getTree(rfobj = rf, k = tree, labelVar = TRUE)
  rf.tree <- data.frame(Node = 1:nrow(rf.tree), rf.tree)
  n.row <- 2*nrow(rf.tree)
  rel <- data.frame(from.node.id = numeric(length = n.row),
                    to.node.id = numeric(length = n.row))
  counter <- 1
  for(j in 1:nrow(rf.tree)){
    if(!is.na(rf.tree[j, 'split.var'])){
      rel[counter, 'from.node.id'] <- rf.tree[j, 'Node']
      rel[counter, 'to.node.id'] <- rf.tree[rf.tree[j, 'left.daughter'], 'Node']
      counter <- counter + 1
      rel[counter, 'from.node.id'] <- rf.tree[j, 'Node']
      rel[counter, 'to.node.id'] <- rf.tree[rf.tree[j, 'right.daughter'], 'Node']
      counter <- counter + 1
    }
  }  
  rel <- rel[!(rel$from.node.id  == 0 & rel$to.node.id == 0),]
  rel.ig <- igraph::graph_from_data_frame(d = rel)
  rf.tree$Node.Label <- factor(x = as.character(rf.tree$split.var), levels = c(levels(rf.tree$split.var), 'Terminus'))
  rf.tree[is.na(rf.tree$split.var),'Node.Label'] <- 'Terminus'
  rel.ig.l2 <- igraph::set_vertex_attr(graph = rel.ig, name = 'label', index = igraph::V(rel.ig)$name, value = as.character(rf.tree$Node.Label[as.numeric(igraph::V(rel.ig)$name)]))
  if(plot.network == TRUE){
      if(label.vert.with.name == TRUE){
        igraph::plot.igraph(rel.ig.l2, layout = igraph::layout_as_tree(graph = rel.ig.l2, circular = circular.layout), vertex.size = plot.vert.size, vertex.label = igraph::V(rel.ig.l2)$label, vertex.label.cex = vertex.label.size, vertex.label.color = 'black', edge.arrow.size = arrow.size)} else{
        igraph::plot.igraph(rel.ig.l2, layout = igraph::layout_as_tree(graph = rel.ig.l2, circular = circular.layout), vertex.size = plot.vert.size, vertex.label = igraph::V(rel.ig.l2)$name, vertex.label.cex = vertex.label.size, vertex.label.color = 'black', edge.arrow.size = arrow.size)
      }
  }  
  paths.ls <- igraph::all_simple_paths(graph = rel.ig.l2, from = 1, to = which(igraph::V(rel.ig.l2)$label == 'Terminus'))
  namer <- function(x){
      igraph::vertex_attr(graph = rel.ig.l2, name = 'label', index = x)
  }
  paths.ls.covars <- purrr::map(.x = paths.ls, .f = namer)
  za <- function(x, l.max){
    out <- c(x, rep(x = x[length(x)], l.max - length(x))) 
    out <- as.list(out)
    names(out) <- paste('X',1:l.max, sep = '')
    return(out)
  }
  purrr::map_int(.x = paths.ls, .f = length) %>%
    max() -> max.leng
  paths.ls.covars.2 <- purrr::map(.x = paths.ls.covars, .f = za, max.leng)
  paths.df <- purrr::map_df(.x = paths.ls.covars.2, .f = magrittr::extract, paste('X',1:max.leng, sep = ''))
  return(paths.df)
}
