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
#' @param search.depth depth to which paths through trees are followed. If search.depth = NA all paths are followed to their respective terminal nodes
#' @seealso \code{\link{rf_all_paths}}
#' @export
tree_paths <- function(rf, tree, plot.network = FALSE, plot.vert.size = 0, label.vert.with.name = TRUE, circular.layout = TRUE, vertex.label.size = 0.75, arrow.size = 0.25, search.depth = NA){
  # add compatibility with ranger here:
    # if randomForest::getTree proceed
    # if ranger::treeInfo rename column of output & proceed
  rf.tree <- randomForest::getTree(rfobj = rf, k = tree, labelVar = TRUE)
  rf.tree <- data.frame(Node = 1:nrow(rf.tree), rf.tree)
  n.row <- 2*nrow(rf.tree)
  rel <- data.frame(from.node.id = numeric(length = n.row),
                    to.node.id = numeric(length = n.row),
                    depth.of.to.node = numeric(length = n.row),
                    depth.of.from.node = numeric(length = n.row),
                    split.var = factor(x = rep(NA, n.row), levels = unique(rf.tree$split.var))
                    )  
  counter <- 1
  if(is.na(search.depth)){
    for(j in 1:nrow(rf.tree)){
      if(!is.na(rf.tree[j, 'split.var'])){
        rel[counter, 'from.node.id'] <- rf.tree[j, 'Node']
        rel[counter, 'split.var']    <- as.character(rf.tree[j, 'split.var'])                        
        rel[counter, 'to.node.id'] <- rf.tree[rf.tree[j, 'left.daughter'], 'Node']
        if(j == 1){
          rel[counter, 'depth.of.to.node'] <- 1
          rel[counter, 'depth.of.from.node'] <- 0
        } else{
            rel[counter, 'depth.of.to.node'] <- rel[rel$to.node.id == rf.tree[j, 'Node'], 'depth.of.to.node'] + 1
            rel[counter, 'depth.of.from.node'] <- rel[counter, 'depth.of.to.node'] - 1
        }
        counter <- counter + 1
        rel[counter, 'from.node.id'] <- rf.tree[j, 'Node']
        rel[counter, 'split.var']    <- as.character(rf.tree[j, 'split.var'])                        
        rel[counter, 'to.node.id'] <- rf.tree[rf.tree[j, 'right.daughter'], 'Node']
        if(j == 1){
          rel[counter, 'depth.of.to.node'] <- 1
          rel[counter, 'depth.of.from.node'] <- 0
        } else{
            rel[counter, 'depth.of.to.node'] <- rel[rel$to.node.id == rf.tree[j, 'Node'], 'depth.of.to.node'] + 1
            rel[counter, 'depth.of.from.node'] <- rel[counter, 'depth.of.to.node'] - 1
        }
        counter <- counter + 1
      }
    }
    rel <- rel[!(rel$from.node.id  == 0 & rel$to.node.id == 0),]
  } else{
    # rel no longer needs to contain quite so many rows...work out the theoretical max for a given search depth and revise to save space and time  
    j <- 1
    while((max(rel$depth.of.from) <= (search.depth + 1)) & (j <= nrow(rf.tree))){
      if(!is.na(rf.tree[j, 'split.var'])){
        rel[counter, 'from.node.id'] <- rf.tree[j, 'Node']
        rel[counter, 'split.var']    <- dplyr::if_else(is.na(rf.tree[j, 'split.var']), 'Terminus', as.character(rf.tree[j, 'split.var']))                
        rel[counter, 'to.node.id'] <- rf.tree[rf.tree[j, 'left.daughter'], 'Node']

        if(j == 1){
          rel[counter, 'depth.of.to.node'] <- 1
          rel[counter, 'depth.of.from.node'] <- 0
        } else{
            rel[counter, 'depth.of.to.node'] <- rel[rel$to.node.id == rf.tree[j, 'Node'], 'depth.of.to.node'] + 1
            rel[counter, 'depth.of.from.node'] <- rel[counter, 'depth.of.to.node'] - 1
        }
        counter <- counter + 1
        rel[counter, 'from.node.id'] <- rf.tree[j, 'Node']
        rel[counter, 'to.node.id'] <- rf.tree[rf.tree[j, 'right.daughter'], 'Node']
        if(j == 1){
          rel[counter, 'depth.of.to.node'] <- 1
          rel[counter, 'depth.of.from.node'] <- 0
        } else{
            rel[counter, 'depth.of.to.node'] <- rel[rel$to.node.id == rf.tree[j, 'Node'], 'depth.of.to.node'] + 1
            rel[counter, 'depth.of.from.node'] <- rel[counter, 'depth.of.to.node'] - 1
        }
        counter <- counter + 1
      }
      j <- j + 1  
    }
   rel <- rel[!(rel$from.node.id  == 0 & rel$to.node.id == 0) & (0 < rel$depth.of.to.node) & (rel$depth.of.to.node <= search.depth), ]    
  }
  rf.tree.trim <- rf.tree[rf.tree$Node <= max(rel$to.node.id), ]
  rf.tree.trim$label <- dplyr::if_else(is.na(rf.tree.trim$split.var), 'Terminus', as.character(rf.tree.trim$split.var))
  rf.tree.trim <- dplyr::select(rel, to.node.id, depth.of.to.node) %>%
    dplyr::rename(Node = to.node.id, Depth = depth.of.to.node) %>%
      dplyr::group_by(Node) %>%
        dplyr::summarise(Depth = unique(Depth)) %>%
         dplyr::right_join(x = ., y = rf.tree.trim, by = 'Node')
  rel.ig <- igraph::graph_from_data_frame(d = rel, vertices = rf.tree.trim[,c('Node', 'label', 'Depth')])
  if(plot.network == TRUE){
      if(label.vert.with.name == TRUE){
        igraph::plot.igraph(rel.ig, layout = igraph::layout_as_tree(graph = rel.ig, circular = circular.layout), vertex.size = plot.vert.size, vertex.label = igraph::V(rel.ig)$label, vertex.label.cex = vertex.label.size, vertex.label.color = 'black', edge.arrow.size = arrow.size)} else{
        igraph::plot.igraph(rel.ig, layout = igraph::layout_as_tree(graph = rel.ig, circular = circular.layout), vertex.size = plot.vert.size, vertex.label = igraph::V(rel.ig)$name, vertex.label.cex = vertex.label.size, vertex.label.color = 'black', edge.arrow.size = arrow.size)
      }
  }
  if(is.na(search.depth)){
    paths.ls <- igraph::all_simple_paths(graph = rel.ig, from = 1, to = which(igraph::V(rel.ig)$label == 'Terminus'))
  } else{    
    paths.ls <- igraph::all_simple_paths(graph = rel.ig, from = 1, to = which(igraph::V(rel.ig)$Depth == search.depth))    
  }  
  namer <- function(x){
      igraph::vertex_attr(graph = rel.ig, name = 'label', index = x)
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
