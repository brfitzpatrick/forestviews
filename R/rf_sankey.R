#' Sankey Diagram of paths through a random forest
#'
#' To produce a Sankey diagram of the paths through a random forest \code{rf_sankey} accepts the output of \code{\link{rf_pathfinder}} and inturn outputs the \code{d3network} object necessary to produce a Sankey diagram with \code{\link[networkD3]{sankeyNetwork}}. Examples of how to produce a Sankey diagram are given in the Examples section. The Sankey diagram will display in a web browser and may be interacted with via the mouse. See \url{https://arxiv.org/abs/xxxx.xxxxx} for an explanation of how to interpret these diagrams.
#' 
#' @param all.paths.out the output of \code{\link{rf_pathfinder}}
#' @param all.nodes If \code{TRUE} the network contains all nodes on all paths through the random forest. If \code{FALSE} the network contains the first \code{plot.node.lim} nodes of all paths through the network
#' @param plot.node.lim this number specifies the maximum rank of nodes to plot. The plot will always start with the root nodes of trees on the far left and proceed to the right with nodes of successively larger ranks until nodes of this rank are reached. This argument only has an effect is \code{all.nodes = FALSE}.
#' @seealso \code{\link{rf_pathfinder}}, \code{\link[networkD3]{sankeyNetwork}}
#' @examples
#' # example 1:
#' library(mlbench)
#' data(Satellite)
#' library(randomForest)
#' library(networkD3)
#' rf.1 <- randomForest(classes ~ ., data = Satellite, mtry = 8, keep.forest = TRUE, ntree = 25, importance = TRUE)
#' rf.1.all.paths <- rf_pathfinder(rf = rf.1)
#' nd3 <- rf_sankey(all.paths.out = rf.1.all.paths, all.nodes = FALSE, plot.node.lim = 6)
#' sankeyNetwork(Links = nd3$links, Nodes = nd3$nodes , Source = 'source', Target = 'target', Value = 'value', NodeID = 'name', units = 'Count', fontSize = 12, nodeWidth = 30, NodeGroup = NULL)
#'
#' # example 2:
#' data(iris)
#' library(randomForest)
#' library(networkD3)
#' rf.2 <- randomForest(Species ~ ., data = iris, mtry = 2, keep.forest = TRUE, ntree = 100, importance = TRUE)
#' rf.2.all.paths <- rf_pathfinder(rf = rf.2)
#' nd3.2 <- rf_sankey(all.paths.out = rf.2.all.paths, all.nodes = TRUE)
#' sankeyNetwork(Links = nd3.2$links, Nodes = nd3.2$nodes , Source = 'source', Target = 'target', Value = 'value', NodeID = 'name', units = 'Count', fontSize = 12, nodeWidth = 30, NodeGroup = NULL)
#' # colour by covariate identity (only a good idea with less than 20 covariates)
#' nd3.2$links$nd3.2_type <- sub(' .*', '', nd3.2$nodes[nd3.2$links$source + 1, 'group'])
#' # while we only need five colours for this example see below for how to use predefined d3js categorical colour scales of various sizes:
#' # d3js: 10 categorical colour scale:
#' sankeyNetwork(Links = nd3.2$links, Nodes = nd3.2$nodes, Source = 'source', Target = 'target', Value = 'value', NodeID = 'name', LinkGroup = 'nd3.2_type', NodeGroup = 'group', colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"))
#' # d3js: 20 categorical colour scale (a):
#' sankeyNetwork(Links = nd3.2$links, Nodes = nd3.2$nodes, Source = 'source', Target = 'target', Value = 'value', NodeID = 'name', LinkGroup = 'nd3.2_type', NodeGroup = 'group', colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"))
#' # d3js: 20 categorical colour scale (b):
#' sankeyNetwork(Links = nd3.2$links, Nodes = nd3.2$nodes, Source = 'source', Target = 'target', Value = 'value', NodeID = 'name', LinkGroup = 'nd3.2_type', NodeGroup = 'group', colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20b);"))
#' # d3js: 20 categorical colour scale (c):
#' sankeyNetwork(Links = nd3.2$links, Nodes = nd3.2$nodes, Source = 'source', Target = 'target', Value = 'value', NodeID = 'name', LinkGroup = 'nd3.2_type', NodeGroup = 'group', colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20c);"))
#' 
#' @export
rf_sankey <- function(all.paths.out, all.nodes = FALSE, plot.node.lim = 6){
  x2n <- function(x){
    gsub(x = x, pattern = 'X', replacement = 'Node.')
  }
  n.a.c <- purrr::map_chr(.x = strsplit(x = c(unique(all.paths.out$Axis.Number)), split = 'X'), 2)
  max.h.grad <- max(as.numeric(n.a.c))
  all.paths.out$Axis.Number.F <- factor(x = all.paths.out$Axis.Number, levels = paste('X', 1:max.h.grad, sep = ''), ordered = TRUE)    
  all.paths.out %>%
    dplyr::select(Count, Seg.ID, Axis.Number.F, Node.ID) -> Plot.tb  
  Plot.tb %>%
    dplyr::group_by(Seg.ID) %>%
      dplyr::arrange(Axis.Number.F) %>%
        dplyr::mutate(Pairs = c('First', 'Second')) %>%
          dplyr::ungroup() %>%
            dplyr::arrange(Seg.ID) -> Plot.tb.2
  Plot.tb.2 %>%
    dplyr::filter(Pairs == 'First') %>%
      dplyr::select(Count, Seg.ID, First.Axis.Number.F = Axis.Number.F, First.Node.ID = Node.ID) -> First.all.tb
  Plot.tb.2 %>%
    dplyr::filter(Pairs == 'Second') %>%
      dplyr::select(Count, Seg.ID, Second.Axis.Number.F = Axis.Number.F, Second.Node.ID = Node.ID) -> Second.all.tb
  All.tb <- dplyr::left_join(x = First.all.tb, y = Second.all.tb, by = c('Seg.ID', 'Count'))
  if(all.nodes == TRUE){
    All.tb %>%
      dplyr::mutate(First.Axis.Number.N  = forcats::fct_relabel(f = First.Axis.Number.F, fun = x2n),
                    Second.Axis.Number.N = forcats::fct_relabel(f = Second.Axis.Number.F, fun = x2n)) %>%
        dplyr::select(Count, First.Axis.Number.N, Second.Axis.Number.N, First.Node.ID, Second.Node.ID) %>%
          tidyr::unite(col = N1, First.Axis.Number.N, First.Node.ID, remove = FALSE) %>%
          tidyr::unite(col = N2, Second.Axis.Number.N, Second.Node.ID, remove = FALSE) %>%
            dplyr::select(N1, N2, Count, First.Node.ID, Second.Node.ID) -> Edges.T
  } else{
      All.tb %>%
        dplyr::mutate(First.Axis.Number.N  = forcats::fct_relabel(f = First.Axis.Number.F, fun = x2n),
                      Second.Axis.Number.N = forcats::fct_relabel(f = Second.Axis.Number.F, fun = x2n)) %>%
          dplyr::select(Count, First.Axis.Number.N, Second.Axis.Number.N, First.Node.ID, Second.Node.ID) %>%
            dplyr::filter(Second.Axis.Number.N %in% paste('Node',1:plot.node.lim,sep='.')) %>%
              tidyr::unite(col = N1, First.Axis.Number.N, First.Node.ID, remove = FALSE) %>%
                tidyr::unite(col = N2, Second.Axis.Number.N, Second.Node.ID, remove = FALSE) %>%
                  dplyr::select(N1, N2, Count, First.Node.ID, Second.Node.ID) -> Edges.T
  }
  Net <- igraph::graph_from_data_frame(d = dplyr::select(.data = Edges.T, N1, N2, Count))  
  Vert.Cov <- igraph::V(Net)$name
  Vert.Cov %>%
    strsplit(x = ., split = '_') %>%
      tibble::tibble(Node = Vert.Cov,
             Axis = purrr::map_chr(., 1),
             Covar = purrr::map_chr(.,2)) -> Vert.Cov.tb
  igraph::vertex_attr(graph = Net, name = 'Covar') <- Vert.Cov.tb$Covar
  nd3 <- networkD3::igraph_to_networkD3(g = Net, group = Vert.Cov.tb$Covar)
  return(nd3)
}
