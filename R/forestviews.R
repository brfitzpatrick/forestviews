#' forestviews: visualise the paths through random forests.
#'
#' Creates visualisations of all paths through the binary trees that constitute a random forest.  Currently two types of visualisations are produced: parallel coordinates plots and Sankey diagrams. To create these visualisations supply the output of \code{\link[randomForest]{randomForest}} to \code{\link{rf_pathfinder}} then supply the output of \code{\link{rf_pathfinder}} to either \code{\link{rf_parcoor}} to produce a parallel coordinates style plot or \code{\link{rf_sankey}} to produce the network for a Sankey diagram.  To display the Sankey diagram you need to supply the output of \code{\link{rf_sankey}} to the \code{\link[networkD3]{sankeyNetwork}} function from the \code{\link[networkD3]{networkD3}} package.  The examples on the help page for \code{\link{rf_sankey}} demonstate this process. A preprint of the manuscript that introduces the visualisation methods this package implements may be obtained from arXiv at \url{https://arxiv.org/abs/1706.08702}. To cite \code{forestviews} in publications please use the citation for the associated article returned by \code{citation('forestviews')}.
#' 
#' @docType package
#' @name forestviews
#' @importFrom magrittr %>%
NULL
## NULL
