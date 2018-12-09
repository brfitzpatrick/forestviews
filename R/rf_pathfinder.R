#' Find all possible paths through a random forest
#' 
#' \code{rf_pathfinder} is a high level function to find all paths through a random forest.  This function returns a dataframe that can be supplied to \code{\link{rf_parcoor}} or \code{\link{rf_sankey}} to produce plots of these paths. Please see the help pages for \code{\link{rf_parcoor}} or \code{\link{rf_sankey}} for examples of workflows flow for visualising the paths through random forests with these functions.
#' @param rf An object of class \code{randomForest} returned by \code{\link[randomForest]{randomForest}}
#' @param search.depth the maximum number of successive nodes to pass when following paths from the root node along branchs to determine the paths through the trees of the random forest
#' @seealso \code{\link{rf_parcoor}} and \code{\link{rf_sankey}}
#' @export
rf_pathfinder <- function(rf, report.progress = TRUE, search.depth = NA){
  paths <- rf_all_paths(rf = rf, report.progress = report.progress, search.depth = search.depth)
  if(report.progress == TRUE){
    print('aggregating paths through forest')
  }
  paths.agg <- aggregate_paths(data = paths, rf = rf)
  return(paths.agg)
}
