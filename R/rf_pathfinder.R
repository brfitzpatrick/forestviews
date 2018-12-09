#' Find all possible paths through a random forest
#' 
#' \code{rf_pathfinder} is a high level function to find all paths through a random forest.  This function returns a dataframe that can be supplied to \code{\link{rf_parcoor}} or \code{\link{rf_sankey}} to produce plots of these paths. Please see the help pages for \code{\link{rf_parcoor}} or \code{\link{rf_sankey}} for examples of workflows flow for visualising the paths through random forests with these functions.
#' @param rf An object of class \code{randomForest} returned by \code{\link[randomForest]{randomForest}}
#' @param parallel Logical: should the paths through the trees of the random forest be explored using parallel computing? TRUE/FALSE. Note you will need to create and register a cluster before calling this function to have it search trees in parallel with this option. See the Examples.
#' @seealso \code{\link{rf_parcoor}} and \code{\link{rf_sankey}}
#' @examples
#' # example 1:
#' library(mlbench)
#' data(Satellite)
#' library(randomForest)
#' library(networkD3)
#' rf.1 <- randomForest(classes ~ ., data = Satellite, mtry = 8, keep.forest = TRUE, ntree = 1e3)
#' library(doMC)
#' library(parallel)
#' makeCluster(spec = 6)
#' registerDoMC(cores = 6)
#' rf.1.all.paths <- rf_pathfinder(rf = rf.1, parallel = TRUE)
#' 
#' @export
rf_pathfinder <- function(rf, parallel = FALSE){
  paths <- rf_all_paths(rf = rf, parallel = parallel)
  paths.agg <- aggregate_paths(data = paths, rf = rf)
  return(paths.agg)
}
