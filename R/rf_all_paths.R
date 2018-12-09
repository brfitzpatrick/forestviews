#' All paths through a random forest
#'
#' \code{rf_all_paths} is a low level function called by \code{\link{rf_pathfinder}} to determine all paths through all trees in a random forest.
#'
#' @param rf An object of class \code{randomForest} returned by \code{\link[randomForest]{randomForest}}
#' @param parallel Logical: should the paths through the trees of the random forest be explored using parallel computing? TRUE/FALSE. Note you will need to create and register a cluster before calling this function to have it search trees in parallel with this option. See the Examples.
#' @export
rf_all_paths <- function(rf, parallel = parallel){ 
    n.trees <- rf$ntree
    if(parallel == TRUE){
      paths <- foreach::foreach(i = 1:n.trees, .combine = dplyr::bind_rows) %dopar% {
      tree_paths(rf = rf, tree = i, plot.network = FALSE)
      }
    } else{
        paths.ls <- vector(mode = 'list', length = n.trees)
        for(i in 1:n.trees){
          paths.ls[[i]] <- tree_paths(rf = rf, tree = i, plot.network = FALSE)
        }
        paths <- Reduce(f = dplyr::bind_rows, x = paths.ls) # would purrr::reduce be better here?
    }
    paths[is.na(paths)] <- 'Terminus' 
    return(paths)
}
