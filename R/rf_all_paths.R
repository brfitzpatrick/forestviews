#' All paths through a random forest
#'
#' \code{rf_all_paths} is a low level function called by \code{\link{rf_pathfinder}} to determine all paths through all trees in a random forest.
#'
#' @param rf An object of class \code{randomForest} returned by \code{\link[randomForest]{randomForest}}
#' @param report.progress if \code{TRUE} the percentage of trees for which all paths have been found is printed to the console as the function runs, if \code{FALSE} no such progress report is printed
#' @export
rf_all_paths <- function(rf, report.progress = TRUE){ 
    n.trees <- rf$ntree
    paths.ls <- vector(mode = 'list', length = n.trees)
    if(report.progress == TRUE){
      for(i in 1:n.trees){
        paths.ls[[i]] <- tree_paths(rf = rf, tree = i, plot.network = FALSE)
        print(paste(round(100*i/n.trees, digits = 4), '% of forest explored'))
      }
      print('combining dataframes')
    } else{  
       # for(i in 1:n.trees){
       #   paths.ls[[i]] <- rf_to_igraph(rf = rf, tree = i, plot.network = FALSE) 
       # }
      forestsize <- 1:n.trees
      paths.ls <- lapply(forestsize, function(temp){tree_paths(rf = rf, tree = temp, plot.network = FALSE)})
    }
    paths <- Reduce(f = dplyr::bind_rows, x = paths.ls)
    paths[is.na(paths)] <- 'Terminus'
    return(paths)
}
