#' Aggregate the paths through a random forest
#' 
#' \code{\link{aggregate_paths}} is a low level function called by \code{\link{rf_pathfinder}} that aggregates all the paths through a random forest ready for visualisation as a parallel coorindate plot with \code{\link{rf_parcoor}} or a Sankey diagram with \code{\link{rf_sankey}}.
#' @param data the output of \code{\link{rf_all_paths}}.
#' @param rf an object of class \code{randomForest} retuned by \code{\link[randomForest]{randomForest}}. This is the random forest through which the paths described in \code{data} were calculated i.e. that which was supplied to \code{\link{rf_all_paths}}.
#' @export
aggregate_paths <- function(data, rf){
  drop.these <- which(colSums(data == 'Terminus') == nrow(data))
  if(length(drop.these) < 2){
    data.2 <- data
  } 
  if(length(drop.these) == 2){
    data.2 <- data[,-drop.these[2]]
  } 
  if(length(drop.these) > 2){
    drop.these <- drop.these[2:length(drop.these)]
    data.2 <- data[,-drop.these]
  } 
  d2cn <- colnames(data.2)
  combs <- cbind(paste('X',1:(length(d2cn)-1),sep = ''), paste('X',2:length(d2cn),sep = ''))
  Seg.G.ls <- vector(mode = 'list', length = nrow(combs))
  leng <- vector(mode = 'numeric', length(Seg.G.ls))
  for(i in 1:length(Seg.G.ls)){
    data.2 %>%
      dplyr::group_by_(.dots = paste(combs[i,])) %>%
        dplyr::summarise(Count = n()) -> data.i
    leng[i] <- nrow(data.i)
    if(i == 1){
      if(leng[i] > 1){
        data.i$Seg.ID <- 1:leng[i]
      } else{
          data.i$Seg.ID <- 1
      }
    } else{
      if(leng[i] > 1){
        data.i$Seg.ID <- (sum(leng[1:(i-1)]) + 1):(sum(leng[1:(i-1)]) + leng[i])
      } else{
          data.i$Seg.ID <- sum(leng[1:(i-1)]) + leng[i]
      }
    }
    data.i.G <- tidyr::gather(data = data.i, key = 'Axis.Number', value = 'Node.ID', -Count, -Seg.ID)
    Seg.G.ls[[i]] <- data.i.G
  }
  Seg.G <- Reduce(f = dplyr::bind_rows, x = Seg.G.ls)
  Seg.G.2 <- dplyr::arrange(Seg.G, Count)
  return(Seg.G.2)
}
